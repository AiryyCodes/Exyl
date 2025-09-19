use std::cell::RefCell;
use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue,
    PointerValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use crate::ast::{BinaryOp, Expr, ExylLLVMType, Program, Stmt, Type, UnaryOp};
use crate::scope::ScopeStack;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,

    pub scopes: RefCell<ScopeStack<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)>>,
    pub functions: RefCell<HashMap<String, (FunctionValue<'ctx>, ExylLLVMType<'ctx>)>>,

    pub current_func_has_return: bool,
    pub current_func_return_type: Type,

    pub inside_function: bool,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        let scopes = RefCell::new(ScopeStack::new());
        let functions = RefCell::new(HashMap::new());

        Self {
            context,
            module,
            builder,
            scopes,
            functions,
            current_func_has_return: false,
            current_func_return_type: Type::Void,
            inside_function: false,
        }
    }

    pub fn codegen_program(&mut self, ast: &Program) -> FunctionValue<'ctx> {
        let i64_type = self.context.i64_type();
        let i32_type = self.context.i32_type();
        let i8_ptr_ptr = self.context.ptr_type(AddressSpace::default());

        // Predeclare all functions (extern and non-extern) so calls can resolve
        for stmt in &ast.body {
            if let Stmt::Func {
                name,
                return_type,
                inferred_return,
                arguments,
                is_extern,
                is_variadic,
                ..
            } = stmt
            {
                let mut name = name.to_string();
                if name == "main" {
                    name = "exyl_main".to_string();
                }
                // Build function type
                let param_types: Vec<Type> = arguments.iter().map(|(_, ty)| ty.clone()).collect();
                let fn_type = self.llvm_function_type(
                    return_type.as_ref().unwrap_or(&Type::Void),
                    &param_types,
                    *is_variadic,
                );
                // Add if not exists
                let function = if let Some(f) = self.module.get_function(&name) {
                    f
                } else {
                    let f = self.module.add_function(&name, fn_type, None);
                    if *is_extern {
                        f.set_linkage(Linkage::External);
                    }
                    f
                };
                // Register in functions map
                self.functions.borrow_mut().insert(
                    name.clone(),
                    (
                        function,
                        self.llvm_type(return_type.as_ref().unwrap_or(inferred_return)),
                    ),
                );
            }
        }

        // Generate all top-level functions (bodies) and find user main
        let mut user_main: Option<FunctionValue> = None;
        for stmt in &ast.body {
            if let Stmt::Func { name, .. } = stmt {
                if let Some(func_val) = self.codegen_stmt(stmt) {
                    if name == "main" {
                        user_main = Some(func_val);
                    }
                }
            }
        }

        // --- Generate OS entry point main ---
        let main_type = i32_type.fn_type(&[i32_type.into(), i8_ptr_ptr.into()], false);
        let main_fn = self.module.add_function("main", main_type, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Declare runtime init
        let runtime_init_fn = self.module.add_function(
            "exyl_runtime_init",
            self.context
                .void_type()
                .fn_type(&[i32_type.into(), i8_ptr_ptr.into()], false),
            None,
        );

        // Call exyl_runtime_init(argc, argv)
        let argc_val = main_fn.get_nth_param(0).unwrap().into_int_value();
        let argv_val = main_fn.get_nth_param(1).unwrap().into_pointer_value();
        let _ = self
            .builder
            .build_call(runtime_init_fn, &[argc_val.into(), argv_val.into()], "");

        // Call user main if it exists
        let user_ret = if let Some(main_func) = user_main {
            let ret_val = self
                .builder
                .build_call(main_func, &[], "user_main_ret")
                .unwrap()
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_int_value();
            ret_val
        } else {
            // Auto-generate main to run global statements
            for stmt in &ast.body {
                match stmt {
                    Stmt::Func { .. } => {} // already generated
                    _ => {
                        self.codegen_stmt(stmt);
                    }
                }
            }
            i64_type.const_zero()
        };

        // Truncate i64 return to i32 for OS
        let ret32 = self
            .builder
            .build_int_truncate(user_ret, i32_type, "ret32")
            .unwrap();
        let _ = self.builder.build_return(Some(&ret32));

        main_fn
    }

    pub fn codegen_stmt(&mut self, stmt: &Stmt) -> Option<FunctionValue<'ctx>> {
        match stmt {
            Stmt::Expr(expr) => {
                self.codegen_expr(expr);
                None
            }
            Stmt::Let { name, ty, value } => {
                self.codegen_let(name, ty.as_ref(), value);
                None
            }
            Stmt::Func {
                name,
                return_type,
                inferred_return,
                arguments,
                body,
                is_extern,
                is_variadic,
            } => self.codegen_function(
                name,
                return_type.as_ref(),
                inferred_return,
                arguments,
                body.as_ref(),
                *is_extern,
                *is_variadic,
            ),
            Stmt::Return(ret_opt) => self.codegen_return(ret_opt),
            Stmt::Block(stmts) => {
                self.codegen_block(stmts);
                None
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.codegen_if(condition, then_branch, else_branch.as_deref());
                None
            }
        }
    }

    pub fn codegen_expr(&self, expr: &Expr) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            Expr::NumberInt(n) => self.context.i64_type().const_int(*n as u64, false).into(),
            Expr::NumberFloat(n) => self.context.f64_type().const_float(*n as f64).into(),
            Expr::StringLiteral(s) => self.build_global_string_literal(s),
            Expr::Identifier(name) => self.load_identifier(name),
            Expr::FunctionCall { name, args } => self.build_function_call(name, args),
            Expr::BoolLiteral(b) => self.context.bool_type().const_int(*b as u64, false).into(),

            Expr::Binary { op, left, right } => {
                let left_val = self.codegen_expr(left);
                let right_val = self.codegen_expr(right);
                self.codegen_binary(op, left_val, right_val)
            }

            Expr::Unary { op, expr } => self.codegen_unary(op, expr),

            Expr::ArrayLiteral(elements) => self.codegen_array_literal(elements),

            Expr::Index(array_expr, index_expr) => self.codegen_index(array_expr, index_expr),

            Expr::Assign(left, right) => self.codegen_assign(left, right),

            Expr::Typed(inner, _ty) => self.codegen_expr(inner),
        }
    }

    pub fn build_logical(
        &self,
        op: BinaryOp,
        left_val: inkwell::values::BasicValueEnum<'ctx>,
        right_val: inkwell::values::BasicValueEnum<'ctx>,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        let func = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let lhs_block = self.builder.get_insert_block().unwrap();
        let rhs_block = self.context.append_basic_block(func, "logical_rhs");
        let merge_block = self.context.append_basic_block(func, "logical_merge");

        // Convert left to bool
        let left_bool = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                left_val.into_int_value(),
                self.context.bool_type().const_int(0, false),
                "left_bool",
            )
            .unwrap();

        match op {
            BinaryOp::LogicalAnd => {
                // AND: if left true, evaluate right; else jump to merge (false)
                let _ = self
                    .builder
                    .build_conditional_branch(left_bool, rhs_block, merge_block);
            }
            BinaryOp::LogicalOr => {
                // OR: if left true, jump to merge (true); else evaluate right
                let _ = self
                    .builder
                    .build_conditional_branch(left_bool, merge_block, rhs_block);
            }
            _ => unreachable!(),
        }

        // RHS block
        self.builder.position_at_end(rhs_block);
        let right_bool = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                right_val.into_int_value(),
                self.context.bool_type().const_int(0, false),
                "right_bool",
            )
            .unwrap();
        let _ = self.builder.build_unconditional_branch(merge_block);

        // Merge block
        self.builder.position_at_end(merge_block);
        let phi = self
            .builder
            .build_phi(self.context.bool_type(), "logical_phi")
            .unwrap();

        match op {
            BinaryOp::LogicalAnd => {
                // AND: false if left was false, right if left true
                phi.add_incoming(&[
                    (&self.context.bool_type().const_int(0, false), lhs_block),
                    (&right_bool, rhs_block),
                ]);
            }
            BinaryOp::LogicalOr => {
                // OR: true if left was true, right if left false
                phi.add_incoming(&[
                    (&self.context.bool_type().const_int(1, false), lhs_block),
                    (&right_bool, rhs_block),
                ]);
            }
            _ => unreachable!(),
        }

        phi.as_basic_value().into()
    }

    pub fn llvm_var_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::I32 => self.context.i32_type().into(),
            Type::I64 => self.context.i64_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::String => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Void => panic!(
                "Cannot generate an LLVM value type for 'void'. Variables cannot have type 'void'."
            ),
            Type::Array(elem_ty, len_opt) => {
                let llvm_elem = self.llvm_var_type(elem_ty);
                let array_len = len_opt.unwrap_or(0); // 0 for dynamic later

                match llvm_elem {
                    BasicTypeEnum::IntType(int_ty) => int_ty.array_type(array_len as u32).into(),
                    BasicTypeEnum::FloatType(float_ty) => {
                        float_ty.array_type(array_len as u32).into()
                    }
                    BasicTypeEnum::PointerType(ptr_ty) => {
                        ptr_ty.array_type(array_len as u32).into()
                    }
                    BasicTypeEnum::ArrayType(arr_ty) => arr_ty.array_type(array_len as u32).into(),
                    _ => unimplemented!("Unsupported array element type"),
                }
            }
        }
    }

    fn llvm_function_type(
        &self,
        ret_ty: &Type,
        param_tys: &[Type],
        is_variadic: bool,
    ) -> FunctionType<'ctx> {
        let llvm_param_tys: Vec<BasicMetadataTypeEnum> = param_tys
            .iter()
            .map(|ty| match self.llvm_type(ty) {
                ExylLLVMType::Basic(b) => b.into(),
                ExylLLVMType::Void(_) => panic!("Function parameters cannot have type 'void'."),
            })
            .collect();

        match self.llvm_type(ret_ty) {
            ExylLLVMType::Basic(b) => b.fn_type(&llvm_param_tys, is_variadic),
            ExylLLVMType::Void(v) => v.fn_type(&llvm_param_tys, is_variadic),
        }
    }

    // --- helpers: statements ---

    fn codegen_let(&self, name: &str, ty: Option<&Type>, value: &Expr) {
        let ty = ty.expect("Variable type must be set by type checker");
        let llvm_type = self.llvm_var_type(ty);
        let init_val = self.codegen_expr(value);
        let alloca = self.builder.build_alloca(llvm_type, &name).unwrap();
        self.builder.build_store(alloca, init_val).unwrap();
        self.scopes
            .borrow_mut()
            .insert(name.to_string(), (alloca, llvm_type))
            .unwrap_or_else(|err| panic!("Scope insertion failed: {}", err));
    }

    fn codegen_function(
        &mut self,
        name: &str,
        return_type: Option<&Type>,
        inferred_return: &Type,
        arguments: &[(String, Type)],
        body: Option<&Vec<Stmt>>,
        is_extern: bool,
        is_variadic: bool,
    ) -> Option<FunctionValue<'ctx>> {
        let mut name = name.to_string();

        if name == "main" {
            name = "exyl_main".to_string();
        }

        // Build function type
        let param_types: Vec<Type> = arguments.iter().map(|(_, ty)| ty.clone()).collect();
        let fn_type = self.llvm_function_type(
            return_type.unwrap_or(&Type::Void),
            &param_types,
            is_variadic,
        );

        // Get or add function to module
        let function = if let Some(f) = self.module.get_function(&name) {
            f
        } else {
            let f = self.module.add_function(&name, fn_type, None);
            f
        };

        if is_extern {
            // External function, ensure linkage, and return
            function.set_linkage(Linkage::External);
            return Some(function);
        }

        self.inside_function = true;

        // Create entry block and position builder
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Push new function scope
        self.scopes.borrow_mut().push();

        // Allocate and store parameters
        for (i, (arg_name, arg_ty)) in arguments.iter().enumerate() {
            let llvm_ty = self.llvm_var_type(arg_ty);
            let alloca = self.builder.build_alloca(llvm_ty, arg_name).unwrap();
            let param_val = function.get_nth_param(i as u32).unwrap();
            let _ = self.builder.build_store(alloca, param_val);
            self.scopes
                .borrow_mut()
                .insert(arg_name.clone(), (alloca, llvm_ty))
                .unwrap_or_else(|err| {
                    panic!(
                        "Scope insertion failed for parameter '{}': {}",
                        arg_name, err
                    )
                });
        }

        // Function body
        if let Some(body_stmts) = body {
            self.current_func_has_return = false;
            for stmt in body_stmts {
                self.codegen_stmt(stmt);
            }
        }

        // Ensure function termination
        if !self.current_func_has_return {
            let _ = match return_type.unwrap_or(&Type::Void) {
                Type::Void => self.builder.build_return(None),
                Type::I64 => self
                    .builder
                    .build_return(Some(&self.context.i64_type().const_int(0, false))),
                Type::F64 => self
                    .builder
                    .build_return(Some(&self.context.f64_type().const_float(0.0))),
                _ => unimplemented!(),
            };
        }

        // Update inferred return after body generation
        self.functions.borrow_mut().insert(
            name.to_string(),
            (function, self.llvm_type(inferred_return)),
        );

        // Pop function scope
        self.scopes.borrow_mut().pop();
        self.inside_function = false;

        Some(function)
    }

    fn codegen_return(&mut self, ret_opt: &Option<Expr>) -> Option<FunctionValue<'ctx>> {
        if !self.inside_function {
            panic!("'return' is not allowed outside of a function body.");
        }

        let func_ret_ty = self.current_func_return_type.clone();
        if let Some(expr) = ret_opt {
            let val = self.codegen_expr(expr);
            let _ = self.builder.build_return(Some(&val));
        } else {
            assert!(matches!(func_ret_ty, Type::Void));
            let _ = self.builder.build_return(None);
        }
        self.current_func_has_return = true;
        None
    }

    fn codegen_block(&mut self, stmts: &Vec<Stmt>) {
        self.scopes.borrow_mut().push();
        for stmt in stmts {
            self.codegen_stmt(stmt);
        }
        self.scopes.borrow_mut().pop();
    }

    fn codegen_if(&mut self, condition: &Expr, then_branch: &Stmt, else_branch: Option<&Stmt>) {
        let cond_val = self.codegen_expr(condition).into_int_value();

        let func = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let then_bb = self.context.append_basic_block(func, "then");
        let merge_bb = self.context.append_basic_block(func, "ifcont");
        let else_bb = else_branch.map(|_| self.context.append_basic_block(func, "else"));

        match else_bb {
            Some(ref else_block) => {
                let _ = self
                    .builder
                    .build_conditional_branch(cond_val, then_bb, *else_block);
            }
            None => {
                let _ = self
                    .builder
                    .build_conditional_branch(cond_val, then_bb, merge_bb);
            }
        }

        // Then
        self.builder.position_at_end(then_bb);
        self.codegen_stmt(then_branch);
        let _ = self.builder.build_unconditional_branch(merge_bb);

        // Else
        if else_branch.is_some() {
            self.builder.position_at_end(else_bb.unwrap());
            self.codegen_stmt(else_branch.unwrap());
            let _ = self.builder.build_unconditional_branch(merge_bb);
        }

        // Merge
        self.builder.position_at_end(merge_bb);
    }

    // --- helpers: expressions ---

    fn build_global_string_literal(&self, s: &str) -> BasicValueEnum<'ctx> {
        let global_str = self.module.add_global(
            self.context.i8_type().array_type((s.len() + 1) as u32),
            None,
            "str_literal",
        );
        let mut bytes = s.as_bytes().to_vec();
        bytes.push(0);
        global_str.set_initializer(&self.context.const_string(&bytes, false));
        global_str.as_pointer_value().into()
    }

    fn load_identifier(&self, name: &str) -> BasicValueEnum<'ctx> {
        let variables = self.scopes.borrow();
        let (var_ptr, ty) = variables.lookup(name).unwrap_or_else(|| {
            panic!(
                "Undefined variable or symbol '{}' in the current scope. Did you declare it?",
                name
            )
        });
        self.builder.build_load(ty, var_ptr, name).unwrap().into()
    }

    fn build_function_call(&self, name: &str, args: &Vec<Expr>) -> BasicValueEnum<'ctx> {
        let func_ref = self.functions.borrow();
        let func = func_ref
            .get(name)
            .unwrap_or_else(|| panic!("Call to undefined function '{}'. Ensure it is declared before use or marked extern.", name));

        let arg_vals: Vec<BasicMetadataValueEnum> = args
            .iter()
            .map(|arg| self.codegen_expr(arg).into())
            .collect();

        let call_site = self
            .builder
            .build_call(func.0, &arg_vals, "call_tmp")
            .unwrap();

        match func.1 {
            ExylLLVMType::Basic(_) => call_site.try_as_basic_value().left().unwrap(),
            ExylLLVMType::Void(_) => self.context.i64_type().const_zero().into(),
        }
    }

    fn codegen_unary(&self, op: &UnaryOp, expr: &Expr) -> BasicValueEnum<'ctx> {
        let val = self.codegen_expr(expr);
        match op {
            UnaryOp::Not => {
                let int_val = val.into_int_value();
                let zero = self.context.bool_type().const_int(0, false);
                self.builder
                    .build_int_compare(IntPredicate::EQ, int_val, zero, "not")
                    .unwrap()
                    .into()
            }
            UnaryOp::Negate => match val {
                BasicValueEnum::IntValue(i) => self.builder.build_int_neg(i, "neg").unwrap().into(),
                BasicValueEnum::FloatValue(f) => {
                    self.builder.build_float_neg(f, "neg").unwrap().into()
                }
                _ => panic!(
                    "Unary negate is only supported for integers and floats. Got unsupported value type."
                ),
            },
        }
    }

    fn codegen_binary(
        &self,
        op: &BinaryOp,
        left_val: BasicValueEnum<'ctx>,
        right_val: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match left_val {
            BasicValueEnum::IntValue(left_int) => {
                let right_int = right_val.into_int_value();
                self.codegen_binary_int(op, left_int, right_int)
            }
            BasicValueEnum::FloatValue(left_float) => {
                let right_float = right_val.into_float_value();
                self.codegen_binary_float(op, left_float, right_float, left_val, right_val)
            }
            BasicValueEnum::PointerValue(left_ptr) => {
                // Only handle equality/inequality for now
                match op {
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        // Assume left_ptr and right_val are also i8* (strings)
                        let right_ptr = right_val.into_pointer_value();
                        let strcmp_fn = self.module.get_function("strcmp").unwrap();

                        let call = self
                            .builder
                            .build_call(
                                strcmp_fn,
                                &[left_ptr.into(), right_ptr.into()],
                                "strcmp_call",
                            )
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                            .into_int_value();

                        // strcmp returns 0 if equal
                        let eq = self
                            .builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                call,
                                self.context.i32_type().const_zero(),
                                "str_eq",
                            )
                            .unwrap();

                        if *op == BinaryOp::Equal {
                            eq.as_basic_value_enum()
                        } else {
                            // !=
                            self.builder
                                .build_not(eq, "str_neq")
                                .unwrap()
                                .as_basic_value_enum()
                        }
                    }
                    _ => panic!(
                        "Unsupported binary operation for strings. Only '==' and '!=' are allowed."
                    ),
                }
            }

            _ => panic!(
                "Binary operation received unsupported operand types. Ensure both operands are integers or both are floats."
            ),
        }
    }

    fn codegen_binary_int(
        &self,
        op: &BinaryOp,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match op {
            BinaryOp::Add => self
                .builder
                .build_int_add(left, right, "add")
                .unwrap()
                .into(),
            BinaryOp::Subtract => self
                .builder
                .build_int_sub(left, right, "sub")
                .unwrap()
                .into(),
            BinaryOp::Multiply => self
                .builder
                .build_int_mul(left, right, "mul")
                .unwrap()
                .into(),
            BinaryOp::Divide => self
                .builder
                .build_int_signed_div(left, right, "div")
                .unwrap()
                .into(),
            BinaryOp::Modulo => self
                .builder
                .build_int_signed_rem(left, right, "mod")
                .unwrap()
                .into(),
            BinaryOp::Equal => self
                .builder
                .build_int_compare(IntPredicate::EQ, left, right, "eq")
                .unwrap()
                .into(),
            BinaryOp::NotEqual => self
                .builder
                .build_int_compare(IntPredicate::NE, left, right, "neq")
                .unwrap()
                .into(),
            BinaryOp::LessThan => self
                .builder
                .build_int_compare(IntPredicate::SLT, left, right, "lt")
                .unwrap()
                .into(),
            BinaryOp::LessThanOrEqual => self
                .builder
                .build_int_compare(IntPredicate::SLE, left, right, "le")
                .unwrap()
                .into(),
            BinaryOp::GreaterThan => self
                .builder
                .build_int_compare(IntPredicate::SGT, left, right, "gt")
                .unwrap()
                .into(),
            BinaryOp::GreaterThanOrEqual => self
                .builder
                .build_int_compare(IntPredicate::SGE, left, right, "ge")
                .unwrap()
                .into(),
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                self.build_logical(op.clone(), left.into(), right.into())
            }
        }
    }

    fn codegen_binary_float(
        &self,
        op: &BinaryOp,
        left: FloatValue<'ctx>,
        right: FloatValue<'ctx>,
        left_raw: BasicValueEnum<'ctx>,
        right_raw: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match op {
            BinaryOp::Add => self
                .builder
                .build_float_add(left, right, "add")
                .unwrap()
                .into(),
            BinaryOp::Subtract => self
                .builder
                .build_float_sub(left, right, "sub")
                .unwrap()
                .into(),
            BinaryOp::Multiply => self
                .builder
                .build_float_mul(left, right, "mul")
                .unwrap()
                .into(),
            BinaryOp::Divide => self
                .builder
                .build_float_div(left, right, "div")
                .unwrap()
                .into(),
            BinaryOp::Equal => self
                .builder
                .build_float_compare(FloatPredicate::OEQ, left, right, "eq")
                .unwrap()
                .into(),
            BinaryOp::NotEqual => self
                .builder
                .build_float_compare(FloatPredicate::ONE, left, right, "neq")
                .unwrap()
                .into(),
            BinaryOp::LessThan => self
                .builder
                .build_float_compare(FloatPredicate::OLT, left, right, "lt")
                .unwrap()
                .into(),
            BinaryOp::LessThanOrEqual => self
                .builder
                .build_float_compare(FloatPredicate::OLE, left, right, "le")
                .unwrap()
                .into(),
            BinaryOp::GreaterThan => self
                .builder
                .build_float_compare(FloatPredicate::OGT, left, right, "gt")
                .unwrap()
                .into(),
            BinaryOp::GreaterThanOrEqual => self
                .builder
                .build_float_compare(FloatPredicate::OGE, left, right, "ge")
                .unwrap()
                .into(),
            BinaryOp::Modulo => self.build_fmod_call(left_raw, right_raw),
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                panic!(
                    "Logical operators (&&, ||) are only valid for boolean/integer conditions, not floats."
                );
            }
        }
    }

    fn build_fmod_call(
        &self,
        left_raw: BasicValueEnum<'ctx>,
        right_raw: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let fmod_fn = self.module.get_function("fmod").unwrap_or_else(|| {
            let f64_type = self.context.f64_type();
            let fn_type = f64_type.fn_type(&[f64_type.into(), f64_type.into()], false);
            self.module
                .add_function("fmod", fn_type, Some(inkwell::module::Linkage::External))
        });

        let call_site = self
            .builder
            .build_call(
                fmod_fn,
                &[
                    left_raw.into_float_value().into(),
                    right_raw.into_float_value().into(),
                ],
                "fmod",
            )
            .unwrap();

        call_site.try_as_basic_value().left().unwrap()
    }

    fn codegen_array_literal(&self, elements: &Vec<Expr>) -> BasicValueEnum<'ctx> {
        if elements.is_empty() {
            panic!("Empty array literal is not supported yet. Provide at least one element.");
        }

        let vals: Vec<BasicValueEnum> = elements.iter().map(|e| self.codegen_expr(e)).collect();
        if vals.is_empty() {
            panic!("Internal error: computed empty element list for array literal.");
        }
        let elem_ty = vals[0].get_type();
        for v in &vals {
            if v.get_type() != elem_ty {
                panic!(
                    "Mismatched element types in array literal. All elements must have the same type."
                );
            }
        }

        match vals[0] {
            BasicValueEnum::IntValue(_) => {
                let int_vals: Vec<IntValue> =
                    vals.into_iter().map(|v| v.into_int_value()).collect();
                elem_ty
                    .into_int_type()
                    .const_array(&int_vals)
                    .as_basic_value_enum()
            }
            BasicValueEnum::FloatValue(_) => {
                let float_vals: Vec<FloatValue> =
                    vals.into_iter().map(|v| v.into_float_value()).collect();
                elem_ty
                    .into_float_type()
                    .const_array(&float_vals)
                    .as_basic_value_enum()
            }
            BasicValueEnum::PointerValue(_) => {
                let ptr_vals: Vec<PointerValue> =
                    vals.into_iter().map(|v| v.into_pointer_value()).collect();
                elem_ty
                    .into_pointer_type()
                    .const_array(&ptr_vals)
                    .as_basic_value_enum()
            }
            _ => unimplemented!("Unsupported array element type for const_array"),
        }
    }

    fn codegen_index(&self, array_expr: &Expr, index_expr: &Expr) -> BasicValueEnum<'ctx> {
        // If array is an identifier, use pointer + GEP so index can be dynamic
        if let &Expr::Identifier(ref name) = array_expr {
            let variables = self.scopes.borrow();
            let (array_ptr, array_ty) = variables
                .lookup(name)
                .unwrap_or_else(|| panic!("Undefined array variable '{}'", name));

            let idx_val = self.codegen_expr(index_expr).into_int_value();
            let idx_i32 = self
                .builder
                .build_int_truncate(idx_val, self.context.i32_type(), "idx_i32")
                .unwrap();
            let zero = self.context.i32_type().const_int(0, false);

            let elem_ptr = unsafe {
                self.builder
                    .build_gep(array_ty, array_ptr, &[zero, idx_i32], "elem_ptr")
                    .unwrap()
            };

            return self
                .builder
                .build_load(elem_ptr.get_type(), elem_ptr, "load_idx")
                .unwrap()
                .into();
        }

        // Otherwise, evaluate array value. If it's a constant array literal, only const index works
        let array_val = self.codegen_expr(array_expr);
        match array_val {
            BasicValueEnum::ArrayValue(arr) => {
                let index_val = self.codegen_expr(index_expr);
                let idx_u32 = match index_val {
                    BasicValueEnum::IntValue(i) => i
                        .get_zero_extended_constant()
                        .expect("Index into array literal must be a compile-time constant"),
                    _ => panic!("Array literal indexing requires a constant integer index."),
                } as u32;

                let extracted = self
                    .builder
                    .build_extract_value(arr, idx_u32, "extract")
                    .expect("Failed to extract array element: out-of-bounds index or invalid array value.");
                extracted.as_basic_value_enum()
            }
            _ => panic!("Indexing is only supported on array variables or array literals."),
        }
    }

    fn codegen_assign(&self, left: &Expr, right: &Expr) -> BasicValueEnum<'ctx> {
        let right_val = self.codegen_expr(right);

        match left {
            Expr::Identifier(name) => {
                let variables = self.scopes.borrow();
                let (var_ptr, ty) = variables.lookup(name).unwrap_or_else(|| {
                    panic!(
                        "Assignment to undefined variable '{}'. Declare it before use.",
                        name
                    )
                });
                let _ = self.builder.build_store(var_ptr, right_val);
                self.builder.build_load(ty, var_ptr, name).unwrap().into()
            }
            Expr::Index(array_expr, index_expr) => {
                // Only support assignment into alloca-based arrays for now
                // Resolve array variable pointer
                let array_ident = match **array_expr {
                    Expr::Identifier(ref n) => n.clone(),
                    _ => panic!("Assignment to array elements only supported for named arrays."),
                };

                let variables = self.scopes.borrow();
                let (array_ptr, array_ty) = variables
                    .lookup(&array_ident)
                    .expect("Assignment to undefined array variable.");

                // Compute GEP for the element index (runtime indices allowed)
                let idx_i64 = self.codegen_expr(index_expr).into_int_value();
                let idx_i32 = self
                    .builder
                    .build_int_truncate(idx_i64, self.context.i32_type(), "idx_i32")
                    .unwrap();

                let zero = self.context.i32_type().const_int(0, false);

                let element_ptr = unsafe {
                    self.builder
                        .build_gep(array_ty, array_ptr, &[zero, idx_i32], "elem_ptr")
                        .unwrap()
                };

                let _ = self.builder.build_store(element_ptr, right_val);
                right_val
            }
            _ => panic!("Left-hand side of assignment must be an identifier or array index."),
        }
    }
}
