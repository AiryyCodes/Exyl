use std::cell::RefCell;
use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

use crate::ast::{Expr, ExylLLVMType, Program, Stmt, Type};
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
        // 1️⃣ Generate all top-level functions first
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

        if let Some(main_fn) = user_main {
            main_fn
        } else {
            // Auto-generate main to run global statements
            let i64_type = self.context.i64_type();
            let main_type = i64_type.fn_type(&[], false);
            let main_fn = self.module.add_function("main", main_type, None);
            let entry = self.context.append_basic_block(main_fn, "entry");
            self.builder.position_at_end(entry);

            for stmt in &ast.body {
                match stmt {
                    Stmt::Func { .. } => {} // already generated
                    _ => {
                        self.codegen_stmt(stmt);
                    }
                }
            }

            let _ = self.builder.build_return(Some(&i64_type.const_zero()));

            main_fn
        }
    }

    fn codegen_stmt(&mut self, stmt: &Stmt) -> Option<FunctionValue<'ctx>> {
        match stmt {
            Stmt::Expr(expr) => {
                self.codegen_expr(expr);
                None
            }
            Stmt::Let { name, ty, value } => {
                let ty = ty
                    .as_ref()
                    .expect("Variable type must be set by type checker");
                let llvm_type = self.llvm_var_type(ty);

                let init_val = self.codegen_expr(value);
                let alloca = self.builder.build_alloca(llvm_type, &name).unwrap();
                self.builder.build_store(alloca, init_val).unwrap();

                self.scopes
                    .borrow_mut()
                    .insert(name.clone(), (alloca, llvm_type))
                    .unwrap_or_else(|err| panic!("{}", err));

                None
            }
            Stmt::Func {
                name,
                return_type,
                inferred_return,
                arguments,
                body,
                is_extern,
            } => {
                // 1️⃣ Build function type
                let param_types: Vec<Type> = arguments.iter().map(|(_, ty)| ty.clone()).collect();
                let fn_type = self
                    .llvm_function_type(return_type.as_ref().unwrap_or(&Type::Void), &param_types);

                // 2️⃣ Add function to module
                let function = self.module.add_function(name, fn_type, None);

                if *is_extern {
                    // Just mark linkage as external and skip codegen
                    function.set_linkage(Linkage::External);

                    // Store function info for calls
                    self.functions
                        .borrow_mut()
                        .insert(name.clone(), (function, self.llvm_type(inferred_return)));

                    return Some(function); // early return
                }

                self.inside_function = true;

                // 3️⃣ Create a new entry block
                let entry = self.context.append_basic_block(function, "entry");

                // 4️⃣ Temporarily move builder to this function
                self.builder.position_at_end(entry);

                // Push new function scope
                self.scopes.borrow_mut().push();

                // 5️⃣ Allocate and store function parameters
                for (i, (arg_name, arg_ty)) in arguments.iter().enumerate() {
                    let llvm_ty = self.llvm_var_type(arg_ty);
                    let alloca = self.builder.build_alloca(llvm_ty, arg_name).unwrap();
                    let param_val = function.get_nth_param(i as u32).unwrap();
                    let _ = self.builder.build_store(alloca, param_val);
                    self.scopes
                        .borrow_mut()
                        .insert(arg_name.clone(), (alloca, llvm_ty))
                        .unwrap_or_else(|err| panic!("{}", err));
                }

                // 6️⃣ Codegen the function body
                if let Some(body_stmts) = body {
                    self.current_func_has_return = false;
                    for stmt in body_stmts {
                        self.codegen_stmt(stmt);
                    }
                }

                if !self.current_func_has_return {
                    // 7️⃣ Ensure function is terminated
                    let _ = match return_type.as_ref().unwrap_or(&Type::Void) {
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

                self.current_func_return_type = inferred_return.clone();

                self.functions
                    .borrow_mut()
                    .insert(name.clone(), (function, self.llvm_type(inferred_return)));

                // Pop function scope
                self.scopes.borrow_mut().pop();

                self.inside_function = false;

                Some(function)
            }
            Stmt::Return(ret_opt) => {
                if !self.inside_function {
                    panic!("Error: 'return' is not allowed outside of a function");
                }

                let func_ret_ty = self.current_func_return_type.clone(); // store this when starting codegen of the function

                if let Some(expr) = ret_opt {
                    let val = self.codegen_expr(expr); // generate LLVM value for the expression

                    // For now, assume types match exactly
                    let _ = self.builder.build_return(Some(&val));
                } else {
                    assert!(matches!(func_ret_ty, Type::Void));
                    let _ = self.builder.build_return(None); // bare return
                }

                self.current_func_has_return = true;
                None
            }
            Stmt::Block(stmts) => {
                // Push a new nested scope
                self.scopes.borrow_mut().push();

                // Codegen each statement inside the block
                for stmt in stmts {
                    self.codegen_stmt(stmt);
                }

                // Pop the block scope
                self.scopes.borrow_mut().pop();

                None
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Generate condition
                let cond_val = self.codegen_expr(&condition); // i1 now
                let cond_val = cond_val.into_int_value();

                let func = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                // Create basic blocks
                let then_bb = self.context.append_basic_block(func, "then");
                let merge_bb = self.context.append_basic_block(func, "ifcont");
                let else_bb = else_branch
                    .as_ref()
                    .map(|_| self.context.append_basic_block(func, "else"));

                // Build conditional branch directly with i1
                match else_bb {
                    Some(ref else_block) => {
                        let _ =
                            self.builder
                                .build_conditional_branch(cond_val, then_bb, *else_block);
                    }
                    None => {
                        let _ = self
                            .builder
                            .build_conditional_branch(cond_val, then_bb, merge_bb);
                    }
                }

                // Then block
                self.builder.position_at_end(then_bb);
                self.codegen_stmt(then_branch);
                let _ = self.builder.build_unconditional_branch(merge_bb);

                // Else block
                if let Some(else_block) = else_branch {
                    self.builder.position_at_end(else_bb.unwrap());
                    self.codegen_stmt(&else_block);
                    let _ = self.builder.build_unconditional_branch(merge_bb);
                }

                // Continue after if
                self.builder.position_at_end(merge_bb);

                None
            }
            _ => unimplemented!(),
        }
    }

    fn codegen_expr(&self, expr: &Expr) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            Expr::NumberInt(n) => self.context.i64_type().const_int(*n as u64, false).into(),
            Expr::NumberFloat(n) => self.context.f64_type().const_float(*n as f64).into(),
            Expr::StringLiteral(s) => {
                // Create a global string constant in the module
                let global_str = self.module.add_global(
                    self.context.i8_type().array_type((s.len() + 1) as u32),
                    None,
                    "str_literal",
                );

                let mut bytes = s.as_bytes().to_vec();
                bytes.push(0);

                // Initialize it with the string bytes
                global_str.set_initializer(&self.context.const_string(&bytes, false));

                // Get a pointer to the first element (i8*) to use in LLVM
                global_str.as_pointer_value().into()
            }
            Expr::Identifier(name) => {
                let variables = self.scopes.borrow();
                let (var_ptr, ty) = variables
                    .lookup(name)
                    .unwrap_or_else(|| panic!("Undefined symbol {}", name));
                self.builder.build_load(ty, var_ptr, name).unwrap().into()
            }
            Expr::FunctionCall { name, args } => {
                // Lookup function
                let func_ref = self.functions.borrow(); // keep the Ref alive
                let func = func_ref
                    .get(name)
                    .unwrap_or_else(|| panic!("Undefined function {}", name));

                // Generate code for each argument
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| self.codegen_expr(arg).into()) // convert BasicValueEnum -> BasicMetadataValueEnum
                    .collect();

                // Build the call
                let call_site = self
                    .builder
                    .build_call(func.0, &arg_vals, "call_tmp")
                    .unwrap();

                // Return value (if not void)
                match func.1 {
                    ExylLLVMType::Basic(_) => call_site.try_as_basic_value().left().unwrap(),
                    ExylLLVMType::Void(_) => self.context.i64_type().const_zero().into(),
                }
            }
            Expr::BoolLiteral(b) => self.context.bool_type().const_int(*b as u64, false).into(),
            Expr::Typed(inner, _ty) => self.codegen_expr(inner),
            _ => unimplemented!(),
        }
    }

    pub fn llvm_var_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::I64 => self.context.i64_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::String => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Void => panic!("Cannot generate LLVM type for void"),
        }
    }

    fn llvm_function_type(&self, ret_ty: &Type, param_tys: &[Type]) -> FunctionType<'ctx> {
        let llvm_param_tys: Vec<BasicMetadataTypeEnum> = param_tys
            .iter()
            .map(|ty| match self.llvm_type(ty) {
                ExylLLVMType::Basic(b) => b.into(),
                ExylLLVMType::Void(_) => panic!("Parameters cannot be void"),
            })
            .collect();

        match self.llvm_type(ret_ty) {
            ExylLLVMType::Basic(b) => b.fn_type(&llvm_param_tys, false),
            ExylLLVMType::Void(v) => v.fn_type(&llvm_param_tys, false),
        }
    }
}
