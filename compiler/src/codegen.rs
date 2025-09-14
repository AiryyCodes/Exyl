use std::cell::RefCell;
use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::FunctionValue;
use inkwell::{builder::Builder, values::PointerValue};

use crate::ast::{Expr, Program, Stmt, Type};

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub variables: RefCell<HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let variables = RefCell::new(HashMap::new());

        Self {
            context,
            module,
            builder,
            variables,
        }
    }

    pub fn declare_print(&self) -> FunctionValue<'ctx> {
        let i64_type = self.context.f64_type();
        let fn_type = self.context.void_type().fn_type(&[i64_type.into()], false);
        self.module.add_function("print_string", fn_type, None)
    }

    pub fn codegen_program(&self, ast: &Program) -> FunctionValue<'ctx> {
        let i64_type = self.context.i64_type();
        let main_type = i64_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_type, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        for stmt in &ast.body {
            match stmt {
                Stmt::Expr(expr) => {
                    self.codegen_expr(expr);
                }
                Stmt::Let { name, ty, value } => {
                    let ty = ty
                        .as_ref()
                        .expect("Variable type must be set by type checker");
                    let llvm_type = self.llvm_var_type(ty);

                    let init_val = self.codegen_expr(value);
                    let alloca = self.builder.build_alloca(llvm_type, &name).unwrap();
                    self.builder.build_store(alloca, init_val).unwrap();
                    self.variables
                        .borrow_mut()
                        .insert(name.clone(), (alloca, llvm_type));
                }
                _ => unimplemented!(),
            }
        }

        let _ = self
            .builder
            .build_return(Some(&i64_type.const_int(0, false)));
        main_fn
    }

    fn codegen_expr(&self, expr: &Expr) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            Expr::NumberInt(n) => self.context.i64_type().const_int(*n as u64, false).into(),
            Expr::NumberFloat(n) => self.context.f64_type().const_float(*n as f64).into(),
            Expr::StringLiteral(s) => {
                // Create a global string constant in the module
                let global_str = self.module.add_global(
                    self.context.i8_type().array_type(s.len() as u32),
                    None,
                    "str_literal",
                );

                // Initialize it with the string bytes
                global_str.set_initializer(&self.context.const_string(s.as_bytes(), false));

                // Get a pointer to the first element (i8*) to use in LLVM
                global_str.as_pointer_value().into()
            }
            Expr::Identifier(name) => {
                let variables = self.variables.borrow();
                let (var_ptr, ty) = variables
                    .get(name)
                    .unwrap_or_else(|| panic!("Undefined variable {}", name));
                self.builder.build_load(*ty, *var_ptr, name).unwrap().into()
            }
            Expr::FunctionCall { name, args } if name == "print" => {
                let print_fn = self.module.get_function("print_string").unwrap();
                let arg_val = self.codegen_expr(&args[0]);
                let _ = self
                    .builder
                    .build_call(print_fn, &[arg_val.into()], "call_print");
                self.context.i64_type().const_zero().into() // dummy return
            }
            Expr::Typed(inner, _ty) => self.codegen_expr(inner),
            _ => unimplemented!(),
        }
    }

    pub fn llvm_var_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::I64 => self.context.i64_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::String => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Void => panic!("Cannot generate LLVM type for void"),
        }
    }
}
