use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::FunctionValue;

use crate::ast::{Expr, Program, Stmt};

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
        }
    }

    pub fn declare_print(&self) -> FunctionValue<'ctx> {
        let i64_type = self.context.i64_type();
        let fn_type = self.context.void_type().fn_type(&[i64_type.into()], false);
        self.module.add_function("print_int", fn_type, None)
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
            Expr::FunctionCall { name, args } if name == "print" => {
                let print_fn = self.module.get_function("print_int").unwrap();
                let arg_val = self.codegen_expr(&args[0]);
                let _ = self
                    .builder
                    .build_call(print_fn, &[arg_val.into()], "call_print");
                self.context.i64_type().const_zero().into() // dummy return
            }
            _ => unimplemented!(),
        }
    }
}
