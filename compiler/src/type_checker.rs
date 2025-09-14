use std::collections::HashMap;

use crate::{
    ast::{Expr, Program, Stmt, Type},
    builtin::BuiltinFunction,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UnknownVariable(String),
    UnknownFunction(String),
    TypeMismatch {
        expected: Type,
        found: Type,
    },
    ArgumentMismatch(String),
    InvalidOperation {
        operator: String,
        left: Type,
        right: Type,
    },
    CannotInferType(String),
    Other(String),
}

pub struct TypeChecker {
    pub variables: HashMap<String, Type>,
    pub builtins: HashMap<String, BuiltinFunction>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut builtins = HashMap::new();
        builtins.insert(
            "print".to_string(),
            BuiltinFunction {
                name: "print".to_string(),
                param_types: vec![Type::String],
                return_type: Type::Void,
            },
        );

        Self {
            variables: HashMap::new(),
            builtins,
        }
    }

    // ----------------------------
    // Program & Statement Checking
    // ----------------------------

    pub fn type_check_program(&mut self, program: Program) -> Result<Program, TypeError> {
        let body: Result<Vec<_>, _> = program
            .body
            .into_iter()
            .map(|stmt| self.type_check_stmt(stmt))
            .collect();

        Ok(Program { body: body? })
    }

    fn type_check_stmt(&mut self, stmt: Stmt) -> Result<Stmt, TypeError> {
        match stmt {
            Stmt::Let { name, ty, value } => self.type_check_let(name, ty, value),
            Stmt::Expr(expr) => Ok(Stmt::Expr(self.type_check_expr(expr)?)),
        }
    }

    fn type_check_let(
        &mut self,
        name: String,
        declared_ty: Option<Type>,
        value: Expr,
    ) -> Result<Stmt, TypeError> {
        let typed_value = self.type_check_expr(value)?;
        let inferred_ty = typed_value.get_type().unwrap();

        // Determine the final variable type
        let var_type = match declared_ty {
            Some(ref ty) => {
                if *ty != inferred_ty {
                    return Err(TypeError::TypeMismatch {
                        expected: ty.clone(),
                        found: inferred_ty,
                    });
                }
                ty.clone()
            }
            None => inferred_ty, // ← inferred type
        };

        // Insert variable into type environment
        self.variables.insert(name.clone(), var_type.clone());

        Ok(Stmt::Let {
            name,
            ty: Some(var_type),
            value: typed_value,
        })
    }

    // ----------------------------
    // Expression Checking
    // ----------------------------

    pub fn type_check_expr(&mut self, expr: Expr) -> Result<Expr, TypeError> {
        match expr {
            Expr::NumberInt(_) => Ok(Expr::Typed(Box::new(expr), Type::I64)),
            Expr::NumberFloat(_) => Ok(Expr::Typed(Box::new(expr), Type::F64)),
            Expr::StringLiteral(_) => Ok(Expr::Typed(Box::new(expr), Type::String)),

            Expr::Identifier(name) => {
                let ty = self
                    .variables
                    .get(&name)
                    .ok_or(TypeError::UnknownVariable(name.clone()))?
                    .clone();
                Ok(Expr::Typed(Box::new(Expr::Identifier(name)), ty))
            }

            Expr::FunctionCall { name, args } => self.type_check_function_call(name, args),

            Expr::Typed(_, _) => Ok(expr), // already typed
        }
    }

    // ----------------------------
    // Helper: Function Calls
    // ----------------------------

    fn type_check_function_call(
        &mut self,
        name: String,
        args: Vec<Expr>,
    ) -> Result<Expr, TypeError> {
        // Clone function info
        let (param_types, ret_type) = {
            let func = self
                .builtins
                .get(&name)
                .ok_or(TypeError::UnknownFunction(name.clone()))?;
            (func.param_types.clone(), func.return_type.clone())
        };

        if args.len() != param_types.len() {
            return Err(TypeError::ArgumentMismatch(name.clone()));
        }

        // Type check each argument, inferring if needed
        let typed_args: Result<Vec<_>, _> = args
            .into_iter()
            .zip(param_types.iter())
            .map(|(arg, expected_ty)| {
                let typed_arg = self.type_check_expr(arg)?;
                let actual_ty = typed_arg.get_type().unwrap();

                // If the expected type is a generic placeholder (None), infer from argument
                if actual_ty != *expected_ty {
                    return Err(TypeError::TypeMismatch {
                        expected: expected_ty.clone(),
                        found: actual_ty,
                    });
                }

                Ok(typed_arg)
            })
            .collect();

        Ok(Expr::Typed(
            Box::new(Expr::FunctionCall {
                name,
                args: typed_args?,
            }),
            ret_type,
        ))
    }
}
