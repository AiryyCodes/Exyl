use std::collections::HashMap;

use crate::{
    ast::{BinaryOp, Expr, Program, Stmt, Type, UnaryOp},
    function::FunctionInfo,
    scope::ScopeStack,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UnknownVariable(String),
    UnknownFunction(String),
    TypeMismatch { expected: Type, found: Type },
    BinaryTypeMismatch { left: Type, right: Type },
    UnaryTypeMismatch { op: UnaryOp, ty: Type },
    ArgumentMismatch(String),
    ReturnOutsideFunction,
    UnknownType,
}

pub struct TypeChecker {
    pub scopes: ScopeStack<Type>,
    pub functions: HashMap<String, FunctionInfo>,

    pub inside_function: bool,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        functions.insert(
            "print".to_string(),
            FunctionInfo::new_builtin("print".to_string(), vec![Type::String], Type::Void),
        );

        Self {
            functions,
            inside_function: false,
            scopes: ScopeStack::new(),
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
            Stmt::Func {
                name,
                return_type,
                inferred_return,
                arguments,
                body,
                is_extern,
            } => self.type_check_func(
                name,
                return_type,
                inferred_return,
                arguments,
                body,
                is_extern,
            ),
            Stmt::Return(ret_opt) => {
                if !self.inside_function {
                    return Err(TypeError::ReturnOutsideFunction);
                }

                let typed_expr_opt = if let Some(expr) = ret_opt {
                    Some(self.type_check_expr(expr)?)
                } else {
                    None
                };
                Ok(Stmt::Return(typed_expr_opt))
            }
            Stmt::Block(stmts) => {
                self.scopes.push(); // push a new scope for the block

                let mut typed_stmts = Vec::new();
                for stmt in stmts {
                    typed_stmts.push(self.type_check_stmt(stmt)?);
                }

                self.scopes.pop(); // pop the scope after block ends
                Ok(Stmt::Block(typed_stmts))
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_typed = self.type_check_expr(condition)?;
                if cond_typed.get_type() != Some(Type::Bool) {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Bool,
                        found: cond_typed.get_type().unwrap(),
                    });
                }

                let then_checked = Box::new(self.type_check_stmt(*then_branch)?);
                let else_checked = match else_branch {
                    Some(else_stmt) => Some(Box::new(self.type_check_stmt(*else_stmt)?)),
                    None => None,
                };

                Ok(Stmt::If {
                    condition: cond_typed,
                    then_branch: then_checked,
                    else_branch: else_checked,
                })
            }
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
        self.scopes
            .insert(name.clone(), var_type.clone())
            .unwrap_or_else(|err| panic!("{}", err));

        Ok(Stmt::Let {
            name,
            ty: Some(var_type),
            value: typed_value,
        })
    }

    fn type_check_func(
        &mut self,
        name: String,
        return_type: Option<Type>,
        _inferred_return: Type,
        arguments: Vec<(String, Type)>,
        body: Option<Vec<Stmt>>,
        is_extern: bool,
    ) -> Result<Stmt, TypeError> {
        self.inside_function = true;

        // 1. Arguments: nothing fancy yet, just trust parser
        let arg_types = arguments.clone();

        self.scopes.push();

        for (arg_name, arg_ty) in &arguments {
            let _ = self.scopes.insert(arg_name.clone(), arg_ty.clone());
        }

        // 2. If body is present, check it
        let inferred_return = if let Some(stmts) = &body {
            let mut return_types: Vec<Type> = Vec::new();

            for stmt in stmts {
                self.type_check_stmt(stmt.clone())?; // type check each statement

                // Collect return types
                if let Stmt::Return(expr_opt) = stmt {
                    let ty = if let Some(expr) = expr_opt {
                        let typed_expr = self.type_check_expr(expr.clone())?;
                        typed_expr.get_type().unwrap_or(Type::Void)
                    } else {
                        Type::Void
                    };
                    return_types.push(ty);
                }
            }

            if return_types.is_empty() {
                // No explicit return → Void
                Type::Void
            } else {
                // Ensure all return types are consistent
                let first = &return_types[0];
                for other in &return_types[1..] {
                    if other != first {
                        return Err(TypeError::TypeMismatch {
                            expected: first.clone(),
                            found: other.clone(),
                        });
                    }
                }
                first.clone()
            }
        } else {
            // Declaration only
            return_type.clone().unwrap_or(Type::Void)
        };

        // 3. Validate return type
        if let Some(declared) = &return_type {
            if declared != &inferred_return {
                return Err(TypeError::TypeMismatch {
                    expected: declared.clone(),
                    found: inferred_return,
                });
            }
        }

        let param_types = arg_types.clone().iter().map(|(_, ty)| ty.clone()).collect();

        self.functions.insert(
            name.clone(),
            FunctionInfo::new_user(
                name.clone(),
                param_types,
                return_type.clone(),
                inferred_return.clone(),
            ),
        );

        self.scopes.pop();

        self.inside_function = false;

        // 4. Return function statement
        Ok(Stmt::Func {
            name,
            return_type: return_type.or(Some(inferred_return.clone())),
            inferred_return,
            arguments: arg_types,
            body,
            is_extern,
        })
    }

    // ----------------------------
    // Expression Checking
    // ----------------------------

    fn type_check_expr(&mut self, expr: Expr) -> Result<Expr, TypeError> {
        match expr {
            Expr::NumberInt(_) => Ok(Expr::Typed(Box::new(expr), Type::I64)),
            Expr::NumberFloat(_) => Ok(Expr::Typed(Box::new(expr), Type::F64)),

            Expr::BoolLiteral(_) => Ok(Expr::Typed(Box::new(expr), Type::Bool)),

            Expr::StringLiteral(_) => Ok(Expr::Typed(Box::new(expr), Type::String)),

            Expr::Identifier(name) => {
                let ty = self
                    .scopes
                    .lookup(&name)
                    .ok_or(TypeError::UnknownVariable(name.clone()))?
                    .clone();
                Ok(Expr::Typed(Box::new(Expr::Identifier(name)), ty))
            }

            Expr::FunctionCall { name, args } => self.type_check_function_call(name, args),

            Expr::Binary { op, left, right } => {
                let left_typed = self.type_check_expr(*left)?;
                let right_typed = self.type_check_expr(*right)?;

                let left_type = left_typed.get_type().ok_or(TypeError::UnknownType)?;
                let right_type = right_typed.get_type().ok_or(TypeError::UnknownType)?;

                // Check compatibility
                let result_type = match op {
                    BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Modulo => {
                        if left_type.is_numeric() && right_type.is_numeric() {
                            if left_type == right_type {
                                left_type
                            } else {
                                // Optionally: promote smaller type to larger type
                                return Err(TypeError::BinaryTypeMismatch {
                                    left: left_type,
                                    right: right_type,
                                });
                            }
                        } else {
                            return Err(TypeError::BinaryTypeMismatch {
                                left: left_type,
                                right: right_type,
                            });
                        }
                    }

                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        if left_type == right_type {
                            Type::Bool
                        } else {
                            return Err(TypeError::BinaryTypeMismatch {
                                left: left_type,
                                right: right_type,
                            });
                        }
                    }

                    BinaryOp::LessThan
                    | BinaryOp::LessThanOrEqual
                    | BinaryOp::GreaterThan
                    | BinaryOp::GreaterThanOrEqual => {
                        if left_type.is_numeric() && left_type == right_type {
                            Type::Bool
                        } else {
                            return Err(TypeError::BinaryTypeMismatch {
                                left: left_type,
                                right: right_type,
                            });
                        }
                    }

                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                        if left_type == Type::Bool && right_type == Type::Bool {
                            Type::Bool
                        } else {
                            return Err(TypeError::BinaryTypeMismatch {
                                left: left_type,
                                right: right_type,
                            });
                        }
                    }
                };

                Ok(Expr::Typed(
                    Box::new(Expr::Binary {
                        op,
                        left: Box::new(left_typed),
                        right: Box::new(right_typed),
                    }),
                    result_type,
                ))
            }

            Expr::Unary { op, expr } => {
                let typed_expr = self.type_check_expr(*expr)?;
                let expr_type = typed_expr.get_type().ok_or(TypeError::UnknownType)?;

                let result_type = match op {
                    UnaryOp::Not => {
                        if expr_type == Type::Bool {
                            Type::Bool
                        } else {
                            return Err(TypeError::UnaryTypeMismatch {
                                op: op.clone(),
                                ty: expr_type,
                            });
                        }
                    }
                    UnaryOp::Negate => {
                        if expr_type.is_numeric() {
                            expr_type
                        } else {
                            return Err(TypeError::UnaryTypeMismatch {
                                op: op.clone(),
                                ty: expr_type,
                            });
                        }
                    }
                };

                Ok(Expr::Typed(
                    Box::new(Expr::Unary {
                        op: op.clone(),
                        expr: Box::new(typed_expr),
                    }),
                    result_type,
                ))
            }

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
        let func_info = self
            .functions
            .get(&name)
            .ok_or(TypeError::UnknownFunction(name.clone()))?;

        let param_types = func_info.param_types.clone();
        let ret_type = func_info.inferred_return.clone();

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
