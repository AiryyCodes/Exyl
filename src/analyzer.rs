use crate::{
    ast::{Expr, Program, Stmt, TypeExpr},
    environment::{Environment, Symbol},
    token::TokenKind,
    types::{Type, TypedExpr, TypedProgram, TypedStmt},
};

pub struct SemanticAnalyzer {
    environment: Environment,
    current_return_type: Option<Type>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(None),
            current_return_type: None,
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<TypedProgram, String> {
        let mut typed_nodes = vec![];
        for stmt in &program.nodes {
            typed_nodes.push(self.statement(stmt)?);
        }
        Ok(TypedProgram { nodes: typed_nodes })
    }

    fn statement(&mut self, stmt: &Stmt) -> Result<TypedStmt, String> {
        match stmt {
            Stmt::Let { name, ty, value } => {
                let explicit_type = ty.as_ref().map(|t| self.type_expression(t));

                let typed_value = self.expression(value, explicit_type.as_ref())?;
                let inferred_type = typed_value.get_type();

                let final_type = match explicit_type {
                    Some(explicit) => {
                        if !inferred_type.is_assignable_to(&explicit) {
                            return Err(format!(
                                "Error: Type mismatch for variable '{}'. Cannot assign type {:?} to explicit type {:?}",
                                name, inferred_type, explicit
                            ));
                        }
                        explicit
                    }
                    None => inferred_type,
                };

                self.environment.define(
                    name.clone(),
                    Symbol::Variable {
                        ty: final_type.clone(),
                    },
                )?;

                Ok(TypedStmt::Let {
                    name: name.clone(),
                    ty: final_type,
                    value: typed_value,
                })
            }

            Stmt::Fun {
                name,
                parameters,
                return_type,
                body,
            } => {
                let mut resolved_params = vec![];
                for (param_name, param_type) in parameters {
                    resolved_params.push((param_name.clone(), self.type_expression(param_type)));
                }

                let resolved_return = match return_type {
                    Some(expr) => self.type_expression(expr),
                    None => Type::Void,
                };

                let previous_return = self.current_return_type.take();
                self.current_return_type = Some(resolved_return.clone());

                let symbol = Symbol::Function {
                    params: resolved_params.clone(),
                    return_type: resolved_return.clone(),
                };

                self.environment.define(name.clone(), symbol)?;

                let current_env = std::mem::replace(&mut self.environment, Environment::new(None));
                let inner_env = Environment::new(Some(Box::new(current_env)));
                self.environment = inner_env;

                for (param_name, param_type) in &resolved_params {
                    self.environment.define(
                        param_name.clone(),
                        Symbol::Variable {
                            ty: param_type.clone(),
                        },
                    )?;
                }

                let typed_body = if let Stmt::Block(statements) = &**body {
                    let mut typed_statements = vec![];
                    for s in statements {
                        typed_statements.push(self.statement(s)?);
                    }
                    TypedStmt::Block(typed_statements)
                } else {
                    return Err(format!(
                        "Compiler Error: Function body for '{}' must be a block.",
                        name
                    ));
                };

                self.current_return_type = previous_return;

                if let Some(parent) = self.environment.parent.take() {
                    self.environment = *parent;
                } else {
                    self.environment = Environment::new(None)
                }

                Ok(TypedStmt::Fun {
                    name: name.clone(),
                    parameters: resolved_params,
                    return_type: resolved_return,
                    body: Box::new(typed_body),
                })
            }

            Stmt::Return { value } => {
                let expected_type = match &self.current_return_type {
                    Some(ty) => ty.clone(),
                    None => {
                        return Err(format!("Error: Cannot use 'return' outside of a function."));
                    }
                };

                let typed_value = match value {
                    Some(expr) => {
                        let ev_expr = self.expression(expr, Some(&expected_type))?;
                        let actual_type = ev_expr.get_type();
                        if !actual_type.is_assignable_to(&expected_type) {
                            return Err(format!(
                                "Type Error: Function return type mismatch. Expected {:?}, but got {:?}.",
                                expected_type, actual_type
                            ));
                        }
                        Some(ev_expr)
                    }
                    None => {
                        if expected_type != Type::Void {
                            return Err(format!(
                                "Type Error: Function return type mismatch. Expected {:?}, but got Void.",
                                expected_type
                            ));
                        }
                        None
                    }
                };

                Ok(TypedStmt::Return { value: typed_value })
            }

            Stmt::Block(statements) => {
                let current_env = std::mem::replace(&mut self.environment, Environment::new(None));
                let inner_env = Environment::new(Some(Box::new(current_env)));
                self.environment = inner_env;

                let mut typed_statements = vec![];
                for s in statements {
                    typed_statements.push(self.statement(s)?);
                }

                if let Some(parent) = self.environment.parent.take() {
                    self.environment = *parent;
                } else {
                    self.environment = Environment::new(None)
                }

                Ok(TypedStmt::Block(typed_statements))
            }

            Stmt::Expr(expr) => {
                let typed_expr = self.expression(expr, None)?;

                match expr {
                    Expr::Binary { operator, .. } => match operator.kind {
                        TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                            return Err(format!(
                                "Error: Statement with no effect. The result of '{}' is discarded. Did you mean to assign or return it?",
                                operator.lexeme
                            ));
                        }
                        _ => {}
                    },
                    Expr::Number(_) | Expr::String(_) | Expr::Identifier(_) => {
                        return Err("Error: Statement with no effect.".to_string());
                    }
                    _ => {}
                }

                Ok(TypedStmt::Expr(typed_expr))
            }
        }
    }

    fn expression(&self, expr: &Expr, expected: Option<&Type>) -> Result<TypedExpr, String> {
        match expr {
            Expr::Number(val) => {
                let resolved_type = if val.fract() == 0.0 {
                    if let Some(expected_type) = expected {
                        if expected_type.is_integer() || expected_type.is_float() {
                            (*expected_type).clone()
                        } else {
                            Type::I32
                        }
                    } else {
                        Type::I32
                    }
                } else {
                    if let Some(expected_type) = expected {
                        if expected_type.is_float() {
                            (*expected_type).clone()
                        } else {
                            Type::F64
                        }
                    } else {
                        Type::F64
                    }
                };
                Ok(TypedExpr::Number(*val, resolved_type))
            }

            Expr::String(val) => Ok(TypedExpr::String(val.clone())),

            Expr::Bool(val) => Ok(TypedExpr::Bool(*val)),

            Expr::Identifier(name) => match self.environment.lookup(name) {
                Some(Symbol::Variable { ty }) => Ok(TypedExpr::Identifier(name.clone(), ty)),
                Some(Symbol::Function { .. }) => {
                    Err(format!("Error: '{}' is a function, not a variable.", name))
                }
                None => Err(format!("Error: Undefined variable '{}'.", name)),
            },

            Expr::Binary {
                left,
                right,
                operator,
            } => {
                let typed_left = self.expression(left, expected)?;
                let typed_right = self.expression(right, expected)?;

                let lhs_type = typed_left.get_type();
                let rhs_type = typed_right.get_type();

                match self.resolve_binary(&operator.kind, &lhs_type, &rhs_type) {
                    Some(return_type) => Ok(TypedExpr::Binary {
                        left: Box::new(typed_left),
                        right: Box::new(typed_right),
                        operator: operator.kind.clone(),
                        ty: return_type,
                    }),
                    None => Err(format!(
                        "Type Error: Operator '{}' is not defined for types {:?} and {:?}.",
                        operator.lexeme, lhs_type, rhs_type
                    )),
                }
            }

            Expr::Call { callee, arguments } => {
                if let Expr::Identifier(func_name) = &**callee {
                    match self.environment.lookup(func_name) {
                        Some(Symbol::Function {
                            params,
                            return_type,
                        }) => {
                            if params.len() != arguments.len() {
                                return Err(format!(
                                    "Error: Function '{}' expects {} arguments, but got {}.",
                                    func_name,
                                    params.len(),
                                    arguments.len()
                                ));
                            }

                            let mut typed_arguments = vec![];
                            for (i, arg_expr) in arguments.iter().enumerate() {
                                let (_, param_type) = &params[i];
                                let typed_arg = self.expression(arg_expr, Some(param_type))?;

                                if !typed_arg.get_type().is_assignable_to(param_type) {
                                    return Err(format!(
                                        "Error: Argument mismatch at position {}. Expected {:?}, got {:?}.",
                                        i + 1,
                                        param_type,
                                        typed_arg.get_type()
                                    ));
                                }
                                typed_arguments.push(typed_arg);
                            }

                            // Reconstruct callee identifier with its function signature/type
                            let typed_callee =
                                TypedExpr::Identifier(func_name.clone(), return_type.clone());

                            Ok(TypedExpr::Call {
                                callee: Box::new(typed_callee),
                                arguments: typed_arguments,
                                return_type,
                            })
                        }
                        _ => Err(format!(
                            "Error: '{}' is not a callable function.",
                            func_name
                        )),
                    }
                } else {
                    Err("Error: Complex function calls are not supported yet.".to_string())
                }
            }

            Expr::Unary { operator, right } => {
                let typed_right = self.expression(right, expected)?;
                let right_type = typed_right.get_type();

                match operator.kind {
                    TokenKind::Minus => {
                        if right_type.is_signed_integer() || right_type.is_float() {
                            Ok(TypedExpr::Unary {
                                operator: operator.kind.clone(),
                                right: Box::new(typed_right),
                                ty: right_type,
                            })
                        } else {
                            Err(format!(
                                "Type Error: Cannot apply unary '-' to non-signable type {:?}.",
                                right_type
                            ))
                        }
                    }
                    TokenKind::Bang => {
                        if right_type == Type::Bool {
                            Ok(TypedExpr::Unary {
                                operator: operator.kind.clone(),
                                right: Box::new(typed_right),
                                ty: Type::Bool,
                            })
                        } else {
                            Err(format!(
                                "Type Error: Cannot apply logical '!' to non-boolean type {:?}.",
                                right_type
                            ))
                        }
                    }
                    _ => todo!(),
                }
            }

            Expr::Error(msg) => Err(format!("Parser error: {}", msg)),
        }
    }

    fn resolve_binary(&self, operator: &TokenKind, lhs: &Type, rhs: &Type) -> Option<Type> {
        match operator {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                if lhs == rhs && lhs.is_numeric() {
                    return Some(lhs.clone());
                }
                if lhs == &Type::String && rhs == &Type::String && operator == &TokenKind::Plus {
                    return Some(Type::String);
                }
                None
            }
            TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual => {
                if lhs == rhs && lhs.is_numeric() {
                    return Some(Type::Bool);
                }
                None
            }
            TokenKind::EqualEqual | TokenKind::BangEqual => {
                if lhs == rhs {
                    return Some(Type::Bool);
                }
                None
            }
            _ => None,
        }
    }

    fn type_expression(&self, expr: &TypeExpr) -> Type {
        match expr {
            TypeExpr::Primitive(name) => match name.as_str() {
                "i8" => Type::I8,
                "i16" => Type::I16,
                "i32" => Type::I32,
                "i64" => Type::I64,
                "u8" => Type::U8,
                "u16" => Type::U16,
                "u32" => Type::U32,
                "u64" => Type::U64,
                "f32" => Type::F32,
                "f64" => Type::F64,
                "bool" => Type::Bool,
                "string" => Type::String,
                "void" => Type::Void,
                _ => panic!("Unknown primitive type: {name}"),
            },
        }
    }
}
