use crate::{
    ast::{Expr, Program, Stmt, TypeExpr},
    environment::{Environment, Symbol},
    token::TokenKind,
    types::Type,
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

    pub fn analyze(&mut self, program: &Program) -> Result<(), String> {
        for stmt in &program.nodes {
            self.statement(stmt)?;
        }

        Ok(())
    }

    fn statement(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let { name, ty, value } => {
                let explicit_type = ty.as_ref().map(|t| self.type_expression(t));

                let inferred_type = self.expression(value, explicit_type.as_ref())?;

                let final_type = match ty {
                    Some(type_expr) => {
                        let explicit_type = self.type_expression(type_expr);

                        if !inferred_type.is_assignable_to(&explicit_type) {
                            return Err(format!(
                                "Error: Type mismatch for variable '{}'. Cannot assign type {:?} to explicit type {:?}",
                                name, inferred_type, explicit_type
                            ));
                        }

                        explicit_type
                    }
                    None => inferred_type,
                };

                self.environment
                    .define(name.clone(), Symbol::Variable { ty: final_type })?;

                Ok(())
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
                    return_type: resolved_return,
                };

                self.environment.define(name.clone(), symbol)?;

                let current_env = std::mem::replace(&mut self.environment, Environment::new(None));
                let inner_env = Environment::new(Some(Box::new(current_env)));
                self.environment = inner_env;

                for (param_name, param_type) in resolved_params {
                    self.environment
                        .define(param_name, Symbol::Variable { ty: param_type })?;
                }

                if let Stmt::Block(statements) = &**body {
                    for s in statements {
                        self.statement(s)?;
                    }
                } else {
                    return Err(format!(
                        "Compiler Error: Function body for '{}' must be a block.",
                        name
                    ));
                }

                self.current_return_type = previous_return;

                if let Some(parent) = self.environment.parent.take() {
                    self.environment = *parent;
                } else {
                    self.environment = Environment::new(None)
                }

                Ok(())
            }
            Stmt::Return { value } => {
                let expected_type = match &self.current_return_type {
                    Some(ty) => ty.clone(),
                    None => {
                        return Err(format!("Error: Cannot use 'return' outside of a function."));
                    }
                };
                let actual_type = match value {
                    Some(expr) => self.expression(expr, Some(&expected_type))?,
                    None => Type::Void,
                };

                if !actual_type.is_assignable_to(&expected_type) {
                    return Err(format!(
                        "Type Error: Function return type mismatch. Expected {:?}, but got {:?}.",
                        expected_type, actual_type
                    ));
                }

                Ok(())
            }
            Stmt::Block(statements) => {
                let current_env = std::mem::replace(&mut self.environment, Environment::new(None));
                let inner_env = Environment::new(Some(Box::new(current_env)));
                self.environment = inner_env;

                for s in statements {
                    self.statement(s)?;
                }

                if let Some(parent) = self.environment.parent.take() {
                    self.environment = *parent;
                } else {
                    self.environment = Environment::new(None)
                }

                Ok(())
            }
            Stmt::Expr(expr) => {
                self.expression(expr, None)?;

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
                    Expr::Call { .. } => {}
                    _ => {}
                }

                Ok(())
            }
        }
    }

    fn expression(&self, expr: &Expr, expected: Option<&Type>) -> Result<Type, String> {
        match expr {
            Expr::Number(val) => {
                if val.fract() == 0.0 {
                    if let Some(expected_type) = expected {
                        if expected_type.is_integer() {
                            return Ok((*expected_type).clone());
                        }

                        if expected_type.is_float() {
                            return Ok((*expected_type).clone());
                        }
                    }
                    Ok(Type::I32) // Default fallback
                } else {
                    if let Some(expected_type) = expected {
                        if expected_type.is_float() {
                            return Ok((*expected_type).clone());
                        }
                    }
                    Ok(Type::F64) // Default fallback
                }
            }
            Expr::String(_) => Ok(Type::String),
            Expr::Bool(_) => Ok(Type::Bool),

            Expr::Identifier(name) => match self.environment.lookup(name) {
                Some(Symbol::Variable { ty }) => Ok(ty),
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
                let lhs = self.expression(left, expected)?;
                let rhs = self.expression(right, expected)?;

                match operator.kind {
                    TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                        if lhs == rhs && lhs.is_numeric() {
                            return Ok(lhs);
                        }

                        if lhs == Type::String
                            && rhs == Type::String
                            && operator.kind == TokenKind::Plus
                        {
                            return Ok(Type::String);
                        }

                        Err(format!(
                            "Error: Cannot apply operator '{}' to types {:?} and {:?}.",
                            operator.lexeme, lhs, rhs
                        ))
                    }
                    TokenKind::Less
                    | TokenKind::LessEqual
                    | TokenKind::Greater
                    | TokenKind::GreaterEqual => {
                        if lhs == rhs && lhs.is_numeric() {
                            return Ok(Type::Bool);
                        }

                        Err(format!(
                            "Type Error: Cannot compare types {:?} and {:?}.",
                            lhs, rhs
                        ))
                    }
                    TokenKind::EqualEqual | TokenKind::BangEqual => {
                        if lhs == rhs {
                            return Ok(Type::Bool);
                        }

                        Err(format!(
                            "Type Error: Cannot check equality between mismatched types {:?} and {:?}.",
                            lhs, rhs
                        ))
                    }
                    _ => todo!("Implement other binary operators"),
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

                            for (i, arg_expr) in arguments.iter().enumerate() {
                                let arg_type = self.expression(arg_expr, expected)?;
                                let (_, param_type) = &params[i];

                                if !arg_type.is_assignable_to(param_type) {
                                    return Err(format!(
                                        "Error: Argument mismatch at position {}. Expected {:?}, got {:?}.",
                                        i + 1,
                                        param_type,
                                        arg_type
                                    ));
                                }
                            }

                            Ok(return_type)
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
                let right_type = self.expression(right, expected)?;

                match operator.kind {
                    // Numeric negation
                    TokenKind::Minus => {
                        // You can negate signed integers and floats, but NOT unsigned integers!
                        if right_type.is_signed_integer() || right_type.is_float() {
                            Ok(right_type)
                        } else {
                            Err(format!(
                                "Type Error: Cannot apply unary '-' to non-signable type {:?}.",
                                right_type
                            ))
                        }
                    }
                    // Logical NOT
                    TokenKind::Bang => {
                        if right_type == Type::Bool {
                            Ok(Type::Bool)
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
