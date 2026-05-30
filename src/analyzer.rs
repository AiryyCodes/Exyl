use std::collections::HashMap;

use crate::{
    ast::{Expr, Program, Stmt, TypeExpr},
    environment::{Environment, Symbol},
    span::Span,
    token::TokenKind,
    types::{Type, TypedExpr, TypedProgram, TypedStmt},
};

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

pub struct SemanticAnalyzer {
    environment: Environment,
    current_return_type: Option<Type>,
    errors: Vec<TypeError>,
    struct_types: HashMap<String, Type>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(None),
            current_return_type: None,
            errors: vec![],
            struct_types: HashMap::new(),
        }
    }

    pub fn analyze(mut self, program: &Program) -> (TypedProgram, Vec<TypeError>) {
        let mut typed_nodes = vec![];

        for stmt in &program.nodes {
            match self.statement(stmt) {
                Ok(typed_stmt) => typed_nodes.push(typed_stmt),
                Err(err) => {
                    self.errors.push(err);
                }
            }
        }

        (TypedProgram { nodes: typed_nodes }, self.errors)
    }

    fn record_error(&mut self, message: String, span: Span) -> TypeError {
        TypeError { message, span }
    }

    fn statement(&mut self, stmt: &Stmt) -> Result<TypedStmt, TypeError> {
        match stmt {
            Stmt::Let {
                name,
                ty,
                value,
                span,
            } => {
                let explicit_type = ty.as_ref().map(|t| self.type_expression(t));

                // Fallback to expecting explicit type or none
                let typed_value = self.expression(value, explicit_type.as_ref())?;
                let inferred_type = typed_value.get_type();

                let final_type = match explicit_type {
                    Some(explicit) => {
                        if !inferred_type.is_assignable_to(&explicit) {
                            return Err(self.record_error(
                                format!(
                                    "Type Error: Type mismatch for variable '{}'. Cannot assign value type {:?} to explicit type {:?}",
                                    name, inferred_type, explicit
                                ),
                                value.span().clone(),
                            ));
                        }
                        explicit
                    }
                    None => inferred_type,
                };

                // Define in environment safely
                if let Err(env_err) = self.environment.define(
                    name.clone(),
                    Symbol::Variable {
                        ty: final_type.clone(),
                    },
                ) {
                    return Err(
                        self.record_error(format!("Scope Error: {}", env_err), span.clone())
                    );
                }

                Ok(TypedStmt::Let {
                    name: name.clone(),
                    ty: final_type,
                    value: typed_value,
                })
            }

            Stmt::Fun {
                name,
                parameters,
                is_variadic,
                return_type,
                is_extern,
                body,
                span,
            } => {
                let mut resolved_params = vec![];
                for (param_name, param_type) in parameters {
                    match self.try_resolve_type_expression(param_type) {
                        Ok(t) => resolved_params.push((param_name.clone(), t)),
                        Err(e) => return Err(e),
                    }
                }

                let resolved_return = match return_type {
                    Some(expr) => match self.try_resolve_type_expression(expr) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    },
                    None => Type::Void,
                };

                let previous_return = self.current_return_type.clone();
                self.current_return_type = Some(resolved_return.clone());

                let symbol = Symbol::Function {
                    params: resolved_params.clone(),
                    is_variadic: *is_variadic,
                    return_type: resolved_return.clone(),
                };

                if let Err(env_err) = self.environment.define(name.clone(), symbol) {
                    return Err(
                        self.record_error(format!("Scope Error: {}", env_err), span.clone())
                    );
                }

                let current_env = std::mem::replace(&mut self.environment, Environment::new(None));
                self.environment = Environment::new(Some(Box::new(current_env)));

                for (param_name, param_type) in &resolved_params {
                    if let Err(env_err) = self.environment.define(
                        param_name.clone(),
                        Symbol::Variable {
                            ty: param_type.clone(),
                        },
                    ) {
                        self.pop_scope();
                        return Err(self.record_error(
                            format!("Scope Parameter Error: {}", env_err),
                            span.clone(),
                        ));
                    }
                }

                let typed_body = match body.as_deref() {
                    Some(Stmt::Block(statements, _)) => {
                        let mut typed_statements = vec![];
                        for s in statements {
                            match self.statement(s) {
                                Ok(ts) => typed_statements.push(ts),
                                Err(err) => {
                                    self.errors.push(err);
                                }
                            }
                        }
                        Some(Box::new(TypedStmt::Block(typed_statements)))
                    }
                    Some(other_stmt) => {
                        self.pop_scope();
                        return Err(self.record_error(
                            format!("Semantic Error: Function body for '{}' must be an enclosed block statement layout.", name),
                            other_stmt.span().clone()
                        ));
                    }
                    None => None,
                };

                self.current_return_type = previous_return;
                self.pop_scope();

                Ok(TypedStmt::Fun {
                    name: name.clone(),
                    parameters: resolved_params,
                    is_variadic: *is_variadic,
                    return_type: resolved_return,
                    is_extern: *is_extern,
                    body: typed_body,
                })
            }

            Stmt::Return { value, span } => {
                let expected_type = match &self.current_return_type {
                    Some(ty) => ty.clone(),
                    None => {
                        return Err(self.record_error("Scope Error: Isolated 'return' context keyword found outside of function body limits.".to_string(), span.clone()));
                    }
                };

                let typed_value = match value {
                    Some(expr) => {
                        let ev_expr = self.expression(expr, Some(&expected_type))?;
                        let actual_type = ev_expr.get_type();
                        if !actual_type.is_assignable_to(&expected_type) {
                            return Err(self.record_error(
                                format!("Type Error: Return signature mismatch. Expected returning type {:?}, but processed value context type {:?}", expected_type, actual_type),
                                expr.span().clone(),
                            ));
                        }
                        Some(ev_expr)
                    }
                    None => {
                        if expected_type != Type::Void {
                            return Err(self.record_error(
                                format!("Type Error: Missing return value. Expected non-void typed data expression of type {:?}", expected_type),
                                span.clone(),
                            ));
                        }
                        None
                    }
                };

                Ok(TypedStmt::Return { value: typed_value })
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let typed_condition = self.expression(condition, None)?;

                if typed_condition.get_type() != Type::Bool {
                    return Err(TypeError {
                        message: format!(
                            "Type Error: 'if' condition statement must evaluate strictly to a 'bool' type. Found: {:?}",
                            typed_condition.get_type()
                        ),
                        span: condition.span().clone(),
                    });
                }

                let typed_then = self.statement(then_branch)?;

                let typed_else = match else_branch {
                    Some(else_stmt) => Some(Box::new(self.statement(else_stmt)?)),
                    None => None,
                };

                Ok(TypedStmt::If {
                    condition: typed_condition,
                    then_branch: Box::new(typed_then),
                    else_branch: typed_else,
                })
            }

            Stmt::While {
                condition, body, ..
            } => {
                let typed_condition = self.expression(condition, None)?;

                if typed_condition.get_type() != Type::Bool {
                    return Err(TypeError {
                        message: format!(
                            "Type Error: 'while' condition statement must evaluate strictly to a 'bool' type. Found: {:?}",
                            typed_condition.get_type()
                        ),
                        span: condition.span().clone(),
                    });
                }

                let typed_body = self.statement(body)?;

                Ok(TypedStmt::While {
                    condition: typed_condition,
                    body: Box::new(typed_body),
                })
            }

            Stmt::Block(statements, ..) => {
                let current_env = std::mem::replace(&mut self.environment, Environment::new(None));
                self.environment = Environment::new(Some(Box::new(current_env)));

                let mut typed_statements = vec![];
                for s in statements {
                    match self.statement(s) {
                        Ok(ts) => typed_statements.push(ts),
                        Err(err) => self.errors.push(err),
                    }
                }

                self.pop_scope();
                Ok(TypedStmt::Block(typed_statements))
            }

            Stmt::Expr(expr, _) => {
                let typed_expr = self.expression(expr, None)?;

                // Code impact analysis safety check triggers
                match expr {
                    Expr::Binary { operator, .. } => match operator.kind {
                        TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                            return Err(self.record_error(
                                format!("Warning Context: Statement has no computational side-effect. Result of operator '{}' is immediately discarded.", operator.lexeme),
                                expr.span().clone(),
                            ));
                        }
                        _ => {}
                    },
                    Expr::Number(_, span) | Expr::String(_, span) | Expr::Identifier(_, span) => {
                        return Err(self.record_error("Warning Context: Evaluation statement value has no state updates or downstream side-effect.".to_string(), span.clone()));
                    }
                    _ => {}
                }

                Ok(TypedStmt::Expr(typed_expr))
            }

            Stmt::Struct { name, fields, .. } => {
                let mut resolved_fields = vec![];

                for (field_name, field_type) in fields {
                    let ty = self.try_resolve_type_expression(field_type)?;
                    resolved_fields.push((field_name.clone(), ty));
                }
                let struct_type = Type::Struct {
                    name: name.clone(),
                    fields: resolved_fields,
                };
                self.struct_types.insert(name.clone(), struct_type.clone());
                Ok(TypedStmt::Struct {
                    name: name.clone(),
                    ty: struct_type,
                })
            }

            Stmt::Impl {
                target,
                methods,
                span,
            } => {
                let struct_ty = self.struct_types.get(target).cloned().ok_or_else(|| {
                    self.record_error(
                        format!("impl target '{}' is not a known struct", target),
                        *span,
                    )
                })?;

                let mut typed_methods = vec![];

                for method in methods {
                    if let Stmt::Fun {
                        name,
                        parameters,
                        return_type,
                        is_variadic,
                        body,
                        ..
                    } = method
                    {
                        let mangled = format!("{}::{}", target, name);

                        // Resolve params — replace self/const self with the actual struct type
                        let mut resolved_params: Vec<(String, Type)> = vec![];
                        for (param_name, param_type) in parameters {
                            let ty = if param_name == "self" || param_name == "const self" {
                                Type::Ref(Box::new(struct_ty.clone()))
                            } else {
                                self.try_resolve_type_expression(param_type)?
                            };
                            resolved_params.push((param_name.clone(), ty));
                        }

                        let resolved_return = match return_type {
                            Some(t) => self.try_resolve_type_expression(t)?,
                            None => Type::Void,
                        };

                        // Register mangled name in environment BEFORE typechecking body
                        // so recursive calls work
                        self.environment
                            .define(
                                mangled.clone(),
                                Symbol::Function {
                                    params: resolved_params.clone(),
                                    is_variadic: *is_variadic,
                                    return_type: resolved_return.clone(),
                                },
                            )
                            .map_err(|e| self.record_error(e, *span))?;

                        // Push a new scope for the method body
                        let previous_return = self.current_return_type.clone();
                        self.current_return_type = Some(resolved_return.clone());

                        let current_env =
                            std::mem::replace(&mut self.environment, Environment::new(None));
                        self.environment = Environment::new(Some(Box::new(current_env)));

                        // Register params into scope (self becomes a variable too)
                        for (param_name, param_type) in &resolved_params {
                            if let Err(e) = self.environment.define(
                                param_name.clone(),
                                Symbol::Variable {
                                    ty: param_type.clone(),
                                },
                            ) {
                                self.pop_scope();
                                return Err(self.record_error(e, *span));
                            }
                        }

                        // Typecheck body
                        let typed_body = match body.as_deref() {
                            Some(Stmt::Block(statements, _)) => {
                                let mut typed_statements = vec![];
                                for s in statements {
                                    match self.statement(s) {
                                        Ok(ts) => typed_statements.push(ts),
                                        Err(err) => self.errors.push(err),
                                    }
                                }
                                Some(Box::new(TypedStmt::Block(typed_statements)))
                            }
                            Some(other) => {
                                self.pop_scope();
                                return Err(self.record_error(
                                    format!("Method '{}' body must be a block", name),
                                    other.span(),
                                ));
                            }
                            None => None,
                        };

                        self.current_return_type = previous_return;
                        self.pop_scope();

                        typed_methods.push(TypedStmt::Fun {
                            name: mangled, // ← store mangled name so LLVM emits it correctly
                            parameters: resolved_params,
                            is_variadic: *is_variadic,
                            return_type: resolved_return,
                            is_extern: false,
                            body: typed_body,
                        });
                    }
                }

                Ok(TypedStmt::Impl {
                    target: target.clone(),
                    methods: typed_methods,
                })
            }
        }
    }

    fn expression(&mut self, expr: &Expr, expected: Option<&Type>) -> Result<TypedExpr, TypeError> {
        match expr {
            Expr::Number(val, _) => {
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

            Expr::String(val, _) => Ok(TypedExpr::String(val.clone())),
            Expr::Char(val, _) => Ok(TypedExpr::Char(*val)),

            Expr::Bool(val, _) => Ok(TypedExpr::Bool(*val)),

            Expr::Identifier(name, span) => match self.environment.lookup(name) {
                Some(Symbol::Variable { ty }) => Ok(TypedExpr::Identifier(name.clone(), ty)),
                Some(Symbol::Function { .. }) => Err(self.record_error(
                    format!("Type Error: Identified label '{}' references a function, expected evaluation symbol variable here.", name),
                    span.clone(),
                )),
                None => Err(self.record_error(
                    format!("Symbol Error: Accessing undefined variable identifier allocation '{}'.", name),
                    span.clone(),
                )),
            },

            Expr::Assignment { name, value, span } => {
                let target_type = match self.environment.lookup(name) {
                    Some(Symbol::Variable { ty }) => ty.clone(),
                    Some(Symbol::Function { .. }) => {
                        return Err(self.record_error(
                            format!("Type Error: Forbidden operation assignment target '{}' is allocated immutably as a standalone function descriptor.", name),
                            span.clone(),
                        ));
                    }
                    None => {
                        return Err(self.record_error(
                            format!("Scope Error: Cannot assign value to unallocated or undeclared target name space reference '{}'.", name),
                            span.clone(),
                        ));
                    }
                };

                let typed_value = self.expression(value, Some(&target_type))?;
                let value_type = typed_value.get_type();

                if !value_type.is_assignable_to(&target_type) {
                    return Err(self.record_error(
                        format!("Type Error: Bad assignment constraints for identifier '{}'. Cannot safe cast value assignment of type {:?} directly into destination type {:?}.", name, value_type, target_type),
                        value.span().clone(),
                    ));
                }

                Ok(TypedExpr::Assignment {
                    name: name.clone(),
                    value: Box::new(typed_value),
                    ty: Type::Void,
                })
            }

            Expr::Binary { left, right, operator, span } => {
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
                    None => Err(self.record_error(
                        format!("Type Error: The validation evaluation operator '{}' is not standardly supported between type instances {:?} and {:?}", operator.lexeme, lhs_type, rhs_type),
                        span.clone(),
                    )),
                }
            }

            Expr::Call { callee, arguments, span } => {
                if let Expr::FieldAccess { object, field, .. } = &**callee {
                    let typed_obj = self.expression(object, None)?;
                    let obj_type = typed_obj.get_type();

                    let struct_name = match &obj_type {
                        Type::Struct { name, .. } => name.clone(),
                        other => return Err(self.record_error(
                            format!("Type Error: Cannot call method '{}' on non-struct type {:?}", field, other),
                            *span,
                        )),
                    };

                    let mangled = format!("{}::{}", struct_name, field);

                    match self.environment.lookup(&mangled) {
                        Some(Symbol::Function { params, is_variadic, return_type }) => {
                            // params[0] is self — skip it for argument count check
                            let user_params = if params.first()
                                .map(|(n, _)| n == "self" || n == "const self")
                                .unwrap_or(false)
                            {
                                &params[1..]
                            } else {
                                &params[..]
                            };

                            if !is_variadic && user_params.len() != arguments.len() {
                                return Err(self.record_error(
                                    format!("Call Error: Method '{}' expects {} arguments, got {}",
                                        field, user_params.len(), arguments.len()),
                                    *span,
                                ));
                            }

                            let mut typed_arguments = vec![];
                            for (i, arg) in arguments.iter().enumerate() {
                                let expected_ty = user_params.get(i).map(|(_, t)| t);
                                let typed_arg = self.expression(arg, expected_ty)?;
                                typed_arguments.push(typed_arg);
                            }

                            return Ok(TypedExpr::MethodCall {
                                mangled_name: mangled,
                                self_arg: Box::new(typed_obj),
                                arguments: typed_arguments,
                                return_type,
                            });
                        }
                        _ => return Err(self.record_error(
                            format!("Type Error: No method '{}' on struct '{}'", field, struct_name),
                            *span,
                        )),
                    }
                }
                
                if let Expr::Identifier(func_name, id_span) = &**callee {
                    match self.environment.lookup(func_name) {
                        Some(Symbol::Function { params, is_variadic, return_type }) => {
                            if is_variadic {
                                if arguments.len() < params.len() {
                                    return Err(self.record_error(
                                        format!("Call Parameter Error: Variadic function pipeline execution '{}' expects minimum argument count of {} but only populated {} elements.", func_name, params.len(), arguments.len()),
                                        span.clone(),
                                    ));
                                }
                            } else if params.len() != arguments.len() {
                                return Err(self.record_error(
                                    format!("Call Parameter Error: Function processing for target '{}' expects exactly {} bounds parameters, but evaluated count is {}.", func_name, params.len(), arguments.len()),
                                    span.clone(),
                                ));
                            }

                            let mut typed_arguments = vec![];
                            for i in 0..params.len() {
                                let arg_expr = &arguments[i];
                                let (_, param_type) = &params[i];
                                let typed_arg = self.expression(arg_expr, Some(param_type))?;

                                if !typed_arg.get_type().is_assignable_to(param_type) {
                                    return Err(self.record_error(
                                        format!("Call Mismatch Error: Parameter validation mapping at position {} failed. Intended target type expects {:?}, but processed evaluation yielded type {:?}.", i + 1, param_type, typed_arg.get_type()),
                                        arg_expr.span().clone(),
                                    ));
                                }
                                typed_arguments.push(typed_arg);
                            }

                            if is_variadic {
                                for i in params.len()..arguments.len() {
                                    let arg_expr = &arguments[i];
                                    let typed_arg = self.expression(arg_expr, None)?;
                                    typed_arguments.push(typed_arg);
                                }
                            }

                            let typed_callee = TypedExpr::Identifier(func_name.clone(), return_type.clone());

                            Ok(TypedExpr::Call {
                                callee: Box::new(typed_callee),
                                arguments: typed_arguments,
                                return_type,
                            })
                        }
                        _ => Err(self.record_error(
                            format!("Invocation Error: Checked expression descriptor path target '{}' matches a non-callable environment primitive variable reference.", func_name),
                            id_span.clone(),
                        )),
                    }
                } else {
                    Err(self.record_error("Unsupported Feature: Evaluation mapping across complex computed functional lookup calls is currently un-implemented.".to_string(), callee.span().clone()))
                }
            }

            Expr::Unary { operator, right, span } => {
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
                            Err(self.record_error(
                                format!("Type Error: Unary negation '-' rule restrictions cannot process transformation on un-signable data type: {:?}", right_type),
                                span.clone(),
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
                            Err(self.record_error(
                                format!("Type Error: Negation evaluation logic symbol '!' cannot be forced upon non-boolean type representation: {:?}", right_type),
                                span.clone(),
                            ))
                        }
                    }
                    _ => Err(self.record_error("Syntax Error: Unknown unary transformation operator instruction matched parsing layer.".to_string(), span.clone())),
                }
            }

            Expr::Error(msg, span) => Err(self.record_error(format!("Parser Fallback Error: {}", msg), span.clone())),

            Expr::AddressOf(inner, _) => {
                let typed_inner = self.expression(inner, None)?;
                let inner_type = typed_inner.get_type().clone();
                Ok(TypedExpr::AddressOf(Box::new(typed_inner), Type::Ref(Box::new(inner_type))))
            },
            Expr::Deref(inner, span) => {
                let typed_inner = self.expression(inner, None)?;
                match typed_inner.get_type() {
                    Type::Ref(inner_type) => Ok(TypedExpr::Deref(
                        Box::new(typed_inner),
                        *inner_type,
                    )),
                    other => Err(self.record_error(
                        format!("Type Error: Cannot dereference non-pointer type {:?}", other),
                        span.clone(),
                    )),
                }
            },

            Expr::FieldAccess { object, field, span } => {
                let typed_obj = self.expression(object, None)?;
                let obj_type = typed_obj.get_type();

                let inner_type = match &obj_type {
                    Type::Ref(inner) => *inner.clone(),
                    other => other.clone(),
                };


                match &inner_type {
                    Type::Struct { fields, .. } => {
                        let found = fields.iter().enumerate()
                            .find(|(_, (name, _))| name == field);
                        match found {
                            Some((idx, (_, field_ty))) => Ok(TypedExpr::FieldAccess {
                                object: Box::new(typed_obj),
                                field: field.clone(),
                                field_index: idx,
                                ty: field_ty.clone(),
                            }),
                            None => Err(self.record_error(
                                format!("Type Error: No field '{}' on struct", field),
                                *span,
                            )),
                        }
                    }
                    other => Err(self.record_error(
                        format!("Type Error: Cannot access field '{}' on non-struct type {:?}", field, other),
                        *span,
                    )),
                }
            },
            Expr::FieldAssignment { object, field, value, span } => {
                let typed_obj = self.expression(object, None)?;
                let obj_type = typed_obj.get_type();

                let inner_type = match &obj_type {
                    Type::Ref(inner) => *inner.clone(),
                    other => other.clone(),
                };
                
                let (field_index, field_ty) = match &inner_type {
                    Type::Struct { .. } => {
                        inner_type.get_field(field).ok_or_else(|| self.record_error(
                            format!("Type Error: No field '{}' on struct", field),
                            *span,
                        ))?
                    }
                    other => return Err(self.record_error(
                        format!("Type Error: Cannot assign field on non-struct type {:?}", other),
                        *span,
                    )),
                };

                let field_ty = field_ty.clone();
                let typed_val = self.expression(value, Some(&field_ty))?;

                if !typed_val.get_type().is_assignable_to(&field_ty) {
                    return Err(self.record_error(
                        format!("Type Error: Cannot assign {:?} to field '{}' of type {:?}",
                            typed_val.get_type(), field, field_ty),
                        value.span(),
                    ));
                }

                // Flatten to object name — only identifiers supported for now
                let object_name = match object.as_ref() {
                    Expr::Identifier(name, _) => name.clone(),
                    _ => return Err(self.record_error(
                        "Field assignment only supported on direct variables for now.".to_string(),
                        *span,
                    )),
                };

                Ok(TypedExpr::FieldAssignment {
                    object_name,
                    field_index,
                    value: Box::new(typed_val),
                    object_ty: inner_type,
                })
            }

            Expr::StructLiteral { name, fields, span } => {
                let struct_ty = self.struct_types.get(name).cloned()
                    .ok_or_else(|| self.record_error(
                        format!("Type Error: Unknown struct '{}'", name),
                        *span,
                    ))?;

                let declared_fields = match &struct_ty {
                    Type::Struct { fields, .. } => fields.clone(),
                    _ => unreachable!(),
                };

                let mut typed_fields = vec![];
                for (field_name, field_expr) in fields {
                    let declared_type = declared_fields.iter()
                        .find(|(n, _)| n == field_name)
                        .map(|(_, t)| t.clone())
                        .ok_or_else(|| self.record_error(
                            format!("Type Error: No field '{}' on struct '{}'", field_name, name),
                            *span,
                        ))?;
                    let typed_val = self.expression(field_expr, Some(&declared_type))?;
                    if !typed_val.get_type().is_assignable_to(&declared_type) {
                        return Err(self.record_error(
                            format!("Type Error: Field '{}' expects {:?}, got {:?}", field_name, declared_type, typed_val.get_type()),
                            field_expr.span(),
                        ));
                    }
                    typed_fields.push((field_name.clone(), typed_val));
                }

                Ok(TypedExpr::StructLiteral {
                    name: name.clone(),
                    fields: typed_fields,
                    ty: struct_ty,
                })
            }

            Expr::StaticCall { type_name, method, arguments, span } => {
                let mangled = format!("{}::{}", type_name, method);

                match self.environment.lookup(&mangled) {
                    Some(Symbol::Function { params, is_variadic, return_type }) => {
                        if !is_variadic && params.len() != arguments.len() {
                            return Err(self.record_error(
                                format!("Call Error: '{}::{}' expects {} arguments, got {}",
                                    type_name, method, params.len(), arguments.len()),
                                *span,
                            ));
                        }

                        let mut typed_arguments = vec![];
                        for (i, arg) in arguments.iter().enumerate() {
                            let expected_ty = params.get(i).map(|(_, t)| t);
                            let typed_arg = self.expression(arg, expected_ty)?;
                            typed_arguments.push(typed_arg);
                        }

                        Ok(TypedExpr::StaticCall {
                            mangled_name: mangled,
                            arguments: typed_arguments,
                            return_type,
                        })
                    }
                    _ => Err(self.record_error(
                        format!("Type Error: No static method '{}' on type '{}'", method, type_name),
                        *span,
                    )),
                }
            }

            Expr::ArrayLiteral(elements, _) => {
                let elem_hint = match expected {
                    Some(Type::Array(elem_ty, _)) => Some(elem_ty.as_ref()),
                    _ => None,
                };
                let mut typed_elems = vec![];
                for e in elements {
                    typed_elems.push(self.expression(e, elem_hint)?);
                }
                let elem_ty = typed_elems.first()
                    .map(|e| e.get_type())
                    .unwrap_or(Type::I32);
                let len = typed_elems.len();
                Ok(TypedExpr::ArrayLiteral {
                    elements: typed_elems,
                    ty: Type::Array(Box::new(elem_ty), len),
                })
            }

            Expr::Index { object, index, span } => {
                let typed_obj = self.expression(object, None)?;
                let typed_idx = self.expression(index, Some(&Type::I64))?;
                match typed_obj.get_type() {
                    Type::Array(elem_ty, _) => Ok(TypedExpr::Index {
                        object: Box::new(typed_obj),
                        index: Box::new(typed_idx),
                        ty: *elem_ty,
                    }),
                    other => Err(self.record_error(
                        format!("Type Error: Cannot index into {:?}", other),
                        *span,
                    )),
                }
            }

            Expr::IndexAssignment { object, index, value, span } => {
                let typed_obj = self.expression(object, None)?;
                let elem_ty = match typed_obj.get_type() {
                    Type::Array(elem_ty, _) => *elem_ty,
                    other => return Err(self.record_error(
                        format!("Type Error: Cannot index into {:?}", other),
                        *span,
                    )),
                };
                let typed_idx = self.expression(index, Some(&Type::I64))?;
                let typed_val = self.expression(value, Some(&elem_ty))?;
                if !typed_val.get_type().is_assignable_to(&elem_ty) {
                    return Err(self.record_error(
                        format!("Type Error: Cannot assign {:?} to array of {:?}", typed_val.get_type(), elem_ty),
                        *span,
                    ));
                }
                let object_name = match object.as_ref() {
                    Expr::Identifier(name, _) => name.clone(),
                    _ => return Err(self.record_error(
                        "Index assignment only supported on direct variables for now.".to_string(),
                        *span,
                    )),
                };
                Ok(TypedExpr::IndexAssignment {
                    object_name,
                    index: Box::new(typed_idx),
                    value: Box::new(typed_val),
                    elem_ty,
                })
            }
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
            TokenKind::AmpersandAmpersand | TokenKind::PipePipe => {
                if lhs == &Type::Bool && rhs == &Type::Bool {
                    return Some(Type::Bool);
                }
                None
            }
            _ => None,
        }
    }

    fn type_expression(&self, expr: &TypeExpr) -> Type {
        match self.try_resolve_type_expression(expr) {
            Ok(t) => t,
            Err(e) => panic!(
                "Internal Compiler Core Mismatch Fatal Incident: {}",
                e.message
            ),
        }
    }

    fn try_resolve_type_expression(&self, expr: &TypeExpr) -> Result<Type, TypeError> {
        match expr {
            TypeExpr::Primitive(name, span) => match name.as_str() {
                "i8" => Ok(Type::I8),
                "i16" => Ok(Type::I16),
                "i32" => Ok(Type::I32),
                "i64" => Ok(Type::I64),
                "u8" => Ok(Type::U8),
                "u16" => Ok(Type::U16),
                "u32" => Ok(Type::U32),
                "u64" => Ok(Type::U64),
                "f32" => Ok(Type::F32),
                "f64" => Ok(Type::F64),
                "bool" => Ok(Type::Bool),
                "string" => Ok(Type::String),
                "void" => Ok(Type::Void),
                "char" => Ok(Type::Char),
                _ => Err(TypeError {
                    message: format!(
                        "Type Validation Error: The token identifier descriptor reference '{}' does not match a valid compiler core primitive variant type choice.",
                        name
                    ),
                    span: span.clone(),
                }),
            },
            TypeExpr::Named(name, span) => {
                // "Self" is resolved during impl processing, shouldn't appear here raw
                if name == "Self" {
                    return Err(TypeError {
                        message: "'Self' can only be used inside an impl block".to_string(),
                        span: *span,
                    });
                }
                self.struct_types
                    .get(name)
                    .cloned()
                    .ok_or_else(|| TypeError {
                        message: format!("Type Error: Unknown type '{}'", name),
                        span: *span,
                    })
            },

            TypeExpr::Array(elem, size, _) => {
                let elem_ty = self.try_resolve_type_expression(elem)?;
                Ok(Type::Array(Box::new(elem_ty), *size))
            }
        }
    }

    /// Safely pops the current environment block stack frame back into its parent container
    fn pop_scope(&mut self) {
        if let Some(parent) = self.environment.parent.take() {
            self.environment = *parent;
        } else {
            self.environment = Environment::new(None);
        }
    }
}
