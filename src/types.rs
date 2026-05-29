use crate::{
    codegen::{BuilderBackend, Emit},
    token::TokenKind,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    String,
    Void,
}

impl Type {
    /// Checks if this type is assignable to another
    pub fn is_assignable_to(&self, target: &Type) -> bool {
        match (self, target) {
            (source, target) => source == target,
        }
    }

    pub fn is_signed_integer(&self) -> bool {
        matches!(self, Type::I8 | Type::I16 | Type::I32 | Type::I64)
    }

    pub fn is_unsigned_integer(&self) -> bool {
        matches!(self, Type::U8 | Type::U16 | Type::U32 | Type::U64)
    }

    pub fn is_integer(&self) -> bool {
        self.is_signed_integer() || self.is_unsigned_integer()
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }

    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }
}

#[derive(Debug)]
pub struct TypedProgram {
    pub nodes: Vec<TypedStmt>,
}

#[derive(Debug)]
pub enum TypedStmt {
    Let {
        name: String,
        ty: Type,
        value: TypedExpr,
    },
    Fun {
        name: String,
        parameters: Vec<(String, Type)>,
        return_type: Type,
        is_extern: bool,
        body: Option<Box<TypedStmt>>, // Must be a TypedStmt::Block
    },
    Return {
        value: Option<TypedExpr>,
    },
    Block(Vec<TypedStmt>),
    Expr(TypedExpr),
}

impl<B: BuilderBackend> Emit<B> for TypedStmt {
    type Output = ();

    fn emit(&self, backend: &mut B) -> Self::Output {
        match self {
            TypedStmt::Let { name, ty, value } => {
                let val_code = value.emit(backend);
                backend.build_alloca(name, ty);
                backend.build_store(name, val_code);
            }

            TypedStmt::Expr(expr) => {
                expr.emit(backend);
            }

            TypedStmt::Block(statements) => {
                for stmt in statements {
                    stmt.emit(backend);
                }
            }

            TypedStmt::Return { value } => {
                let llvm_val = value.as_ref().map(|v| v.emit(backend));
                backend.build_return(llvm_val);
            }

            TypedStmt::Fun {
                name,
                parameters,
                return_type,
                is_extern,
                body,
            } => {
                let param_values =
                    backend.begin_function(name, parameters, *is_extern, return_type);

                if *is_extern {
                    return;
                }

                for ((param_name, param_type), raw_value) in parameters.iter().zip(param_values) {
                    backend.build_alloca(param_name, param_type);
                    backend.build_store(param_name, raw_value);
                }

                if let Some(body) = body {
                    body.emit(backend);
                }

                if !backend.is_block_terminated() {
                    match return_type {
                        Type::Void => backend.build_return(None),
                        _ => panic!("Function '{}' is missing a return statement", name),
                    }
                }

                backend.end_function();
            }
        }
    }
}

#[derive(Debug)]
pub enum TypedExpr {
    Number(f64, Type),
    String(String),
    Bool(bool),
    Identifier(String, Type),
    Binary {
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
        operator: TokenKind,
        ty: Type,
    },
    Call {
        callee: Box<TypedExpr>,
        arguments: Vec<TypedExpr>,
        return_type: Type,
    },
    Unary {
        operator: TokenKind,
        right: Box<TypedExpr>,
        ty: Type,
    },
}

impl TypedExpr {
    pub fn get_type(&self) -> Type {
        match self {
            TypedExpr::Number(_, ty) => ty.clone(),
            TypedExpr::Identifier(_, ty) => ty.clone(),
            TypedExpr::Binary { ty, .. } => ty.clone(),
            TypedExpr::Unary { ty, .. } => ty.clone(),
            TypedExpr::Call { return_type, .. } => return_type.clone(),

            TypedExpr::String(_) => Type::String,
            TypedExpr::Bool(_) => Type::Bool,
        }
    }
}

impl<B: BuilderBackend> Emit<B> for TypedExpr {
    type Output = B::Value;

    fn emit(&self, backend: &mut B) -> Self::Output {
        match self {
            TypedExpr::Number(val, ty) => backend.const_number(*val, ty),
            TypedExpr::String(val) => backend.const_string(val.clone()),
            TypedExpr::Bool(val) => backend.const_bool(*val),

            TypedExpr::Identifier(name, ty) => backend.build_load(name, ty),

            TypedExpr::Binary {
                left,
                right,
                operator,
                ty,
            } => {
                let lhs = left.emit(backend);
                let rhs = right.emit(backend);

                match operator {
                    TokenKind::Plus => backend.build_add(lhs, rhs, ty),
                    TokenKind::Minus => backend.build_sub(lhs, rhs, ty),
                    TokenKind::Star => backend.build_mul(lhs, rhs, ty),
                    TokenKind::Slash => backend.build_div(lhs, rhs, ty),
                    TokenKind::EqualEqual => backend.build_eq(lhs, rhs, ty),
                    TokenKind::BangEqual => backend.build_neq(lhs, rhs, ty),
                    TokenKind::Less => backend.build_lt(lhs, rhs, ty),
                    TokenKind::LessEqual => backend.build_lte(lhs, rhs, ty),
                    TokenKind::Greater => backend.build_gt(lhs, rhs, ty),
                    TokenKind::GreaterEqual => backend.build_gte(lhs, rhs, ty),
                    _ => unreachable!(),
                }
            }
            TypedExpr::Unary {
                operator,
                right,
                ty,
            } => {
                let val = right.emit(backend);
                match operator {
                    TokenKind::Bang => backend.build_not(val),
                    TokenKind::Minus => backend.build_neg(val, ty),
                    _ => panic!("Invalid unary operator"),
                }
            }
            TypedExpr::Call {
                callee,
                arguments,
                return_type,
            } => {
                let name = match &**callee {
                    TypedExpr::Identifier(name, _) => name,
                    _ => panic!("Indirect function calls are not supported."),
                };

                let args_compiled: Vec<B::Value> =
                    arguments.iter().map(|arg| arg.emit(backend)).collect();

                backend.build_call(name, args_compiled, return_type)
            }
        }
    }
}
