#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<Type>,
        value: Expr,
    },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    NumberInt(i64),
    NumberFloat(f64),
    StringLiteral(String),
    Identifier(String),
    FunctionCall { name: String, args: Vec<Expr> },

    Typed(Box<Expr>, Type),
}

impl Expr {
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expr::NumberInt(_) => Some(Type::I64),
            Expr::NumberFloat(_) => Some(Type::F64),
            Expr::StringLiteral(_) => Some(Type::String),
            Expr::Identifier(_) => None, // only known after type check
            Expr::FunctionCall { .. } => None, // only known after type check
            Expr::Typed(_, ty) => Some(ty.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I64,
    F64,
    String,
    Void,
}
