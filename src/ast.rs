use crate::token::Token;

#[derive(Debug)]
pub struct Program {
    pub nodes: Vec<Stmt>,
}

impl Program {
    pub fn new(nodes: Vec<Stmt>) -> Self {
        Self { nodes }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<TypeExpr>,
        value: Expr,
    },
    Fun {
        name: String,
        parameters: Vec<(String, TypeExpr)>,
        return_type: Option<TypeExpr>,
        is_extern: bool,
        body: Option<Box<Stmt>>,
    },
    Return {
        value: Option<Expr>,
    },

    Block(Vec<Stmt>),

    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    String(String),
    Bool(bool),

    Identifier(String),
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },

    Error(String),

    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        operator: Token,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Primitive(String),
}
