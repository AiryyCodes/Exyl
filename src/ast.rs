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

#[derive(Debug)]
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
        body: Box<Stmt>,
    },

    Block(Vec<Stmt>),

    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    String(String),

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

#[derive(Debug)]
pub enum TypeExpr {
    Primitive(String),
}
