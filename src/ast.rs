use crate::token::Token;

#[derive(Debug)]
pub struct Program {
    pub nodes: Vec<Node>,
}

impl Program {
    pub fn new(nodes: Vec<Node>) -> Self {
        Self { nodes }
    }
}

#[derive(Debug)]
pub enum Node {
    Statement(Stmt),
}

#[derive(Debug)]
pub enum Stmt {
    Let { name: String, value: Expr },

    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    String(String),

    Variable(String),

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
