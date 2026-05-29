use crate::{span::Span, token::Token};

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
        span: Span,
    },
    Fun {
        name: String,
        parameters: Vec<(String, TypeExpr)>,
        is_variadic: bool,
        return_type: Option<TypeExpr>,
        is_extern: bool,
        body: Option<Box<Stmt>>,
        span: Span,
    },
    Return {
        value: Option<Expr>,
        span: Span,
    },
    Block(Vec<Stmt>, Span),
    Expr(Expr, Span),
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Let { span, .. } => *span,
            Stmt::Fun { span, .. } => *span,
            Stmt::Return { span, .. } => *span,
            Stmt::Block(_, span) => *span,
            Stmt::Expr(_, span) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64, Span),
    String(String, Span),
    Bool(bool, Span),
    Identifier(String, Span),
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
        span: Span,
    },
    Assignment {
        name: String,
        value: Box<Expr>,
        span: Span,
    },
    Error(String, Span),
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        operator: Token,
        span: Span,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Number(_, span) => *span,
            Expr::String(_, span) => *span,
            Expr::Bool(_, span) => *span,
            Expr::Identifier(_, span) => *span,
            Expr::Call { span, .. } => *span,
            Expr::Assignment { span, .. } => *span,
            Expr::Error(_, span) => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Primitive(String, Span),
}

impl TypeExpr {
    pub fn span(&self) -> Span {
        match self {
            TypeExpr::Primitive(_, span) => *span,
        }
    }
}
