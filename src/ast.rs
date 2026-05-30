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
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: Span,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
        span: Span,
    },

    Block(Vec<Stmt>, Span),
    Expr(Expr, Span),

    Struct {
        name: String,
        fields: Vec<(String, TypeExpr)>,
        span: Span,
    },
    Impl {
        target: String,
        methods: Vec<Stmt>, // Vec of Stmt::Fun
        span: Span,
    },
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Let { span, .. } => *span,
            Stmt::Fun { span, .. } => *span,
            Stmt::Return { span, .. } => *span,
            Stmt::Block(_, span) => *span,
            Stmt::Expr(_, span) => *span,
            Stmt::If { span, .. } => *span,
            Stmt::While { span, .. } => *span,
            Stmt::Struct { span, .. } => *span,
            Stmt::Impl { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64, Span),
    String(String, Span),
    Bool(bool, Span),
    Char(char, Span),

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

    AddressOf(Box<Expr>, Span),
    Deref(Box<Expr>, Span),

    FieldAccess {
        object: Box<Expr>,
        field: String,
        span: Span,
    },
    FieldAssignment {
        object: Box<Expr>, // the struct
        field: String,
        value: Box<Expr>,
        span: Span,
    },
    StructLiteral {
        name: String,
        fields: Vec<(String, Expr)>, // named: Vec2 { x: 1.0, y: 2.0 }
        span: Span,
    },
    StaticCall {
        type_name: String,
        method: String,
        arguments: Vec<Expr>,
        span: Span,
    },

    // Arrays
    ArrayLiteral(Vec<Expr>, Span),
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    IndexAssignment {
        object: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Number(_, span) => *span,
            Expr::String(_, span) => *span,
            Expr::Char(_, span) => *span,
            Expr::Bool(_, span) => *span,
            Expr::Identifier(_, span) => *span,
            Expr::Call { span, .. } => *span,
            Expr::Assignment { span, .. } => *span,
            Expr::Error(_, span) => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::AddressOf(_, span) => *span,
            Expr::Deref(_, span) => *span,
            Expr::FieldAccess { span, .. } => *span,
            Expr::FieldAssignment { span, .. } => *span,
            Expr::StructLiteral { span, .. } => *span,
            Expr::StaticCall { span, .. } => *span,

            Expr::ArrayLiteral(_, span) => *span,
            Expr::Index { span, .. } => *span,
            Expr::IndexAssignment { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Primitive(String, Span),
    Named(String, Span),

    Array(Box<TypeExpr>, usize, Span),
    Pointer(Box<TypeExpr>, Span),
}

impl TypeExpr {
    pub fn span(&self) -> Span {
        match self {
            TypeExpr::Primitive(_, span) => *span,
            TypeExpr::Named(_, span) => *span,

            TypeExpr::Array(_, _, span) => *span,
            TypeExpr::Pointer(_, span) => *span,
        }
    }
}
