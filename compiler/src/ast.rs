#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let { name: String, value: Expr },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    NumberInt(i64),
    NumberFloat(f64),
    Identifier(String),
    FunctionCall { name: String, args: Vec<Expr> },
}
