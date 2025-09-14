#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,

    Identifier(String),

    NumberInt(i64),
    NumberFloat(f64),
    StringLiteral(String),

    Equals,

    LParen,
    RParen,

    Colon,
    Semicolon,
    Comma,
}
