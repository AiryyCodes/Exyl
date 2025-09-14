#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,

    Identifier(String),

    NumberInt(i64),
    NumberFloat(f64),

    Equals,

    LParen,
    RParen,
    Semicolon,
    Comma,
}
