#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,
    Function,
    Return,
    Extern,

    If,
    Else,

    True,
    False,

    Identifier(String),

    NumberInt(i64),
    NumberFloat(f64),
    StringLiteral(String),

    Equals,
    Divide,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Colon,
    Semicolon,
    Comma,
}
