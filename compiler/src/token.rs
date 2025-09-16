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

    // single-char
    Plus,
    Minus,
    Star,
    Divide,
    Modulo,

    Equals,  // =
    Less,    // <
    Greater, // >
    Bang,    // !

    // two-char
    EqualEqual,   // ==
    BangEqual,    // !=
    LessEqual,    // <=
    GreaterEqual, // >=
    AndAnd,
    OrOr,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Colon,
    Semicolon,
    Comma,
}
