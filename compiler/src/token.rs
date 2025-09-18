#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // --- keywords ---
    Let,
    Function,
    Return,
    Extern,
    If,
    Else,
    True,
    False,

    // --- literals & identifiers ---
    Identifier(String),
    NumberInt(i64),
    NumberFloat(f64),
    StringLiteral(String),

    // --- arithmetic (single-char operators) ---
    Plus,
    Minus,
    Star,
    Divide,
    Modulo,

    // --- comparisons, assignment, logical ---
    Equals,       // =
    Less,         // <
    Greater,      // >
    Bang,         // !
    EqualEqual,   // ==
    BangEqual,    // !=
    LessEqual,    // <=
    GreaterEqual, // >=
    AndAnd,       // &&
    OrOr,         // ||

    // --- delimiters ---
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // --- punctuation ---
    Colon,
    Semicolon,
    Comma,
    Dot,
    DotDotDot, // Variadic arguments
}
