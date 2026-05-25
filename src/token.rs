#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Symbols
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Star,
    StarEqual,
    Slash,
    SlashEqual,
    Bang,
    BangEqual,
    LeftParen,
    RightParen,
    Semicolon,

    // Literals
    Number,
    String,

    // Keywords
    Let,

    // Other
    Identifier,
    EndOfFile,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
}
