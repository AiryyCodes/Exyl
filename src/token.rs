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
    LeftBracket,
    RightBracket,
    Semicolon,
    Comma,

    // Literals
    Number,
    String,

    // Keywords
    Let,
    Fun,

    // Other
    Identifier,
    EndOfFile,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
}
