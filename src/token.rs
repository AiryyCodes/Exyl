#[derive(Debug)]
pub enum TokenKind {
    // Symbols
    Equals,
    Semicolon,

    // Literals
    Number,

    // Keywords
    Let,

    // Other
    Identifier,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
}
