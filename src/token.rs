use crate::span::Span;

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
    LeftBrace,
    RightBrace,
    Colon,
    Semicolon,
    Comma,
    Ellipsis,

    // Literals
    Number,
    String,

    // Keywords
    Let,
    Fun,
    Return,
    True,
    False,
    Extern,
    If,
    Else,

    // Other
    Identifier,
    EndOfFile,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}
