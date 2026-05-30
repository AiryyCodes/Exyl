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
    ColonColon,
    Semicolon,
    Comma,
    Ellipsis,
    Ampersand,
    AmpersandAmpersand,
    PipePipe,
    Dot,

    // Literals
    Number,
    String,
    Char,

    // Keywords
    Let,
    Const,
    Fun,
    Return,
    True,
    False,
    Extern,
    If,
    Else,
    While,
    Struct,
    Impl,
    SelfKw,

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
