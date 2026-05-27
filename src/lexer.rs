use crate::token::{Token, TokenKind};

pub struct Lexer {
    current: usize,
    start: usize,
    source: Vec<char>,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Self {
            current: 0,
            start: 0,
            source: source.chars().collect(),
        }
    }

    pub fn analyze(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];

        while !self.is_at_end() {
            self.start = self.current;

            let c = self.advance();

            match c {
                '=' => {
                    if self.peek() == '=' {
                        // Consume '='
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::EqualEqual);
                    } else {
                        self.add_token(&mut tokens, TokenKind::Equal);
                    }
                }
                '!' => {
                    if self.peek() == '=' {
                        // Consume '='
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::BangEqual);
                    } else {
                        self.add_token(&mut tokens, TokenKind::Bang);
                    }
                }
                '>' => {
                    if self.peek() == '=' {
                        // Consume '='
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::GreaterEqual);
                    } else {
                        self.add_token(&mut tokens, TokenKind::Greater);
                    }
                }
                '<' => {
                    if self.peek() == '=' {
                        // Consume '='
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::LessEqual);
                    } else {
                        self.add_token(&mut tokens, TokenKind::Less);
                    }
                }
                '+' => {
                    if self.peek() == '=' {
                        // Consume '='
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::PlusEqual);
                    } else {
                        self.add_token(&mut tokens, TokenKind::Plus);
                    }
                }
                '-' => {
                    if self.peek() == '=' {
                        // Consume '='
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::MinusEqual);
                    } else {
                        self.add_token(&mut tokens, TokenKind::Minus);
                    }
                }
                '*' => {
                    if self.peek() == '=' {
                        // Consume '='
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::StarEqual);
                    } else {
                        self.add_token(&mut tokens, TokenKind::Star);
                    }
                }

                '(' => self.add_token(&mut tokens, TokenKind::LeftParen),
                ')' => self.add_token(&mut tokens, TokenKind::RightParen),
                '{' => self.add_token(&mut tokens, TokenKind::LeftBracket),
                '}' => self.add_token(&mut tokens, TokenKind::RightBracket),

                ':' => self.add_token(&mut tokens, TokenKind::Colon),
                ';' => self.add_token(&mut tokens, TokenKind::Semicolon),
                ',' => self.add_token(&mut tokens, TokenKind::Comma),

                '"' => {
                    self.analyze_string(&mut tokens);
                }

                c if self.is_alpha(c) => {
                    self.analyze_identifier(&mut tokens);
                }

                c if c.is_ascii_digit() || c == '.' => {
                    self.analyze_number(&mut tokens);
                }

                // Skip whitespace
                ' ' | '\t' | '\r' | '\n' => {}

                _ => panic!("Uexpected character: '{c}'"),
            }
        }

        tokens.push(Token {
            kind: TokenKind::EndOfFile,
            lexeme: "EOF".to_string(),
        });

        tokens
    }

    fn analyze_identifier(&mut self, tokens: &mut Vec<Token>) {
        while !self.is_at_end() && self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();

        let kind = match text.as_str() {
            "let" => TokenKind::Let,
            "fun" => TokenKind::Fun,
            "return" => TokenKind::Return,
            _ => TokenKind::Identifier,
        };

        tokens.push(Token { kind, lexeme: text });
    }

    fn analyze_number(&mut self, tokens: &mut Vec<Token>) {
        while !self.is_at_end() && (self.peek().is_ascii_digit() || self.peek() == '.') {
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();

        tokens.push(Token {
            kind: TokenKind::Number,
            lexeme: text,
        });
    }

    fn analyze_string(&mut self, tokens: &mut Vec<Token>) {
        while !self.is_at_end() && self.peek() != '"' {
            self.advance();
        }

        if self.is_at_end() {
            panic!("Unterminated string");
        }

        self.advance();

        let text: String = self.source[self.start + 1..self.current - 1]
            .iter()
            .collect();

        tokens.push(Token {
            kind: TokenKind::String,
            lexeme: text,
        });
    }

    fn add_token(&mut self, tokens: &mut Vec<Token>, kind: TokenKind) {
        let text: String = self.source[self.start..self.current].iter().collect();

        tokens.push(Token { kind, lexeme: text });
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];

        self.current += 1;

        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source[self.current]
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || c.is_ascii_digit()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}
