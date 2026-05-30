use crate::{
    span::Span,
    token::{Token, TokenKind},
};

pub struct Lexer {
    current: usize,
    start: usize,
    source: Vec<char>,

    line: usize,
    col: usize,
    token_start_col: usize,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Self {
            current: 0,
            start: 0,
            source: source.chars().collect(),
            line: 1,
            col: 1,
            token_start_col: 1,
        }
    }

    pub fn analyze(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];

        while !self.is_at_end() {
            self.start = self.current;
            self.token_start_col = self.col;

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
                '/' => {
                    if self.peek() == '=' {
                        // Consume '='
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::SlashEqual);
                    } else {
                        self.add_token(&mut tokens, TokenKind::Slash);
                    }
                }

                '(' => self.add_token(&mut tokens, TokenKind::LeftParen),
                ')' => self.add_token(&mut tokens, TokenKind::RightParen),
                '{' => self.add_token(&mut tokens, TokenKind::LeftBrace),
                '}' => self.add_token(&mut tokens, TokenKind::RightBrace),

                ':' => self.add_token(&mut tokens, TokenKind::Colon),
                ';' => self.add_token(&mut tokens, TokenKind::Semicolon),
                ',' => self.add_token(&mut tokens, TokenKind::Comma),

                '.' => {
                    if self.peek() == '.' {
                        if self.current + 1 < self.source.len()
                            && self.source[self.current + 1] == '.'
                        {
                            self.advance(); // Consume the 2nd dot
                            self.advance(); // Consume the 3rd dot
                            self.add_token(&mut tokens, TokenKind::Ellipsis);
                        } else {
                            panic!(
                                "Lexical Error: Unexpected character: '.' at line {}, col {}",
                                self.line, self.col
                            );
                        }
                    } else {
                        // self.add_token(&mut tokens, TokenKind::Dot);
                    }
                }

                '"' => {
                    self.analyze_string(&mut tokens);
                }
                '\'' => {
                    self.analyze_char(&mut tokens);
                }

                '&' => {
                    if self.peek() == '&' {
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::AmpersandAmpersand);
                    } else {
                        self.add_token(&mut tokens, TokenKind::Ampersand);
                    }
                }
                '|' => {
                    if self.peek() == '|' {
                        self.advance();
                        self.add_token(&mut tokens, TokenKind::PipePipe);
                    } else {
                        panic!(
                            "Lexical Error: Unexpected character: '|' at line {}, col {}",
                            self.line, self.col
                        );
                    }
                }

                c if self.is_alpha(c) => {
                    self.analyze_identifier(&mut tokens);
                }

                c if c.is_ascii_digit() => {
                    self.analyze_number(&mut tokens);
                }

                // Skip whitespace but explicitly handle manual line updates
                ' ' | '\t' | '\r' => {}
                '\n' => {
                    self.line += 1;
                    self.col = 1;
                }

                _ => panic!(
                    "Lexical Error: Unexpected character: '{c}' at line {}, col {}",
                    self.line, self.col
                ),
            }
        }

        tokens.push(Token {
            kind: TokenKind::EndOfFile,
            lexeme: "EOF".to_string(),
            span: Span {
                line: self.line,
                col: self.col,
                start: self.current,
                end: self.current,
            },
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
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "extern" => TokenKind::Extern,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            _ => TokenKind::Identifier,
        };

        tokens.push(Token {
            kind,
            lexeme: text,
            span: Span {
                line: self.line,
                col: self.token_start_col,
                start: self.start,
                end: self.current,
            },
        });
    }

    fn analyze_number(&mut self, tokens: &mut Vec<Token>) {
        while !self.is_at_end() && (self.peek().is_ascii_digit() || self.peek() == '.') {
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();

        tokens.push(Token {
            kind: TokenKind::Number,
            lexeme: text,
            span: Span {
                line: self.line,
                col: self.token_start_col,
                start: self.start,
                end: self.current,
            },
        });
    }

    fn analyze_string(&mut self, tokens: &mut Vec<Token>) {
        let mut parsed_string = String::new();
        let start_line = self.line;

        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                self.line += 1;
                self.col = 1;
            }

            let c = self.advance();

            if c == '\\' {
                if self.is_at_end() {
                    panic!(
                        "Lexical Error: Unterminated string escape sequence at line {}, col {}",
                        self.line, self.col
                    );
                }

                match self.advance() {
                    'n' => parsed_string.push('\n'),
                    't' => parsed_string.push('\t'),
                    'r' => parsed_string.push('\r'),
                    '\\' => parsed_string.push('\\'),
                    '"' => parsed_string.push('"'),
                    other => panic!(
                        "Lexical Error: Unknown escape sequence: \\{} at line {}, col {}",
                        other, self.line, self.col
                    ),
                }
            } else {
                parsed_string.push(c);
            }
        }

        if self.is_at_end() {
            panic!(
                "Lexical Error: Unterminated string starting at line {}, col {}",
                start_line, self.token_start_col
            );
        }

        self.advance();

        tokens.push(Token {
            kind: TokenKind::String,
            lexeme: parsed_string,
            span: Span {
                line: start_line,
                col: self.token_start_col,
                start: self.start,
                end: self.current,
            },
        });
    }

    fn analyze_char(&mut self, tokens: &mut Vec<Token>) {
        if self.peek() == '\'' {
            panic!(
                "Lexical Error: Empty char literal at line {}, col {}",
                self.line, self.col
            );
        }

        if self.is_at_end() {
            panic!(
                "Lexical Error: Unterminated char literal at line {}, col {}",
                self.line, self.col
            );
        }

        let c = self.advance();

        let parsed_char = if c == '\\' {
            if self.is_at_end() {
                panic!(
                    "Lexical Error: Unterminated char escape sequence at line {}, col {}",
                    self.line, self.col
                );
            }
            match self.advance() {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '\'' => '\'',
                '0' => '\0',
                other => panic!(
                    "Lexical Error: Unknown char escape sequence: \\{} at line {}, col {}",
                    other, self.line, self.col
                ),
            }
        } else {
            c
        };

        if self.is_at_end() || self.peek() != '\'' {
            panic!(
                "Lexical Error: Expected closing \"'\" after char literal at line {}, col {}",
                self.line, self.col
            );
        }
        self.advance(); // consume closing '

        tokens.push(Token {
            kind: TokenKind::Char,
            lexeme: parsed_char.to_string(),
            span: Span {
                line: self.line,
                col: self.token_start_col,
                start: self.start,
                end: self.current,
            },
        });
    }

    fn add_token(&mut self, tokens: &mut Vec<Token>, kind: TokenKind) {
        let text: String = self.source[self.start..self.current].iter().collect();

        tokens.push(Token {
            kind,
            lexeme: text,
            span: Span {
                line: self.line,
                col: self.token_start_col,
                start: self.start,
                end: self.current,
            },
        });
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];

        self.current += 1;
        self.col += 1;

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
