use std::{iter::Peekable, str::Chars};

use crate::token::Token;

pub struct LexError {
    pub message: String,
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            'a'..='z' | 'A'..='Z' => {
                let mut ident = String::new();

                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                let token = match ident.as_str() {
                    "let" => Token::Let,
                    _ => Token::Identifier(ident),
                };
                tokens.push(token);
            }

            '0'..='9' | '.' => {
                let mut number = String::new();
                let mut has_dot = false;

                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        number.push(c);
                        chars.next();
                    } else if c == '.' && !has_dot {
                        has_dot = true;
                        number.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                if number.starts_with('.') {
                    number.insert(0, '0');
                }
                if number.ends_with('.') {
                    number.push('0');
                }

                if has_dot {
                    let value = number.parse::<f64>().map_err(|err| LexError {
                        message: format!("Invalid number '{}': {}", number, err),
                    })?;
                    tokens.push(Token::NumberFloat(value));
                } else {
                    let value = number.parse::<i64>().map_err(|err| LexError {
                        message: format!("Invalid number '{}': {}", number, err),
                    })?;
                    tokens.push(Token::NumberInt(value));
                }
            }

            '"' => {
                chars.next(); // Consume the opening quote
                let mut string_content = String::new();

                while let Some(&c) = chars.peek() {
                    match c {
                        '"' => {
                            chars.next(); // Consume the closing quote
                            break;
                        }
                        '\\' => {
                            chars.next(); // Consume the backslash
                            if let Some(escaped) = chars.next() {
                                let escaped_char = match escaped {
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    '"' => '"',
                                    '\\' => '\\',
                                    other => other, // Default: keep the character
                                };
                                string_content.push(escaped_char);
                            } else {
                                return Err(LexError {
                                    message: "Unfinished escape sequence in string".to_string(),
                                });
                            }
                        }
                        _ => {
                            string_content.push(c);
                            chars.next();
                        }
                    }
                }

                tokens.push(Token::StringLiteral(string_content));
            }

            '=' => push_token(Token::Equals, &mut tokens, &mut chars),

            '(' => push_token(Token::LParen, &mut tokens, &mut chars),
            ')' => push_token(Token::RParen, &mut tokens, &mut chars),

            ':' => push_token(Token::Colon, &mut tokens, &mut chars),
            ';' => push_token(Token::Semicolon, &mut tokens, &mut chars),

            c if c.is_whitespace() || c == '\r' => {
                chars.next(); // Skip whitespace
            }

            _ => {
                return Err(LexError {
                    message: format!("Unexpected character '{}'", ch),
                });
            }
        }
    }

    Ok(tokens)
}

fn push_token(token: Token, tokens: &mut Vec<Token>, chars: &mut Peekable<Chars<'_>>) {
    tokens.push(token);
    chars.next();
}
