use std::{iter::Peekable, str::Chars};

use crate::token::Token;

pub struct LexError {
    pub message: String,
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(&current_char) = chars.peek() {
        match current_char {
            // --- identifiers & keywords ---
            'a'..='z' | 'A'..='Z' => {
                let ident = read_identifier(&mut chars);
                let token = match ident.as_str() {
                    "let" => Token::Let,
                    "fun" => Token::Function,
                    "return" => Token::Return,
                    "extern" => Token::Extern,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "true" => Token::True,
                    "false" => Token::False,
                    _ => Token::Identifier(ident),
                };
                tokens.push(token);
            }

            // --- dot / ellipsis ---
            '.' => {
                if try_push_ellipsis(&mut tokens, &mut chars) {
                    continue;
                }
                // If '.' is followed by a digit, lex a float like .5
                let mut la = chars.clone(); 
                la.next(); // skip '.'
                if let Some(&next_after) = la.peek() {
                    if next_after.is_ascii_digit() {
                        let token = lex_number(&mut chars)?;
                        tokens.push(token);
                        continue;
                    }
                }
                // single dot
                chars.next();
                tokens.push(Token::Dot);
            }

            // --- numbers (decimal / float) ---
            '0'..='9' => {
                let token = lex_number(&mut chars)?;
                tokens.push(token);
            }

            // --- strings ---
            '"' => {
                let token = lex_string(&mut chars)?;
                tokens.push(token);
            }

            // --- arithmetic ---
            '+' => push_token(Token::Plus, &mut tokens, &mut chars),
            '-' => push_token(Token::Minus, &mut tokens, &mut chars),
            '*' => push_token(Token::Star, &mut tokens, &mut chars),
            '%' => push_token(Token::Modulo, &mut tokens, &mut chars),

            // --- comparisons / equality ---
            '=' => {
                chars.next();
                if consume_if_next(&mut chars, '=') {
                    tokens.push(Token::EqualEqual);
                } else {
                    tokens.push(Token::Equals);
                }
            }
            '!' => {
                chars.next();
                if consume_if_next(&mut chars, '=') {
                    tokens.push(Token::BangEqual);
                } else {
                    tokens.push(Token::Bang);
                }
            }
            '<' => {
                chars.next();
                if consume_if_next(&mut chars, '=') {
                    tokens.push(Token::LessEqual);
                } else {
                    tokens.push(Token::Less);
                }
            }
            '>' => {
                chars.next();
                if consume_if_next(&mut chars, '=') {
                    tokens.push(Token::GreaterEqual);
                } else {
                    tokens.push(Token::Greater);
                }
            }

            // --- logical ---
            '&' => {
                chars.next();
                if consume_if_next(&mut chars, '&') {
                    tokens.push(Token::AndAnd);
                }
            }
            '|' => {
                chars.next();
                if consume_if_next(&mut chars, '|') {
                    tokens.push(Token::OrOr);
                }
            }

            // --- delimiters ---
            '(' => push_token(Token::LParen, &mut tokens, &mut chars),
            ')' => push_token(Token::RParen, &mut tokens, &mut chars),
            '{' => push_token(Token::LBrace, &mut tokens, &mut chars),
            '}' => push_token(Token::RBrace, &mut tokens, &mut chars),
            '[' => push_token(Token::LBracket, &mut tokens, &mut chars),
            ']' => push_token(Token::RBracket, &mut tokens, &mut chars),
            ':' => push_token(Token::Colon, &mut tokens, &mut chars),
            ';' => push_token(Token::Semicolon, &mut tokens, &mut chars),
            ',' => push_token(Token::Comma, &mut tokens, &mut chars),

            // --- whitespace ---
            c if c.is_whitespace() || c == '\r' => {
                chars.next();
            }

            // --- comments / divide ---
            '/' => {
                chars.next(); // consumed '/'
                match chars.peek().copied() {
                    Some('/') => skip_line_comment(&mut chars),
                    Some('*') => skip_block_comment(&mut chars),
                    _ => tokens.push(Token::Divide),
                }
            }

            // --- unexpected ---
            other => {
                return Err(LexError {
                    message: format!("Unexpected character '{}'", other),
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

fn read_identifier(chars: &mut Peekable<Chars<'_>>) -> String {
    let mut ident = String::new();
    while let Some(&next_char) = chars.peek() {
        if next_char.is_alphanumeric() || next_char == '_' {
            ident.push(next_char);
            chars.next();
        } else {
            break;
        }
    }
    ident
}

fn try_push_ellipsis(tokens: &mut Vec<Token>, chars: &mut Peekable<Chars<'_>>) -> bool {
    // Lookahead for "..." without consuming unless matched
    let mut lookahead = chars.clone();
    lookahead.next(); // first '.' already matched by caller
    if matches!(lookahead.peek(), Some('.')) {
        lookahead.next();
        if matches!(lookahead.peek(), Some('.')) {
            // Confirm and consume three characters
            chars.next();
            chars.next();
            chars.next();
            tokens.push(Token::DotDotDot);
            return true;
        }
    }
    false
}

fn lex_number(chars: &mut Peekable<Chars<'_>>) -> Result<Token, LexError> {
    let mut number_literal = String::new();
    let mut has_decimal_point = false;

    while let Some(&next_char) = chars.peek() {
        if next_char.is_ascii_digit() {
            number_literal.push(next_char);
            chars.next();
        } else if next_char == '.' && !has_decimal_point {
            has_decimal_point = true;
            number_literal.push('.');
            chars.next();
        } else {
            break;
        }
    }

    if number_literal == "." {
        // Maintain prior behavior: single '.' becomes Dot token and advance once more
        // Caller already handled '...' and a single '.' arm, but we keep this for parity
        // with the original implementation in edge paths.
        return Ok(Token::Dot);
    }

    if number_literal.starts_with('.') {
        number_literal.insert(0, '0');
    }
    if number_literal.ends_with('.') {
        number_literal.push('0');
    }

    if has_decimal_point {
        let value = number_literal.parse::<f64>().map_err(|err| LexError {
            message: format!("Invalid float '{}': {}", number_literal, err),
        })?;
        Ok(Token::NumberFloat(value))
    } else {
        let value = number_literal.parse::<i64>().map_err(|err| LexError {
            message: format!("Invalid int '{}': {}", number_literal, err),
        })?;
        Ok(Token::NumberInt(value))
    }
}

fn lex_string(chars: &mut Peekable<Chars<'_>>) -> Result<Token, LexError> {
    // Opening quote is at peek position
    chars.next();
    let mut content = String::new();

    while let Some(&next_char) = chars.peek() {
        match next_char {
            '"' => {
                chars.next();
                break;
            }
            '\\' => {
                chars.next(); // consume '\\'
                if let Some(escaped) = chars.next() {
                    let actual = match escaped {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    };
                    content.push(actual);
                } else {
                    return Err(LexError {
                        message: "Unfinished escape sequence in string".to_string(),
                    });
                }
            }
            _ => {
                content.push(next_char);
                chars.next();
            }
        }
    }

    Ok(Token::StringLiteral(content))
}

fn skip_line_comment(chars: &mut Peekable<Chars<'_>>) {
    // We are positioned at the second '/'
    // Consume it first
    chars.next();
    while let Some(&next_char) = chars.peek() {
        chars.next();
        if next_char == '\n' {
            break;
        }
    }
}

fn skip_block_comment(chars: &mut Peekable<Chars<'_>>) {
    // We are positioned at '*'
    chars.next(); // consume '*'
    let mut last = '\0';
    while let Some(next_char) = chars.next() {
        if last == '*' && next_char == '/' {
            break;
        }
        last = next_char;
    }
}

fn consume_if_next(chars: &mut Peekable<Chars<'_>>, expected: char) -> bool {
    if let Some(&next_char) = chars.peek() {
        if next_char == expected {
            chars.next();
            return true;
        }
    }
    false
}
