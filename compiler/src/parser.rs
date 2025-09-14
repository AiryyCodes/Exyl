use crate::{
    ast::{Expr, Program, Stmt, Type},
    token::Token,
};

pub struct ParseError {
    pub message: String,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<&Token> {
        let tok = self.tokens.get(self.pos);
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: &Token) -> Result<(), ParseError> {
        match self.next() {
            Some(tok) if tok == expected => Ok(()),
            Some(tok) => Err(ParseError {
                message: format!("Expected {:?}, found {:?}", expected, tok),
            }),
            None => Err(ParseError {
                message: format!("Expected {:?}, found end of input", expected),
            }),
        }
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let stmt = match self.peek() {
            Some(Token::Let) => self.parse_let()?,
            Some(_) => {
                // fallback: expression as a statement
                let expr = self.parse_expr()?;
                Stmt::Expr(expr)
            }
            None => {
                return Err(ParseError {
                    message: "Unexpected end of input".to_string(),
                });
            }
        };

        // Require a semicolon after every statement
        self.expect(&Token::Semicolon)?;

        Ok(stmt)
    }

    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        self.next(); // consume 'let'

        let name = match self.next() {
            Some(Token::Identifier(ident)) => ident.clone(),
            other => {
                return Err(ParseError {
                    message: format!("Expected identifier after let, found {:?}", other),
                });
            }
        };

        // check for optional type annotation
        let ty = if let Some(Token::Colon) = self.peek() {
            self.next(); // consume ':'
            Some(self.parse_type()?) // you need a parse_type() function
        } else {
            None
        };

        self.expect(&Token::Equals)?; // expect '='

        let value = self.parse_expr()?;

        Ok(Stmt::Let { name, ty, value })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.next() {
            Some(Token::Identifier(type_name)) => match type_name.as_str() {
                "i64" => Ok(Type::I64),
                "f64" => Ok(Type::F64),
                "void" => Ok(Type::Void),
                other => Err(ParseError {
                    message: format!("Unknown type '{}'", other),
                }),
            },
            other => Err(ParseError {
                message: format!("Expected type name, found {:?}", other),
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.next() {
            Some(Token::NumberInt(n)) => Ok(Expr::NumberInt(*n)),
            Some(Token::NumberFloat(f)) => Ok(Expr::NumberFloat(*f)),
            Some(Token::StringLiteral(s)) => Ok(Expr::StringLiteral(s.clone())),
            Some(Token::Identifier(ident)) => {
                let name = ident.clone();
                if let Some(Token::LParen) = self.peek() {
                    self.parse_func_call(name)
                } else {
                    Ok(Expr::Identifier(name))
                }
            }
            other => Err(ParseError {
                message: format!("Unexpected token in expression: {:?}", other),
            }),
        }
    }

    fn parse_func_call(&mut self, name: String) -> Result<Expr, ParseError> {
        self.expect(&Token::LParen)?;

        let mut args = Vec::new();

        // Parse zero or more arguments until ')'
        while let Some(token) = self.peek() {
            if let Token::RParen = token {
                break;
            }

            let arg = self.parse_expr()?;
            args.push(arg);

            // If next is a comma, consume it and continue
            if let Some(Token::Comma) = self.peek() {
                self.next();
            } else {
                break;
            }
        }

        self.expect(&Token::RParen)?;

        Ok(Expr::FunctionCall { name, args })
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    let mut body = Vec::new();

    while parser.peek().is_some() {
        let stmt = parser.parse_stmt()?;
        body.push(stmt);
    }

    Ok(Program { body })
}
