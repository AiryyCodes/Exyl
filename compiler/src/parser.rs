use crate::{
    ast::{BinaryOp, Expr, Program, Stmt, Type, UnaryOp},
    token::Token,
};

pub struct ParseError {
    pub message: String,
}

impl ParseError {
    pub fn new(message: &str) -> Self {
        Self {
            message: message.to_string(),
        }
    }
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
            Some(Token::Extern) | Some(Token::Function) => self.parse_function()?,
            Some(Token::Return) => self.parse_return()?, // semicolon handled inside
            Some(Token::LBrace) => self.parse_block()?,

            Some(Token::If) => self.parse_if()?,

            Some(_) => {
                let expr = self.parse_primary()?;
                Stmt::Expr(expr)
            }
            None => return Err(ParseError::new("Unexpected end of input")),
        };

        // Only require semicolon for expressions and let-statements
        match &stmt {
            Stmt::Expr(_) | Stmt::Let { .. } => {
                self.expect(&Token::Semicolon)?;
            }
            _ => {} // Return, function, block statements do NOT require semicolon
        }

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

    fn parse_function(&mut self) -> Result<Stmt, ParseError> {
        let mut is_extern = false;

        if let Some(Token::Extern) = self.peek() {
            self.next();
            is_extern = true;
        }

        self.next(); // consume 'fun'

        let name = match self.next() {
            Some(Token::Identifier(ident)) => ident.clone(),
            other => {
                return Err(ParseError {
                    message: format!("Expected identifier after fun, found {:?}", other),
                });
            }
        };

        self.expect(&Token::LParen)?;

        let mut args: Vec<(String, Type)> = Vec::new();

        while let Some(token) = self.peek() {
            if let Token::RParen = token {
                break;
            }

            let arg_name = if let Token::Identifier(ident) = self.next().unwrap() {
                ident.clone()
            } else {
                return Err(ParseError::new("Expected identifier in parameter list"));
            };

            self.expect(&Token::Colon)?;
            let ty = self.parse_type()?;
            args.push((arg_name, ty));

            if let Some(Token::Comma) = self.peek() {
                self.next();
            } else {
                break;
            }
        }

        self.expect(&Token::RParen)?;

        let return_type = if let Some(Token::Colon) = self.peek() {
            self.next();
            Some(self.parse_type()?)
        } else {
            None
        };

        // Handle function declaration vs definition
        let body = match self.peek() {
            Some(Token::LBrace) => {
                self.next(); // consume '{'
                let mut stmts = Vec::new();
                while let Some(token) = self.peek() {
                    if let Token::RBrace = token {
                        break;
                    }
                    stmts.push(self.parse_stmt()?);
                }
                self.expect(&Token::RBrace)?;
                Some(stmts)
            }
            Some(Token::Semicolon) => {
                self.next(); // consume ';'
                None // function declaration
            }
            other => {
                return Err(ParseError {
                    message: format!("Expected function body '{{' or ';', found {:?}", other),
                });
            }
        };

        Ok(Stmt::Func {
            name,
            return_type,
            inferred_return: Type::Void,
            arguments: args,
            body,
            is_extern,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.next() {
            Some(Token::Identifier(type_name)) => match type_name.as_str() {
                "i64" => Ok(Type::I64),

                "f64" => Ok(Type::F64),

                "bool" => Ok(Type::Bool),

                "string" => Ok(Type::String),

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

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.next() {
            Some(Token::NumberInt(n)) => Ok(Expr::NumberInt(*n)),
            Some(Token::NumberFloat(f)) => Ok(Expr::NumberFloat(*f)),

            Some(Token::True) => Ok(Expr::BoolLiteral(true)),
            Some(Token::False) => Ok(Expr::BoolLiteral(false)),

            Some(Token::StringLiteral(s)) => Ok(Expr::StringLiteral(s.clone())),

            Some(Token::Identifier(ident)) => {
                let name = ident.clone();
                if let Some(Token::LParen) = self.peek() {
                    self.parse_func_call(name)
                } else {
                    Ok(Expr::Identifier(name))
                }
            }

            Some(Token::LParen) => {
                let expr = self.parse_expr()?; // parse inside
                self.expect(&Token::RParen)?; // consume `)`
                Ok(expr) // return just the inner expression
            }

            other => Err(ParseError {
                message: format!("Unexpected token in expression: {:?}", other),
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_expr(0)
    }

    fn parse_func_call(&mut self, name: String) -> Result<Expr, ParseError> {
        self.expect(&Token::LParen)?;

        let mut args = Vec::new();

        // Parse zero or more arguments until ')'
        while let Some(token) = self.peek() {
            if let Token::RParen = token {
                break;
            }

            // TODO: Maybe use self.parse_binary_expr()
            let arg = self.parse_primary()?;
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

    fn parse_return(&mut self) -> Result<Stmt, ParseError> {
        self.next(); // consume 'return'

        // Parse optional expression
        let expr_opt = match self.peek() {
            Some(&Token::Semicolon) => None,
            Some(_) => Some(self.parse_expr()?),
            None => return Err(ParseError::new("Unexpected end of input after return")),
        };

        // Consume semicolon that ends the return
        self.expect(&Token::Semicolon)?;

        Ok(Stmt::Return(expr_opt))
    }

    fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        self.next(); // consume '{'

        let mut stmts = Vec::new();

        while let Some(token) = self.peek() {
            if let Token::RBrace = token {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }

        self.expect(&Token::RBrace)?;

        Ok(Stmt::Block(stmts))
    }

    fn parse_if(&mut self) -> Result<Stmt, ParseError> {
        self.expect(&Token::If)?; // consume 'if'

        self.expect(&Token::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(&Token::RParen)?;

        // Parse then-branch
        let then_branch = if let Some(Token::LBrace) = self.peek() {
            self.parse_block()? // parse a block `{ ... }` returning Stmt::Block
        } else {
            self.parse_stmt()? // single statement
        };

        // Optional else-branch
        let else_branch = if let Some(Token::Else) = self.peek() {
            self.next(); // consume 'else'
            Some(Box::new(if let Some(Token::LBrace) = self.peek() {
                self.parse_block()?
            } else {
                self.parse_stmt()?
            }))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    fn parse_binary_expr(&mut self, min_prec: u8) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?; // first part

        loop {
            // Check next token
            let op_token = match self.peek() {
                Some(tok) if Parser::token_to_binary_op(tok).is_some() => tok.clone(),
                _ => break,
            };

            let op = Parser::token_to_binary_op(&op_token).unwrap();
            let prec = Parser::precedence(&op);

            // If operator precedence is too low, stop
            if prec < min_prec {
                break;
            }

            self.next(); // consume operator

            // Parse right-hand side with higher precedence
            let right = self.parse_binary_expr(prec + 1)?;

            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Some(Token::Bang) => {
                self.next();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                })
            }
            Some(Token::Minus) => {
                self.next();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_primary(),
        }
    }

    fn precedence(op: &BinaryOp) -> u8 {
        match op {
            // Highest
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => 20,
            BinaryOp::Add | BinaryOp::Subtract => 10,

            // Comparisons
            BinaryOp::LessThan
            | BinaryOp::LessThanOrEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterThanOrEqual
            | BinaryOp::Equal
            | BinaryOp::NotEqual => 5,

            // Logical
            BinaryOp::LogicalAnd => 3,
            BinaryOp::LogicalOr => 2,
        }
    }

    fn token_to_binary_op(token: &Token) -> Option<BinaryOp> {
        match token {
            Token::Plus => Some(BinaryOp::Add),
            Token::Minus => Some(BinaryOp::Subtract),
            Token::Star => Some(BinaryOp::Multiply),
            Token::Divide => Some(BinaryOp::Divide),
            Token::Modulo => Some(BinaryOp::Modulo),

            Token::EqualEqual => Some(BinaryOp::Equal),
            Token::BangEqual => Some(BinaryOp::NotEqual),
            Token::Less => Some(BinaryOp::LessThan),
            Token::LessEqual => Some(BinaryOp::LessThanOrEqual),
            Token::Greater => Some(BinaryOp::GreaterThan),
            Token::GreaterEqual => Some(BinaryOp::GreaterThanOrEqual),

            Token::AndAnd => Some(BinaryOp::LogicalAnd),
            Token::OrOr => Some(BinaryOp::LogicalOr),
            _ => None,
        }
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
