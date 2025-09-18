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
        let mut is_variadic = false;

        while let Some(token) = self.peek() {
            if let Token::RParen = token {
                break;
            }

            // Check for variadic "..."
            if let Token::DotDotDot = token {
                self.next(); // consume ...
                is_variadic = true;

                // variadic must be last
                if let Some(Token::Comma) = self.peek() {
                    return Err(ParseError::new("Variadic parameter must be last"));
                }
                break;
            }

            // Normal argument
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
            is_variadic,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let base_type = match self.next() {
            Some(Token::Identifier(type_name)) => match type_name.as_str() {
                "i64" => Type::I64,
                "f64" => Type::F64,
                "bool" => Type::Bool,
                "string" => Type::String,
                "void" => Type::Void,
                other => {
                    return Err(ParseError {
                        message: format!("Unknown type '{}'", other),
                    });
                }
            },
            other => {
                return Err(ParseError {
                    message: format!("Expected type name, found {:?}", other),
                });
            }
        };

        // Check for array brackets
        if let Some(&Token::LBracket) = self.peek() {
            self.next(); // consume '['

            // Check for fixed array
            if let Some(&Token::NumberInt(n)) = self.peek() {
                self.next();
                self.expect(&Token::RBracket)?;
                return Ok(Type::Array(Box::new(base_type), Some(n as usize)));
            }

            self.expect(&Token::RBracket)?; // consume ']'
            Ok(Type::Array(Box::new(base_type), None))
        } else {
            Ok(base_type)
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Some(Token::NumberInt(_)) => {
                let n = if let Some(Token::NumberInt(n)) = self.next() {
                    *n
                } else {
                    unreachable!()
                };
                Ok(Expr::NumberInt(n))
            }
            Some(Token::NumberFloat(_)) => {
                let f = if let Some(Token::NumberFloat(f)) = self.next() {
                    *f
                } else {
                    unreachable!()
                };
                Ok(Expr::NumberFloat(f))
            }
            Some(Token::True) => {
                self.next();
                Ok(Expr::BoolLiteral(true))
            }
            Some(Token::False) => {
                self.next();
                Ok(Expr::BoolLiteral(false))
            }
            Some(Token::StringLiteral(_)) => {
                let s = if let Some(Token::StringLiteral(s)) = self.next() {
                    s.clone()
                } else {
                    unreachable!()
                };
                Ok(Expr::StringLiteral(s))
            }
            Some(Token::Identifier(ident)) => {
                let name = ident.clone();
                if let Some(Token::LParen) = self.tokens.get(self.pos + 1) {
                    self.next(); // consume identifier
                    self.parse_func_call(Expr::Identifier(name))
                } else {
                    self.next();
                    Ok(Expr::Identifier(name))
                }
            }
            Some(Token::LParen) => {
                self.next(); // consume '('
                let expr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            Some(Token::LBracket) => self.parse_array_literal(),
            other => Err(ParseError {
                message: format!("Unexpected token in expression: {:?}", other),
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_expr(0)
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.peek() {
                Some(Token::LParen) => {
                    expr = self.parse_func_call(expr)?;
                }
                Some(Token::LBracket) => {
                    self.next(); // consume '['
                    let index_expr = self.parse_expr()?;
                    self.expect(&Token::RBracket)?;
                    expr = Expr::Index(Box::new(expr), Box::new(index_expr));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_func_call(&mut self, func_expr: Expr) -> Result<Expr, ParseError> {
        let name = match func_expr {
            Expr::Identifier(n) => n,
            _ => {
                return Err(ParseError::new("Can only call identifiers right now"));
            }
        };

        self.expect(&Token::LParen)?;
        let mut args = Vec::new();

        while let Some(token) = self.peek() {
            if let Token::RParen = token {
                break;
            }

            // parse full expression INCLUDING [] and other postfix ops
            let arg = self.parse_postfix_expr()?;
            args.push(arg);

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

    fn parse_array_literal(&mut self) -> Result<Expr, ParseError> {
        self.expect(&Token::LBracket)?;
        let mut elements = Vec::new();

        while let Some(tok) = self.peek() {
            if let Token::RBracket = tok {
                break;
            }
            elements.push(self.parse_expr()?);
            if let Some(Token::Comma) = self.peek() {
                self.next();
            } else {
                break;
            }
        }

        self.expect(&Token::RBracket)?;
        Ok(Expr::ArrayLiteral(elements))
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
            _ => self.parse_postfix_expr(),
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
