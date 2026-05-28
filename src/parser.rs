use crate::{
    ast::{Expr, Program, Stmt, TypeExpr},
    error::ParseError,
    token::{Token, TokenKind},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> (Program, Vec<ParseError>) {
        let mut nodes = vec![];
        let mut errors = vec![];

        while !self.is_at_end() {
            match self.declaration() {
                Ok(node) => nodes.push(node),
                Err(e) => {
                    errors.push(e);
                    self.synchronize();
                }
            }
        }

        (Program::new(nodes), errors)
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::EndOfFile)
    }

    fn peek(&self) -> &Token {
        if self.current >= self.tokens.len() {
            return &self.tokens[self.tokens.len() - 1];
        }
        &self.tokens[self.current]
    }

    fn advance(&mut self) -> Token {
        if self.current >= self.tokens.len() {
            return self.tokens[self.tokens.len() - 1].clone();
        }
        let token = self.tokens[self.current].clone();
        self.current += 1;
        token
    }

    fn check(&self, kind: TokenKind) -> bool {
        !self.is_at_end() && self.peek().kind == kind
    }

    fn consume(&mut self, kind: TokenKind, msg: &str) -> Result<Token, ParseError> {
        if self.check(kind.clone()) {
            Ok(self.advance())
        } else {
            Err(self.error(msg))
        }
    }

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn previous(&self) -> Token {
        debug_assert!(self.current > 0);

        self.tokens[self.current - 1].clone()
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().kind == TokenKind::Semicolon {
                return;
            }

            // Keywords
            match self.peek().kind {
                TokenKind::Let => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn error(&self, message: &str) -> ParseError {
        ParseError {
            message: message.to_string(),
        }
    }

    fn error_at(&self, token: &Token, message: &str) -> ParseError {
        ParseError {
            message: format!("{} at '{}'", message, token.lexeme),
        }
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_token(TokenKind::Let) {
            return self.let_decl();
        } else if self.match_token(TokenKind::Fun) {
            return self.fun_decl();
        }

        self.statement()
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.check(TokenKind::LeftBracket) {
            return self.block();
        }

        if self.match_token(TokenKind::Return) {
            return self.return_stmt();
        }

        self.expression_stmt()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn expression_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenKind::Semicolon, "Expected ';' after expression")?;

        Ok(Stmt::Expr(expr))
    }

    fn let_decl(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenKind::Identifier, "Expected variable name")?;

        let mut ty = None;
        if self.match_token(TokenKind::Colon) {
            ty = Some(self.type_expression()?);
        }

        self.consume(TokenKind::Equal, "Expected '='")?;

        let value = self.expression()?;

        if !self.check(TokenKind::Semicolon) {
            let token = self.peek().clone();
            return Err(self.error_at(&token, "Expected ';' or operator"));
        }

        self.advance();

        Ok(Stmt::Let {
            name: name.lexeme,
            ty,
            value,
        })
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let mut value = None;

        if !self.check(TokenKind::Semicolon) {
            value = Some(self.expression()?);
        }

        self.consume(TokenKind::Semicolon, "Expected ';' after return value")?;

        Ok(Stmt::Return { value })
    }

    fn fun_decl(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenKind::Identifier, "Expected function name")?;

        self.consume(TokenKind::LeftParen, "Expected '('")?;

        let mut params = vec![];

        if !self.check(TokenKind::RightParen) {
            loop {
                let param_name = self.consume(TokenKind::Identifier, "Expected parameter name")?;

                self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;

                let param_type = self.type_expression()?;

                params.push((param_name.lexeme, param_type));

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expected ')'")?;

        let mut return_type = None;
        if self.match_token(TokenKind::Colon) {
            return_type = Some(self.type_expression()?);
        }

        let body = self.block()?;

        Ok(Stmt::Fun {
            name: name.lexeme,
            body: Box::new(body),
            parameters: params,
            return_type,
        })
    }

    fn block(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenKind::LeftBracket, "Expected '{'")?;

        let mut body = vec![];
        while !self.is_at_end() && !self.check(TokenKind::RightBracket) {
            body.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBracket, "Expected '}'")?;

        Ok(Stmt::Block(body))
    }

    fn type_expression(&mut self) -> Result<TypeExpr, ParseError> {
        let token = self.consume(TokenKind::Identifier, "Expected type name")?;
        let current_type = TypeExpr::Primitive(token.lexeme);

        // TODO: add support for union types, and more.

        Ok(current_type)
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        while self.match_token(TokenKind::LeftParen) {
            let mut arguments = vec![];

            if !self.check(TokenKind::RightParen) {
                loop {
                    arguments.push(self.expression()?);

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
            }

            self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;

            expr = Expr::Call {
                callee: Box::new(expr),
                arguments,
            }
        }

        Ok(expr)
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality()?;

        if self.match_token(TokenKind::Equal) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expr::Identifier(name) => {
                    return Ok(Expr::Binary {
                        left: Box::new(Expr::Identifier(name)),
                        operator: equals,
                        right: Box::new(value),
                    });
                }
                _ => {
                    return Err(self.error_at(&equals, "Invalid assignment target"));
                }
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while self.match_token(TokenKind::BangEqual) || self.match_token(TokenKind::EqualEqual) {
            let operator = self.previous();
            let right = self.comparison()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while self.match_token(TokenKind::Greater)
            || self.match_token(TokenKind::GreaterEqual)
            || self.match_token(TokenKind::Less)
            || self.match_token(TokenKind::LessEqual)
        {
            let operator = self.previous();
            let right = self.term()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while self.match_token(TokenKind::Plus) || self.match_token(TokenKind::Minus) {
            let operator = self.previous();
            let right = self.factor()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while self.match_token(TokenKind::Star) || self.match_token(TokenKind::Slash) {
            let operator = self.previous();
            let right = self.unary()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(TokenKind::Minus) || self.match_token(TokenKind::Bang) {
            let operator = self.previous();
            let right = self.unary()?;

            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.call()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.advance();

        match token.kind {
            TokenKind::Number => Ok(Expr::Number(token.lexeme.parse().unwrap())),
            TokenKind::String => Ok(Expr::String(token.lexeme)),
            TokenKind::True => Ok(Expr::Bool(token.lexeme.parse().unwrap())),
            TokenKind::False => Ok(Expr::Bool(token.lexeme.parse().unwrap())),
            TokenKind::Identifier => Ok(Expr::Identifier(token.lexeme)),

            TokenKind::LeftParen => {
                let expr = self.expression()?;
                self.consume(TokenKind::RightParen, "Expected ')' after expression")?;
                Ok(expr)
            }

            TokenKind::Equal => Err(self.error_at(&token, "Unexpected '=' in expression")),
            TokenKind::Bang => Err(self.error_at(&token, "Unexpected '!' in expression")),

            _ => Err(self.error_at(&token, "Unexpected token in expression")),
        }
    }
}
