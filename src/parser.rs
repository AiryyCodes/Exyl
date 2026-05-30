use crate::{
    ast::{Expr, Program, Stmt, TypeExpr},
    error::ParseError,
    span::Span,
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
            match self.declaration(false) {
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

    fn consume_expected(
        &mut self,
        kind: TokenKind,
        expected_description: &str,
    ) -> Result<Token, ParseError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            let actual_token = self.peek().clone();
            let msg = if actual_token.kind == TokenKind::EndOfFile {
                format!(
                    "Expected {}, but hit the End of File (EOF)",
                    expected_description
                )
            } else {
                format!(
                    "Expected {}, but found '{}'",
                    expected_description, actual_token.lexeme
                )
            };
            Err(self.error_at(&actual_token, &msg))
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
                TokenKind::Fun => return,
                TokenKind::Extern => return,
                TokenKind::If => return,
                TokenKind::Else => return,
                TokenKind::While => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn error_at(&self, token: &Token, message: &str) -> ParseError {
        ParseError {
            message: format!("{} at '{}'", message, token.lexeme),
            span: token.span,
        }
    }

    fn declaration(&mut self, is_extern: bool) -> Result<Stmt, ParseError> {
        if self.check(TokenKind::Let) {
            if is_extern {
                let token = self.peek().clone();
                return Err(self.error_at(&token, "Syntax Error: 'extern' modifiers cannot be applied to 'let' variable bindings."));
            }
            self.advance();
            return self.let_decl();
        }

        if self.match_token(TokenKind::Fun) {
            return self.fun_decl(is_extern);
        }

        if self.match_token(TokenKind::Extern) {
            if is_extern {
                let token = self.previous();
                return Err(self.error_at(
                    &token,
                    "Syntax Error: Duplicate 'extern' modifier encountered.",
                ));
            }
            return self.declaration(true);
        }

        if is_extern {
            let token = self.peek().clone();
            return Err(self.error_at(&token, "Syntax Error: Expected a global top-level function declaration directly after 'extern'."));
        }

        self.statement()
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.check(TokenKind::LeftBrace) {
            return self.block();
        }

        if self.match_token(TokenKind::Return) {
            return self.return_stmt();
        }

        if self.match_token(TokenKind::If) {
            return self.if_stmt();
        }

        if self.match_token(TokenKind::While) {
            return self.while_stmt();
        }

        self.expression_stmt()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn expression_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        let semi = self.consume_expected(
            TokenKind::Semicolon,
            "a trailing ';' terminating the expression statement",
        )?;

        let span = Span {
            line: expr.span().line,
            col: expr.span().col,
            start: expr.span().start,
            end: semi.span.end,
        };

        Ok(Stmt::Expr(expr, span))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let if_token = self.previous();

        let condition = self.expression()?;
        let then_branch = self.block()?;

        let mut else_branch = None;
        if self.match_token(TokenKind::Else) {
            if self.match_token(TokenKind::If) {
                else_branch = Some(self.if_stmt()?);
            } else {
                else_branch = Some(self.block()?);
            }
        }

        let end_offset = match &else_branch {
            Some(else_stmt) => else_stmt.span().end,
            None => then_branch.span().end,
        };

        let span = Span {
            line: if_token.span.line,
            col: if_token.span.col,
            start: if_token.span.start,
            end: end_offset,
        };

        Ok(Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
            span,
        })
    }

    fn while_stmt(&mut self) -> Result<Stmt, ParseError> {
        let while_token = self.previous();

        let condition = self.expression()?;
        let body = self.block()?;

        let span = Span {
            line: while_token.span.line,
            col: while_token.span.col,
            start: while_token.span.start,
            end: body.span().end,
        };

        Ok(Stmt::While {
            condition,
            body: Box::new(body),
            span,
        })
    }

    fn let_decl(&mut self) -> Result<Stmt, ParseError> {
        let let_token = self.previous();
        let name = self.consume_expected(
            TokenKind::Identifier,
            "a variable name identifier following 'let'",
        )?;

        let mut ty = None;
        if self.match_token(TokenKind::Colon) {
            ty = Some(self.type_expression()?);
        }

        self.consume_expected(
            TokenKind::Equal,
            "an assignment operator '=' following the variable name definition",
        )?;
        let value = self.expression()?;

        let semi = self.consume_expected(
            TokenKind::Semicolon,
            "a terminating ';' at the end of the variable declaration",
        )?;

        let span = Span {
            line: let_token.span.line,
            col: let_token.span.col,
            start: let_token.span.start,
            end: semi.span.end,
        };

        Ok(Stmt::Let {
            name: name.lexeme,
            ty,
            value,
            span,
        })
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let ret_token = self.previous();
        let mut value = None;

        if !self.check(TokenKind::Semicolon) {
            value = Some(self.expression()?);
        }

        let semi = self.consume_expected(
            TokenKind::Semicolon,
            "a terminating ';' directly following your return value statement",
        )?;

        let span = Span {
            line: ret_token.span.line,
            col: ret_token.span.col,
            start: ret_token.span.start,
            end: semi.span.end,
        };

        Ok(Stmt::Return { value, span })
    }

    fn fun_decl(&mut self, is_extern: bool) -> Result<Stmt, ParseError> {
        let fun_token = self.previous();
        let name = self.consume_expected(
            TokenKind::Identifier,
            "a function name identifier following 'fun'",
        )?;

        self.consume_expected(
            TokenKind::LeftParen,
            "an opening '(' before defining function arguments",
        )?;

        let mut params = vec![];
        let mut is_variadic = false;

        if !self.check(TokenKind::RightParen) {
            loop {
                if self.match_token(TokenKind::Ellipsis) {
                    is_variadic = true;

                    if self.check(TokenKind::Comma) {
                        let token = self.peek().clone();
                        return Err(self.error_at(&token, "Syntax Error: A variadic parameter (...) must be the final entry in a parameter signature."));
                    }
                    break;
                }

                let param_name =
                    self.consume_expected(TokenKind::Identifier, "a parameter name declaration")?;
                self.consume_expected(
                    TokenKind::Colon,
                    "a type annotation separator ':' directly following the parameter name",
                )?;
                let param_type = self.type_expression()?;

                params.push((param_name.lexeme, param_type));

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume_expected(
            TokenKind::RightParen,
            "a closing ')' directly after the formal parameter list specifications",
        )?;

        let mut return_type = None;
        if self.match_token(TokenKind::Colon) {
            return_type = Some(self.type_expression()?);
        }

        let mut body = None;
        let end_offset;

        if !is_extern {
            let block_node = self.block()?;
            end_offset = block_node.span().end;
            body = Some(Box::new(block_node));
        } else {
            let semi = self.consume_expected(
                TokenKind::Semicolon,
                "a terminating ';' directly after an 'extern' function signature prototype",
            )?;
            end_offset = semi.span.end;
        }

        let span = Span {
            line: fun_token.span.line,
            col: fun_token.span.col,
            start: fun_token.span.start,
            end: end_offset,
        };

        Ok(Stmt::Fun {
            name: name.lexeme,
            parameters: params,
            is_variadic,
            return_type,
            is_extern,
            body,
            span,
        })
    }

    fn block(&mut self) -> Result<Stmt, ParseError> {
        let open_bracket =
            self.consume_expected(TokenKind::LeftBrace, "an opening block delimiter '{'")?;

        let mut body = vec![];
        while !self.is_at_end() && !self.check(TokenKind::RightBrace) {
            body.push(self.declaration(false)?);
        }

        let close_bracket = self.consume_expected(
            TokenKind::RightBrace,
            "a closing block brace structure completion '}'",
        )?;

        let span = Span {
            line: open_bracket.span.line,
            col: open_bracket.span.col,
            start: open_bracket.span.start,
            end: close_bracket.span.end,
        };

        Ok(Stmt::Block(body, span))
    }

    fn type_expression(&mut self) -> Result<TypeExpr, ParseError> {
        let token = self.consume_expected(
            TokenKind::Identifier,
            "a primitive valid data type identifier name (e.g., 'i32', 'string')",
        )?;
        Ok(TypeExpr::Primitive(token.lexeme, token.span))
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        while self.match_token(TokenKind::LeftParen) {
            let mut arguments = vec![];

            if !self.check(TokenKind::RightParen) {
                loop {
                    match self.expression() {
                        Ok(arg) => arguments.push(arg),
                        Err(e) => {
                            return Err(self.error_at(&self.peek().clone(), &format!("Malformed argument item evaluation inside target call bounds: {}", e.message)));
                        }
                    }

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
            }

            let right_paren = self.consume_expected(
                TokenKind::RightParen,
                "a closing function application call bracket match group ')'",
            )?;

            let span = Span {
                line: expr.span().line,
                col: expr.span().col,
                start: expr.span().start,
                end: right_paren.span.end,
            };

            expr = Expr::Call {
                callee: Box::new(expr),
                arguments,
                span,
            };
        }

        Ok(expr)
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality()?;

        if self.match_token(TokenKind::Equal) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expr::Identifier(name, id_span) => {
                    let span = Span {
                        line: id_span.line,
                        col: id_span.col,
                        start: id_span.start,
                        end: value.span().end,
                    };
                    return Ok(Expr::Assignment {
                        name,
                        value: Box::new(value),
                        span,
                    });
                }
                _ => {
                    return Err(self.error_at(&equals, &format!("Syntax Error: Assignment target must be a mutable identifier variable name, found unexpected target: {:?}", expr)));
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

            let span = Span {
                line: expr.span().line,
                col: expr.span().col,
                start: expr.span().start,
                end: right.span().end,
            };

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
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

            let span = Span {
                line: expr.span().line,
                col: expr.span().col,
                start: expr.span().start,
                end: right.span().end,
            };

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while self.match_token(TokenKind::Plus) || self.match_token(TokenKind::Minus) {
            let operator = self.previous();
            let right = self.factor()?;

            let span = Span {
                line: expr.span().line,
                col: expr.span().col,
                start: expr.span().start,
                end: right.span().end,
            };

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while self.match_token(TokenKind::Star) || self.match_token(TokenKind::Slash) {
            let operator = self.previous();
            let right = self.unary()?;

            let span = Span {
                line: expr.span().line,
                col: expr.span().col,
                start: expr.span().start,
                end: right.span().end,
            };

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(TokenKind::Ampersand) {
            let operator = self.previous();
            let right = self.unary()?;
            
            let span = Span {
        line: operator.span.line,
        col: operator.span.col,
        start: operator.span.start, 
        end: right.span().end,
    };

            return Ok(Expr::AddressOf(Box::new(right), span))
        }

        if self.match_token(TokenKind::Minus) || self.match_token(TokenKind::Bang) {
            let operator = self.previous();
            let right = self.unary()?;

            let span = Span {
                line: operator.span.line,
                col: operator.span.col,
                start: operator.span.start,
                end: right.span().end,
            };

            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
                span,
            });
        }

        self.call()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.advance();

        match token.kind {
            TokenKind::Number => {
                let val = token.lexeme.parse::<f64>().map_err(|_| {
                    self.error_at(&token, "Compiler Core Error: Malformed or out-of-bounds float literal representation.")
                })?;
                Ok(Expr::Number(val, token.span))
            }
            TokenKind::String => Ok(Expr::String(token.lexeme, token.span)),
            TokenKind::True => Ok(Expr::Bool(true, token.span)),
            TokenKind::False => Ok(Expr::Bool(false, token.span)),
            TokenKind::Identifier => Ok(Expr::Identifier(token.lexeme, token.span)),

            TokenKind::LeftParen => {
                let expr = self.expression()?;
                let right_paren = self.consume_expected(TokenKind::RightParen, "a corresponding matching grouping close symbol ')'")?;
                
                let span = Span {
                    line: token.span.line,
                    col: token.span.col,
                    start: token.span.start,
                    end: right_paren.span.end,
                };
                
                let updated_expr = match expr {
                    Expr::Number(v, _) => Expr::Number(v, span),
                    Expr::String(s, _) => Expr::String(s, span),
                    Expr::Bool(b, _) => Expr::Bool(b, span),
                    Expr::Identifier(i, _) => Expr::Identifier(i, span),
                    Expr::Call { callee, arguments, .. } => Expr::Call { callee, arguments, span },
                    Expr::Assignment { name, value, .. } => Expr::Assignment { name, value, span },
                    Expr::Error(e, _) => Expr::Error(e, span),
                    Expr::Binary { left, right, operator, .. } => Expr::Binary { left, right, operator, span },
                    Expr::Unary { operator, right, .. } => Expr::Unary { operator, right, span },
                    Expr::AddressOf(inner, _) => Expr::AddressOf(inner, span),
                };

                Ok(updated_expr)
            }

            TokenKind::Equal | TokenKind::Bang => {
                Err(self.error_at(&token, &format!("Syntax Error: Leading mathematical logic operator '{}' cannot begin an evaluation expression.", token.lexeme)))
            }

            _ => {
                Err(self.error_at(&token, &format!("Syntax Error: Expected an expression value, literal baseline, or bracket block grouping structure here but encountered '{}'.", token.lexeme)))
            }
        }
    }
}
