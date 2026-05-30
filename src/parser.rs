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
                TokenKind::Struct => return,
                TokenKind::Impl => return,
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

        if self.match_token(TokenKind::Struct) {
            if is_extern {
                let token = self.previous();
                return Err(self.error_at(&token, "Syntax Error: 'extern' cannot be applied to struct definitions."));
            }
            return self.struct_decl();
        }

        if self.match_token(TokenKind::Impl) {
            if is_extern {
                let token = self.previous();
                return Err(self.error_at(&token, "Syntax Error: 'extern' cannot be applied to impl blocks."));
            }
            return self.impl_decl();
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

    fn impl_decl(&mut self) -> Result<Stmt, ParseError> {
        let impl_token = self.previous();

        let target = self.consume_expected(
            TokenKind::Identifier,
            "a struct name following 'impl'",
        )?;

        self.consume_expected(TokenKind::LeftBrace, "an opening '{' for impl body")?;

        let mut methods = vec![];

        while !self.is_at_end() && !self.check(TokenKind::RightBrace) {
            if self.match_token(TokenKind::Fun) {
                methods.push(self.fun_decl(false)?);
            } else {
                let token = self.peek().clone();
                return Err(self.error_at(
                    &token,
                    "Syntax Error: Only 'fun' declarations are allowed inside an impl block.",
                ));
            }
        }

        let close = self.consume_expected(TokenKind::RightBrace, "a closing '}' for impl body")?;

        let span = Span {
            line: impl_token.span.line,
            col: impl_token.span.col,
            start: impl_token.span.start,
            end: close.span.end,
        };

        Ok(Stmt::Impl { target: target.lexeme, methods, span })
    }

    fn struct_decl(&mut self) -> Result<Stmt, ParseError> {
        let struct_token = self.previous(); // the `struct` keyword

        let name = self.consume_expected(
            TokenKind::Identifier,
            "a struct name following 'struct'",
        )?;

        self.consume_expected(TokenKind::LeftBrace, "an opening '{' for struct body")?;

        let mut fields = vec![];

        while !self.is_at_end() && !self.check(TokenKind::RightBrace) {
            let field_name = self.consume_expected(
                TokenKind::Identifier,
                "a field name",
            )?;
            self.consume_expected(TokenKind::Colon, "a ':' after field name")?;
            let field_type = self.type_expression()?;

            fields.push((field_name.lexeme, field_type));

            // Trailing comma is optional on the last field
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        let close = self.consume_expected(TokenKind::RightBrace, "a closing '}' for struct body")?;

        let span = Span {
            line: struct_token.span.line,
            col: struct_token.span.col,
            start: struct_token.span.start,
            end: close.span.end,
        };

        Ok(Stmt::Struct { name: name.lexeme, fields, span })
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

                let is_const_self = self.check(TokenKind::Const)
                    && self.tokens.get(self.current + 1)
                        .map(|t| t.kind == TokenKind::SelfKw)
                        .unwrap_or(false);

                if is_const_self {
                    self.advance(); // consume `const`
                    let self_tok = self.advance(); // consume `self`
                    // Use a sentinel type — semantic layer will fill in the real struct type
                    params.push((
                        "const self".to_string(),
                        TypeExpr::Named("Self".to_string(), self_tok.span),
                    ));
                } else if self.check(TokenKind::SelfKw) {
                    let self_tok = self.advance(); // consume `self`
                    params.push((
                        "self".to_string(),
                        TypeExpr::Named("Self".to_string(), self_tok.span),
                    ));
                } else {
                    // Normal param: name: Type
                    let param_name = self.consume_expected(TokenKind::Identifier, "a parameter name")?;
                    self.consume_expected(TokenKind::Colon, "a ':' after parameter name")?;
                    let param_type = self.type_expression()?;
                    params.push((param_name.lexeme, param_type));
                }

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
            "a type name (e.g. 'i32', 'Vec2')",
        )?;

        // Primitives stay as Primitive, everything else becomes Named
        let is_primitive = matches!(
            token.lexeme.as_str(),
            "i8" | "i16" | "i32" | "i64"
            | "u8" | "u16" | "u32" | "u64"
            | "f32" | "f64" | "bool"
            | "string" | "void" | "char"
        );

        if is_primitive {
            Ok(TypeExpr::Primitive(token.lexeme, token.span))
        } else {
            Ok(TypeExpr::Named(token.lexeme, token.span))
        }
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

          loop {
            if self.match_token(TokenKind::LeftParen) {
                // Function call: expr(args)
                let mut arguments = vec![];

                if !self.check(TokenKind::RightParen) {
                    loop {
                        arguments.push(self.expression()?);
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                }

                let right_paren = self.consume_expected(TokenKind::RightParen, "a closing ')'")?;

                let span = Span {
                    line: expr.span().line,
                    col: expr.span().col,
                    start: expr.span().start,
                    end: right_paren.span.end,
                };

                expr = Expr::Call { callee: Box::new(expr), arguments, span };

            } else if self.match_token(TokenKind::Dot) {
                // Field access or method call: expr.name or expr.name(args)
                let field = self.consume_expected(
                    TokenKind::Identifier,
                    "a field or method name after '.'",
                )?;

                let access_span = Span {
                    line: expr.span().line,
                    col: expr.span().col,
                    start: expr.span().start,
                    end: field.span.end,
                };

                // Peek — if next token is `(` it's a method call
                // We still produce FieldAccess here; the semantic layer
                // will see Call { callee: FieldAccess{..}, .. } and desugar it
                expr = Expr::FieldAccess {
                    object: Box::new(expr),
                    field: field.lexeme,
                    span: access_span,
                };

            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical()?;

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
                },

                Expr::FieldAccess { object, field, span: fa_span } => {
                    let span = Span {
                        line: fa_span.line,
                        col: fa_span.col,
                        start: fa_span.start,
                        end: value.span().end,
                    };
                    return Ok(Expr::FieldAssignment {
                        object,
                        field,
                        value: Box::new(value),
                        span,
                    });
                },

                _ => {
                    return Err(self.error_at(&equals, &format!("Syntax Error: Assignment target must be a mutable identifier variable name, found unexpected target: {:?}", expr)));
                }
            }
        }

        let compound_op = if self.match_token(TokenKind::PlusEqual) {
            Some(TokenKind::Plus)
        } else if self.match_token(TokenKind::MinusEqual) {
            Some(TokenKind::Minus)
        } else if self.match_token(TokenKind::StarEqual) {
            Some(TokenKind::Star)
        } else if self.match_token(TokenKind::SlashEqual) {
            Some(TokenKind::Slash)
        } else {
            None
        };

        if let Some(op_kind) = compound_op {
            let op_token = self.previous();
            let value = self.assignment()?;

            match expr {
                Expr::Identifier(name, id_span) => {
                    let span = Span {
                        line: id_span.line,
                        col: id_span.col,
                        start: id_span.start,
                        end: value.span().end,
                    };
                    // Desugar: x += y  =>  x = x + y
                    let binary = Expr::Binary {
                        left: Box::new(Expr::Identifier(name.clone(), id_span)),
                        right: Box::new(value),
                        operator: Token { kind: op_kind, lexeme: op_token.lexeme, span: op_token.span },
                        span,
                    };
                    return Ok(Expr::Assignment {
                        name,
                        value: Box::new(binary),
                        span,
                    });
                }
                _ => {
                    return Err(self.error_at(&op_token, "Syntax Error: Compound assignment target must be an identifier."));
                }
            }
        }

        Ok(expr)
    }

    fn logical(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while self.match_token(TokenKind::AmpersandAmpersand)
            || self.match_token(TokenKind::PipePipe)
        {
            let operator = self.previous();
            let right = self.equality()?;
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

        if self.match_token(TokenKind::Star) {
            let operator = self.previous();
            let right = self.unary()?;
            let span = Span {
                line: operator.span.line,
                col: operator.span.col,
                start: operator.span.start,
                end: right.span().end,
            };
            return Ok(Expr::Deref(Box::new(right), span));
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
            TokenKind::Char => Ok(Expr::Char(token.lexeme.chars().next().unwrap(), token.span)),

            TokenKind::True => Ok(Expr::Bool(true, token.span)),
            TokenKind::False => Ok(Expr::Bool(false, token.span)),

            TokenKind::SelfKw => Ok(Expr::Identifier("self".to_string(), token.span)),

            TokenKind::Identifier => {
                if self.match_token(TokenKind::ColonColon) {
                    let method = self.consume_expected(
                        TokenKind::Identifier,
                        "a method name after '::'",
                    )?;
                    self.consume_expected(TokenKind::LeftParen, "a '(' after method name")?;

                    let mut arguments = vec![];
                    if !self.check(TokenKind::RightParen) {
                        loop {
                            arguments.push(self.expression()?);
                            if !self.match_token(TokenKind::Comma) {
                                break;
                            }
                        }
                    }
                    let right_paren = self.consume_expected(TokenKind::RightParen, "a closing ')'")?;

                    let span = Span {
                        line: token.span.line,
                        col: token.span.col,
                        start: token.span.start,
                        end: right_paren.span.end,
                    };

                    return Ok(Expr::StaticCall {
                        type_name: token.lexeme,
                        method: method.lexeme,
                        arguments,
                        span,
                    });
                }

                // Only treat as struct literal if name starts with uppercase
                // This avoids ambiguity with function calls like printf(...)
                if self.check(TokenKind::LeftBrace) 
                    && token.lexeme.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) 
                {
                    return self.struct_literal(token);
                }
                Ok(Expr::Identifier(token.lexeme, token.span))
            }

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
                    Expr::Char(s, _) => Expr::Char(s, span),
                    Expr::Bool(b, _) => Expr::Bool(b, span),
                    Expr::Identifier(i, _) => Expr::Identifier(i, span),
                    Expr::Call { callee, arguments, .. } => Expr::Call { callee, arguments, span },
                    Expr::Assignment { name, value, .. } => Expr::Assignment { name, value, span },
                    Expr::Error(e, _) => Expr::Error(e, span),
                    Expr::Binary { left, right, operator, .. } => Expr::Binary { left, right, operator, span },
                    Expr::Unary { operator, right, .. } => Expr::Unary { operator, right, span },
                    Expr::AddressOf(inner, _) => Expr::AddressOf(inner, span),
                    Expr::Deref(inner, _) => Expr::Deref(inner, span),
                    Expr::FieldAccess { object, field, .. } => Expr::FieldAccess { object, field, span },
                    Expr::FieldAssignment { object, field, value, span } => Expr::FieldAssignment { object, field, value, span },
                    Expr::StructLiteral { name, fields, .. } => Expr::StructLiteral { name, fields, span },
                    Expr::StaticCall { type_name, method, arguments, span } => Expr::StaticCall { type_name, method, arguments, span },
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

    fn struct_literal(&mut self, name_token: Token) -> Result<Expr, ParseError> {
        self.consume_expected(TokenKind::LeftBrace, "'{' for struct literal")?;

        let mut fields = vec![];

        while !self.is_at_end() && !self.check(TokenKind::RightBrace) {
            let field_name = self.consume_expected(TokenKind::Identifier, "a field name")?;
            self.consume_expected(TokenKind::Colon, "a ':' after field name")?;
            let value = self.expression()?;

            fields.push((field_name.lexeme, value));

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        let close = self.consume_expected(TokenKind::RightBrace, "a closing '}'")?;

        let span = Span {
            line: name_token.span.line,
            col: name_token.span.col,
            start: name_token.span.start,
            end: close.span.end,
        };

        Ok(Expr::StructLiteral {
            name: name_token.lexeme,
            fields,
            span,
        })
    }
}
