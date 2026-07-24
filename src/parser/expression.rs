//! Expression parsing for the Pascal parser

use crate::ast::{Expr, Literal};use crate::parser::{ParseResult, Parser};
use crate::tokens::Token;
use crate::utils::ast_helpers::*;
use crate::ParseError;

impl<'a> Parser<'a> {
    /// Parse expression
    pub fn parse_expression(&mut self) -> ParseResult<Option<Expr>> {
        self.parse_binary_op(0)
    }

    /// Parse binary operations with precedence
    pub fn parse_binary_op(&mut self, precedence: u8) -> ParseResult<Option<Expr>> {
        let mut left = self.parse_unary_op()?;

        while let Some(token) = self.peek() {
            let (op, prec) = self.get_binary_op_info(token);

            if let Some(op_str) = op {
                if prec < precedence {
                    break;
                }

                // Clone the operator string before advancing
                let op_string = op_str.to_string();
                self.advance();
                let right = self.parse_binary_op(prec + 1)?;

                left = Some(binop(
                    left.ok_or(ParseError::UnexpectedToken(
                        "expected left operand".to_string(),
                    ))?,
                    &op_string,
                    right.ok_or(ParseError::UnexpectedToken(
                        "expected right operand".to_string(),
                    ))?,
                ));
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Get binary operator info (operator string and precedence)
    fn get_binary_op_info(&self, token: &Token) -> (Option<&str>, u8) {
        match token {
            Token::Or => (Some("or"), 1),
            Token::Xor => (Some("xor"), 2),
            Token::And => (Some("and"), 3),
            Token::Equal => (Some("="), 4),
            Token::NotEqual => (Some("<>"), 4),
            Token::LessThan => (Some("<"), 4),
            Token::GreaterThan => (Some(">"), 4),
            Token::LessEqual => (Some("<="), 4),
            Token::GreaterEqual => (Some(">="), 4),
            Token::In => (Some("in"), 4),
            Token::Plus => (Some("+"), 5),
            Token::Minus => (Some("-"), 5),
            Token::Star => (Some("*"), 6),
            Token::Slash => (Some("/"), 6),
            Token::Divide => (Some("div"), 6),
            Token::Mod => (Some("mod"), 6),
            Token::Shl => (Some("shl"), 6),
            Token::Shr => (Some("shr"), 6),
            _ => (None, 0),
        }
    }

    /// Parse unary operations
    pub fn parse_unary_op(&mut self) -> ParseResult<Option<Expr>> {
        let op = match self.peek() {
            Some(Token::Not) => Some("not"),
            Some(Token::Minus) => Some("-"),
            Some(Token::Plus) => Some("+"),
            Some(Token::AddressOf) => Some("@"),
            _ => None,
        };

        if let Some(op_str) = op {
            self.advance();
            let operand = self.parse_unary_op()?;
            Ok(Some(unop(
                op_str,
                operand.ok_or(ParseError::UnexpectedToken("expected operand".to_string()))?,
            )))
        } else {
            self.parse_primary()
        }
    }

    /// Parse primary expression
    pub fn parse_primary(&mut self) -> ParseResult<Option<Expr>> {
        Ok(match self.peek() {
            Some(Token::Identifier(name)) => {
                let name = name.clone();
                self.advance();

                let mut result = if self.check(Token::LeftParen) {
                    // Function call
                    self.advance();
                    let args = self.parse_argument_list()?;
                    call(&name, args)
                } else {
                    var(&name)
                };

                // Handle dot-notation and bracket indexing
                loop {
                    if self.check(Token::Dot) {
                        self.advance();
                        if let Some(Token::Identifier(member)) = self.peek() {
                            let member = member.clone();
                            self.advance();
                            let base_name = match &result {
                                Expr::Variable(n) => n.clone(),
                                _ => "self".to_string(),
                            };
                            let dotted = format!("{}.{}", base_name, member);
                            if self.check(Token::LeftParen) {
                                self.advance();
                                let args = self.parse_argument_list()?;
                                result = Expr::FunctionCall {
                                    name: dotted,
                                    arguments: args,
                                };
                            } else {
                                result = var(&dotted);
                            }
                        } else {
                            break;
                        }
                    } else if self.check(Token::LeftBracket) {
                        // Array/string indexing: arr[i] or s[i]
                        self.advance();
                        let index = self.parse_expression()?;
                        self.consume_or_skip(Token::RightBracket, &[Token::Semicolon, Token::End]);
                        if let Some(idx_expr) = index {
                            // Encode as FunctionCall "__index__(arr, i)"
                            result = Expr::FunctionCall {
                                name: "__index__".to_string(),
                                arguments: vec![result, idx_expr],
                            };
                        }
                    } else if self.check(Token::Caret) {
                        // Pointer dereference: p^
                        self.advance();
                        result = Expr::Dereference { expression: Box::new(result) };
                    } else {
                        break;
                    }
                }

                Some(result)
            }
            Some(Token::IntegerLiteral(_))
            | Some(Token::RealLiteral(_))
            | Some(Token::StringLiteral(_))
            | Some(Token::PascalStringLiteral(_))
            | Some(Token::CharLiteral(_))
            | Some(Token::True)
            | Some(Token::False) => {
                self.parse_literal()?.map(Expr::Literal)
            }
            Some(Token::LeftParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume_or_skip(
                    Token::RightParen,
                    &[Token::Semicolon, Token::End, Token::Comma],
                );
                expr
            }
            Some(Token::Function) | Some(Token::Procedure) => self.parse_lambda_expression()?,
            Some(Token::LeftBracket) => {
                // Set literal: [a, b, c]
                self.advance();
                let mut elements = vec![];
                if !self.check(Token::RightBracket) {
                    loop {
                        if let Some(elem) = self.parse_expression()? {
                            elements.push(elem);
                        }
                        match self.peek() {
                            Some(Token::Comma) => {
                                self.advance();
                                continue;
                            }
                            Some(Token::RightBracket) => break,
                            _ => break,
                        }
                    }
                }
                self.consume_or_skip(Token::RightBracket, &[Token::Semicolon, Token::End]);
                Some(Expr::Set { elements })
            }
            _ => None,
        })
    }

    /// Parse argument list: (arg1, arg2, ...)
    pub fn parse_argument_list(&mut self) -> ParseResult<Vec<Expr>> {
        let mut args = Vec::new();

        if !self.check(Token::RightParen) {
            loop {
                if let Some(arg) = self.parse_expression()? {
                    args.push(arg);
                }

                match self.peek() {
                    Some(Token::Comma) => {
                        self.advance();
                        continue;
                    }
                    Some(Token::RightParen) => break,
                    _ => break,
                }
            }
        }

        self.consume_or_skip(Token::RightParen, &[Token::Semicolon, Token::End]);
        Ok(args)
    }

    /// Parse an anonymous function / lambda expression.
    ///
    /// Syntax: `function(params): type begin ... end` or `procedure(params) begin ... end`
    pub fn parse_lambda_expression(&mut self) -> ParseResult<Option<Expr>> {
        let is_function = self.check(Token::Function);
        self.advance(); // function | procedure

        let mut params: Vec<crate::ast::Parameter> = Vec::new();
        if self.check(Token::LeftParen) {
            self.advance();
            if !self.check(Token::RightParen) {
                loop {
                    // collect names
                    let mut names: Vec<String> = Vec::new();
                    loop {
                        if let Some(Token::Identifier(name)) = self.peek() {
                            names.push(name.clone());
                            self.advance();
                        }
                        if self.check(Token::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.consume_or_skip(Token::Colon, &[Token::Semicolon, Token::RightParen]);
                    let ptype = self.parse_type().unwrap_or(crate::ast::Type::Simple(
                        crate::ast::SimpleType::Integer,
                    ));
                    for n in names {
                        params.push(crate::ast::Parameter {
                            name: n,
                            param_type: ptype.clone(),
                            is_var: false,
                            is_const: false,
                            is_out: false,
                            default_value: None,
                        });
                    }
                    if self.check(Token::Semicolon) {
                        self.advance();
                        continue;
                    }
                    break;
                }
            }
            self.consume_or_skip(Token::RightParen, &[Token::Colon, Token::Begin]);
        }

        let return_type = if is_function {
            self.consume_or_skip(Token::Colon, &[Token::Begin]);
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = if self.check(Token::Begin) {
            let stmts = self.parse_compound_statement()?;
            crate::ast::Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                statements: stmts,
            }
        } else {
            crate::ast::Block::empty()
        };

        Ok(Some(Expr::Lambda {
            params,
            body: Box::new(body),
            return_type,
            captures: true,
        }))
    }

    /// Parse a literal value
    pub fn parse_literal(&mut self) -> ParseResult<Option<Literal>> {
        Ok(match self.peek() {
            Some(Token::IntegerLiteral(n)) => {
                let lit = Some(Literal::Integer(*n));
                self.advance();
                lit
            }
            Some(Token::RealLiteral(r)) => {
                let lit = Some(Literal::Real(*r));
                self.advance();
                lit
            }
            Some(Token::StringLiteral(s)) => {
                let lit = Some(Literal::String(s.clone()));
                self.advance();
                lit
            }
            Some(Token::PascalStringLiteral(s)) => {
                let s = s.clone();
                self.advance();
                if s.len() == 1 {
                    Some(Literal::Char(s.chars().next().unwrap()))
                } else {
                    Some(Literal::String(s))
                }
            }
            Some(Token::CharLiteral(c)) => {
                let lit = Some(Literal::Char(*c));
                self.advance();
                lit
            }
            Some(Token::True) => {
                self.advance();
                Some(Literal::Boolean(true))
            }
            Some(Token::False) => {
                self.advance();
                Some(Literal::Boolean(false))
            }
            _ => None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_expression() {
        let source = "42";
        let mut parser = Parser::new(source);
        let expr = parser.parse_expression();
        assert!(expr.is_ok());
    }

    #[test]
    fn test_parse_binary_op() {
        let source = "1 + 2 * 3";
        let mut parser = Parser::new(source);
        let expr = parser.parse_expression();
        assert!(expr.is_ok());
    }

    #[test]
    fn test_parse_function_call() {
        let source = "foo(1, 2, 3)";
        let mut parser = Parser::new(source);
        let expr = parser.parse_expression();
        assert!(expr.is_ok());
    }
}
