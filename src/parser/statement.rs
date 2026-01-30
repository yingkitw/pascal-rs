//! Statement parsing for the Pascal parser

use crate::ast::{Block, Stmt};
use crate::parser::{ParseResult, Parser};
use crate::ParseError;
use crate::tokens::Token;

impl<'a> Parser<'a> {
    /// Parse a statement
    pub fn parse_statement(&mut self) -> ParseResult<Option<Stmt>> {
        Ok(match self.peek() {
            Some(Token::Identifier(_)) => self.parse_identifier_statement()?,
            Some(Token::If) => Some(self.parse_if_statement()?),
            Some(Token::While) => Some(self.parse_while_statement()?),
            Some(Token::For) => Some(self.parse_for_statement()?),
            Some(Token::Repeat) => Some(self.parse_repeat_statement()?),
            Some(Token::Begin) => Some(self.parse_begin_block()?),
            _ => None,
        })
    }

    /// Parse statement starting with identifier (assignment or call)
    fn parse_identifier_statement(&mut self) -> ParseResult<Option<Stmt>> {
        let name = if let Some(Token::Identifier(n)) = self.current_token.take() {
            self.advance();
            n
        } else {
            unreachable!()
        };

        if self.check(Token::ColonEquals) {
            // Assignment
            self.advance();
            if let Some(value) = self.parse_expression()? {
                Ok(Some(Stmt::Assignment { target: name, value }))
            } else {
                Ok(None)
            }
        } else if self.check(Token::LeftParen) {
            // Procedure call
            self.advance();
            let args = self.parse_argument_list()?;
            Ok(Some(Stmt::ProcedureCall { name, arguments: args }))
        } else {
            Ok(None)
        }
    }

    /// Parse if-then-else statement
    fn parse_if_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let condition = self
            .parse_expression()?
            .ok_or(ParseError::UnexpectedToken("expected condition".to_string()))?;
        self.consume_or_skip(Token::Then, &[Token::Begin]);

        let _then_block = self.parse_statement_block()?;

        if self.check(Token::Else) {
            self.advance();
            let _else_block = self.parse_statement_block()?;
            Ok(Stmt::If {
                condition,
                then_branch: vec![],
                else_branch: Some(vec![]),
            })
        } else {
            Ok(Stmt::If {
                condition,
                then_branch: vec![],
                else_branch: None,
            })
        }
    }

    /// Parse while loop statement
    fn parse_while_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let condition = self
            .parse_expression()?
            .ok_or(ParseError::UnexpectedToken("expected condition".to_string()))?;
        self.consume_or_skip(Token::Do, &[Token::Begin]);

        let _block = self.parse_statement_block()?;

        Ok(Stmt::While {
            condition,
            body: vec![],
        })
    }

    /// Parse for loop statement
    fn parse_for_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();

        let variable = if let Some(Token::Identifier(n)) = self.current_token.take() {
            self.advance();
            n
        } else {
            return Err(ParseError::UnexpectedToken(
                "expected loop variable".to_string(),
            ));
        };

        self.consume_or_skip(Token::ColonEquals, &[Token::To, Token::DownTo]);
        let start = self
            .parse_expression()?
            .ok_or(ParseError::UnexpectedToken("expected start value".to_string()))?;

        let is_downto = self.check(Token::DownTo);
        self.consume_or_skip(if is_downto { Token::DownTo } else { Token::To }, &[Token::Do]);

        let _end = self
            .parse_expression()?
            .ok_or(ParseError::UnexpectedToken("expected end value".to_string()))?;
        self.consume_or_skip(Token::Do, &[Token::Begin]);

        let _block = self.parse_statement_block()?;

        Ok(Stmt::Assignment {
            target: variable,
            value: start,
        })
    }

    /// Parse repeat-until loop statement
    fn parse_repeat_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();

        let _block = Block {
            const_decls: std::collections::HashMap::new(),
            type_decls: std::collections::HashMap::new(),
            var_decls: std::collections::HashMap::new(),
            statements: {
                let mut stmts = Vec::new();
                while !self.check(Token::Until) {
                    if let Some(stmt) = self.parse_statement()? {
                        stmts.push(stmt);
                    }
                    self.consume_or_skip(Token::Semicolon, &[Token::Until]);
                }
                stmts
            },
        };

        self.consume_or_skip(Token::Until, &[Token::Semicolon]);
        let condition = self
            .parse_expression()?
            .ok_or(ParseError::UnexpectedToken("expected condition".to_string()))?;

        Ok(Stmt::While {
            condition,
            body: vec![],
        })
    }

    /// Parse begin...end block
    fn parse_begin_block(&mut self) -> ParseResult<Stmt> {
        let statements = self.parse_compound_statement()?;
        Ok(Stmt::Block(statements))
    }

    /// Parse compound statement: begin ... end
    pub fn parse_compound_statement(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut statements = Vec::new();

        self.consume(Token::Begin)?;

        while !self.check(Token::End) && !self.check(Token::Dot) {
            if let Some(stmt) = self.parse_statement()? {
                statements.push(stmt);
            }
            self.consume_or_skip(Token::Semicolon, &[Token::End, Token::Else, Token::Until]);
        }

        self.consume(Token::End)?;
        Ok(statements)
    }

    /// Parse a statement block (begin-end or single statement)
    fn parse_statement_block(&mut self) -> ParseResult<Block> {
        Ok(if self.check(Token::Begin) {
            let statements = self.parse_compound_statement()?;
            Block {
                const_decls: std::collections::HashMap::new(),
                type_decls: std::collections::HashMap::new(),
                var_decls: std::collections::HashMap::new(),
                statements,
            }
        } else {
            Block {
                const_decls: std::collections::HashMap::new(),
                type_decls: std::collections::HashMap::new(),
                var_decls: std::collections::HashMap::new(),
                statements: vec![self.parse_statement()?.ok_or(ParseError::UnexpectedToken(
                    "expected statement".to_string(),
                ))?],
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_assignment() {
        let source = "x := 42";
        let mut parser = Parser::new(source);
        let stmt = parser.parse_statement();
        assert!(stmt.is_ok());
    }

    #[test]
    fn test_parse_while_loop() {
        let source = "while x > 0 do x := x - 1";
        let mut parser = Parser::new(source);
        let stmt = parser.parse_statement();
        assert!(stmt.is_ok());
    }

    #[test]
    fn test_parse_compound_statement() {
        let source = "begin x := 1; y := 2; end";
        let mut parser = Parser::new(source);
        let stmts = parser.parse_compound_statement();
        assert!(stmts.is_ok());
    }
}
