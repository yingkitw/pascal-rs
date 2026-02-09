//! Statement parsing for the Pascal parser

use crate::ast::{Block, CaseBranch, ExceptClause, ForDirection, Stmt};
use crate::parser::{ParseResult, Parser};
use crate::tokens::Token;
use crate::ParseError;

impl<'a> Parser<'a> {
    /// Parse a statement
    pub fn parse_statement(&mut self) -> ParseResult<Option<Stmt>> {
        Ok(match self.peek() {
            Some(Token::Identifier(_)) => self.parse_identifier_statement()?,
            Some(Token::If) => Some(self.parse_if_statement()?),
            Some(Token::While) => Some(self.parse_while_statement()?),
            Some(Token::For) => Some(self.parse_for_statement()?),
            Some(Token::Repeat) => Some(self.parse_repeat_statement()?),
            Some(Token::Case) => Some(self.parse_case_statement()?),
            Some(Token::Begin) => Some(self.parse_begin_block()?),
            Some(Token::Try) => Some(self.parse_try_statement()?),
            Some(Token::Raise) => Some(self.parse_raise_statement()?),
            Some(Token::Exit) => {
                self.advance();
                let args = if self.check(Token::LeftParen) {
                    self.advance();
                    let a = self.parse_argument_list()?;
                    a
                } else {
                    vec![]
                };
                Some(Stmt::ProcedureCall {
                    name: "exit".to_string(),
                    arguments: args,
                })
            }
            Some(Token::Break) => {
                self.advance();
                Some(Stmt::ProcedureCall {
                    name: "break".to_string(),
                    arguments: vec![],
                })
            }
            _ => None,
        })
    }

    /// Parse statement starting with identifier (assignment or call)
    fn parse_identifier_statement(&mut self) -> ParseResult<Option<Stmt>> {
        let mut name = if let Some(Token::Identifier(n)) = self.current_token.take() {
            self.advance();
            n
        } else {
            unreachable!()
        };

        // Handle dot notation for record field access: p.x, p.x.y, etc.
        while self.check(Token::Dot) {
            self.advance();
            if let Some(Token::Identifier(field)) = self.peek() {
                let field = field.clone();
                self.advance();
                name = format!("{}.{}", name, field);
            } else {
                break;
            }
        }

        if self.check(Token::ColonEquals) {
            // Assignment
            self.advance();
            if let Some(value) = self.parse_expression()? {
                Ok(Some(Stmt::Assignment {
                    target: name,
                    value,
                }))
            } else {
                Ok(None)
            }
        } else if self.check(Token::LeftParen) {
            // Procedure call with arguments
            self.advance();
            let args = self.parse_argument_list()?;
            Ok(Some(Stmt::ProcedureCall {
                name,
                arguments: args,
            }))
        } else {
            // Procedure call without arguments
            Ok(Some(Stmt::ProcedureCall {
                name,
                arguments: vec![],
            }))
        }
    }

    /// Parse if-then-else statement
    fn parse_if_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let condition = self.parse_expression()?.ok_or(ParseError::UnexpectedToken(
            "expected condition".to_string(),
        ))?;
        self.consume_or_skip(Token::Then, &[Token::Begin]);

        let then_block = self.parse_statement_block()?;

        if self.check(Token::Else) {
            self.advance();
            let else_block = self.parse_statement_block()?;
            Ok(Stmt::If {
                condition,
                then_branch: then_block.statements,
                else_branch: Some(else_block.statements),
            })
        } else {
            Ok(Stmt::If {
                condition,
                then_branch: then_block.statements,
                else_branch: None,
            })
        }
    }

    /// Parse while loop statement
    fn parse_while_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let condition = self.parse_expression()?.ok_or(ParseError::UnexpectedToken(
            "expected condition".to_string(),
        ))?;
        self.consume_or_skip(Token::Do, &[Token::Begin]);

        let block = self.parse_statement_block()?;

        Ok(Stmt::While {
            condition,
            body: block.statements,
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
        let start = self.parse_expression()?.ok_or(ParseError::UnexpectedToken(
            "expected start value".to_string(),
        ))?;

        let is_downto = self.check(Token::DownTo);
        let direction = if is_downto {
            ForDirection::DownTo
        } else {
            ForDirection::To
        };
        self.consume_or_skip(
            if is_downto { Token::DownTo } else { Token::To },
            &[Token::Do],
        );

        let end = self.parse_expression()?.ok_or(ParseError::UnexpectedToken(
            "expected end value".to_string(),
        ))?;
        self.consume_or_skip(Token::Do, &[Token::Begin]);

        let block = self.parse_statement_block()?;

        Ok(Stmt::For {
            var_name: variable,
            start,
            end,
            direction,
            body: block.statements,
        })
    }

    /// Parse repeat-until loop statement
    fn parse_repeat_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();

        let mut stmts = Vec::new();
        while !self.check(Token::Until) && self.peek().is_some() {
            if let Some(stmt) = self.parse_statement()? {
                stmts.push(stmt);
            }
            // Semicolons between statements in repeat block
            if self.check(Token::Semicolon) {
                self.advance();
            }
        }

        self.consume_or_skip(Token::Until, &[Token::Semicolon]);
        let condition = self.parse_expression()?.ok_or(ParseError::UnexpectedToken(
            "expected condition".to_string(),
        ))?;

        Ok(Stmt::Repeat {
            body: stmts,
            until_condition: condition,
        })
    }

    /// Parse case statement: case expr of value: stmt; ... else stmt; end
    fn parse_case_statement(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'case'
        let expression = self.parse_expression()?.ok_or(ParseError::UnexpectedToken(
            "expected case expression".to_string(),
        ))?;
        self.consume_or_skip(Token::Of, &[Token::End]);

        let mut branches = Vec::new();
        let mut else_branch = None;

        while !self.check(Token::End) && self.peek().is_some() {
            if self.check(Token::Else) {
                self.advance();
                // Parse else branch statements
                let mut stmts = Vec::new();
                while !self.check(Token::End) && !self.check(Token::Semicolon) && self.peek().is_some() {
                    if let Some(stmt) = self.parse_statement()? {
                        stmts.push(stmt);
                    }
                    if self.check(Token::Semicolon) {
                        self.advance();
                    }
                }
                else_branch = Some(stmts);
                break;
            }

            // Parse case values (comma-separated expressions)
            let mut values = Vec::new();
            loop {
                if let Some(val) = self.parse_expression()? {
                    values.push(val);
                }
                if self.check(Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            self.consume_or_skip(Token::Colon, &[Token::End, Token::Semicolon]);

            // Parse the branch body (single statement)
            let mut body = Vec::new();
            if let Some(stmt) = self.parse_statement()? {
                body.push(stmt);
            }

            branches.push(CaseBranch { values, body });

            if self.check(Token::Semicolon) {
                self.advance();
            }
        }

        self.consume_or_skip(Token::End, &[Token::Semicolon, Token::Dot]);

        Ok(Stmt::Case {
            expression,
            branches,
            else_branch,
        })
    }

    /// Parse begin...end block
    fn parse_begin_block(&mut self) -> ParseResult<Stmt> {
        let statements = self.parse_compound_statement()?;
        Ok(Stmt::Block(Block::with_statements(statements)))
    }

    /// Parse try statement: try ... except ... end or try ... finally ... end
    fn parse_try_statement(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'try'

        // Parse try block statements
        let mut try_block = Vec::new();
        while !self.check(Token::Except) && !self.check(Token::Finally) && self.peek().is_some() {
            if let Some(stmt) = self.parse_statement()? {
                try_block.push(stmt);
            }
            if self.check(Token::Semicolon) {
                self.advance();
            }
        }

        let mut except_clauses = Vec::new();
        let mut finally_block = None;

        if self.check(Token::Except) {
            self.advance();

            // Parse except clauses
            while !self.check(Token::End) && !self.check(Token::Finally) && self.peek().is_some() {
                // Check for 'on ExceptionType do' or 'on E: ExceptionType do'
                let is_on = matches!(self.peek(), Some(Token::Identifier(s)) if s == "on");
                if is_on {
                    self.advance(); // consume 'on'

                    let mut exception_type = None;
                    let mut variable = None;

                    // Parse 'E: ExceptionType' or just 'ExceptionType'
                    if let Some(Token::Identifier(first)) = self.peek() {
                        let first = first.clone();
                        self.advance();

                        if self.check(Token::Colon) {
                            // E: ExceptionType
                            self.advance();
                            variable = Some(first);
                            if let Some(Token::Identifier(etype)) = self.peek() {
                                exception_type = Some(etype.clone());
                                self.advance();
                            }
                        } else {
                            // Just ExceptionType
                            exception_type = Some(first);
                        }
                    }

                    self.consume_or_skip(Token::Do, &[Token::Begin, Token::End]);

                    // Parse handler body
                    let mut body = Vec::new();
                    if self.check(Token::Begin) {
                        body = self.parse_compound_statement()?;
                    } else if let Some(stmt) = self.parse_statement()? {
                        body.push(stmt);
                    }

                    except_clauses.push(ExceptClause {
                        exception_type,
                        variable,
                        body,
                    });

                    if self.check(Token::Semicolon) {
                        self.advance();
                    }
                    continue;
                }

                // Default except handler (no 'on' clause)
                let mut body = Vec::new();
                while !self.check(Token::End) && self.peek().is_some() {
                    if let Some(stmt) = self.parse_statement()? {
                        body.push(stmt);
                    }
                    if self.check(Token::Semicolon) {
                        self.advance();
                    }
                }
                except_clauses.push(ExceptClause {
                    exception_type: None,
                    variable: None,
                    body,
                });
                break;
            }
        }

        if self.check(Token::Finally) {
            self.advance();
            let mut final_stmts = Vec::new();
            while !self.check(Token::End) && self.peek().is_some() {
                if let Some(stmt) = self.parse_statement()? {
                    final_stmts.push(stmt);
                }
                if self.check(Token::Semicolon) {
                    self.advance();
                }
            }
            finally_block = Some(final_stmts);
        }

        self.consume_or_skip(Token::End, &[Token::Semicolon, Token::Dot]);

        Ok(Stmt::Try {
            try_block,
            except_clauses,
            finally_block,
        })
    }

    /// Parse raise statement: raise or raise Exception.Create('msg')
    fn parse_raise_statement(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'raise'

        // raise; (re-raise current exception)
        if self.check(Token::Semicolon) || self.check(Token::End) {
            return Ok(Stmt::Raise {
                exception: None,
                message: None,
            });
        }

        // raise Expression
        let exception = self.parse_expression()?;

        Ok(Stmt::Raise {
            exception,
            message: None,
        })
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
            Block::with_statements(statements)
        } else {
            Block::with_statements(vec![self.parse_statement()?.ok_or(
                ParseError::UnexpectedToken("expected statement".to_string()),
            )?])
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
