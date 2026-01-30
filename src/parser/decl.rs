//! Declaration parsing for the Pascal parser

use crate::ast::{Expr, SimpleType, Type};
use crate::enhanced_ast;
use crate::parser::{ParseResult, Parser};
use crate::ParseError;
use crate::tokens::Token;

impl<'a> Parser<'a> {
    /// Parse a complete program
    pub fn parse_program(&mut self) -> ParseResult<enhanced_ast::Program> {
        let name = self.parse_program_header()?;
        let uses = self.parse_uses_clause()?;
        let block = self.parse_block()?;

        // Expect final period
        self.consume(Token::Dot)?;

        Ok(enhanced_ast::Program { name, uses, block })
    }

    /// Parse program header: program Name;
    fn parse_program_header(&mut self) -> ParseResult<String> {
        self.consume(Token::Program)?;

        let name = match self.peek() {
            Some(Token::Identifier(_)) => {
                if let Some(Token::Identifier(name)) = self.current_token.take() {
                    self.advance();
                    name
                } else {
                    unreachable!()
                }
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    "expected program name".to_string(),
                ))
            }
        };

        self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin, Token::Uses]);
        Ok(name)
    }

    /// Parse uses clause: uses Unit1, Unit2;
    fn parse_uses_clause(&mut self) -> ParseResult<Vec<String>> {
        let mut uses = Vec::new();

        if self.check(Token::Uses) {
            self.advance();

            loop {
                if let Some(Token::Identifier(name)) = self.current_token.take() {
                    uses.push(name);
                    self.advance();

                    match self.peek() {
                        Some(Token::Comma) => {
                            self.advance();
                            continue;
                        }
                        Some(Token::Semicolon) => {
                            self.advance();
                            break;
                        }
                        _ => break,
                    }
                } else {
                    break;
                }
            }
        }

        Ok(uses)
    }

    /// Parse a block (declarations + compound statement)
    pub fn parse_block(&mut self) -> ParseResult<enhanced_ast::Block> {
        let mut consts = vec![];
        let mut types = vec![];
        let mut vars = vec![];

        // Parse const section
        while self.check(Token::Const) {
            self.advance();
            while let Some(Token::Identifier(name)) = self.peek() {
                let name = name.clone();
                self.advance();
                self.consume_or_skip(Token::Equal, &[Token::Var, Token::Begin]);

                if let Some(literal) = self.parse_literal()? {
                    consts.push(enhanced_ast::Constant {
                        name,
                        constant_type: None,
                        value: Expr::Literal(literal),
                    });
                }

                self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin]);
            }
        }

        // Parse type section
        while self.check(Token::Type) {
            self.advance();
            while let Some(Token::Identifier(name)) = self.peek() {
                let name = name.clone();
                self.advance();
                self.consume_or_skip(Token::Equal, &[Token::Var, Token::Begin]);

                if let Ok(_type_def) = self.parse_type() {
                    types.push(enhanced_ast::TypeDefinition {
                        name,
                        type_definition: enhanced_ast::EnhancedType::Integer, // Simplified
                    });
                }

                self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin]);
            }
        }

        // Parse var section
        while self.check(Token::Var) {
            self.advance();
            while let Some(Token::Identifier(name)) = self.peek() {
                let name = name.clone();
                self.advance();
                self.consume_or_skip(Token::Colon, &[Token::Begin]);

                if let Ok(_var_type) = self.parse_type() {
                    vars.push(enhanced_ast::Variable {
                        name,
                        variable_type: enhanced_ast::EnhancedType::Integer, // Simplified
                        initial_value: None,
                        is_absolute: false,
                        absolute_address: None,
                        is_external: false,
                        external_name: None,
                        is_public: false,
                        is_exported: false,
                    });
                }

                self.consume_or_skip(Token::Semicolon, &[Token::Begin]);
            }
        }

        // Parse compound statement
        let statements = self.parse_compound_statement()?;

        Ok(enhanced_ast::Block {
            consts,
            types,
            vars,
            procedures: vec![],
            functions: vec![],
            statements,
            labels: vec![],
        })
    }

    /// Parse a type
    pub fn parse_type(&mut self) -> ParseResult<Type> {
        Ok(match self.peek() {
            Some(Token::Integer) => {
                self.advance();
                Type::Simple(SimpleType::Integer)
            }
            Some(Token::Real) => {
                self.advance();
                Type::Simple(SimpleType::Real)
            }
            Some(Token::Boolean) => {
                self.advance();
                Type::Simple(SimpleType::Boolean)
            }
            Some(Token::Char) => {
                self.advance();
                Type::Simple(SimpleType::Char)
            }
            Some(Token::String) => {
                self.advance();
                Type::Simple(SimpleType::String)
            }
            Some(Token::Array) => {
                self.advance();
                self.consume_or_skip(Token::LeftBracket, &[Token::Of, Token::Semicolon]);
                // Parse index type (simplified)
                let index_type = Box::new(Type::Simple(SimpleType::Integer));
                self.consume_or_skip(Token::RightBracket, &[Token::Of, Token::Semicolon]);
                self.consume_or_skip(Token::Of, &[Token::Semicolon]);
                let element_type = Box::new(self.parse_type()?);
                Type::Array {
                    index_type,
                    element_type,
                    range: None,
                }
            }
            Some(Token::Record) => {
                self.advance();
                let mut fields = std::collections::HashMap::new();
                self.consume_or_skip(Token::LeftBrace, &[Token::End, Token::Semicolon]);

                while !self.check(Token::RightBrace) && !self.check(Token::End) {
                    if let Some(Token::Identifier(name)) = self.peek() {
                        let name = name.clone();
                        self.advance();
                        self.consume_or_skip(Token::Colon, &[Token::End, Token::Semicolon]);
                        if let Ok(field_type) = self.parse_type() {
                            fields.insert(name, Box::new(field_type));
                        }
                        self.consume_or_skip(Token::Semicolon, &[Token::End, Token::RightBrace]);
                    } else {
                        break;
                    }
                }

                self.consume_or_skip(Token::RightBrace, &[Token::Semicolon]);
                Type::Record {
                    fields,
                    is_packed: false,
                }
            }
            Some(Token::Pointer) => {
                self.advance();
                Type::Pointer(Box::new(self.parse_type()?))
            }
            _ => Type::Simple(SimpleType::Integer), // Default
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_program_header() {
        let source = "program Test;";
        let mut parser = Parser::new(source);
        let name = parser.parse_program_header();
        assert!(name.is_ok());
        assert_eq!(name.unwrap(), "Test");
    }

    #[test]
    fn test_parse_uses_clause() {
        let source = "uses SysUtils, Classes;";
        let mut parser = Parser::new(source);
        let uses = parser.parse_uses_clause();
        assert!(uses.is_ok());
        assert_eq!(uses.unwrap().len(), 2);
    }

    #[test]
    fn test_parse_type() {
        let source = "integer";
        let mut parser = Parser::new(source);
        let typ = parser.parse_type();
        assert!(typ.is_ok());
    }
}
