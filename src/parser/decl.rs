//! Declaration parsing for the Pascal parser

use crate::ast::{Block, FunctionDecl, Parameter, ProcedureDecl, SimpleType, Type, VariableDecl, FieldVisibility};
use crate::parser::{ParseResult, Parser};
use crate::tokens::Token;
use crate::ParseError;

impl<'a> Parser<'a> {
    /// Parse a complete program
    pub fn parse_program(&mut self) -> ParseResult<crate::ast::Program> {
        let name = self.parse_program_header()?;
        let uses = self.parse_uses_clause()?;
        let block = self.parse_block()?;

        // Expect final period
        self.consume(Token::Dot)?;

        Ok(crate::ast::Program { name, uses, block })
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
    /// Declaration sections (const, type, var, function, procedure) can appear in any order.
    pub fn parse_block(&mut self) -> ParseResult<Block> {
        let mut consts = vec![];
        let mut types = vec![];
        let mut vars = vec![];
        let mut procedures = vec![];
        let mut functions = vec![];

        loop {
            if self.check(Token::Const) {
                self.advance();
                while let Some(Token::Identifier(name)) = self.peek() {
                    let name = name.clone();
                    self.advance();
                    self.consume_or_skip(Token::Equal, &[Token::Var, Token::Begin]);

                    if let Some(literal) = self.parse_literal()? {
                        consts.push(crate::ast::ConstDecl {
                            name,
                            value: literal,
                            visibility: FieldVisibility::Public,
                        });
                    }

                    self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin]);
                }
            } else if self.check(Token::Type) {
                self.advance();
                while let Some(Token::Identifier(name)) = self.peek() {
                    let name = name.clone();
                    self.advance();
                    self.consume_or_skip(Token::Equal, &[Token::Var, Token::Begin]);

                    if let Ok(type_def) = self.parse_type() {
                        types.push(crate::ast::TypeDecl {
                            name,
                            type_definition: type_def,
                            visibility: FieldVisibility::Public,
                        });
                    }

                    self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin]);
                }
            } else if self.check(Token::Var) {
                self.advance();
                while let Some(Token::Identifier(_)) = self.peek() {
                    let mut names = vec![];
                    loop {
                        if let Some(Token::Identifier(name)) = self.current_token.take() {
                            names.push(name);
                            self.advance();
                        }
                        if self.check(Token::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.consume_or_skip(Token::Colon, &[Token::Begin]);

                    let var_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));

                    for name in names {
                        vars.push(VariableDecl {
                            name,
                            variable_type: var_type.clone(),
                            initial_value: None,
                            visibility: FieldVisibility::Public,
                            is_absolute: false,
                            absolute_address: None,
                        });
                    }

                    self.consume_or_skip(Token::Semicolon, &[Token::Begin]);
                }
            } else if self.check(Token::Procedure) {
                self.advance();
                let proc = self.parse_procedure_decl()?;
                procedures.push(proc);
            } else if self.check(Token::Function) {
                self.advance();
                let func = self.parse_function_decl()?;
                functions.push(func);
            } else {
                break;
            }
        }

        // Parse compound statement
        let statements = self.parse_compound_statement()?;

        Ok(Block {
            consts,
            types,
            vars,
            procedures,
            functions,
            statements,
        })
    }

    /// Parse procedure declaration (after 'procedure' keyword consumed)
    fn parse_procedure_decl(&mut self) -> ParseResult<ProcedureDecl> {
        let name = match self.peek() {
            Some(Token::Identifier(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err(ParseError::UnexpectedToken("expected procedure name".to_string())),
        };

        let parameters = self.parse_parameters()?;
        self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin, Token::Forward]);

        // Check for forward declaration
        if self.check(Token::Forward) {
            self.advance();
            self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);
            return Ok(ProcedureDecl {
                name,
                parameters,
                block: Block::empty(),
                visibility: FieldVisibility::Public,
                is_external: false,
                external_name: None,
                is_inline: false,
                is_forward: true,
                is_class_method: false,
                is_virtual: false,
                is_override: false,
                is_overload: false,
            });
        }

        let block = self.parse_block()?;
        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);

        Ok(ProcedureDecl {
            name,
            parameters,
            block,
            visibility: FieldVisibility::Public,
            is_external: false,
            external_name: None,
            is_inline: false,
            is_forward: false,
            is_class_method: false,
            is_virtual: false,
            is_override: false,
            is_overload: false,
        })
    }

    /// Parse function declaration (after 'function' keyword consumed)
    fn parse_function_decl(&mut self) -> ParseResult<FunctionDecl> {
        let name = match self.peek() {
            Some(Token::Identifier(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err(ParseError::UnexpectedToken("expected function name".to_string())),
        };

        let parameters = self.parse_parameters()?;

        // Parse return type: ': type'
        self.consume_or_skip(Token::Colon, &[Token::Semicolon, Token::Begin]);
        let return_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));
        self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin, Token::Forward]);

        // Check for forward declaration
        if self.check(Token::Forward) {
            self.advance();
            self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);
            return Ok(FunctionDecl {
                name,
                parameters,
                return_type,
                block: Block::empty(),
                visibility: FieldVisibility::Public,
                is_external: false,
                external_name: None,
                is_inline: false,
                is_forward: true,
                is_class_method: false,
                is_virtual: false,
                is_override: false,
                is_overload: false,
            });
        }

        let block = self.parse_block()?;
        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);

        Ok(FunctionDecl {
            name,
            parameters,
            return_type,
            block,
            visibility: FieldVisibility::Public,
            is_external: false,
            external_name: None,
            is_inline: false,
            is_forward: false,
            is_class_method: false,
            is_virtual: false,
            is_override: false,
            is_overload: false,
        })
    }

    /// Parse parameter list: (a, b: integer; c: real) or empty
    fn parse_parameters(&mut self) -> ParseResult<Vec<Parameter>> {
        let mut params = vec![];

        if !self.check(Token::LeftParen) {
            return Ok(params);
        }
        self.advance(); // consume '('

        if self.check(Token::RightParen) {
            self.advance();
            return Ok(params);
        }

        loop {
            let is_var = if self.check(Token::Var) {
                self.advance();
                true
            } else {
                false
            };

            let is_const = if self.check(Token::Const) {
                self.advance();
                true
            } else {
                false
            };

            // Collect comma-separated parameter names
            let mut names = vec![];
            loop {
                if let Some(Token::Identifier(name)) = self.peek() {
                    let name = name.clone();
                    self.advance();
                    names.push(name);
                }
                if self.check(Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            // Parse ': type'
            self.consume_or_skip(Token::Colon, &[Token::Semicolon, Token::RightParen]);
            let param_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));

            for name in names {
                params.push(Parameter {
                    name,
                    param_type: param_type.clone(),
                    is_var,
                    is_const,
                    is_out: false,
                    default_value: None,
                });
            }

            if self.check(Token::Semicolon) {
                self.advance();
            } else {
                break;
            }
        }

        self.consume_or_skip(Token::RightParen, &[Token::Semicolon, Token::Colon, Token::Begin]);
        Ok(params)
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

                while !self.check(Token::End) && self.peek().is_some() {
                    if let Some(Token::Identifier(_)) = self.peek() {
                        // Collect comma-separated field names
                        let mut names = vec![];
                        loop {
                            if let Some(Token::Identifier(name)) = self.peek() {
                                let name = name.clone();
                                self.advance();
                                names.push(name);
                            }
                            if self.check(Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.consume_or_skip(Token::Colon, &[Token::End, Token::Semicolon]);
                        if let Ok(field_type) = self.parse_type() {
                            for name in names {
                                fields.insert(name, Box::new(field_type.clone()));
                            }
                        }
                        self.consume_or_skip(Token::Semicolon, &[Token::End]);
                    } else {
                        break;
                    }
                }

                self.consume_or_skip(Token::End, &[Token::Semicolon]);
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
