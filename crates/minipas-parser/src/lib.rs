use minipas_ast::*;
use minipas_lexer::{Lexer, Token, LexerError};
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::fmt;

pub mod enhanced_parser;
pub mod traits;
pub mod mocks;

pub use enhanced_parser::{EnhancedParser, SymbolInfo, SymbolType};
pub use traits::*;
pub use mocks::*;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: Vec<String>,
        found: Option<Token>,
        position: usize,
    },
    UnexpectedEof {
        expected: Vec<String>,
    },
    LexerError(LexerError),
    Other(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                position,
            } => {
                let expected_str = expected.join(", ");
                if let Some(token) = found {
                    write!(
                        f,
                        "Expected one of: {} at position {}, but found {:?}",
                        expected_str, position, token
                    )
                } else {
                    write!(
                        f,
                        "Expected one of: {} at position {}, but reached end of input",
                        expected_str, position
                    )
                }
            }
            ParseError::UnexpectedEof { expected } => {
                let expected_str = expected.join(", ");
                write!(
                    f,
                    "Unexpected end of file, expected one of: {}",
                    expected_str
                )
            }
            ParseError::LexerError(e) => write!(f, "Lexer error: {}", e),
            ParseError::Other(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl std::error::Error for ParseError {}

impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> Self {
        ParseError::LexerError(err)
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peeked: Option<Result<(usize, Token, usize), LexerError>>,
    current_scope: Vec<HashMap<String, Type>>,
}

// Implement Clone manually for Parser since the Lexer doesn't implement Clone
impl<'a> Clone for Parser<'a> {
    fn clone(&self) -> Self {
        // Create a new lexer with the same input
        let lexer = Lexer::new(self.lexer.source());
        
        // Clone the peeked token if it exists
        let peeked = match &self.peeked {
            Some(Ok((start, token, end))) => {
                Some(Ok((*start, token.clone(), *end)))
            }
            Some(Err(e)) => Some(Err(e.clone())),
            None => None,
        };
        
        // Clone the current scope
        let current_scope = self.current_scope.clone();
        
        Self {
            lexer,
            peeked,
            current_scope,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
            peeked: None,
            current_scope: vec![HashMap::new()],
        }
    }
    
    fn enter_scope(&mut self) {
        self.current_scope.push(HashMap::new());
    }
    
    fn exit_scope(&mut self) -> Option<HashMap<String, Type>> {
        self.current_scope.pop()
    }
    
    fn declare_variable(&mut self, name: &str, typ: Type) -> Result<()> {
        if let Some(scope) = self.current_scope.last_mut() {
            if scope.contains_key(name) {
                return Err(anyhow!("Variable '{}' is already defined in this scope", name));
            }
            scope.insert(name.to_string(), typ);
            Ok(())
        } else {
            Err(anyhow!("No active scope"))
        }
    }
    
    fn lookup_variable(&self, name: &str) -> Option<Type> {
        for scope in self.current_scope.iter().rev() {
            if let Some(typ) = scope.get(name) {
                return Some(typ.clone());
            }
        }
        None
    }

    fn next_token(&mut self) -> Option<Result<(usize, Token, usize), LexerError>> {
        if let Some(peeked) = self.peeked.take() {
            Some(peeked)
        } else {
            self.lexer.next()
        }
    }

    fn peek_token(&mut self) -> Option<Result<(usize, Token, usize), LexerError>> {
        if self.peeked.is_none() {
            self.peeked = self.lexer.next_token();
        }
        // Create a new result with cloned data instead of cloning the entire Option
        match &self.peeked {
            Some(Ok((start, token, end))) => {
                Some(Ok((*start, token.clone(), *end)))
            }
            Some(Err(e)) => Some(Err(e.clone())),
            None => None,
        }
    }
    
    fn skip_whitespace_and_comments(&mut self) {
        while let Some(Ok((_, Token::Whitespace | Token::LineComment | Token::BlockComment, _))) = self.peek_token() {
            self.next_token();
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        // Skip any initial whitespace/comments
        self.skip_whitespace_and_comments();

        // Parse program header
        self.expect_token(Token::Program).map_err(|_| {
            ParseError::Other("Expected 'program' at the beginning of the file".to_string())
        })?;

        let name = self.expect_identifier()?;
        self.expect_token(Token::Semicolon)?;

        // Parse uses clause
        let mut uses = Vec::new();
        if let Some(Ok((_, Token::Identifier(id), _))) = self.peek_token() {
            if id.to_lowercase() == "uses" {
                self.next_token(); // Consume 'uses'
                uses = self.parse_identifier_list()?;
                self.expect_token(Token::Semicolon)?;
            }
        }

        // Parse block
        let block = self.parse_block()?;
        self.expect_token(Token::Dot)?;

        Ok(Program { name, uses, block })
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.enter_scope();
        
        // Parse const declarations
        let mut consts = Vec::new();
        while let Some(Ok((_, Token::Const, _))) = self.peek_token() {
            self.next_token(); // Consume 'const'
            consts.extend(self.parse_const_decl()?);
        }
        
        // Parse type declarations
        let mut types = Vec::new();
        while let Some(Ok((_, Token::Type, _))) = self.peek_token() {
            self.next_token(); // Consume 'type'
            types.extend(self.parse_type_decl()?);
        }
        
        // Parse var declarations
        let mut vars = Vec::new();
        while let Some(Ok((_, Token::Var, _))) = self.peek_token() {
            self.next_token(); // Consume 'var'
            vars.extend(self.parse_var_decl()?);
        }
        
        // Parse procedures and functions
        let mut procedures = Vec::new();
        let mut functions = Vec::new();
        
        while let Some(Ok((_, token, _))) = self.peek_token() {
            match token {
                Token::Procedure => {
                    self.next_token(); // Consume 'procedure'
                    procedures.push(self.parse_procedure_decl()?);
                }
                Token::Function => {
                    self.next_token(); // Consume 'function'
                    functions.push(self.parse_function_decl()?);
                }
                _ => break,
            }
        }
        
        // Parse main statements
        let statements = if let Some(Ok((_, Token::Begin, _))) = self.peek_token() {
            self.next_token(); // Consume 'begin'
            let stmts = self.parse_statements()?;
            self.expect_token(Token::End)?;
            stmts
        } else {
            Vec::new()
        };
        
        self.exit_scope();
        
        Ok(Block {
            consts,
            types,
            vars,
            procedures,
            functions,
            statements,
        })
    }
    
    fn parse_const_decl(&mut self) -> Result<Vec<ConstDecl>, ParseError> {
        let mut declarations = Vec::new();
        
        loop {
            let name = self.expect_identifier()?;
            self.expect_token(Token::Equal)?;
            let value = self.parse_expression()?;
            self.expect_token(Token::Semicolon)?;
            
            declarations.push(ConstDecl { name, value });
            
            // Check for next const declaration
            if let Some(Ok((_, Token::Identifier(_), _))) = self.peek_token() {
                // Continue parsing more consts
            } else {
                break;
            }
        }
        
        Ok(declarations)
    }
    
    fn parse_type_decl(&mut self) -> Result<Vec<TypeDecl>, ParseError> {
        let mut declarations = Vec::new();
        
        loop {
            let name = self.expect_identifier()?;
            self.expect_token(Token::Equal)?;
            let typ = self.parse_type()?;
            self.expect_token(Token::Semicolon)?;
            
            declarations.push(TypeDecl { name, typ });
            
            // Check for next type declaration
            if let Some(Ok((_, Token::Identifier(_), _))) = self.peek_token() {
                // Continue parsing more types
            } else {
                break;
            }
        }
        
        Ok(declarations)
    }
    
    fn parse_procedure_decl(&mut self) -> Result<ProcedureDecl, ParseError> {
        // Parse procedure header
        self.expect_token(Token::Procedure)?;
        let name = self.expect_identifier()?;
        
        // Parse parameters if any
        let mut params = Vec::new();
        if let Some(Ok((_, Token::LParen, _))) = self.peek_token() {
            self.next_token(); // Consume '('
            
            // Parse parameter list
            while let Some(Ok((_, Token::Identifier(_), _))) = self.peek_token() {
                let names = self.parse_identifier_list()?;
                self.expect_token(Token::Colon)?;
                let typ = self.parse_type()?;
                
                // Add each parameter
                for name in names {
                    params.push(Parameter {
                        name,
                        param_type: typ.clone(),
                        is_var: false, // Default to value parameter
                        is_const: false,
                        is_out: false, // Default to not out parameter
                        default_value: None,
                    });
                }
                
                // Check for more parameters
                if let Some(Ok((_, Token::Semicolon, _))) = self.peek_token() {
                    self.next_token(); // Consume ';'
                } else {
                    break;
                }
            }
            
            self.expect_token(Token::RParen)?;
        }
        
        // Parse return type if this is a function
        let return_type = if let Some(Ok((_, Token::Colon, _))) = self.peek_token() {
            self.next_token(); // Consume ':'
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        
        // Parse the block
        self.expect_token(Token::Semicolon)?;
        let block = self.parse_block()?;
        
        Ok(ProcedureDecl {
            name,
            params,
            block,
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: false,
            calling_convention: None,
        })
    }
    
    fn parse_function_decl(&mut self) -> Result<FunctionDecl, ParseError> {
        // Parse function header
        self.expect_token(Token::Function)?;
        let name = self.expect_identifier()?;
        
        // Parse parameters if any
        let mut params = Vec::new();
        if let Some(Ok((_, Token::LParen, _))) = self.peek_token() {
            self.next_token(); // Consume '('
            
            // Parse parameter list
            while let Some(Ok((_, Token::Identifier(_), _))) = self.peek_token() {
                let names = self.parse_identifier_list()?;
                self.expect_token(Token::Colon)?;
                let typ = self.parse_type()?;
                
                // Add each parameter
                for name in names {
                    params.push(Parameter {
                        name,
                        param_type: typ.clone(),
                        is_var: false, // Default to value parameter
                        is_const: false,
                        is_out: false, // Default to not out parameter
                        default_value: None,
                    });
                }
                
                // Check for more parameters
                if let Some(Ok((_, Token::Semicolon, _))) = self.peek_token() {
                    self.next_token(); // Consume ';'
                } else {
                    break;
                }
            }
            
            self.expect_token(Token::RParen)?;
        }
        
        // Parse return type
        self.expect_token(Token::Colon)?;
        let return_type = Box::new(self.parse_type()?);
        
        // Parse the block
        self.expect_token(Token::Semicolon)?;
        let block = self.parse_block()?;
        
        Ok(FunctionDecl {
            name,
            params,
            return_type: *return_type,
            block,
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: false,
            calling_convention: None,
        })
    }
    
    fn parse_var_decl(&mut self) -> Result<Vec<VariableDecl>, ParseError> {
        let mut declarations = Vec::new();
        
        loop {
            let mut identifiers = vec![self.expect_identifier()?];
            
            // Parse comma-separated identifiers
            while let Some(Ok((_, Token::Comma, _))) = self.peek_token() {
                self.next_token(); // Consume ','
                identifiers.push(self.expect_identifier()?);
            }
            
            self.expect_token(Token::Colon)?;
            let var_type = self.parse_type()?;
            
            // Check for initialization
            let has_initializer = if let Some(Ok((_, Token::Equal, _))) = self.peek_token() {
                self.next_token(); // Consume '='
                let expr = self.parse_expression()?;
                for id in &identifiers {
                    // Register variable in symbol table
                    self.declare_variable(id, var_type.clone())
                        .map_err(|e| ParseError::Other(e.to_string()))?;
                    declarations.push(VariableDecl {
                        name: id.clone(),
                        var_type: var_type.clone(),
                        initializer: Some(expr.clone()),
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    });
                }
                true
            } else {
                for id in &identifiers {
                    // Register variable in symbol table
                    self.declare_variable(id, var_type.clone())
                        .map_err(|e| ParseError::Other(e.to_string()))?;
                    declarations.push(VariableDecl {
                        name: id.clone(),
                        var_type: var_type.clone(),
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    });
                }
                false
            };
            
            self.expect_token(Token::Semicolon)?;
            
            // Check for next var declaration
            if let Some(Ok((_, Token::Identifier(_), _))) = self.peek_token() {
                // Continue parsing more vars
            } else {
                break;
            }
        }
        
        Ok(declarations)
    }

    fn parse_statements(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        
        while let Some(token_result) = self.peek_token() {
            match token_result {
                Ok((_, token, _)) => {
                    // Check if we've reached the end of the statement block
                    match token {
                        Token::End | Token::Until | Token::Else | Token::Semicolon => break,
                        _ => {
                            // Parse a single statement
                            let stmt = self.parse_statement()?;
                            statements.push(stmt);
                            
                            // Check for statement separator
                            if let Some(Ok((_, Token::Semicolon, _))) = self.peek_token() {
                                self.next_token(); // Consume ';'
                            } else {
                                // No semicolon, check if we should continue
                                match self.peek_token() {
                                    Some(Ok((_, Token::End, _))) |
                                    Some(Ok((_, Token::Until, _))) |
                                    Some(Ok((_, Token::Else, _))) => {
                                        // End of statement block
                                        break;
                                    },
                                    _ => {
                                        // Continue to next statement
                                    }
                                }
                            }
                        }
                    }
                },
                Err(e) => return Err(ParseError::LexerError(e)),
            }
        }
        
        Ok(statements)
    }
    
    fn parse_if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.next_token(); // Consume 'if'
        let condition = self.parse_expression()?;
        self.expect_token(Token::Then)?;
        let then_branch = self.parse_statements()?;
        
        let else_branch = if let Some(Ok((_, Token::Else, _))) = self.peek_token() {
            self.next_token(); // Consume 'else'
            Some(self.parse_statements()?)
        } else {
            None
        };
        
        Ok(Stmt::If {
            condition: *Box::new(condition),
            then_branch,
            else_branch,
        })
    }
    
    fn parse_while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.next_token(); // Consume 'while'
        let condition = self.parse_expression()?;
        self.expect_token(Token::Do)?;
        let body = self.parse_statements()?;
        
        Ok(Stmt::While {
            condition: *Box::new(condition),
            body,
        })
    }
    
    fn parse_for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.next_token(); // Consume 'for'
        let var = self.expect_identifier()?;
        self.expect_token(Token::Assign)?;
        let start = self.parse_expression()?;
        
        let direction = if let Some(Ok((_, Token::To, _))) = self.peek_token() {
            self.next_token(); // Consume 'to'
            ForDirection::To
        } else if let Some(Ok((_, Token::DownTo, _))) = self.peek_token() {
            self.next_token(); // Consume 'downto'
            ForDirection::DownTo
        } else {
            return Err(ParseError::Other("Expected 'to' or 'downto' in for loop".to_string()));
        };
        
        let end = self.parse_expression()?;
        self.expect_token(Token::Do)?;
        let body = self.parse_statements()?;
        
        Ok(Stmt::For {
            var_name: var,
            start: *Box::new(start),
            direction,
            end: *Box::new(end),
            body,
        })
    }
    
    fn parse_repeat_statement(&mut self) -> Result<Stmt, ParseError> {
        self.next_token(); // Consume 'repeat'
        let body = self.parse_statements()?;
        self.expect_token(Token::Until)?;
        let condition = self.parse_expression()?;
        
        Ok(Stmt::RepeatUntil {
            condition: *Box::new(condition),
            body,
        })
    }
    
    fn parse_compound_statement(&mut self) -> Result<Stmt, ParseError> {
        self.next_token(); // Consume 'begin'
        let stmts = self.parse_statements()?;
        self.expect_token(Token::End)?;
        
        Ok(Stmt::Block(Block {
            consts: Vec::new(),
            types: Vec::new(),
            vars: Vec::new(),
            procedures: Vec::new(),
            functions: Vec::new(),
            statements: stmts,
        }))
    }
    
    fn parse_case_statement(&mut self) -> Result<Stmt, ParseError> {
        self.next_token(); // Consume 'case'
        let expr = self.parse_expression()?;
        self.expect_token(Token::Of)?;
        
        let mut arms = Vec::new();
        let mut else_arm = None;
        
        loop {
            if let Some(Ok((_, Token::Else, _))) = self.peek_token() {
                self.next_token(); // Consume 'else'
                self.expect_token(Token::Colon)?;
                else_arm = Some(self.parse_statements()?);
                break;
            }
            
            let mut values = vec![self.parse_expression()?];
            
            // Check for multiple values separated by comma
            while let Some(Ok((_, Token::Comma, _))) = self.peek_token() {
                self.next_token(); // Consume ','
                values.push(self.parse_expression()?);
            }
            
            self.expect_token(Token::Colon)?;
            let body = self.parse_statements()?;
            
            // Convert Expr to Literal for case constants
            let constants: Vec<Literal> = values.into_iter().map(|expr| {
                match expr {
                    Expr::Literal(lit) => lit,
                    _ => Literal::Integer(0), // Fallback - should be improved
                }
            }).collect();
            arms.push(CaseArm { constants, stmts: body });
            
            // Check for semicolon or end of case
            if let Some(Ok((_, Token::Semicolon, _))) = self.peek_token() {
                self.next_token(); // Consume ';'
            } else {
                break;
            }
        }
        
        self.expect_token(Token::End)?;
        
        Ok(Stmt::Case {
            expr: *Box::new(expr),
            arms,
            else_arm,
        })
    }
    
    fn parse_with_statement(&mut self) -> Result<Stmt, ParseError> {
        self.next_token(); // Consume 'with'
        
        let mut records = Vec::new();
        
        // Parse record variables
        loop {
            let expr = self.parse_expression()?;
            records.push(expr);
            
            if let Some(Ok((_, Token::Comma, _))) = self.peek_token() {
                self.next_token(); // Consume ','
            } else {
                break;
            }
        }
        
        self.expect_token(Token::Do)?;
        let body = self.parse_statements()?;
        
        Ok(Stmt::With { expr: Expr::Set(records), body })
    }
    
    fn parse_assignment_or_procedure_call(&mut self) -> Result<Stmt, ParseError> {
        // This is a simplified version - should be expanded based on the actual grammar
        let name = self.expect_identifier()?;
        
        // Check if it's a procedure call
        if let Some(Ok((_, Token::LParen, _))) = self.peek_token() {
            self.next_token(); // Consume '('
            
            let mut args = Vec::new();
            
            // Parse arguments if any
            if let Some(Ok((_, Token::RParen, _))) = self.peek_token() {
                self.next_token(); // Consume ')'
            } else {
                loop {
                    args.push(self.parse_expression()?);
                    
                    if let Some(Ok((_, Token::Comma, _))) = self.peek_token() {
                        self.next_token(); // Consume ','
                    } else {
                        self.expect_token(Token::RParen)?;
                        break;
                    }
                }
            }
            
            Ok(Stmt::ProcedureCall { name, args })
        } else {
            // It's an assignment
            self.expect_token(Token::Assign)?;
            let expr = self.parse_expression()?;
            
            Ok(Stmt::Assignment {
                target: Expr::Variable(name),
                value: *Box::new(expr),
            })
        }
    }
    
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        self.skip_whitespace_and_comments();
        
        let next_token = self.peek_token();
        let pos = match &next_token {
            Some(Ok((pos, _, _))) => *pos,
            _ => 0, // Default position if we can't get it
        };
        
        match next_token {
            Some(Ok((_, Token::If, _))) => {
                self.parse_if_statement()
            },
            Some(Ok((_, Token::While, _))) => {
                self.parse_while_statement()
            },
            Some(Ok((_, Token::For, _))) => {
                self.parse_for_statement()
            },
            Some(Ok((_, Token::Repeat, _))) => {
                self.parse_repeat_statement()
            },
            Some(Ok((_, Token::Begin, _))) => {
                self.parse_compound_statement()
            },
            Some(Ok((_, Token::Case, _))) => {
                self.parse_case_statement()
            },
            Some(Ok((_, Token::With, _))) => {
                self.parse_with_statement()
            },
            Some(Ok((_, Token::Identifier(_), _))) => {
                self.parse_assignment_or_procedure_call()
            },
            // Handle empty statements (just a semicolon)
            Some(Ok((_, Token::Semicolon, _))) => {
                self.next_token(); // Consume the semicolon
                Ok(Stmt::Empty)
            },
            // Handle exit statement
            Some(Ok((_, Token::Exit, _))) => {
                self.next_token(); // Consume 'exit'
                Ok(Stmt::Exit { expr: None })
            },
            // Handle break statement
            Some(Ok((_, Token::Break, _))) => {
                self.next_token(); // Consume 'break'
                Ok(Stmt::Break)
            },
            // Handle continue statement
            Some(Ok((_, Token::Continue, _))) => {
                self.next_token(); // Consume 'continue'
                Ok(Stmt::Continue)
            },
            Some(Ok((_, token, _))) => {
                Err(ParseError::UnexpectedToken {
                    expected: vec!["statement".to_string()],
                    found: Some(token.clone()),
                    position: pos,
                })
            },
            Some(Err(e)) => Err(ParseError::LexerError(e)),
            None => {
                Err(ParseError::UnexpectedEof {
                    expected: vec!["statement".to_string()],
                })
            },
        }
    }
    
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_logical_or()
    }
    
    fn parse_logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_logical_and()?;
        
        while let Some(Ok((pos, Token::Or, _))) = self.peek_token() {
            self.next_token(); // Consume 'or'
            let right = self.parse_logical_and()?;
            expr = Expr::BinaryOp {
                op: BinaryOp::Or,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;
        
        while let Some(Ok((_, Token::And, _))) = self.peek_token() {
            self.next_token(); // Consume 'and'
            let right = self.parse_equality()?;
            expr = Expr::BinaryOp {
                op: BinaryOp::And,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_comparison()?;
        
        while let Some(Ok((_, token, _))) = self.peek_token() {
            let op = match token {
                Token::Equal => Some(BinaryOp::Equal),
                Token::NotEqual => Some(BinaryOp::NotEqual),
                _ => None,
            };
            
            if let Some(op) = op {
                self.next_token(); // Consume the operator
                let right = self.parse_comparison()?;
                expr = Expr::BinaryOp {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_term()?;
        
        while let Some(Ok((_, token, _))) = self.peek_token() {
            let op = match token {
                Token::Less => Some(BinaryOp::Less),
                Token::LessOrEqual => Some(BinaryOp::LessOrEqual),
                Token::Greater => Some(BinaryOp::Greater),
                Token::GreaterOrEqual => Some(BinaryOp::GreaterOrEqual),
                _ => None,
            };
            
            if let Some(op) = op {
                self.next_token(); // Consume the operator
                let right = self.parse_term()?;
                expr = Expr::BinaryOp {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;
        
        while let Some(Ok((_, token, _))) = self.peek_token() {
            let op = match token {
                Token::Plus => Some(BinaryOp::Add),
                Token::Minus => Some(BinaryOp::Subtract),
                Token::Or => Some(BinaryOp::BitwiseOr),
                Token::Xor => Some(BinaryOp::BitwiseXor),
                _ => None,
            };
            
            if let Some(op) = op {
                self.next_token(); // Consume the operator
                let right = self.parse_factor()?;
                expr = Expr::BinaryOp {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        
        while let Some(Ok((_, token, _))) = self.peek_token() {
            let op = match token {
                Token::Multiply => Some(BinaryOp::Multiply),
                Token::Divide => Some(BinaryOp::Divide),
                Token::Div => Some(BinaryOp::IntDivide),
                Token::Mod => Some(BinaryOp::Modulo),
                Token::And => Some(BinaryOp::BitwiseAnd),
                Token::Shl => Some(BinaryOp::ShiftLeft),
                Token::Shr => Some(BinaryOp::ShiftRight),
                _ => None,
            };
            
            if let Some(op) = op {
                self.next_token(); // Consume the operator
                let right = self.parse_unary()?;
                expr = Expr::BinaryOp {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(Ok((_, token, _))) = self.peek_token() {
            match token {
                Token::Minus => {
                    self.next_token(); // Consume '-'
                    let expr = self.parse_unary()?;
                    return Ok(Expr::UnaryOp {
                        op: UnaryOp::Negate,
                        expr: Box::new(expr),
                    });
                },
                Token::Not => {
                    self.next_token(); // Consume 'not'
                    let expr = self.parse_unary()?;
                    return Ok(Expr::UnaryOp {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    });
                },
                Token::At => {
                    self.next_token(); // Consume '@'
                    let expr = self.parse_unary()?;
                    return Ok(Expr::UnaryOp {
                        op: UnaryOp::AddressOf,
                        expr: Box::new(expr),
                    });
                },
                Token::Caret => {
                    self.next_token(); // Consume '^'
                    let expr = self.parse_unary()?;
                    return Ok(Expr::UnaryOp {
                        op: UnaryOp::Dereference,
                        expr: Box::new(expr),
                    });
                },
                _ => {}
            }
        }
        
        self.parse_primary()
    }
    
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.next_token() {
            // Literals
            Some(Ok((_, Token::Number(num), _))) => {
                if let Ok(value) = num.parse::<i64>() {
                    Ok(Expr::Literal(Literal::Integer(value)))
                } else if let Ok(value) = num.parse::<f64>() {
                    Ok(Expr::Literal(Literal::Real(value)))
                } else {
                    Err(ParseError::Other("Invalid number format".to_string()))
                }
            },
            Some(Ok((_, Token::StringLiteral(s), _))) => {
                Ok(Expr::Literal(Literal::String(s)))
            },
            Some(Ok((_, Token::True, _))) => {
                Ok(Expr::Literal(Literal::Boolean(true)))
            },
            Some(Ok((_, Token::False, _))) => {
                Ok(Expr::Literal(Literal::Boolean(false)))
            },
            Some(Ok((_, Token::Nil, _))) => {
                Ok(Expr::Literal(Literal::Nil))
            },
            
            // Identifier (variable or function call)
            Some(Ok((_, Token::Identifier(name), _))) => {
                // Check if it's a function call
                if let Some(Ok((_, Token::LParen, _))) = self.peek_token() {
                    // Function call
                    self.next_token(); // Consume '('
                    let mut args = Vec::new();
                    
                    // Parse arguments if any
                    if let Some(Ok((_, Token::RParen, _))) = self.peek_token() {
                        self.next_token(); // Consume ')'
                    } else {
                        loop {
                            args.push(self.parse_expression()?);
                            
                            match self.peek_token() {
                                Some(Ok((_, Token::Comma, _))) => {
                                    self.next_token(); // Consume ','
                                },
                                Some(Ok((_, Token::RParen, _))) => {
                                    self.next_token(); // Consume ')'
                                    break;
                                },
                                _ => return Err(ParseError::Other("Expected ',' or ')' in function call".to_string())),
                            }
                        }
                    }
                    
                    Ok(Expr::Call { name, args })
                } else {
                    // Variable reference
                    Ok(Expr::Variable(name))
                }
            },
            
            // Parenthesized expression
            Some(Ok((_, Token::LParen, _))) => {
                let expr = self.parse_expression()?;
                self.expect_token(Token::RParen)?;
                Ok(expr)
            },
            
            // Set constructor
            Some(Ok((_, Token::LBracket, _))) => {
                self.next_token(); // Consume '['
                let mut elements = Vec::new();
                
                if let Some(Ok((_, Token::RBracket, _))) = self.peek_token() {
                    self.next_token(); // Consume ']'
                } else {
                    loop {
                        elements.push(self.parse_expression()?);
                        
                        match self.peek_token() {
                            Some(Ok((_, Token::Comma, _))) => {
                                self.next_token(); // Consume ','
                            },
                            Some(Ok((_, Token::RBracket, _))) => {
                                self.next_token(); // Consume ']'
                                break;
                            },
                            _ => return Err(ParseError::Other("Expected ',' or ']' in set constructor".to_string())),
                        }
                    }
                }
                
                Ok(Expr::Set(elements))
            },
            
            // Error cases
            Some(Ok((pos, token, _))) => Err(ParseError::UnexpectedToken {
                expected: vec!["expression".to_string()],
                found: Some(token),
                position: pos,
            }),
            Some(Err(e)) => Err(ParseError::LexerError(e)),
            None => Err(ParseError::UnexpectedEof {
                expected: vec!["expression".to_string()],
            }),
        }
    }
    
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.next_token() {
            Some(Ok((_, Token::Integer, _))) => Ok(Type::Integer),
            Some(Ok((_, Token::Real, _))) => Ok(Type::Real),
            Some(Ok((_, Token::Boolean, _))) => Ok(Type::Boolean),
            Some(Ok((_, Token::Char, _))) => Ok(Type::Char),
            Some(Ok((_, Token::String, _))) => {
                // Check for string size specification
                if let Some(Ok((_, Token::LBracket, _))) = self.peek_token() {
                    self.next_token(); // Consume '['
                    let size = self.parse_expression()?;
                    self.expect_token(Token::RBracket)?;
                    
                    if let Expr::Literal(Literal::Integer(size)) = size {
                        Ok(Type::String(Some(size as usize)))
                    } else {
                        Err(ParseError::Other("String size must be a constant integer".to_string()))
                    }
                } else {
                    Ok(Type::String(None))
                }
            },
            Some(Ok((_, Token::Array, _))) => {
                self.expect_token(Token::LBracket)?;
                let start = self.parse_expression()?;
                self.expect_token(Token::Range)?;
                let end = self.parse_expression()?;
                self.expect_token(Token::RBracket)?;
                self.expect_token(Token::Of)?;
                let element_type = Box::new(self.parse_type()?);
                
                // Convert the range to a tuple of i64 to match the expected type
                let range = match (start, end) {
                    (Expr::Literal(Literal::Integer(s)), Expr::Literal(Literal::Integer(e))) => Some((s, e)),
                    _ => None,
                };
                
                // For now, we'll use Integer as the index type, but this should be configurable
                let index_type = Box::new(Type::Integer);
                
                Ok(Type::Array { index_type, element_type, range })
            },
            Some(Ok((_, Token::Record, _))) => {
                self.enter_scope();
                let mut fields = HashMap::new();
                
                while let Some(Ok((_, Token::Identifier(_), _))) = self.peek_token() {
                    let names = self.parse_identifier_list()?;
                    self.expect_token(Token::Colon)?;
                    let field_type = self.parse_type()?;
                    
                    for name in names {
                        fields.insert(name, field_type.clone());
                    }
                    
                    // Check for semicolon or end of record
                    if let Some(Ok((_, Token::End, _))) = self.peek_token() {
                        break;
                    }
                    
                    self.expect_token(Token::Semicolon)?;
                }
                
                self.expect_token(Token::End)?;
                let _ = self.exit_scope();
                
                Ok(Type::Record { fields, is_packed: false, variant_part: None })
            },
            Some(Ok((_, Token::Caret, _))) => {
                let ident = self.expect_identifier()?;
                // TODO: Handle pointer types based on the identifier
                Ok(Type::Pointer(Box::new(Type::Integer))) // Default to pointer to integer
            },
            Some(Ok((_, Token::Identifier(name), _))) => {
                // Check if it's a previously defined type
                if let Some(typ) = self.lookup_variable(&name) {
                    Ok(typ)
                } else {
                    Ok(Type::Custom(name))
                }
            },
            Some(Ok((pos, token, _))) => Err(ParseError::UnexpectedToken {
                expected: vec!["type".to_string()],
                found: Some(token),
                position: pos,
            }),
            Some(Err(e)) => Err(ParseError::LexerError(e)),
            None => Err(ParseError::UnexpectedEof {
                expected: vec!["type".to_string()],
            }),
        }
    }
    
    fn parse_identifier_list(&mut self) -> Result<Vec<String>, ParseError> {
        let mut identifiers = Vec::new();
        
        // Get the first identifier
        identifiers.push(self.expect_identifier()?);
        
        // Check for more identifiers separated by commas
        while let Some(Ok((_, Token::Comma, _))) = self.peek_token() {
            self.next_token(); // Consume the comma
            identifiers.push(self.expect_identifier()?);
        }
        
        Ok(identifiers)
    }
    
    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        self.skip_whitespace_and_comments();
        match self.next_token() {
            Some(Ok((_, Token::Identifier(name), _))) => Ok(name),
            Some(Ok((pos, found, _))) => Err(ParseError::UnexpectedToken {
                expected: vec!["identifier".to_string()],
                found: Some(found),
                position: pos,
            }),
            Some(Err(e)) => Err(ParseError::LexerError(e)),
            None => Err(ParseError::UnexpectedEof {
                expected: vec!["identifier".to_string()],
            }),
        }
    }
    
    fn expect_token(&mut self, expected: Token) -> Result<(), ParseError> {
        self.skip_whitespace_and_comments();
        match self.next_token() {
            Some(Ok((_, ref token, _))) if std::mem::discriminant(token) == std::mem::discriminant(&expected) => {
                Ok(())
            }
            Some(Ok((pos, found, _))) => {
                if matches!(found, Token::Whitespace | Token::LineComment | Token::BlockComment) {
                    return self.expect_token(expected);
                }
                Err(ParseError::UnexpectedToken {
                    expected: vec![format!("{:?}", expected)],
                    found: Some(found.clone()),
                    position: pos,
                })
            }
            Some(Err(e)) => Err(ParseError::LexerError(e)),
            None => Err(ParseError::UnexpectedEof {
                expected: vec![format!("{:?}", expected)],
            }),
        }
    }

} // End of impl Parser

// Note: Parser<'a> doesn't implement ParserCapability directly due to lifetime constraints
// Use MockParser for trait-based testing instead

impl<'a> SymbolTable for Parser<'a> {
    fn declare_variable(&mut self, name: &str, typ: Type) -> Result<(), ParseError> {
        Parser::declare_variable(self, name, typ).map_err(|e| ParseError::Other(e.to_string()))
    }
    
    fn lookup_variable(&self, name: &str) -> Option<Type> {
        Parser::lookup_variable(self, name)
    }
    
    fn enter_scope(&mut self) {
        Parser::enter_scope(self);
    }
    
    fn exit_scope(&mut self) -> Option<HashMap<String, Type>> {
        Parser::exit_scope(self)
    }
}

impl<'a> SyntaxValidator for Parser<'a> {
    fn validate_program(&self, program: &Program) -> Result<(), ParseError> {
        // Basic validation - check that program has a name and block
        if program.name.is_empty() {
            return Err(ParseError::Other("Program must have a name".to_string()));
        }
        Ok(())
    }
    
    fn validate_statement(&self, stmt: &Stmt) -> Result<(), ParseError> {
        // Basic validation - could be enhanced with more specific rules
        match stmt {
            Stmt::Assignment { target, value } => {
                // Validate that target is a valid assignment target
                match target {
                    Expr::Variable(_) | Expr::Identifier(_) => Ok(()),
                    _ => Err(ParseError::Other("Invalid assignment target".to_string())),
                }
            }
            _ => Ok(()),
        }
    }
    
    fn validate_expression(&self, expr: &Expr) -> Result<(), ParseError> {
        // Basic validation - could be enhanced with type checking
        match expr {
            Expr::BinaryOp { left, right, .. } => {
                self.validate_expression(left)?;
                self.validate_expression(right)
            }
            Expr::UnaryOp { expr, .. } => self.validate_expression(expr),
            _ => Ok(()),
        }
    }
}

impl<'a> ErrorRecovery for Parser<'a> {
    fn recover_from_error(&mut self, error: &ParseError) -> bool {
        // Simple recovery strategy - skip to next semicolon or end
        match error {
            ParseError::UnexpectedToken { .. } => {
                // Try to skip to next statement
                while let Some(Ok((_, token, _))) = self.peek_token() {
                    if matches!(token, Token::Semicolon | Token::End) {
                        break;
                    }
                    self.next_token();
                }
                true
            }
            _ => false,
        }
    }
    
    fn skip_to_safe_point(&mut self) -> Result<(), ParseError> {
        // Skip to next semicolon, end, or similar safe point
        while let Some(Ok((_, token, _))) = self.peek_token() {
            if matches!(token, Token::Semicolon | Token::End | Token::Dot) {
                break;
            }
            self.next_token();
        }
        Ok(())
    }
    
    fn report_error_with_suggestion(&self, error: &ParseError) -> String {
        match error {
            ParseError::UnexpectedToken { expected, found, position } => {
                format!(
                    "Unexpected token at position {}: {:?}. Expected one of: {}",
                    position,
                    found,
                    expected.join(", ")
                )
            }
            _ => format!("Parse error: {}", error),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_program() {
        let input = r#"
            program Test;
            var x, y: integer;
            begin
                x := 1;
                y := x + 1;
            end.
        "#;

        let mut parser = Parser::new(input);
        let program = parser.parse_program();
        assert!(program.is_ok());
        let program = program.unwrap();
        assert_eq!(program.name, "Test");
        // The parser creates separate VariableDecl for each variable in a var declaration
        assert_eq!(program.block.vars.len(), 2); // Two variables: x and y
        // More assertions can be added here
    }
}

#[cfg(test)]
mod comprehensive_tests;
