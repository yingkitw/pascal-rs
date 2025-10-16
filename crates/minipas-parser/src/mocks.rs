use super::traits::*;
use super::ParseError;
use minipas_ast::*;
use std::collections::HashMap;

/// Mock parser for testing purposes
pub struct MockParser {
    program: Program,
    variables: HashMap<String, Type>,
    scopes: Vec<HashMap<String, Type>>,
}

impl MockParser {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            variables: HashMap::new(),
            scopes: vec![HashMap::new()],
        }
    }
    
    pub fn with_simple_program() -> Self {
        let program = Program {
            name: "test".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![
                    Stmt::Assignment {
                        target: Expr::Identifier(vec!["x".to_string()]),
                        value: Expr::Literal(Literal::Integer(42)),
                    },
                ],
            },
        };
        Self::new(program)
    }
}

impl ParserCapability for MockParser {
    fn new(_input: &str) -> Self {
        // Create a simple mock program based on input
        let program = Program {
            name: "mock".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![],
            },
        };
        Self::new(program)
    }
    
    fn parse_program(&mut self) -> Result<Program, ParseError> {
        Ok(self.program.clone())
    }
    
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        if let Some(stmt) = self.program.block.statements.first() {
            Ok(stmt.clone())
        } else {
            Err(ParseError::Other("No statements available".to_string()))
        }
    }
    
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        Ok(Expr::Literal(Literal::Integer(42)))
    }
    
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        Ok(Type::Integer)
    }
}

impl SymbolTable for MockParser {
    fn declare_variable(&mut self, name: &str, typ: Type) -> Result<(), ParseError> {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name.to_string(), typ);
            Ok(())
        } else {
            Err(ParseError::Other("No active scope".to_string()))
        }
    }
    
    fn lookup_variable(&self, name: &str) -> Option<Type> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(typ) = scope.get(name) {
                return Some(typ.clone());
            }
        }
        None
    }
    
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    
    fn exit_scope(&mut self) -> Option<HashMap<String, Type>> {
        self.scopes.pop()
    }
}

impl SyntaxValidator for MockParser {
    fn validate_program(&self, program: &Program) -> Result<(), ParseError> {
        if program.name.is_empty() {
            Err(ParseError::Other("Program must have a name".to_string()))
        } else {
            Ok(())
        }
    }
    
    fn validate_statement(&self, stmt: &Stmt) -> Result<(), ParseError> {
        match stmt {
            Stmt::Assignment { target, value: _ } => {
                match target {
                    Expr::Variable(_) | Expr::Identifier(_) => Ok(()),
                    _ => Err(ParseError::Other("Invalid assignment target".to_string())),
                }
            }
            _ => Ok(()),
        }
    }
    
    fn validate_expression(&self, expr: &Expr) -> Result<(), ParseError> {
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

impl ErrorRecovery for MockParser {
    fn recover_from_error(&mut self, _error: &ParseError) -> bool {
        // Mock always recovers successfully
        true
    }
    
    fn skip_to_safe_point(&mut self) -> Result<(), ParseError> {
        // Mock always succeeds
        Ok(())
    }
    
    fn report_error_with_suggestion(&self, error: &ParseError) -> String {
        format!("Mock parser error: {}", error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use minipas_ast::*;
    
    #[test]
    fn test_mock_parser_basic() {
        let mut parser = MockParser::with_simple_program();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.name, "test");
        assert_eq!(program.block.statements.len(), 1);
    }
    
    #[test]
    fn test_mock_parser_symbol_table() {
        let mut parser = MockParser::with_simple_program();
        
        // Declare a variable
        parser.declare_variable("x", Type::Integer).unwrap();
        
        // Look it up
        let typ = parser.lookup_variable("x").unwrap();
        assert_eq!(typ, Type::Integer);
        
        // Look up non-existent variable
        assert!(parser.lookup_variable("y").is_none());
    }
    
    #[test]
    fn test_mock_parser_scope_management() {
        let mut parser = MockParser::with_simple_program();
        
        // Enter scope
        parser.enter_scope();
        parser.declare_variable("x", Type::Integer).unwrap();
        
        // Exit scope
        let scope = parser.exit_scope().unwrap();
        assert_eq!(scope.len(), 1);
        assert_eq!(scope.get("x").unwrap(), &Type::Integer);
    }
    
    #[test]
    fn test_mock_parser_validation() {
        let parser = MockParser::with_simple_program();
        
        // Valid program
        let program = Program {
            name: "test".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![],
            },
        };
        assert!(parser.validate_program(&program).is_ok());
        
        // Invalid program (no name)
        let invalid_program = Program {
            name: "".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![],
            },
        };
        assert!(parser.validate_program(&invalid_program).is_err());
    }
}
