//! Basic parser implementation

use crate::ast::Program;
use crate::basic_lexer::Lexer;

pub struct Parser {
    _lexer: Lexer,
    _current_token: Option<crate::basic_lexer::Token>,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        Parser {
            _lexer: Lexer::new(source),
            _current_token: None,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        // TODO: Implement actual parsing
        Ok(Program {
            name: "test".to_string(),
            statements: vec![],
        })
    }
}
