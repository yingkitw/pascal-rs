use crate::ParseError;
use minipas_ast::Program;
// Note: Token and LexerError are not directly used in this file

/// Trait for parsing capabilities
pub trait ParserCapability {
    /// Creates a new parser instance
    fn new(input: &str) -> Self where Self: Sized;
    
    /// Parses a complete program
    fn parse_program(&mut self) -> Result<Program, ParseError>;
    
    /// Parses a single statement
    fn parse_statement(&mut self) -> Result<minipas_ast::Stmt, ParseError>;
    
    /// Parses an expression
    fn parse_expression(&mut self) -> Result<minipas_ast::Expr, ParseError>;
    
    /// Parses a type definition
    fn parse_type(&mut self) -> Result<minipas_ast::Type, ParseError>;
}

/// Trait for syntax validation capabilities
pub trait SyntaxValidator {
    /// Validates the syntax of a program
    fn validate_program(&self, program: &Program) -> Result<(), ParseError>;
    
    /// Validates the syntax of a statement
    fn validate_statement(&self, stmt: &minipas_ast::Stmt) -> Result<(), ParseError>;
    
    /// Validates the syntax of an expression
    fn validate_expression(&self, expr: &minipas_ast::Expr) -> Result<(), ParseError>;
}

/// Trait for symbol table management
pub trait SymbolTable {
    /// Declares a variable in the current scope
    fn declare_variable(&mut self, name: &str, typ: minipas_ast::Type) -> Result<(), ParseError>;
    
    /// Looks up a variable in the current scope and parent scopes
    fn lookup_variable(&self, name: &str) -> Option<minipas_ast::Type>;
    
    /// Enters a new scope
    fn enter_scope(&mut self);
    
    /// Exits the current scope
    fn exit_scope(&mut self) -> Option<std::collections::HashMap<String, minipas_ast::Type>>;
}

/// Trait for error recovery capabilities
pub trait ErrorRecovery {
    /// Attempts to recover from a parse error
    fn recover_from_error(&mut self, error: &ParseError) -> bool;
    
    /// Skips to the next safe point for parsing
    fn skip_to_safe_point(&mut self) -> Result<(), ParseError>;
    
    /// Reports parse errors with suggestions
    fn report_error_with_suggestion(&self, error: &ParseError) -> String;
}

/// Trait for incremental parsing capabilities
pub trait IncrementalParser {
    /// Parses only the changed portion of the input
    fn parse_incremental(&mut self, start_pos: usize, end_pos: usize) -> Result<Program, ParseError>;
    
    /// Updates the parse tree with changes
    fn update_parse_tree(&mut self, changes: &[ParseChange]) -> Result<(), ParseError>;
}

/// Represents a change in the parse tree
#[derive(Debug, Clone)]
pub struct ParseChange {
    pub position: usize,
    pub change_type: ChangeType,
    pub content: String,
}

#[derive(Debug, Clone)]
pub enum ChangeType {
    Insert,
    Delete,
    Replace,
}
