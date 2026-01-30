//! Parser trait definitions

use crate::ast::{Expr, Stmt, Type};
use crate::ParseError;
use crate::Token;

/// Result type for parsing operations
pub type ParseResult<T> = Result<T, ParseError>;

/// Core parser capability trait
pub trait ParserCapability {
    /// Parse a complete program
    fn parse_program(&mut self) -> ParseResult<crate::ast::Program>;

    /// Parse a statement
    fn parse_statement(&mut self) -> ParseResult<Option<Stmt>>;

    /// Parse an expression
    fn parse_expression(&mut self) -> ParseResult<Option<Expr>>;

    /// Parse a type definition
    fn parse_type(&mut self) -> ParseResult<Type>;

    /// Get all parsing errors
    fn errors(&self) -> Vec<ParseError>;

    /// Check if parsing completed with errors
    fn has_errors(&self) -> bool;
}

/// Declaration parsing trait
pub trait DeclarationParser {
    /// Parse variable declarations
    fn parse_var_decl(&mut self) -> ParseResult<Vec<crate::ast::VariableDecl>>;

    /// Parse constant declarations
    fn parse_const_decl(&mut self) -> ParseResult<Vec<crate::ast::ConstDecl>>;

    /// Parse type declarations
    fn parse_type_decl(&mut self) -> ParseResult<Vec<crate::ast::TypeDecl>>;

    /// Parse function declaration
    fn parse_function_decl(&mut self) -> ParseResult<crate::ast::FunctionDecl>;

    /// Parse procedure declaration
    fn parse_procedure_decl(&mut self) -> ParseResult<crate::ast::ProcedureDecl>;
}

/// Statement parsing trait
pub trait StatementParser {
    /// Parse if statement
    fn parse_if_statement(&mut self) -> ParseResult<Stmt>;

    /// Parse while loop
    fn parse_while_loop(&mut self) -> ParseResult<Stmt>;

    /// Parse for loop
    fn parse_for_loop(&mut self) -> ParseResult<Stmt>;

    /// Parse repeat-until loop
    fn parse_repeat_loop(&mut self) -> ParseResult<Stmt>;

    /// Parse assignment
    fn parse_assignment(&mut self, target: String) -> ParseResult<Stmt>;

    /// Parse procedure call
    fn parse_procedure_call(&mut self, name: String) -> ParseResult<Stmt>;

    /// Parse compound statement (block)
    fn parse_compound_statement(&mut self) -> ParseResult<Vec<Stmt>>;
}

/// Expression parsing trait
pub trait ExpressionParser {
    /// Parse binary operation with precedence
    fn parse_binary_op(&mut self, precedence: u8) -> ParseResult<Option<Expr>>;

    /// Parse unary operation
    fn parse_unary_op(&mut self) -> ParseResult<Option<Expr>>;

    /// Parse primary expression
    fn parse_primary(&mut self) -> ParseResult<Option<Expr>>;

    /// Parse function call
    fn parse_function_call(&mut self, name: String) -> ParseResult<Expr>;

    /// Parse argument list
    fn parse_argument_list(&mut self) -> ParseResult<Vec<Expr>>;
}

/// Error recovery trait
pub trait ErrorRecovery {
    /// Synchronize parser after error
    fn synchronize(&mut self, sync_tokens: &[Token]);

    /// Consume token or skip to sync point
    fn consume_or_skip(&mut self, expected: Token, sync_tokens: &[Token]);

    /// Enter recovery mode
    fn enter_recovery_mode(&mut self);

    /// Exit recovery mode
    fn exit_recovery_mode(&mut self);

    /// Check if in recovery mode
    fn in_recovery_mode(&self) -> bool;
}
