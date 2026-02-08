//! Trait definitions for interpreter components
//!
//! This module defines traits that make the interpreter more maintainable and testable.
//! Traits enable:
//! - Dependency inversion (depend on abstractions, not concretions)
//! - Easy mocking for testing
//! - Multiple implementations
//! - Clear interfaces between components

use crate::interpreter_value::Value;
use anyhow::Result;

/// Trait for type conversions
///
/// Allows values to be converted between different types.
/// Implementations can provide custom conversion logic.
pub trait TryAs<T> {
    /// Attempt to convert this value to type T
    fn try_as(&self) -> Result<T>;
}

/// Trait for value display formatting
///
/// Provides display functionality for values with different formatting options.
pub trait FormattedDisplay {
    /// Format the value with given precision and width
    fn format_with(&self, precision: u32, width: u32) -> String;
}

/// Trait for scope operations
///
/// Defines the interface for variable storage and retrieval.
/// Enables different scope implementations (e.g., nested scopes, persistent scopes).
pub trait ScopeOperations {
    /// Get a variable value by name
    fn get(&self, name: &str) -> Option<Value>;

    /// Set a variable value
    fn set(&mut self, name: &str, value: Value);

    /// Check if a variable exists
    fn contains(&self, name: &str) -> bool;

    /// List all variable names in this scope
    fn variable_names(&self) -> Vec<String>;
}

/// Trait for function registry
///
/// Defines the interface for storing and retrieving functions.
/// Enables different function storage strategies.
pub trait FunctionRegistry {
    /// Get a function by name
    fn get_function(&self, name: &str) -> Option<&crate::interpreter_function::UserFunction>;

    /// Register a function
    fn register_function(&mut self, name: String, func: crate::interpreter_function::UserFunction);

    /// Check if a function exists
    fn has_function(&self, name: &str) -> bool;
}

/// Trait for statement execution
///
/// Defines how different statement types are executed.
/// Allows for pluggable statement execution strategies.
pub trait StatementExecutor {
    /// Execute a statement and return the result
    fn execute(&mut self, statement: &crate::ast::Stmt) -> Result<ExecutionResult>;
}

/// Result of executing a statement
#[derive(Debug, Clone, PartialEq)]
pub enum ExecutionResult {
    /// Statement executed successfully
    Success,
    /// Break from a loop
    Break,
    /// Continue to next iteration
    Continue,
    /// Return from function
    Return(Value),
}

/// Trait for expression evaluation
///
/// Defines how expressions are evaluated.
/// Enables different evaluation strategies (lazy, eager, etc.).
pub trait ExpressionEvaluator {
    /// Evaluate an expression and return its value
    fn evaluate(&mut self, expr: &crate::ast::Expr) -> Result<Value>;

    /// Evaluate a binary operation
    fn evaluate_binary(&mut self, op: &str, left: &Value, right: &Value) -> Result<Value>;

    /// Evaluate a unary operation
    fn evaluate_unary(&mut self, op: &str, operand: &Value) -> Result<Value>;
}

/// Trait for built-in function implementations
///
/// Defines the interface for built-in functions.
/// Allows adding new built-ins without modifying core interpreter logic.
pub trait BuiltinFunction {
    /// Get the name of this built-in function
    fn name(&self) -> &str;

    /// Get the number of parameters
    fn param_count(&self) -> usize;

    /// Call the built-in function with arguments
    fn call(&self, args: &[Value]) -> Result<Value>;
}

/// Trait for code generation
///
/// Defines how AST nodes are converted to executable code.
/// Enables different backends (x86-64, ARM, WebAssembly, etc.).
pub trait CodeGenerator {
    /// Generate code for a program
    fn generate_program(&mut self, program: &crate::ast::Program) -> Result<String>;

    /// Generate code for a function
    fn generate_function(&mut self, function: &crate::ast::FunctionDecl) -> Result<String>;

    /// Generate code for a statement
    fn generate_statement(&mut self, statement: &crate::ast::Stmt) -> Result<String>;
}

/// Trait for optimization passes
///
/// Defines an optimization pass that can be applied to the AST or generated code.
pub trait OptimizationPass {
    /// Get the name of this optimization pass
    fn name(&self) -> &str;

    /// Run the optimization pass on a program
    fn optimize(&mut self, program: &crate::ast::Program) -> Result<crate::ast::Program>;

    /// Check if this optimization should be enabled
    fn is_enabled(&self, options: &OptimizationOptions) -> bool;
}

/// Options for optimization passes
#[derive(Debug, Clone)]
pub struct OptimizationOptions {
    /// Enable constant folding
    pub constant_folding: bool,
    /// Enable dead code elimination
    pub dead_code_elimination: bool,
    /// Enable common subexpression elimination
    pub cse: bool,
    /// Enable function inlining
    pub inlining: bool,
    /// Optimization level (0-3)
    pub level: u32,
}

impl Default for OptimizationOptions {
    fn default() -> Self {
        Self {
            constant_folding: true,
            dead_code_elimination: true,
            cse: true,
            inlining: false,
            level: 1,
        }
    }
}

/// Trait for symbol resolution
///
/// Defines how symbols (variables, functions, types) are resolved.
/// Enables different symbol resolution strategies.
pub trait SymbolResolver {
    /// Resolve a symbol name to its definition
    fn resolve_symbol(&self, name: &str) -> Option<SymbolDefinition>;

    /// Enter a new scope
    fn enter_scope(&mut self);

    /// Exit the current scope
    fn exit_scope(&mut self);

    /// Define a symbol in the current scope
    fn define_symbol(&mut self, name: String, definition: SymbolDefinition);
}

/// Definition of a symbol
#[derive(Debug, Clone)]
pub struct SymbolDefinition {
    pub name: String,
    pub symbol_type: SymbolType,
    pub scope_level: usize,
}

/// Types of symbols
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Variable,
    Constant,
    Function,
    Procedure,
    Type,
    Module,
}

/// Trait for type checking
///
/// Defines how types are checked and validated.
/// Enables different type checking strategies.
pub trait TypeChecker {
    /// Check a program for type errors
    fn check_program(&mut self, program: &crate::ast::Program) -> Result<TypeCheckResult>;

    /// Check an expression's type
    fn check_expression(&mut self, expr: &crate::ast::Expr) -> Result<Type>;
}

/// Result of type checking
#[derive(Debug, Clone)]
pub struct TypeCheckResult {
    pub errors: Vec<TypeError>,
    pub warnings: Vec<TypeWarning>,
}

/// Type error information
#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub location: Option<(usize, usize)>,
    pub expected: Option<Type>,
    pub found: Option<Type>,
}

/// Type warning information
#[derive(Debug, Clone)]
pub struct TypeWarning {
    pub message: String,
    pub location: Option<(usize, usize)>,
}

/// Type representation
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Real,
    Boolean,
    Char,
    String,
    Array(Box<Type>, Box<Range>),
    Record(Vec<Field>),
    Pointer(Box<Type>),
    Procedure(Vec<Type>, Option<Box<Type>>),
    Function(Vec<Type>, Box<Type>),
    Set(Box<Type>),
    Void,
    Unknown(String),
}

/// Range for array types
#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub start: i64,
    pub end: i64,
}

/// Field in a record type
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub field_type: Type,
}

/// Trait for parsing
///
/// Defines how source code is parsed into an AST.
/// Enables different language variants and parsing strategies.
pub trait Parser {
    /// Parse source code into an AST
    fn parse(&mut self, source: &str) -> Result<crate::ast::Program>;

    /// Parse a single expression
    fn parse_expression(&mut self, source: &str) -> Result<crate::ast::Expr>;

    /// Parse a single statement
    fn parse_statement(&mut self, source: &str) -> Result<crate::ast::Stmt>;
}

/// Trait for lexical analysis
///
/// Defines how source code is tokenized.
/// Enables different tokenization strategies.
pub trait Lexer {
    /// Tokenize source code
    fn tokenize(&mut self, source: &str) -> Vec<crate::Token>;

    /// Get the current token
    fn current_token(&self) -> Option<&crate::Token>;

    /// Advance to the next token
    fn advance(&mut self) -> Option<&crate::Token>;
}

// Implement traits for Value to improve trait-facing design

impl TryAs<i64> for Value {
    fn try_as(&self) -> Result<i64> {
        self.as_integer()
    }
}

impl TryAs<f64> for Value {
    fn try_as(&self) -> Result<f64> {
        self.as_real()
    }
}

impl TryAs<bool> for Value {
    fn try_as(&self) -> Result<bool> {
        self.as_boolean()
    }
}

impl FormattedDisplay for Value {
    fn format_with(&self, precision: u32, width: u32) -> String {
        match self {
            Value::Integer(n) => format!("{:width$}", n, width = width as usize),
            Value::Real(r) => format!("{:width$.precision$}", r, width = width as usize, precision = precision as usize),
            Value::Boolean(b) => format!("{:>width$}", if *b { "TRUE" } else { "FALSE" }, width = width as usize),
            Value::Char(c) => format!("{:>width$}", c, width = width as usize),
            Value::String(s) => format!("{:>width$}", s, width = width as usize),
            Value::Nil => format!("{:>width$}", "nil", width = width as usize),
        }
    }
}
