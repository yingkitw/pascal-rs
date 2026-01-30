//! Basic AST definitions for Pascal

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub name: String,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    Assignment {
        target: String,
        value: Expression,
    },
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    For {
        var_name: String,
        start: Expression,
        end: Expression,
        body: Vec<Statement>,
        direction: ForDirection,
    },
    ProcedureCall {
        name: String,
        arguments: Vec<Expression>,
    },
    Block(Vec<Statement>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    BinaryOp {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    UnaryOp {
        operator: String,
        operand: Box<Expression>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    Real(f64),
    String(String),
    Char(char),
    Boolean(bool),
    Nil,
}

/// Unit representation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Unit {
    pub name: String,
    pub uses: Vec<String>,
    pub interface: UnitInterface,
    pub implementation: UnitImplementation,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitInterface {
    pub uses: Vec<String>,
    pub constants: std::collections::HashMap<String, Literal>,
    pub types: std::collections::HashMap<String, String>,
    pub variables: std::collections::HashMap<String, String>,
    pub functions: Vec<FunctionDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub classes: Vec<ClassDecl>,
    pub interfaces: Vec<InterfaceDecl>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitImplementation {
    pub uses: Vec<String>,
    pub types: Vec<TypeDecl>,
    pub constants: Vec<ConstDecl>,
    pub variables: Vec<VariableDecl>,
    pub functions: Vec<FunctionDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub classes: Vec<ClassDecl>,
    pub interfaces: Vec<InterfaceDecl>,
    pub initialization: Option<Vec<Statement>>,
    pub finalization: Option<Vec<Statement>>,
}

// Type aliases for convenience and compatibility
pub type Stmt = Statement;
pub type Expr = Expression;

/// Binary operators
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    And,
    Or,
    Xor,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOp {
    Plus,
    Minus,
    Negate,
    Not,
}

// Additional AST types for full Pascal support
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SimpleType {
    Integer,
    Real,
    Boolean,
    Char,
    String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    Simple(SimpleType),
    Array {
        index_type: Box<Type>,
        element_type: Box<Type>,
        range: Option<(i64, i64)>,
    },
    Record {
        fields: HashMap<String, Box<Type>>,
        is_packed: bool,
    },
    Pointer(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Block {
    pub const_decls: HashMap<String, ConstDecl>,
    pub type_decls: HashMap<String, TypeDecl>,
    pub var_decls: HashMap<String, VariableDecl>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstDecl {
    pub name: String,
    pub value: Literal,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeDecl {
    pub name: String,
    pub type_definition: Type,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableDecl {
    pub name: String,
    pub variable_type: Type,
    pub initial_value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionDecl {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub block: Block,
    pub external_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProcedureDecl {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub block: Block,
    pub external_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
    pub is_var: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ForDirection {
    To,
    DownTo,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CallingConvention {
    Cdecl,
    Stdcall,
    Fastcall,
    Pascal,
    Register,
    Safecall,
}

/// Class declaration for OOP support
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassDecl {
    pub name: String,
    pub parent: Option<String>,
    pub fields: Vec<VariableDecl>,
    pub methods: Vec<MethodDecl>,
    pub properties: Vec<PropertyDecl>,
}

/// Method declaration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MethodDecl {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub is_virtual: bool,
    pub is_abstract: bool,
    pub is_override: bool,
    pub block: Option<Block>,
}

/// Property declaration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PropertyDecl {
    pub name: String,
    pub property_type: Type,
    pub getter: Option<String>,
    pub setter: Option<String>,
}

/// Interface declaration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceDecl {
    pub name: String,
    pub parent: Option<String>,
    pub methods: Vec<MethodDecl>,
}

/// Module representation for compilation units
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub unit: Unit,
    pub dependencies: Vec<String>,
}

/// Module-related errors
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleError {
    ModuleNotFound(String),
    CircularDependency(Vec<String>),
    LoadError(String, String),
    ParseError(String),
}

impl std::fmt::Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleError::ModuleNotFound(name) => write!(f, "Module not found: {}", name),
            ModuleError::CircularDependency(cycle) => {
                write!(f, "Circular dependency detected: {}", cycle.join(" -> "))
            }
            ModuleError::LoadError(name, err) => write!(f, "Error loading module {}: {}", name, err),
            ModuleError::ParseError(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl std::error::Error for ModuleError {}

/// Result type for module operations
pub type ModuleResult<T> = Result<T, ModuleError>;
