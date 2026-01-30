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
}

/// Unit representation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Unit {
    pub name: String,
    pub interface: UnitInterface,
    pub implementation: UnitImplementation,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitInterface {
    pub uses: Vec<String>,
    pub constants: std::collections::HashMap<String, Literal>,
    pub types: std::collections::HashMap<String, String>,
    pub variables: std::collections::HashMap<String, String>,
    pub functions: Vec<String>,
    pub procedures: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitImplementation {
    pub functions: Vec<String>,
    pub procedures: Vec<String>,
    pub initialization: Option<Vec<Statement>>,
    pub finalization: Option<Vec<Statement>>,
}

// Type aliases for convenience and compatibility
pub type Stmt = Statement;
pub type Expr = Expression;

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
