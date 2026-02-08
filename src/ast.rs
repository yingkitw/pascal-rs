//! Unified AST definitions for Pascal - combining basic and enhanced features

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ======== Compilation Units ========

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub name: String,
    pub uses: Vec<String>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Unit {
    pub name: String,
    pub uses: Vec<String>,
    pub interface: UnitInterface,
    pub implementation: UnitImplementation,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Library {
    pub name: String,
    pub uses: Vec<String>,
    pub exports: Vec<String>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub uses: Vec<String>,
    pub requires: Vec<String>,
    pub contains: Vec<String>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitInterface {
    pub uses: Vec<String>,
    pub types: Vec<TypeDecl>,
    pub constants: Vec<ConstDecl>,
    pub variables: Vec<VariableDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub functions: Vec<FunctionDecl>,
    pub classes: Vec<ClassDecl>,
    pub interfaces: Vec<InterfaceDecl>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitImplementation {
    pub uses: Vec<String>,
    pub types: Vec<TypeDecl>,
    pub constants: Vec<ConstDecl>,
    pub variables: Vec<VariableDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub functions: Vec<FunctionDecl>,
    pub classes: Vec<ClassDecl>,
    pub interfaces: Vec<InterfaceDecl>,
    pub initialization: Option<Vec<Statement>>,
    pub finalization: Option<Vec<Statement>>,
}

// ======== Type System ========

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
    // Simple types
    Simple(SimpleType),

    // Convenience aliases (map to Simple internally)
    Integer,
    Real,
    Boolean,
    Char,
    String,
    WideString,

    // Complex types
    Alias {
        name: String,
        target_type: Box<Type>,
    },
    Array {
        index_type: Box<Type>,
        element_type: Box<Type>,
        range: Option<(i64, i64)>,
    },
    Record {
        fields: HashMap<String, Box<Type>>,
        is_packed: bool,
    },
    Set {
        base_type: Box<Type>,
    },
    File {
        element_type: Option<Box<Type>>,
    },
    Pointer(Box<Type>),

    // Advanced types
    Generic {
        name: String,
        constraints: Vec<String>,
    },
    GenericInstance {
        base_type: String,
        type_arguments: Vec<Type>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArrayDimension {
    pub lower_bound: i64,
    pub upper_bound: i64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldDecl {
    pub name: String,
    pub field_type: Type,
    pub visibility: FieldVisibility,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FieldVisibility {
    Private,
    Protected,
    Public,
    Published,
}

// ======== Statements ========

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
    Repeat {
        body: Vec<Statement>,
        until_condition: Expression,
    },
    For {
        var_name: String,
        start: Expression,
        end: Expression,
        body: Vec<Statement>,
        direction: ForDirection,
    },
    Case {
        expression: Expression,
        branches: Vec<CaseBranch>,
        else_branch: Option<Vec<Statement>>,
    },
    ProcedureCall {
        name: String,
        arguments: Vec<Expression>,
    },
    Block(Block),

    // Advanced statements
    Try {
        try_block: Vec<Statement>,
        except_clauses: Vec<ExceptClause>,
        finally_block: Option<Vec<Statement>>,
    },
    Raise {
        exception: Option<Expression>,
        message: Option<Expression>,
    },
    Goto {
        label: String,
    },
    Label {
        name: String,
    },
    With {
        variable: Expression,
        statements: Vec<Statement>,
    },

    Empty,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ForDirection {
    To,
    DownTo,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CaseBranch {
    pub values: Vec<Expression>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExceptClause {
    pub exception_type: Option<String>,
    pub variable: Option<String>,
    pub body: Vec<Statement>,
}

// ======== Expressions ========

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    BinaryOp {
        operator: String,
        left: Box<Expression>,
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

    // Advanced expressions
    Set {
        elements: Vec<Expression>,
    },
    TypeCast {
        target_type: Type,
        expression: Box<Expression>,
    },
    SizeOf {
        type_or_expression: Box<Expression>,
    },
    AddressOf {
        expression: Box<Expression>,
    },
    Dereference {
        expression: Box<Expression>,
    },
    Inherited {
        member: Option<String>,
    },
    Is {
        expression: Box<Expression>,
        type_name: String,
    },
    As {
        expression: Box<Expression>,
        type_name: String,
    },
}

// ======== Literals ========

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    Real(f64),
    String(String),
    WideString(String),
    Char(char),
    Boolean(bool),
    Nil,
    Set(Vec<i64>),
}

// ======== Declarations ========

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Block {
    pub consts: Vec<ConstDecl>,
    pub types: Vec<TypeDecl>,
    pub vars: Vec<VariableDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub functions: Vec<FunctionDecl>,
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn empty() -> Self {
        Self {
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![],
            statements: vec![],
        }
    }

    pub fn with_statements(statements: Vec<Statement>) -> Self {
        Self {
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![],
            statements,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstDecl {
    pub name: String,
    pub value: Literal,
    pub visibility: FieldVisibility,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeDecl {
    pub name: String,
    pub type_definition: Type,
    pub visibility: FieldVisibility,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableDecl {
    pub name: String,
    pub variable_type: Type,
    pub initial_value: Option<Expression>,
    pub visibility: FieldVisibility,
    pub is_absolute: bool,
    pub absolute_address: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionDecl {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub block: Block,
    pub visibility: FieldVisibility,
    pub is_external: bool,
    pub external_name: Option<String>,
    pub is_inline: bool,
    pub is_forward: bool,
    pub is_class_method: bool,
    pub is_virtual: bool,
    pub is_override: bool,
    pub is_overload: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProcedureDecl {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub block: Block,
    pub visibility: FieldVisibility,
    pub is_external: bool,
    pub external_name: Option<String>,
    pub is_inline: bool,
    pub is_forward: bool,
    pub is_class_method: bool,
    pub is_virtual: bool,
    pub is_override: bool,
    pub is_overload: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
    pub is_var: bool,
    pub is_const: bool,
    pub is_out: bool,
    pub default_value: Option<Expression>,
}

// ======== OOP Support ========

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassDecl {
    pub name: String,
    pub parent: Option<String>,
    pub interfaces: Vec<String>,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<MethodDecl>,
    pub properties: Vec<PropertyDecl>,
    pub visibility: FieldVisibility,
    pub is_abstract: bool,
    pub is_sealed: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceDecl {
    pub name: String,
    pub parent: Option<String>,
    pub methods: Vec<MethodDecl>,
    pub properties: Vec<PropertyDecl>,
    pub visibility: FieldVisibility,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MethodDecl {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub block: Option<Block>,
    pub visibility: FieldVisibility,
    pub is_class_method: bool,
    pub is_virtual: bool,
    pub is_abstract: bool,
    pub is_override: bool,
    pub is_overload: bool,
    pub is_static: bool,
    pub is_constructor: bool,
    pub is_destructor: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PropertyDecl {
    pub name: String,
    pub property_type: Type,
    pub read_specifier: Option<MethodSpecifier>,
    pub write_specifier: Option<MethodSpecifier>,
    pub stored_field: Option<String>,
    pub default_value: Option<Expression>,
    pub visibility: FieldVisibility,
    pub is_indexed: bool,
    pub index_parameters: Vec<Parameter>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MethodSpecifier {
    Field(String),
    Method(String),
}

// ======== Module System ========

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub unit: Unit,
    pub dependencies: Vec<String>,
}

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
            ModuleError::LoadError(name, err) => {
                write!(f, "Error loading module {}: {}", name, err)
            }
            ModuleError::ParseError(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl std::error::Error for ModuleError {}

pub type ModuleResult<T> = Result<T, ModuleError>;

// ======== Calling Conventions ========

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CallingConvention {
    Cdecl,
    Stdcall,
    Fastcall,
    Pascal,
    Register,
    Safecall,
}

// ======== Type Aliases for Backward Compatibility ========

pub type Expr = Expression;
pub type Stmt = Statement;
