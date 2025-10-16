use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Enhanced AST nodes based on Free Pascal Compiler
/// Provides comprehensive Pascal language support

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EnhancedType {
    /// Basic types
    Integer,
    Real,
    Boolean,
    Char,
    String,
    WideString,
    
    /// Array types
    Array {
        element_type: Box<EnhancedType>,
        dimensions: Vec<ArrayDimension>,
    },
    
    /// Record types
    Record {
        fields: Vec<RecordField>,
        is_packed: bool,
    },
    
    /// Set types
    Set {
        base_type: Box<EnhancedType>,
    },
    
    /// File types
    File {
        element_type: Option<Box<EnhancedType>>,
    },
    
    /// Pointer types
    Pointer {
        target_type: Box<EnhancedType>,
    },
    
    /// Procedure/Function types
    Procedure {
        parameters: Vec<Parameter>,
        return_type: Option<Box<EnhancedType>>,
        calling_convention: CallingConvention,
    },
    
    /// Class types
    Class {
        name: String,
        parent: Option<String>,
        fields: Vec<ClassField>,
        methods: Vec<ClassMethod>,
        properties: Vec<ClassProperty>,
        visibility: Visibility,
    },
    
    /// Object types
    Object {
        name: String,
        parent: Option<String>,
        fields: Vec<ObjectField>,
        methods: Vec<ObjectMethod>,
        visibility: Visibility,
    },
    
    /// Interface types
    Interface {
        name: String,
        parent: Option<String>,
        methods: Vec<InterfaceMethod>,
        properties: Vec<InterfaceProperty>,
    },
    
    /// Enumeration types
    Enumeration {
        values: Vec<EnumValue>,
    },
    
    /// Subrange types
    Subrange {
        base_type: Box<EnhancedType>,
        min_value: Option<Literal>,
        max_value: Option<Literal>,
    },
    
    /// Generic types
    Generic {
        name: String,
        constraints: Vec<TypeConstraint>,
    },
    
    /// Type alias
    Alias {
        name: String,
        target_type: Box<EnhancedType>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArrayDimension {
    pub lower_bound: Option<Expression>,
    pub upper_bound: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordField {
    pub name: String,
    pub field_type: EnhancedType,
    pub offset: Option<usize>,
    pub case_variant: Option<CaseVariant>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CaseVariant {
    pub discriminant: String,
    pub values: Vec<Literal>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassField {
    pub name: String,
    pub field_type: EnhancedType,
    pub visibility: Visibility,
    pub is_static: bool,
    pub is_readonly: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassMethod {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<EnhancedType>,
    pub visibility: Visibility,
    pub is_static: bool,
    pub is_virtual: bool,
    pub is_abstract: bool,
    pub is_override: bool,
    pub calling_convention: CallingConvention,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassProperty {
    pub name: String,
    pub property_type: EnhancedType,
    pub visibility: Visibility,
    pub getter: Option<String>,
    pub setter: Option<String>,
    pub is_readonly: bool,
    pub is_writeonly: bool,
    pub default_value: Option<Literal>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectField {
    pub name: String,
    pub field_type: EnhancedType,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectMethod {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<EnhancedType>,
    pub visibility: Visibility,
    pub is_virtual: bool,
    pub calling_convention: CallingConvention,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceMethod {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<EnhancedType>,
    pub calling_convention: CallingConvention,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceProperty {
    pub name: String,
    pub property_type: EnhancedType,
    pub getter: Option<String>,
    pub setter: Option<String>,
    pub is_readonly: bool,
    pub is_writeonly: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumValue {
    pub name: String,
    pub value: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeConstraint {
    pub constraint_type: ConstraintType,
    pub constraint_value: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConstraintType {
    Class,
    Constructor,
    Record,
    Interface,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Published,
    Protected,
    Private,
    Strict,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CallingConvention {
    Default,
    Cdecl,
    Pascal,
    Register,
    Safecall,
    Stdcall,
    Varargs,
    Cppdecl,
    Mwpascal,
    Syscall,
    Hardfloat,
    Softfloat,
    Vectorcall,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    Real(f64),
    Boolean(bool),
    Char(char),
    String(String),
    WideString(String),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    BinaryOp {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
    },
    MethodCall {
        object: Box<Expression>,
        method: String,
        arguments: Vec<Expression>,
    },
    ArrayAccess {
        array: Box<Expression>,
        index: Vec<Expression>,
    },
    RecordAccess {
        record: Box<Expression>,
        field: String,
    },
    PointerDeref {
        pointer: Box<Expression>,
    },
    AddressOf {
        variable: Box<Expression>,
    },
    TypeCast {
        target_type: EnhancedType,
        expression: Box<Expression>,
    },
    SetLiteral {
        elements: Vec<Expression>,
    },
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    IntDivide,
    Modulus,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    In,
    Is,
    As,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
    AddressOf,
    PointerDeref,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    Assignment {
        target: Expression,
        value: Expression,
    },
    ProcedureCall {
        name: String,
        arguments: Vec<Expression>,
    },
    MethodCall {
        object: Expression,
        method: String,
        arguments: Vec<Expression>,
    },
    If {
        condition: Expression,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    Case {
        expression: Expression,
        cases: Vec<CaseBranch>,
        else_branch: Option<Block>,
    },
    While {
        condition: Expression,
        body: Block,
    },
    Repeat {
        body: Block,
        condition: Expression,
    },
    For {
        variable: String,
        start_value: Expression,
        end_value: Expression,
        step: Option<Expression>,
        body: Block,
        direction: ForDirection,
    },
    With {
        expressions: Vec<Expression>,
        body: Block,
    },
    Try {
        body: Block,
        except_clauses: Vec<ExceptClause>,
        finally_clause: Option<Block>,
    },
    Raise {
        exception_type: Option<String>,
        message: Option<Expression>,
    },
    Goto {
        label: String,
    },
    Exit {
        return_value: Option<Expression>,
    },
    Break,
    Continue,
    Halt {
        exit_code: Option<Expression>,
    },
    Block(Block),
    Empty,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ForDirection {
    To,
    Downto,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CaseBranch {
    pub values: Vec<Expression>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExceptClause {
    pub exception_type: Option<String>,
    pub variable: Option<String>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Block {
    pub consts: Vec<Constant>,
    pub types: Vec<TypeDefinition>,
    pub vars: Vec<Variable>,
    pub procedures: Vec<Procedure>,
    pub functions: Vec<Function>,
    pub statements: Vec<Statement>,
    pub labels: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Constant {
    pub name: String,
    pub constant_type: Option<EnhancedType>,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeDefinition {
    pub name: String,
    pub type_definition: EnhancedType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Variable {
    pub name: String,
    pub variable_type: EnhancedType,
    pub initial_value: Option<Expression>,
    pub is_absolute: bool,
    pub absolute_address: Option<Expression>,
    pub is_external: bool,
    pub external_name: Option<String>,
    pub is_public: bool,
    pub is_exported: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub parameter_type: EnhancedType,
    pub is_var: bool,
    pub is_const: bool,
    pub is_out: bool,
    pub default_value: Option<Expression>,
    pub is_array_of_const: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Procedure {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub calling_convention: CallingConvention,
    pub is_forward: bool,
    pub is_external: bool,
    pub external_name: Option<String>,
    pub is_public: bool,
    pub is_exported: bool,
    pub is_inline: bool,
    pub is_assembler: bool,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: EnhancedType,
    pub calling_convention: CallingConvention,
    pub is_forward: bool,
    pub is_external: bool,
    pub external_name: Option<String>,
    pub is_public: bool,
    pub is_exported: bool,
    pub is_inline: bool,
    pub is_assembler: bool,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Unit {
    pub name: String,
    pub uses: Vec<UseClause>,
    pub interface_section: Option<InterfaceSection>,
    pub implementation_section: Option<ImplementationSection>,
    pub initialization_section: Option<Block>,
    pub finalization_section: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UseClause {
    pub unit_name: String,
    pub alias: Option<String>,
    pub is_in_interface: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceSection {
    pub consts: Vec<Constant>,
    pub types: Vec<TypeDefinition>,
    pub vars: Vec<Variable>,
    pub procedures: Vec<Procedure>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ImplementationSection {
    pub consts: Vec<Constant>,
    pub types: Vec<TypeDefinition>,
    pub vars: Vec<Variable>,
    pub procedures: Vec<Procedure>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub name: String,
    pub uses: Vec<UseClause>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Library {
    pub name: String,
    pub uses: Vec<UseClause>,
    pub exports: Vec<ExportClause>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExportClause {
    pub name: String,
    pub alias: Option<String>,
    pub index: Option<i64>,
    pub is_name: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub uses: Vec<UseClause>,
    pub requires: Vec<String>,
    pub contains: Vec<String>,
    pub block: Block,
}

/// Enhanced AST root node
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EnhancedAst {
    Program(Program),
    Unit(Unit),
    Library(Library),
    Package(Package),
}

impl EnhancedAst {
    /// Gets the name of the compilation unit
    pub fn name(&self) -> &str {
        match self {
            EnhancedAst::Program(p) => &p.name,
            EnhancedAst::Unit(u) => &u.name,
            EnhancedAst::Library(l) => &l.name,
            EnhancedAst::Package(p) => &p.name,
        }
    }
    
    /// Gets the uses clauses
    pub fn uses(&self) -> &[UseClause] {
        match self {
            EnhancedAst::Program(p) => &p.uses,
            EnhancedAst::Unit(u) => &u.uses,
            EnhancedAst::Library(l) => &l.uses,
            EnhancedAst::Package(p) => &p.uses,
        }
    }
    
    /// Gets the main block
    pub fn main_block(&self) -> Option<&Block> {
        match self {
            EnhancedAst::Program(p) => Some(&p.block),
            EnhancedAst::Unit(u) => u.initialization_section.as_ref(),
            EnhancedAst::Library(l) => Some(&l.block),
            EnhancedAst::Package(p) => Some(&p.block),
        }
    }
}
