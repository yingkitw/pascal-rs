use crate::ast::{CallingConvention, ConstDecl, FunctionDecl, Parameter, ProcedureDecl, TypeDecl, VariableDecl};

pub enum EnhancedAst {
    Program(Program),
    Unit(Unit),
    Library(Library),
    Package(Package),
}

pub struct Program {
    pub name: String,
    pub uses: Vec<String>,
    pub block: Block,
}

pub struct Unit {
    pub name: String,
    pub uses: Vec<String>,
    pub interface: UnitInterface,
    pub implementation: UnitImplementation,
}

pub struct Library {
    pub name: String,
    pub uses: Vec<String>,
    pub exports: Vec<String>,
    pub block: Block,
}

pub struct Package {
    pub name: String,
    pub uses: Vec<String>,
    pub requires: Vec<String>,
    pub contains: Vec<String>,
    pub block: Block,
}

pub struct UnitInterface {
    pub uses: Vec<String>,
    pub types: Vec<TypeDecl>,
    pub constants: Vec<ConstDecl>,
    pub variables: Vec<VariableDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub functions: Vec<FunctionDecl>,
}

pub struct UnitImplementation {
    pub uses: Vec<String>,
    pub types: Vec<TypeDecl>,
    pub constants: Vec<ConstDecl>,
    pub variables: Vec<VariableDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub functions: Vec<FunctionDecl>,
    pub initialization: Option<Vec<Statement>>,
    pub finalization: Option<Vec<Statement>>,
}

pub enum EnhancedType {
    Integer,
    Real,
    Boolean,
    Char,
    String,
    WideString,
    Alias {
        name: String,
        target_type: Box<EnhancedType>,
    },
    Array {
        element_type: Box<EnhancedType>,
        dimensions: Vec<ArrayDimension>,
    },
    Record {
        fields: Vec<RecordField>,
        is_packed: bool,
    },
    Set {
        base_type: Box<EnhancedType>,
    },
    File {
        element_type: Option<Box<EnhancedType>>,
    },
    Pointer {
        target_type: Box<EnhancedType>,
    },
    Class {
        name: String,
        parent: Option<String>,
        fields: Vec<FieldDecl>,
        methods: Vec<MethodDecl>,
        properties: Vec<PropertyDecl>,
    },
    Object {
        name: String,
        parent: Option<String>,
        fields: Vec<FieldDecl>,
        methods: Vec<MethodDecl>,
    },
    Interface {
        name: String,
        parent: Option<String>,
        methods: Vec<MethodDecl>,
        properties: Vec<PropertyDecl>,
    },
    Custom(String),
}

pub struct ArrayDimension {
    pub lower_bound: Option<Expr>,
    pub upper_bound: Option<Expr>,
}

pub struct RecordField {
    pub name: String,
    pub field_type: EnhancedType,
    pub offset: Option<i64>,
    pub case_variant: Option<VariantPart>,
}

pub struct VariantPart {
    pub discriminant: String,
    pub discriminant_type: EnhancedType,
    pub cases: Vec<VariantCase>,
}

pub struct VariantCase {
    pub values: Vec<Literal>,
    pub fields: Vec<RecordField>,
}

pub struct FieldDecl {
    pub name: String,
    pub field_type: EnhancedType,
}

pub struct MethodDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<EnhancedType>,
}

pub struct PropertyDecl {
    pub name: String,
    pub property_type: EnhancedType,
}

pub struct Constant {
    pub name: String,
    pub constant_type: Option<EnhancedType>,
    pub value: Expression,
}

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

pub struct Block {
    pub consts: Vec<Constant>,
    pub types: Vec<TypeDefinition>,
    pub vars: Vec<Variable>,
    pub procedures: Vec<Procedure>,
    pub functions: Vec<Function>,
    pub statements: Vec<Statement>,
    pub labels: Vec<String>,
}

pub struct TypeDefinition {
    pub name: String,
    pub type_definition: EnhancedType,
}

pub type Statement = crate::ast::Statement;
pub type Expression = crate::ast::Expression;
pub type Stmt = crate::ast::Statement;
pub type Expr = crate::ast::Expression;
pub type Literal = crate::ast::Literal;

pub struct UseClause {
    pub unit_name: String,
    pub alias: Option<String>,
    pub is_in_interface: bool,
}

pub struct ExportClause {
    pub name: String,
    pub alias: Option<String>,
    pub index: Option<i64>,
    pub is_name: bool,
}

pub enum ForDirection {
    To,
    Downto,
}

pub struct CaseBranch {
    pub values: Vec<Expression>,
    pub body: Block,
}

pub struct ExceptClause {
    pub exception_type: Option<String>,
    pub variable: Option<String>,
    pub body: Block,
}

// Add more as needed
