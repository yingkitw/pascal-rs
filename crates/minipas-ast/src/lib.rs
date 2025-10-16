use std::collections::HashMap;
use std::fmt;

pub mod enhanced_ast;

pub use enhanced_ast::*;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
    pub is_var: bool,
    pub is_const: bool,
    pub is_out: bool,
    pub default_value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Type {
    Integer,
    Real,
    Boolean,
    Char,
    String(Option<usize>), // Optional maximum length
    Array {
        index_type: Box<Type>,
        element_type: Box<Type>,
        range: Option<(i64, i64)>,
    },
    Record {
        fields: HashMap<String, Type>,
        is_packed: bool,
        variant_part: Option<Box<VariantPart>>,
    },
    VariantRecord {
        common_fields: HashMap<String, Type>,
        variant_part: Box<VariantPart>,
        is_packed: bool,
    },
    DynamicArray {
        element_type: Box<Type>,
    },
    OpenArray {
        element_type: Box<Type>,
    },
    Variant,
    File {
        element_type: Option<Box<Type>>,
    },
    Enum {
        values: Vec<String>,
        custom_values: Option<HashMap<String, i64>>,
    },
    Set {
        base_type: Box<Type>,
    },
    Range {
        base_type: Box<Type>,
        min: i64,
        max: i64,
    },
    Procedure {
        params: Vec<Parameter>,
        is_forward: bool,
    },
    Function {
        return_type: Box<Type>,
        params: Vec<Parameter>,
        is_forward: bool,
    },
    Custom(String),
    Pointer(Box<Type>),
    Class {
        name: String,
        parent: Option<String>,
        fields: HashMap<String, Type>,
        methods: HashMap<String, Type>,
    },
    Interface {
        name: String,
        methods: HashMap<String, Type>,
    },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Literal {
    Integer(i64),
    Real(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Array(Vec<Expr>),
    Record(HashMap<String, Expr>),
    Set(Vec<Expr>),
    Enum(String, i64), // enum name and value
    Range(i64, i64),   // min, max
    Nil,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Expr {
    Literal(Literal),
    Identifier(Vec<String>), // For record fields and nested scopes
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
    ArrayAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    RecordAccess {
        record: Box<Expr>,
        field: String,
    },
    AddressOf(Box<Expr>),
    Dereference(Box<Expr>),
    TypeCast {
        target_type: Type,
        expr: Box<Expr>,
    },
    Variable(String),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Set(Vec<Expr>),
    In {
        expr: Box<Expr>,
        set_expr: Box<Expr>,
    },
    Is {
        expr: Box<Expr>,
        type_name: String,
    },
    As {
        expr: Box<Expr>,
        type_name: String,
    },
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
    },
    With {
        expr: Box<Expr>,
        body: Box<Expr>,
    },
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
    PropertyAccess {
        object: Box<Expr>,
        property: String,
    },
    PropertyAssignment {
        object: Box<Expr>,
        property: String,
        value: Box<Expr>,
    },
    New {
        type_name: String,
        args: Vec<Expr>,
    },
    Dispose {
        expr: Box<Expr>,
    },
    GetMem {
        size: Box<Expr>,
    },
    FreeMem {
        expr: Box<Expr>,
    },
    SetLength {
        array_expr: Box<Expr>,
        length: Box<Expr>,
    },
    High {
        expr: Box<Expr>,
    },
    Low {
        expr: Box<Expr>,
    },
    SizeOf {
        type_name: Type,
    },
    TypeOf {
        expr: Box<Expr>,
    },
    GenericSpecialization {
        generic_name: String,
        type_arguments: Vec<Type>,
    },
    InlineAssembly {
        code: String,
        inputs: Vec<(String, Expr)>,
        outputs: Vec<(String, String)>,
        clobbers: Vec<String>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    IntDivide,  // div
    Modulo,     // mod
    
    // Comparison
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    
    // Logical
    And,
    Or,
    Xor,
    
    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    
    // Set operations
    In,
    
    // String operations
    Concat,
    
    // Set operations
    Union,
    Intersection,
    Difference,
    SymmetricDifference,
    
    // Custom operators (for operator overloading)
    Custom(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    Negate,
    AddressOf,
    Dereference,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct VariableDecl {
    pub name: String,
    pub var_type: Type,
    pub initializer: Option<Expr>,
    pub is_threadvar: bool,
    pub absolute_address: Option<Expr>,
    pub external_name: Option<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TypeDecl {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ConstDecl {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Stmt {
    Assignment {
        target: Expr,
        value: Expr,
    },
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    Repeat {
        body: Vec<Stmt>,
        condition: Expr,
    },
    RepeatUntil {
        body: Vec<Stmt>,
        condition: Expr,
    },
    For {
        var_name: String,
        start: Expr,
        direction: ForDirection,
        end: Expr,
        body: Vec<Stmt>,
    },
    Case {
        expr: Expr,
        arms: Vec<CaseArm>,
        else_arm: Option<Vec<Stmt>>,
    },
    Try {
        body: Vec<Stmt>,
        except_arms: Vec<ExceptArm>,
        finally_body: Option<Vec<Stmt>>,
    },
    Raise {
        exception_type: Option<String>,
        message: Option<Expr>,
    },
    TryExcept {
        body: Vec<Stmt>,
        except_arms: Vec<ExceptArm>,
    },
    TryFinally {
        body: Vec<Stmt>,
        finally_body: Vec<Stmt>,
    },
    Break,
    Continue,
    Exit {
        expr: Option<Expr>,
    },
    With {
        expr: Expr,
        body: Vec<Stmt>,
    },
    Label {
        name: String,
        stmt: Box<Stmt>,
    },
    Goto {
        label: String,
    },
    Empty,
    ProcedureCall {
        name: String,
        args: Vec<Expr>,
    },
    LabeledStmt {
        label: String,
        stmt: Box<Stmt>,
    },
    Block(Block),
}

#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ForDirection {
    To,
    DownTo,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CaseArm {
    pub constants: Vec<Literal>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExceptArm {
    pub exception_type: Option<String>,
    pub variable: Option<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Block {
    pub consts: Vec<ConstDecl>,
    pub types: Vec<TypeDecl>,
    pub vars: Vec<VariableDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub functions: Vec<FunctionDecl>,
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ProcedureDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub block: Block,
    pub is_forward: bool,
    pub is_external: bool,
    pub external_name: Option<String>,
    pub external_library: Option<String>,
    pub is_inline: bool,
    pub is_assembler: bool,
    pub calling_convention: Option<CallingConvention>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub block: Block,
    pub is_forward: bool,
    pub is_external: bool,
    pub external_name: Option<String>,
    pub external_library: Option<String>,
    pub is_inline: bool,
    pub is_assembler: bool,
    pub calling_convention: Option<CallingConvention>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Program {
    pub name: String,
    pub uses: Vec<String>,
    pub block: Block,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Unit {
    pub name: String,
    pub uses: Vec<String>,
    pub interface: UnitInterface,
    pub implementation: UnitImplementation,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct UnitInterface {
    pub types: Vec<TypeDecl>,
    pub constants: Vec<ConstDecl>,
    pub variables: Vec<VariableDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub functions: Vec<FunctionDecl>,
    pub classes: Vec<ClassDecl>,
    pub interfaces: Vec<InterfaceDecl>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct UnitImplementation {
    pub uses: Vec<String>,
    pub types: Vec<TypeDecl>,
    pub constants: Vec<ConstDecl>,
    pub variables: Vec<VariableDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub functions: Vec<FunctionDecl>,
    pub classes: Vec<ClassDecl>,
    pub interfaces: Vec<InterfaceDecl>,
    pub initialization: Option<Vec<Stmt>>,
    pub finalization: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ClassDecl {
    pub name: String,
    pub parent: Option<String>,
    pub visibility: Visibility,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<MethodDecl>,
    pub properties: Vec<PropertyDecl>,
    pub constructors: Vec<ConstructorDecl>,
    pub destructors: Vec<DestructorDecl>,
    pub is_abstract: bool,
    pub is_sealed: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct InterfaceDecl {
    pub name: String,
    pub parent_interfaces: Vec<String>,
    pub methods: Vec<MethodDecl>,
    pub properties: Vec<PropertyDecl>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FieldDecl {
    pub name: String,
    pub field_type: Type,
    pub visibility: Visibility,
    pub is_class: bool,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MethodDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub visibility: Visibility,
    pub is_virtual: bool,
    pub is_override: bool,
    pub is_abstract: bool,
    pub is_class: bool,
    pub is_static: bool,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PropertyDecl {
    pub name: String,
    pub property_type: Type,
    pub visibility: Visibility,
    pub read_accessor: PropertyAccessor,
    pub write_accessor: Option<PropertyAccessor>,
    pub is_class: bool,
    pub is_virtual: bool,
    pub is_override: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum PropertyAccessor {
    Field(String),
    Method(String),
    Indexed(Box<PropertyAccessor>, Vec<Expr>),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ConstructorDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub visibility: Visibility,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct DestructorDecl {
    pub name: String,
    pub visibility: Visibility,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct OperatorDecl {
    pub operator: BinaryOp,
    pub left_type: Type,
    pub right_type: Option<Type>, // None for unary operators
    pub return_type: Type,
    pub params: Vec<Parameter>,
    pub body: Option<Vec<Stmt>>,
    pub is_class: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct UnaryOperatorDecl {
    pub operator: UnaryOp,
    pub operand_type: Type,
    pub return_type: Type,
    pub params: Vec<Parameter>,
    pub body: Option<Vec<Stmt>>,
    pub is_class: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Visibility {
    Private,
    Protected,
    Public,
    Published,
}

#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum CallingConvention {
    Default,
    CDecl,
    Pascal,
    StdCall,
    CppDecl,
    Register,
    SafeCall,
    FastCall,
    VectorCall,
    ThisCall,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct VariantPart {
    pub discriminant: String,
    pub discriminant_type: Box<Type>,
    pub variants: Vec<VariantCase>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct VariantCase {
    pub case_values: Vec<Literal>,
    pub fields: HashMap<String, Type>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct GenericType {
    pub name: String,
    pub type_parameters: Vec<TypeParameter>,
    pub constraints: Vec<TypeConstraint>,
    pub body: Type,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TypeParameter {
    pub name: String,
    pub constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum TypeConstraint {
    Class(String),
    Constructor,
    Record,
    Interface(String),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SpecializedType {
    pub generic_type: String,
    pub type_arguments: Vec<Type>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum TopLevelDecl {
    Program(Program),
    Unit(Unit),
}

// Pretty printing for the AST
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Identifier(parts) => {
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 { write!(f, ".")?; }
                    write!(f, "{}", part)?;
                }
                Ok(())
            },
            Expr::BinaryOp { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            },
            Expr::UnaryOp { op, expr } => {
                write!(f, "{}{}", op, expr)
            },
            Expr::FunctionCall { name, args } => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 { write!(f, ", ")?; }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            },
            Expr::ArrayAccess { array, index } => {
                write!(f, "{}[{}]", array, index)
            },
            Expr::RecordAccess { record, field } => {
                write!(f, "{}.{}", record, field)
            },
            Expr::AddressOf(expr) => {
                write!(f, "@{}", expr)
            },
            Expr::Dereference(expr) => {
                write!(f, "^({})", expr)
            },
            Expr::TypeCast { target_type, expr } => {
                write!(f, "({}) {}", target_type, expr)
            },
            Expr::Variable(name) => write!(f, "{}", name),
            Expr::Call { name, args } => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 { write!(f, ", ")?; }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            },
            Expr::Set(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            },
            Expr::In { expr, set_expr } => {
                write!(f, "{} in {}", expr, set_expr)
            },
            Expr::Is { expr, type_name } => {
                write!(f, "{} is {}", expr, type_name)
            },
            Expr::As { expr, type_name } => {
                write!(f, "{} as {}", expr, type_name)
            },
            Expr::Range { start, end } => {
                write!(f, "{}..{}", start, end)
            },
            Expr::With { expr, body } => {
                write!(f, "with {} do {}", expr, body)
            },
            Expr::MethodCall { object, method, args } => {
                write!(f, "{}.{}(", object, method)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            },
            Expr::PropertyAccess { object, property } => {
                write!(f, "{}.{}", object, property)
            },
            Expr::PropertyAssignment { object, property, value } => {
                write!(f, "{}.{} := {}", object, property, value)
            },
            Expr::New { type_name, args } => {
                write!(f, "new({}", type_name)?;
                if !args.is_empty() {
                    write!(f, ", ")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 { write!(f, ", ")?; }
                        write!(f, "{}", arg)?;
                    }
                }
                write!(f, ")")
            },
            Expr::Dispose { expr } => {
                write!(f, "dispose({})", expr)
            },
            Expr::GetMem { size } => {
                write!(f, "getmem({})", size)
            },
            Expr::FreeMem { expr } => {
                write!(f, "freemem({})", expr)
            },
            Expr::SetLength { array_expr, length } => {
                write!(f, "setlength({}, {})", array_expr, length)
            },
            Expr::High { expr } => {
                write!(f, "high({})", expr)
            },
            Expr::Low { expr } => {
                write!(f, "low({})", expr)
            },
            Expr::SizeOf { type_name } => {
                write!(f, "sizeof({})", type_name)
            },
            Expr::TypeOf { expr } => {
                write!(f, "typeof({})", expr)
            },
            Expr::GenericSpecialization { generic_name, type_arguments } => {
                write!(f, "{}<", generic_name)?;
                for (i, arg) in type_arguments.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")
            },
            Expr::InlineAssembly { code, .. } => {
                write!(f, "asm\n{}\nend", code)
            },
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Subtract => write!(f, "-"),
            BinaryOp::Multiply => write!(f, "*"),
            BinaryOp::Divide => write!(f, "/"),
            BinaryOp::IntDivide => write!(f, "div"),
            BinaryOp::Modulo => write!(f, "mod"),
            BinaryOp::Equal => write!(f, "="),
            BinaryOp::NotEqual => write!(f, "<>"),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessOrEqual => write!(f, "<="),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterOrEqual => write!(f, ">="),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Xor => write!(f, "xor"),
            BinaryOp::BitwiseAnd => write!(f, "&"),
            BinaryOp::BitwiseOr => write!(f, "|"),
            BinaryOp::BitwiseXor => write!(f, "xor"),
            BinaryOp::ShiftLeft => write!(f, "<<"),
            BinaryOp::ShiftRight => write!(f, ">>"),
            BinaryOp::In => write!(f, "in"),
            BinaryOp::Concat => write!(f, "+"),
            BinaryOp::Union => write!(f, "+"),
            BinaryOp::Intersection => write!(f, "*"),
            BinaryOp::Difference => write!(f, "-"),
            BinaryOp::SymmetricDifference => write!(f, "><"),
            BinaryOp::Custom(op) => write!(f, "{}", op),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Plus => write!(f, "+"),
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Not => write!(f, "not"),
            UnaryOp::Negate => write!(f, "-"),
            UnaryOp::AddressOf => write!(f, "@"),
            UnaryOp::Dereference => write!(f, "^"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "integer"),
            Type::Real => write!(f, "real"),
            Type::Boolean => write!(f, "boolean"),
            Type::Char => write!(f, "char"),
            Type::String(None) => write!(f, "string"),
            Type::String(Some(len)) => write!(f, "string[{}]", len),
            Type::Array { index_type, element_type, range: _ } => 
                write!(f, "array [{}] of {}", index_type, element_type),
            Type::Record { fields, .. } => {
                write!(f, "record ")?;
                for (i, (name, typ)) in fields.iter().enumerate() {
                    if i > 0 { write!(f, "; ")?; }
                    write!(f, "{}: {}", name, typ)?;
                }
                write!(f, " end")
            },
            Type::Procedure { .. } => write!(f, "procedure"),
            Type::Function { return_type: typ, .. } => write!(f, "function: {}", typ),
            Type::Pointer(typ) => write!(f, "^{}", typ),
            Type::Custom(name) => write!(f, "{}", name),
            Type::Enum { .. } => write!(f, "enum"),
            Type::Set { .. } => write!(f, "set"),
            Type::Range { .. } => write!(f, "range"),
            Type::File { element_type: _ } => write!(f, "file"),
            Type::Variant => write!(f, "variant"),
            Type::Class { .. } => write!(f, "class"),
            Type::Interface { .. } => write!(f, "interface"),
            Type::VariantRecord { .. } => write!(f, "variant record"),
            Type::DynamicArray { element_type } => write!(f, "array of {}", element_type),
            Type::OpenArray { element_type } => write!(f, "array of {}", element_type),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Real(r) => write!(f, "{}", r),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Char(c) => write!(f, "'{}'", c),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Array(elems) => {
                write!(f, "[")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            },
            Literal::Record(fields) => {
                write!(f, "(")?;
                for (i, (name, value)) in fields.iter().enumerate() {
                    if i > 0 { write!(f, "; ")?; }
                    write!(f, "{} = {}", name, value)?;
                }
                write!(f, ")")
            },
            Literal::Nil => write!(f, "nil"),
            Literal::Set(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            },
            Literal::Enum(name, value) => write!(f, "{}.{}", name, value),
            Literal::Range(min, max) => write!(f, "{}..{}", min, max),
        }
    }
}

#[cfg(test)]
mod comprehensive_tests;
