use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
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
    Record(HashMap<String, Type>),
    Procedure,
    Function(Box<Type>), // Return type
    Custom(String),
    Pointer(Box<Type>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Real(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Array(Vec<Expr>),
    Record(HashMap<String, Expr>),
    Nil,
}

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    Negate,
    AddressOf,
    Dereference,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
    pub is_var: bool, // true for var parameters
    pub is_const: bool, // true for const parameters
}

#[derive(Debug, Clone)]
pub struct VariableDecl {
    pub name: String,
    pub var_type: Type,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone)]
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
    With {
        record: Expr,
        body: Vec<Stmt>,
    },
    Empty,
    ProcedureCall {
        name: String,
        args: Vec<Expr>,
    },
    Exit(Option<Expr>), // Optional return value for functions
    Break,
    Continue,
    Goto(String),
    LabeledStmt {
        label: String,
        stmt: Box<Stmt>,
    },
    Block(Block),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ForDirection {
    To,
    DownTo,
}

#[derive(Debug, Clone)]
pub struct CaseArm {
    pub constants: Vec<Literal>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub consts: Vec<ConstDecl>,
    pub types: Vec<TypeDecl>,
    pub vars: Vec<VariableDecl>,
    pub procedures: Vec<ProcedureDecl>,
    pub functions: Vec<FunctionDecl>,
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ProcedureDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub block: Block,
    pub is_forward: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub block: Block,
    pub is_forward: bool,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub uses: Vec<String>,
    pub block: Block,
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
            Type::Record(fields) => {
                write!(f, "record ")?;
                for (i, (name, typ)) in fields.iter().enumerate() {
                    if i > 0 { write!(f, "; ")?; }
                    write!(f, "{}: {}", name, typ)?;
                }
                write!(f, " end")
            },
            Type::Procedure => write!(f, "procedure"),
            Type::Function(typ) => write!(f, "function: {}", typ),
            Type::Pointer(typ) => write!(f, "^{}", typ),
            Type::Custom(name) => write!(f, "{}", name),
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
        }
    }
}
