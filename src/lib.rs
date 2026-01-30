//! Pascal Compiler - A production-ready optimizing Pascal compiler

pub mod ast;
pub mod basic_lexer;
pub mod basic_parser;
pub mod codegen;
pub mod compiler;
pub mod enhanced_ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod ppu;
pub mod tokens;
pub mod traits;
pub mod utils;

// Re-export key functionality
pub use ast::{
    Block, Expression, ForDirection, FunctionDecl, Literal, Parameter, ProcedureDecl,
    Program, SimpleType, Statement, Type, Unit,
};
pub use basic_lexer::Lexer;
pub use basic_parser::Parser as BasicParser;
pub use codegen::CodeGenerator;
pub use compiler::Compiler;
pub use error::{CompileOptions, CompileResult, CompilerError, ParseError};
pub use lexer::Lexer as FullLexer;
pub use ppu::PpuFile;
pub use tokens::Token;

// Re-export type aliases for convenience
pub use ast::{Expr, Stmt};
