//! Pascal Compiler - A production-ready optimizing Pascal compiler

pub mod advanced_optimizer;
pub mod advanced_types;
pub mod ast;
pub mod build_system;
pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod loader;
#[cfg(feature = "mcp")]
pub mod mcp_server;
pub mod optimizer;
pub mod parallel;
pub mod parser;
pub mod ppu;
pub mod register_allocator;
pub mod resolver;
pub mod simd;
pub mod symbol_table;
pub mod tokens;
pub mod traits;
pub mod type_checker;
pub mod unit_codegen;
pub mod utils;

// Re-export key types
pub use ast::{
    Block, Expr, Expression, ForDirection, FunctionDecl, Literal, Module, ModuleError,
    ModuleResult, Parameter, ProcedureDecl, Program, SimpleType, Statement, Stmt, Type, Unit,
};
pub use error::{CompileOptions, CompileResult, CompilerError, ParseError};
pub use interpreter::Interpreter;
pub use lexer::Lexer;
pub use loader::ModuleLoader;
#[cfg(feature = "mcp")]
pub use mcp_server::{
    CompileRequest, CompileResponse, McpServer, McpServerBuilder, StatusRequest, StatusResponse,
};
pub use parallel::{ParallelCompiler, ParallelConfig, ProgressTracker};
pub use resolver::{ModuleResolver, SymbolInfo, SymbolKind, SymbolTable};
pub use tokens::Token;
pub use unit_codegen::UnitCodeGenerator;
