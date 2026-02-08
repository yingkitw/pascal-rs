//! Pascal Compiler - A production-ready optimizing Pascal compiler

pub mod advanced_optimizer;
pub mod advanced_types;
pub mod ast;
pub mod enhanced_ast;
pub mod enhanced_codegen;
pub mod enhanced_tokens;
pub mod error;
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

// Interpreter module with full implementation
pub mod interpreter;

// Modular interpreter components (for future use and traits)
pub mod interpreter_value;
pub mod interpreter_scope;
pub mod interpreter_function;

// Re-export key functionality
pub use ast::{
    Block, Expression, ForDirection, FunctionDecl, Literal, Module, ModuleError, ModuleResult,
    Parameter, ProcedureDecl, Program, SimpleType, Statement, Type, Unit,
};
pub use ast::{Expr, Stmt};
pub use error::{CompileOptions, CompileResult, CompilerError, ParseError};
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
pub use interpreter::Interpreter;

// Modular interpreter components
pub use interpreter_value::Value;
pub use interpreter_scope::{Scope, ScopeStack};
pub use interpreter_function::{UserFunction, FunctionRegistryImpl};

// Interpreter traits for better maintainability
pub mod interpreter_traits;
pub use interpreter_traits::*;
