//! Pascal Compiler - A production-ready optimizing Pascal compiler

pub mod advanced_types;
pub mod ast;
pub mod build_system;
pub mod conditional_compile;
pub mod constant_eval;
pub mod debugger;
pub mod ide;
pub mod leak_check;
pub mod repl;
#[cfg(feature = "lsp")]
pub mod lsp_server;
pub mod ffi;
pub mod interfaces;
pub mod memory_pool;
pub mod reflection;
pub mod wasm;
pub mod bench;
pub mod deps;
pub mod incremental;
pub mod docgen;
pub mod enhanced_error;
pub mod error;
pub mod error_conversions;
pub mod error_suggestions;
pub mod optimized_symbol_table;
pub mod formatter;
pub mod interpreter;
pub mod lexer;
pub mod loader;
#[cfg(feature = "mcp")]
pub mod mcp_server;
pub mod optimizer;
pub mod parallel;
#[cfg(feature = "profile")]
pub mod profile;
pub mod parser;

pub mod ppu;
pub mod register_allocator;
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
pub use error_suggestions::{did_you_mean, suggest_identifier, suggest_similar};
pub use interpreter::Interpreter;
pub use lexer::Lexer;
pub use loader::ModuleLoader;
#[cfg(feature = "mcp")]
pub use mcp_server::{
    CompileRequest, CompileResponse, McpServer, McpServerBuilder, StatusRequest, StatusResponse,
};
pub use parallel::{ParallelCompiler, ParallelConfig, ProgressTracker};
pub use formatter::{format_file, format_string, format_unit_file, needs_formatting};
pub use constant_eval::{eval_constant, is_constant};
pub use docgen::{generate_docs_from_source, DocFormat};
pub use conditional_compile::{defines_from_args, preprocess};
pub use advanced_types::infer_block_variable_types;
pub use tokens::Token;
pub use unit_codegen::UnitCodeGenerator;
