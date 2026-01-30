//! Simple error types for the Pascal compiler

use std::path::PathBuf;
use thiserror::Error;

/// Compilation result
#[derive(Debug)]
pub struct CompileResult {
    /// Compiled successfully
    pub success: bool,
    /// Any messages
    pub messages: Vec<String>,
}

/// Compilation options
#[derive(Debug, Clone)]
pub struct CompileOptions {
    pub output_dir: PathBuf,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            output_dir: PathBuf::from("."),
        }
    }
}

/// Compilation errors
#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("IO error: {0}")]
    IoError(String),

    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Unexpected token: {0}")]
    UnexpectedToken(String),

    #[error("Compilation error: {0}")]
    CompilationError(String),
}

/// Parse error type alias for convenience
pub type ParseError = CompilerError;

/// Result type
pub type Result<T> = std::result::Result<T, CompilerError>;
