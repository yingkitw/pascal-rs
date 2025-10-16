//! Error types for the compilation driver

use thiserror::Error;
use std::path::PathBuf;

/// Compiler error types
#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("File not found: {0}")]
    FileNotFound(PathBuf),
    
    #[error("Parse error in {file}: {message}")]
    ParseError {
        file: PathBuf,
        message: String,
    },
    
    #[error("Module error: {0}")]
    ModuleError(String),
    
    #[error("Dependency cycle detected: {0}")]
    CircularDependency(String),
    
    #[error("Unit not found: {0}")]
    UnitNotFound(String),
    
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    
    #[error("Multiple errors occurred")]
    MultipleErrors(Vec<CompilerError>),
    
    #[error("{0}")]
    Other(String),
}

/// Result type for compiler operations
pub type CompilerResult<T> = Result<T, CompilerError>;

impl From<minipas_module::ModuleError> for CompilerError {
    fn from(err: minipas_module::ModuleError) -> Self {
        CompilerError::ModuleError(err.to_string())
    }
}

impl From<minipas_parser::ParseError> for CompilerError {
    fn from(err: minipas_parser::ParseError) -> Self {
        CompilerError::ParseError {
            file: PathBuf::from("<unknown>"),
            message: format!("{:?}", err),
        }
    }
}
