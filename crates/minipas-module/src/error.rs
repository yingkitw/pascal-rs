//! Error types for the module system

use thiserror::Error;

/// Result type for module operations
pub type ModuleResult<T> = Result<T, ModuleError>;

/// Errors that can occur in the module system
#[derive(Error, Debug, Clone)]
pub enum ModuleError {
    #[error("Module not found: {0}")]
    ModuleNotFound(String),
    
    #[error("Duplicate module: {0}")]
    DuplicateModule(String),
    
    #[error("Circular dependency detected involving module: {0}")]
    CircularDependency(String),
    
    #[error("Failed to load module {0}: {1}")]
    LoadError(String, String),
    
    #[error("Failed to compile module {0}: {1}")]
    CompileError(String, String),
    
    #[error("Interface CRC mismatch for module {0}")]
    CrcMismatch(String),
    
    #[error("Module {0} is not compiled")]
    NotCompiled(String),
    
    #[error("Invalid module name: {0}")]
    InvalidModuleName(String),
    
    #[error("Module {0} has unresolved dependencies: {1:?}")]
    UnresolvedDependencies(String, Vec<String>),
    
    #[error("I/O error: {0}")]
    IoError(String),
    
    #[error("Parse error in module {0}: {1}")]
    ParseError(String, String),
}

impl From<std::io::Error> for ModuleError {
    fn from(err: std::io::Error) -> Self {
        ModuleError::IoError(err.to_string())
    }
}
