//! Compiler trait definitions
//!
//! This module provides trait-based interfaces for all compiler components,
//! enabling testability, modularity, and multiple implementations.

pub mod lexer;
pub mod parser;
pub mod codegen;
pub mod optimizer;
pub mod diagnostic;

// Re-export commonly used traits
pub use lexer::*;
pub use parser::*;
pub use codegen::*;
pub use optimizer::*;
pub use diagnostic::*;
