//! Compiler trait definitions
//!
//! This module provides trait-based interfaces for all compiler components,
//! enabling testability, modularity, and multiple implementations.

pub mod codegen;
pub mod diagnostic;
pub mod lexer;
pub mod optimizer;
pub mod parser;

// Re-export commonly used traits
pub use codegen::*;
pub use diagnostic::*;
pub use lexer::*;
pub use optimizer::*;
pub use parser::*;
