//! Compilation driver for the MiniPAS Pascal compiler
//!
//! This crate provides the main compilation pipeline that orchestrates:
//! - File discovery and loading
//! - Dependency resolution
//! - Parsing and AST generation
//! - Module management
//! - Error reporting

use std::path::{Path, PathBuf};
use std::collections::{HashMap, HashSet};
use minipas_ast::{Unit, Program};
use minipas_parser::Parser;
use minipas_module::{ModuleLoader, ModuleManager, Module};
use anyhow::{Result, Context};

pub mod error;
pub mod compiler;

pub use error::{CompilerError, CompilerResult};
pub use compiler::Compiler;
pub use minipas_module::PpuFile;

/// Compilation options
#[derive(Debug, Clone)]
pub struct CompileOptions {
    /// Search paths for units
    pub search_paths: Vec<PathBuf>,
    
    /// Output directory for compiled files
    pub output_dir: PathBuf,
    
    /// Whether to generate PPU files
    pub generate_ppu: bool,
    
    /// Whether to use existing PPU files
    pub use_ppu: bool,
    
    /// Optimization level (0-3)
    pub optimization_level: u8,
    
    /// Whether to generate debug information
    pub debug_info: bool,
    
    /// Target architecture
    pub target: String,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            search_paths: vec![PathBuf::from(".")],
            output_dir: PathBuf::from("."),
            generate_ppu: true,
            use_ppu: true,
            optimization_level: 0,
            debug_info: true,
            target: "native".to_string(),
        }
    }
}

/// Compilation result
#[derive(Debug)]
pub struct CompileResult {
    /// Compiled module
    pub module: Module,
    
    /// Path to generated PPU file (if any)
    pub ppu_path: Option<PathBuf>,
    
    /// Compilation warnings
    pub warnings: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_options_default() {
        let opts = CompileOptions::default();
        assert_eq!(opts.optimization_level, 0);
        assert!(opts.debug_info);
        assert!(opts.generate_ppu);
        assert!(opts.use_ppu);
    }
}
