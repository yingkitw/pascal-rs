//! Plugin architecture for extending compiler functionality
//!
//! Provides a trait-based plugin system that allows external components
//! to hook into compilation phases.

use crate::ast::{Program, Unit};
use anyhow::Result;
use std::path::Path;

/// Phase of compilation that plugins can hook into
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilationPhase {
    /// After lexing, before parsing
    PostLex,
    /// After parsing a program
    PostParseProgram,
    /// After parsing a unit
    PostParseUnit,
    /// After type checking
    PostTypeCheck,
    /// Before code generation
    PreCodegen,
    /// After code generation
    PostCodegen,
}

/// Result of a plugin's transformation (optional modified AST)
#[derive(Debug)]
pub enum PluginResult<T> {
    /// Plugin did not modify; use original
    Unchanged,
    /// Plugin produced a replacement
    Modified(T),
}

/// Trait for compiler plugins that can inspect or transform compilation artifacts
pub trait CompilerPlugin: Send + Sync {
    /// Plugin name for diagnostics
    fn name(&self) -> &str;

    /// Called when a program has been parsed. Plugin may return a modified program.
    fn on_program_parsed(&self, _program: &Program) -> Result<PluginResult<Program>> {
        Ok(PluginResult::Unchanged)
    }

    /// Called when a unit has been parsed. Plugin may return a modified unit.
    fn on_unit_parsed(&self, _unit: &Unit, _path: &Path) -> Result<PluginResult<Unit>> {
        Ok(PluginResult::Unchanged)
    }

    /// Phases this plugin is interested in (for optimization)
    fn phases(&self) -> &[CompilationPhase] {
        &[]
    }
}

/// Registry of plugins, invoked in order during compilation
#[derive(Default)]
pub struct PluginRegistry {
    plugins: Vec<Box<dyn CompilerPlugin>>,
}

impl PluginRegistry {
    pub fn new() -> Self {
        Self {
            plugins: Vec::new(),
        }
    }

    /// Register a plugin
    pub fn register(&mut self, plugin: Box<dyn CompilerPlugin>) {
        self.plugins.push(plugin);
    }

    /// Run program-parsed hooks; returns first modified program or original
    pub fn on_program_parsed(&self, program: Program) -> Result<Program> {
        let mut current = program;
        for plugin in &self.plugins {
            match plugin.on_program_parsed(&current)? {
                PluginResult::Unchanged => {}
                PluginResult::Modified(p) => current = p,
            }
        }
        Ok(current)
    }

    /// Run unit-parsed hooks; returns first modified unit or original
    pub fn on_unit_parsed(&self, unit: Unit, path: &Path) -> Result<Unit> {
        let mut current = unit;
        for plugin in &self.plugins {
            match plugin.on_unit_parsed(&current, path)? {
                PluginResult::Unchanged => {}
                PluginResult::Modified(u) => current = u,
            }
        }
        Ok(current)
    }

    /// Number of registered plugins
    pub fn len(&self) -> usize {
        self.plugins.len()
    }

    pub fn is_empty(&self) -> bool {
        self.plugins.is_empty()
    }
}
