//! Unit-aware code generation
//!
//! This module provides code generation for Pascal units with support for:
//! - Interface/implementation sections
//! - Cross-module symbol resolution
//! - Separate compilation
//! - Module linking

use minipas_ast::*;
use anyhow::{Result, anyhow};
use std::collections::HashMap;
use std::fmt::Write;

/// Unit-aware code generator
pub struct UnitCodeGenerator {
    /// Generated code output
    output: String,
    
    /// Label counter for unique labels
    label_counter: u32,
    
    /// Symbol table for variables
    variables: HashMap<String, VariableInfo>,
    
    /// Exported symbols from interface
    exports: Vec<String>,
    
    /// Imported symbols from other units
    imports: HashMap<String, Vec<String>>,
    
    /// Current unit name
    current_unit: Option<String>,
}

#[derive(Debug, Clone)]
struct VariableInfo {
    offset: i32,
    typ: Type,
    is_exported: bool,
}

impl UnitCodeGenerator {
    /// Create a new unit code generator
    pub fn new() -> Self {
        Self {
            output: String::new(),
            label_counter: 0,
            variables: HashMap::new(),
            exports: Vec::new(),
            imports: HashMap::new(),
            current_unit: None,
        }
    }
    
    /// Generate code for a unit
    pub fn generate_unit(&mut self, unit: &Unit) -> Result<String> {
        self.output.clear();
        self.current_unit = Some(unit.name.clone());
        
        // Emit assembly header
        writeln!(&mut self.output, ".intel_syntax noprefix")?;
        writeln!(&mut self.output, ".section .text")?;
        writeln!(&mut self.output)?;
        
        // Process uses clause (imports)
        for used_unit in &unit.uses {
            self.add_import(used_unit.clone());
        }
        
        // Generate code for interface (exports)
        self.generate_interface(&unit.interface)?;
        
        // Generate code for implementation
        self.generate_implementation(&unit.implementation)?;
        
        // Generate initialization section
        if let Some(ref init_stmts) = unit.implementation.initialization {
            self.generate_initialization(&unit.name, init_stmts)?;
        }
        
        // Generate finalization section
        if let Some(ref final_stmts) = unit.implementation.finalization {
            self.generate_finalization(&unit.name, final_stmts)?;
        }
        
        Ok(self.output.clone())
    }
    
    /// Generate code for interface section
    fn generate_interface(&mut self, interface: &UnitInterface) -> Result<()> {
        writeln!(&mut self.output, "# Interface section")?;
        
        // Export functions
        for func in &interface.functions {
            self.exports.push(func.name.clone());
            writeln!(&mut self.output, ".global {}", func.name)?;
        }
        
        // Export procedures
        for proc in &interface.procedures {
            self.exports.push(proc.name.clone());
            writeln!(&mut self.output, ".global {}", proc.name)?;
        }
        
        writeln!(&mut self.output)?;
        Ok(())
    }
    
    /// Generate code for implementation section
    fn generate_implementation(&mut self, implementation: &UnitImplementation) -> Result<()> {
        writeln!(&mut self.output, "# Implementation section")?;
        
        // Generate functions
        for func in &implementation.functions {
            self.generate_function(func)?;
        }
        
        // Generate procedures
        for proc in &implementation.procedures {
            self.generate_procedure(proc)?;
        }
        
        Ok(())
    }
    
    /// Generate code for a function
    fn generate_function(&mut self, func: &FunctionDecl) -> Result<()> {
        writeln!(&mut self.output, "{}:", func.name)?;
        writeln!(&mut self.output, "    push rbp")?;
        writeln!(&mut self.output, "    mov rbp, rsp")?;
        
        // TODO: Generate function body from block
        for stmt in &func.block.statements {
            self.generate_statement(stmt)?;
        }
        
        writeln!(&mut self.output, "    pop rbp")?;
        writeln!(&mut self.output, "    ret")?;
        writeln!(&mut self.output)?;
        
        Ok(())
    }
    
    /// Generate code for a procedure
    fn generate_procedure(&mut self, proc: &ProcedureDecl) -> Result<()> {
        writeln!(&mut self.output, "{}:", proc.name)?;
        writeln!(&mut self.output, "    push rbp")?;
        writeln!(&mut self.output, "    mov rbp, rsp")?;
        
        // TODO: Generate procedure body from block
        for stmt in &proc.block.statements {
            self.generate_statement(stmt)?;
        }
        
        writeln!(&mut self.output, "    pop rbp")?;
        writeln!(&mut self.output, "    ret")?;
        writeln!(&mut self.output)?;
        
        Ok(())
    }
    
    /// Generate code for a statement (placeholder)
    fn generate_statement(&mut self, _stmt: &Stmt) -> Result<()> {
        // TODO: Implement statement generation
        Ok(())
    }
    
    /// Generate initialization section
    fn generate_initialization(&mut self, unit_name: &str, stmts: &[Stmt]) -> Result<()> {
        writeln!(&mut self.output, "# Initialization section")?;
        writeln!(&mut self.output, ".global {}_init", unit_name)?;
        writeln!(&mut self.output, "{}_init:", unit_name)?;
        writeln!(&mut self.output, "    push rbp")?;
        writeln!(&mut self.output, "    mov rbp, rsp")?;
        
        for stmt in stmts {
            self.generate_statement(stmt)?;
        }
        
        writeln!(&mut self.output, "    pop rbp")?;
        writeln!(&mut self.output, "    ret")?;
        writeln!(&mut self.output)?;
        
        Ok(())
    }
    
    /// Generate finalization section
    fn generate_finalization(&mut self, unit_name: &str, stmts: &[Stmt]) -> Result<()> {
        writeln!(&mut self.output, "# Finalization section")?;
        writeln!(&mut self.output, ".global {}_finalize", unit_name)?;
        writeln!(&mut self.output, "{}_finalize:", unit_name)?;
        writeln!(&mut self.output, "    push rbp")?;
        writeln!(&mut self.output, "    mov rbp, rsp")?;
        
        for stmt in stmts {
            self.generate_statement(stmt)?;
        }
        
        writeln!(&mut self.output, "    pop rbp")?;
        writeln!(&mut self.output, "    ret")?;
        writeln!(&mut self.output)?;
        
        Ok(())
    }
    
    /// Add an import from another unit
    fn add_import(&mut self, unit_name: String) {
        self.imports.entry(unit_name).or_insert_with(Vec::new);
    }
    
    /// Get exported symbols
    pub fn get_exports(&self) -> &[String] {
        &self.exports
    }
    
    /// Get imported units
    pub fn get_imports(&self) -> &HashMap<String, Vec<String>> {
        &self.imports
    }
}

impl Default for UnitCodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_unit_codegen_creation() {
        let codegen = UnitCodeGenerator::new();
        assert_eq!(codegen.exports.len(), 0);
        assert_eq!(codegen.imports.len(), 0);
    }
    
    #[test]
    fn test_generate_empty_unit() {
        let mut codegen = UnitCodeGenerator::new();
        
        let unit = Unit {
            name: "TestUnit".to_string(),
            uses: vec![],
            interface: UnitInterface {
                types: vec![],
                constants: vec![],
                variables: vec![],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                interfaces: vec![],
            },
            implementation: UnitImplementation {
                uses: vec![],
                types: vec![],
                constants: vec![],
                variables: vec![],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                interfaces: vec![],
                initialization: None,
                finalization: None,
            },
        };
        
        let result = codegen.generate_unit(&unit);
        assert!(result.is_ok());
        
        let code = result.unwrap();
        assert!(code.contains(".intel_syntax noprefix"));
        assert!(code.contains("# Interface section"));
        assert!(code.contains("# Implementation section"));
    }
}
