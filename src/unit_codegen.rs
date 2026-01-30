//! Unit-aware code generation
//!
//! This module provides code generation for Pascal units with support for:
//! - Interface/implementation sections
//! - Cross-module symbol resolution
//! - Separate compilation
//! - Module linking

use crate::ast::*;
use anyhow::Result;
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
        if let Some(ext_name) = &func.external_name {
            writeln!(&mut self.output, ".extern {}", ext_name)?;
            return Ok(());
        }

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
        if let Some(ext_name) = &proc.external_name {
            writeln!(&mut self.output, ".extern {}", ext_name)?;
            return Ok(());
        }

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

    /// Generate code for a statement
    fn generate_statement(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Assignment { target, value } => {
                // For now, assume target is a simple variable
                writeln!(&mut self.output, "    # Assignment")?;
                self.generate_expression(value)?;
                writeln!(
                    &mut self.output,
                    "    # Store result (target handling TODO)"
                )?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.generate_if(condition, then_branch, else_branch.as_ref())?;
            }
            Stmt::While { condition, body } => {
                self.generate_while(condition, body)?;
            }
            Stmt::For {
                var_name,
                start,
                end,
                body,
                ..
            } => {
                self.generate_for(var_name, start, end, body)?;
            }
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    self.generate_statement(stmt)?;
                }
            }
            Stmt::ProcedureCall { name, arguments } => {
                self.generate_procedure_call(name, arguments)?;
            }
        }
        Ok(())
    }

    /// Generate if statement
    fn generate_if(
        &mut self,
        condition: &Expr,
        then_branch: &Vec<Stmt>,
        else_branch: Option<&Vec<Stmt>>,
    ) -> Result<()> {
        let label_else = self.new_label("else");
        let label_end = self.new_label("endif");

        writeln!(&mut self.output, "    # If statement")?;

        // Evaluate condition
        self.generate_expression(condition)?;
        writeln!(&mut self.output, "    test rax, rax")?;

        if else_branch.is_some() {
            writeln!(&mut self.output, "    jz {}", label_else)?;
        } else {
            writeln!(&mut self.output, "    jz {}", label_end)?;
        }

        // Then branch
        for stmt in then_branch {
            self.generate_statement(stmt)?;
        }

        if let Some(else_stmts) = else_branch {
            writeln!(&mut self.output, "    jmp {}", label_end)?;
            writeln!(&mut self.output, "{}:", label_else)?;

            // Else branch
            for stmt in else_stmts {
                self.generate_statement(stmt)?;
            }
        }

        writeln!(&mut self.output, "{}:", label_end)?;
        Ok(())
    }

    /// Generate while loop
    fn generate_while(&mut self, condition: &Expr, body: &Vec<Stmt>) -> Result<()> {
        let label_start = self.new_label("while_start");
        let label_end = self.new_label("while_end");

        writeln!(&mut self.output, "    # While loop")?;
        writeln!(&mut self.output, "{}:", label_start)?;

        // Evaluate condition
        self.generate_expression(condition)?;
        writeln!(&mut self.output, "    test rax, rax")?;
        writeln!(&mut self.output, "    jz {}", label_end)?;

        // Loop body
        for stmt in body {
            self.generate_statement(stmt)?;
        }

        writeln!(&mut self.output, "    jmp {}", label_start)?;
        writeln!(&mut self.output, "{}:", label_end)?;

        Ok(())
    }

    /// Generate for loop
    fn generate_for(
        &mut self,
        variable: &str,
        start: &Expr,
        end: &Expr,
        body: &Vec<Stmt>,
    ) -> Result<()> {
        let label_start = self.new_label("for_start");
        let label_end = self.new_label("for_end");

        writeln!(&mut self.output, "    # For loop")?;

        // Initialize loop variable
        self.generate_expression(start)?;
        let offset = self.get_variable_offset(variable);
        writeln!(
            &mut self.output,
            "    mov [rbp - {}], rax  # Initialize {}",
            offset, variable
        )?;

        writeln!(&mut self.output, "{}:", label_start)?;

        // Check condition: variable <= end
        let offset2 = self.get_variable_offset(variable);
        writeln!(
            &mut self.output,
            "    mov rax, [rbp - {}]  # Load {}",
            offset2, variable
        )?;
        writeln!(&mut self.output, "    push rax")?;

        self.generate_expression(end)?;
        writeln!(&mut self.output, "    pop rdx")?;
        writeln!(&mut self.output, "    cmp rdx, rax")?;
        writeln!(&mut self.output, "    jg {}", label_end)?;

        // Loop body
        for stmt in body {
            self.generate_statement(stmt)?;
        }

        // Increment loop variable
        let offset3 = self.get_variable_offset(variable);
        writeln!(&mut self.output, "    mov rax, [rbp - {}]", offset3)?;
        writeln!(&mut self.output, "    inc rax")?;
        writeln!(&mut self.output, "    mov [rbp - {}], rax", offset3)?;

        writeln!(&mut self.output, "    jmp {}", label_start)?;
        writeln!(&mut self.output, "{}:", label_end)?;

        Ok(())
    }

    /// Generate procedure call
    fn generate_procedure_call(&mut self, name: &str, args: &[Expr]) -> Result<()> {
        writeln!(&mut self.output, "    # Call procedure: {}", name)?;

        // For simplicity, assume if name contains '.', resolve to external
        // But for now, add comment or basic support
        // TODO: Proper symbol resolution for external calls
        let is_external = name.contains('.');
        let call_name = if is_external {
            writeln!(&mut self.output, ".extern {}", name)?;
            name
        } else {
            name
        };

        // Push arguments in reverse order (right to left)
        for arg in args.iter().rev() {
            self.generate_expression(arg)?;
            writeln!(&mut self.output, "    push rax")?;
        }

        // Call the procedure
        writeln!(&mut self.output, "    call {}", call_name)?;

        // Clean up stack (caller cleanup)
        if !args.is_empty() {
            writeln!(&mut self.output, "    add rsp, {}", args.len() * 8)?;
        }

        Ok(())
    }

    /// Generate expression code
    fn generate_expression(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Literal(lit) => {
                self.generate_literal(lit)?;
            }
            Expr::Variable(name) => {
                let offset = self.get_variable_offset(name);
                writeln!(
                    &mut self.output,
                    "    mov rax, [rbp - {}]  # Load {}",
                    offset, name
                )?;
            }
            Expr::BinaryOp { operator, left, right } => {
                self.generate_binary_op(&operator, left, right)?;
            }
            Expr::UnaryOp { operator, operand } => {
                self.generate_unary_op(&operator, operand)?;
            }
            Expr::FunctionCall { name, arguments } => {
                self.generate_function_call(name, arguments)?;
            }
        }
        Ok(())
    }

    /// Generate literal value
    fn generate_literal(&mut self, lit: &Literal) -> Result<()> {
        match lit {
            Literal::Integer(val) => {
                writeln!(&mut self.output, "    mov rax, {}", val)?;
            }
            Literal::Real(val) => {
                writeln!(&mut self.output, "    # Real literal: {}", val)?;
                writeln!(&mut self.output, "    xor rax, rax  # TODO: Float support")?;
            }
            Literal::String(val) => {
                writeln!(&mut self.output, "    # String literal: \"{}\"", val)?;
                writeln!(&mut self.output, "    xor rax, rax  # TODO: String support")?;
            }
            Literal::Boolean(val) => {
                writeln!(
                    &mut self.output,
                    "    mov rax, {}",
                    if *val { 1 } else { 0 }
                )?;
            }
            Literal::Char(val) => {
                writeln!(&mut self.output, "    mov rax, {}", *val as i32)?;
            }
            Literal::Nil => {
                writeln!(&mut self.output, "    xor rax, rax  # nil")?;
            }
        }
        Ok(())
    }

    /// Generate binary operation
    fn generate_binary_op(
        &mut self,
        op: &str,
        left: &Box<Expr>,
        right: &Box<Expr>,
    ) -> Result<()> {
        // Evaluate left operand
        self.generate_expression(left)?;
        writeln!(&mut self.output, "    push rax")?;

        // Evaluate right operand
        self.generate_expression(right)?;

        // Pop left operand into rdx
        writeln!(&mut self.output, "    pop rdx")?;

        // Perform operation
        match op {
            "+" => {
                writeln!(&mut self.output, "    add rax, rdx")?;
            }
            "-" => {
                writeln!(&mut self.output, "    sub rdx, rax")?;
                writeln!(&mut self.output, "    mov rax, rdx")?;
            }
            "*" => {
                writeln!(&mut self.output, "    imul rax, rdx")?;
            }
            "/" => {
                writeln!(&mut self.output, "    mov rax, rdx")?;
                writeln!(&mut self.output, "    cqo")?;
                writeln!(&mut self.output, "    idiv rdx")?;
            }
            "=" => {
                writeln!(&mut self.output, "    cmp rdx, rax")?;
                writeln!(&mut self.output, "    sete al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            "<>" => {
                writeln!(&mut self.output, "    cmp rdx, rax")?;
                writeln!(&mut self.output, "    setne al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            "<" => {
                writeln!(&mut self.output, "    cmp rdx, rax")?;
                writeln!(&mut self.output, "    setl al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            "<=" => {
                writeln!(&mut self.output, "    cmp rdx, rax")?;
                writeln!(&mut self.output, "    setle al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            ">" => {
                writeln!(&mut self.output, "    cmp rdx, rax")?;
                writeln!(&mut self.output, "    setg al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            ">=" => {
                writeln!(&mut self.output, "    cmp rdx, rax")?;
                writeln!(&mut self.output, "    setge al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            "and" => {
                writeln!(&mut self.output, "    and rax, rdx")?;
            }
            "or" => {
                writeln!(&mut self.output, "    or rax, rdx")?;
            }
            _ => {
                writeln!(&mut self.output, "    # Unsupported binary op: {}", op)?;
                writeln!(&mut self.output, "    xor rax, rax")?;
            }
        }

        Ok(())
    }

    /// Generate unary operation
    fn generate_unary_op(&mut self, op: &str, expr: &Box<Expr>) -> Result<()> {
        self.generate_expression(expr)?;

        match op {
            "-" => {
                writeln!(&mut self.output, "    neg rax")?;
            }
            "not" => {
                writeln!(&mut self.output, "    test rax, rax")?;
                writeln!(&mut self.output, "    setz al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            "+" => {
                // No operation needed
            }
            _ => {
                writeln!(&mut self.output, "    # Unsupported unary op: {}", op)?;
            }
        }

        Ok(())
    }

    /// Generate function call
    fn generate_function_call(&mut self, name: &str, args: &[Expr]) -> Result<()> {
        writeln!(&mut self.output, "    # Call function: {}", name)?;

        // For simplicity, assume if name contains '.', resolve to external
        // But for now, add comment or basic support
        // TODO: Proper symbol resolution for external calls
        let is_external = name.contains('.');
        let call_name = if is_external {
            writeln!(&mut self.output, ".extern {}", name)?;
            name
        } else {
            name
        };

        // Push arguments in reverse order
        for arg in args.iter().rev() {
            self.generate_expression(arg)?;
            writeln!(&mut self.output, "    push rax")?;
        }

        // Call the function
        writeln!(&mut self.output, "    call {}", call_name)?;

        // Clean up stack
        if !args.is_empty() {
            writeln!(&mut self.output, "    add rsp, {}", args.len() * 8)?;
        }

        // Result is in rax
        Ok(())
    }

    /// Get variable offset (simplified - uses fixed offsets)
    fn get_variable_offset(&self, name: &str) -> i32 {
        // Simple hash-based offset for now
        let hash = name.bytes().fold(0u32, |acc, b| acc.wrapping_add(b as u32));
        8 + ((hash % 100) * 8) as i32
    }

    /// Generate a new unique label
    fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        format!(".L{}_{}", prefix, self.label_counter)
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
                uses: vec![],
                types: HashMap::new(),
                constants: HashMap::new(),
                variables: HashMap::new(),
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
