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

    /// Stack frame size for current function
    stack_size: i32,

    /// Next available stack offset (for local variables)
    next_stack_offset: i32,

    /// Parameter offset tracking (starts at 16 for first param)
    param_offset: i32,
}

#[derive(Debug, Clone)]
struct VariableInfo {
    offset: i32,
    typ: Type,
    is_exported: bool,
    is_param: bool,
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
            stack_size: 0,
            next_stack_offset: 0,
            param_offset: 16,
        }
    }

    /// Reset local variable state for new function
    fn reset_local_state(&mut self) {
        self.variables.clear();
        self.stack_size = 0;
        self.next_stack_offset = 0;
        self.param_offset = 16;
    }

    /// Allocate space for a local variable on the stack
    fn allocate_local(&mut self, name: String, typ: Type) -> i32 {
        let size = self.get_type_size(&typ);
        self.next_stack_offset += size;
        self.stack_size = self.stack_size.max(self.next_stack_offset);

        let offset = self.next_stack_offset;
        self.variables.insert(name, VariableInfo {
            offset,
            typ,
            is_exported: false,
            is_param: false,
        });

        offset
    }

    /// Allocate space for a parameter (at fixed stack offset)
    fn allocate_param(&mut self, name: String, typ: Type) -> i32 {
        let offset = self.param_offset;
        self.param_offset += 8; // Parameters are 8-byte aligned

        self.variables.insert(name, VariableInfo {
            offset,
            typ,
            is_exported: false,
            is_param: true,
        });

        offset
    }

    /// Get the size of a type in bytes
    fn get_type_size(&self, typ: &Type) -> i32 {
        match typ {
            Type::Simple(simple) => match simple {
                SimpleType::Integer => 8,
                SimpleType::Real => 8,
                SimpleType::Boolean => 1,
                SimpleType::Char => 1,
                SimpleType::String => 256, // Default string size
            },
            Type::Integer => 8,
            Type::Real => 8,
            Type::Boolean => 1,
            Type::Char => 1,
            Type::String => 256,
            Type::Array { .. } => 64, // Simplified
            Type::Record { fields, .. } => fields.len() as i32 * 8,
            Type::Pointer(_) => 8,
            _ => 8,
        }
    }

    /// Align stack size to 16-byte boundary
    fn align_stack(&self, size: i32) -> i32 {
        ((size + 15) / 16) * 16
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

        // Reset local variable state
        self.reset_local_state();

        // Allocate parameters first
        for param in &func.parameters {
            self.allocate_param(param.name.clone(), param.param_type.clone());
        }

        // Allocate local variables
        for var in &func.block.vars {
            self.allocate_local(var.name.clone(), var.variable_type.clone());
        }

        writeln!(&mut self.output, "{}:", func.name)?;
        writeln!(&mut self.output, "    # Function prologue")?;
        writeln!(&mut self.output, "    push rbp")?;
        writeln!(&mut self.output, "    mov rbp, rsp")?;

        // Allocate stack space for local variables
        let aligned_size = self.align_stack(self.stack_size);
        if aligned_size > 0 {
            writeln!(&mut self.output, "    sub rsp, {}  # Allocate local variables", aligned_size)?;
        }

        writeln!(&mut self.output, "    # Function body")?;

        // Generate function body statements
        for stmt in &func.block.statements {
            self.generate_statement(stmt)?;
        }

        // Set default return value (if no explicit return)
        writeln!(&mut self.output, "    # Default return value")?;
        self.generate_default_return(&func.return_type)?;

        writeln!(&mut self.output, "    # Function epilogue")?;
        // Restore stack
        if aligned_size > 0 {
            writeln!(&mut self.output, "    add rsp, {}", aligned_size)?;
        }
        writeln!(&mut self.output, "    pop rbp")?;
        writeln!(&mut self.output, "    ret")?;
        writeln!(&mut self.output)?;

        Ok(())
    }

    /// Generate default return value for a function
    fn generate_default_return(&mut self, ret_type: &Type) -> Result<()> {
        match ret_type {
            Type::Simple(SimpleType::Integer) => {
                writeln!(&mut self.output, "    mov rax, 0  # Default integer return")?;
            }
            Type::Simple(SimpleType::Real) => {
                writeln!(&mut self.output, "    pxor xmm0, xmm0  # Default real return")?;
            }
            Type::Simple(SimpleType::Boolean) => {
                writeln!(&mut self.output, "    mov rax, 0  # Default boolean return (false)")?;
            }
            Type::Simple(SimpleType::Char) => {
                writeln!(&mut self.output, "    mov rax, 0  # Default char return (#0)")?;
            }
            _ => {
                writeln!(&mut self.output, "    xor rax, rax  # Default return")?;
            }
        }
        Ok(())
    }

    /// Generate code for a procedure
    fn generate_procedure(&mut self, proc: &ProcedureDecl) -> Result<()> {
        if let Some(ext_name) = &proc.external_name {
            writeln!(&mut self.output, ".extern {}", ext_name)?;
            return Ok(());
        }

        // Reset local variable state
        self.reset_local_state();

        // Allocate parameters first
        for param in &proc.parameters {
            self.allocate_param(param.name.clone(), param.param_type.clone());
        }

        // Allocate local variables
        for var in &proc.block.vars {
            self.allocate_local(var.name.clone(), var.variable_type.clone());
        }

        writeln!(&mut self.output, "{}:", proc.name)?;
        writeln!(&mut self.output, "    # Procedure prologue")?;
        writeln!(&mut self.output, "    push rbp")?;
        writeln!(&mut self.output, "    mov rbp, rsp")?;

        // Allocate stack space for local variables
        let aligned_size = self.align_stack(self.stack_size);
        if aligned_size > 0 {
            writeln!(&mut self.output, "    sub rsp, {}  # Allocate local variables", aligned_size)?;
        }

        writeln!(&mut self.output, "    # Procedure body")?;

        // Generate procedure body statements
        for stmt in &proc.block.statements {
            self.generate_statement(stmt)?;
        }

        writeln!(&mut self.output, "    # Procedure epilogue")?;
        // Restore stack
        if aligned_size > 0 {
            writeln!(&mut self.output, "    add rsp, {}", aligned_size)?;
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
                writeln!(&mut self.output, "    # Assignment to {}", target)?;
                self.generate_assignment(target, value)?;
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
            Stmt::Block(block) => {
                for stmt in &block.statements {
                    self.generate_statement(stmt)?;
                }
            }
            Stmt::ProcedureCall { name, arguments } => {
                self.generate_procedure_call(name, arguments)?;
            }
            Stmt::Repeat { body, until_condition } => {
                let label_start = self.new_label("repeat_start");
                writeln!(&mut self.output, "{}:", label_start)?;
                for stmt in body {
                    self.generate_statement(stmt)?;
                }
                self.generate_expression(until_condition)?;
                writeln!(&mut self.output, "    test rax, rax")?;
                writeln!(&mut self.output, "    jz {}", label_start)?;
            }
            Stmt::Empty => {}
            _ => {
                writeln!(&mut self.output, "    # Unsupported statement")?;
            }
        }
        Ok(())
    }

    /// Generate assignment statement
    fn generate_assignment(&mut self, target: &str, value: &Expr) -> Result<()> {
        // Check if target is a float variable
        let is_float_target = self
            .variables
            .get(target)
            .map(|info| matches!(info.typ, Type::Simple(SimpleType::Real)))
            .unwrap_or(false);

        // Generate value expression
        self.generate_expression(value)?;

        let offset = self.get_variable_offset(target);

        if is_float_target {
            // Store float from xmm0
            writeln!(
                &mut self.output,
                "    movq rax, xmm0  # Move float to rax for storage"
            )?;
            writeln!(
                &mut self.output,
                "    mov [rbp - {}], rax  # Store float {}",
                offset, target
            )?;
        } else {
            // Store integer from rax
            writeln!(
                &mut self.output,
                "    mov [rbp - {}], rax  # Store {}",
                offset, target
            )?;
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

                // Check if variable is a float
                let is_float = self
                    .variables
                    .get(name)
                    .map(|info| matches!(info.typ, Type::Simple(SimpleType::Real)))
                    .unwrap_or(false);

                if is_float {
                    // Load into xmm0 for float variables
                    writeln!(
                        &mut self.output,
                        "    mov rax, [rbp - {}]  # Load float {}",
                        offset, name
                    )?;
                    writeln!(&mut self.output, "    movq xmm0, rax  # Move to xmm0")?;
                } else {
                    // Load into rax for integer variables
                    writeln!(
                        &mut self.output,
                        "    mov rax, [rbp - {}]  # Load {}",
                        offset, name
                    )?;
                }
            }
            Expr::BinaryOp {
                operator,
                left,
                right,
            } => {
                self.generate_binary_op(operator, left, right)?;
            }
            Expr::UnaryOp { operator, operand } => {
                self.generate_unary_op(operator, operand)?;
            }
            Expr::FunctionCall { name, arguments } => {
                self.generate_function_call(name, arguments)?;
            }
            _ => {
                writeln!(&mut self.output, "    # Unsupported expression")?;
                writeln!(&mut self.output, "    xor rax, rax")?;
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
                self.generate_float_literal(*val)?;
            }
            Literal::String(val) => {
                writeln!(&mut self.output, "    # String literal: \"{}\"", val)?;
                self.generate_string_literal(val)?;
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
            Literal::WideString(val) => {
                writeln!(&mut self.output, "    # WideString literal: \"{}\"", val)?;
                writeln!(&mut self.output, "    xor rax, rax  # TODO: WideString support")?;
            }
            Literal::Set(_) => {
                writeln!(&mut self.output, "    xor rax, rax  # TODO: Set literal")?;
            }
        }
        Ok(())
    }

    /// Generate string literal
    fn generate_string_literal(&mut self, val: &str) -> Result<()> {
        let label = self.new_label("str");
        let len = val.len();
        let escaped_bytes: String = val
            .bytes()
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<_>>()
            .join(" ");

        // Emit string in data section
        writeln!(&mut self.output, ".section .rodata")?;
        writeln!(&mut self.output, "{}:", label)?;
        writeln!(&mut self.output, "    .byte {}  # String length", len)?;
        writeln!(&mut self.output, "    .byte {}  # String data", escaped_bytes)?;
        writeln!(&mut self.output, ".section .text")?;

        // Load string address into rax
        writeln!(&mut self.output, "    lea rax, [rip + {}]  # Load string address", label)?;
        Ok(())
    }

    /// Generate float literal using XMM registers
    fn generate_float_literal(&mut self, val: f64) -> Result<()> {
        // For float literals, we need to load them from memory
        // In a real implementation, this would use a constant pool
        writeln!(&mut self.output, "    movabs rax, {}  # Float bits (0x{:016x})", val.to_bits(), val.to_bits())?;
        writeln!(&mut self.output, "    movq xmm0, rax")?;
        Ok(())
    }

    /// Generate binary operation
    fn generate_binary_op(&mut self, op: &str, left: &Expr, right: &Expr) -> Result<()> {
        // Check for string concatenation
        if op == "+" && (self.is_string_expression(left) || self.is_string_expression(right)) {
            self.generate_string_concat(left, right)?;
            return Ok(());
        }

        // Try to determine if this is a float operation
        let is_float_op = self.is_float_expression(left) || self.is_float_expression(right);

        if is_float_op {
            self.generate_float_binary_op(op, left, right)?;
        } else {
            self.generate_int_binary_op(op, left, right)?;
        }

        Ok(())
    }

    /// Check if an expression produces a float value
    fn is_float_expression(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(Literal::Real(_)) => true,
            Expr::Variable(name) => {
                if let Some(info) = self.variables.get(name) {
                    matches!(info.typ, Type::Simple(SimpleType::Real)) || matches!(info.typ, Type::Real)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Generate integer binary operation
    fn generate_int_binary_op(&mut self, op: &str, left: &Expr, right: &Expr) -> Result<()> {
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

    /// Generate float binary operation using XMM registers
    fn generate_float_binary_op(&mut self, op: &str, left: &Expr, right: &Expr) -> Result<()> {
        // Evaluate left operand into xmm0
        self.generate_expression(left)?;
        writeln!(&mut self.output, "    movq rax, xmm0  # Move float result from xmm0")?;
        writeln!(&mut self.output, "    push rax")?;

        // Evaluate right operand into xmm0
        self.generate_expression(right)?;

        // Pop left operand into xmm1
        writeln!(&mut self.output, "    pop rax")?;
        writeln!(&mut self.output, "    movq xmm1, rax")?;

        // Perform operation
        match op {
            "+" => {
                writeln!(&mut self.output, "    addsd xmm0, xmm1  # Float add")?;
            }
            "-" => {
                writeln!(&mut self.output, "    subsd xmm1, xmm0  # Float sub")?;
                writeln!(&mut self.output, "    movq xmm0, xmm1")?;
            }
            "*" => {
                writeln!(&mut self.output, "    mulsd xmm0, xmm1  # Float mul")?;
            }
            "/" => {
                writeln!(&mut self.output, "    divsd xmm1, xmm0  # Float div")?;
                writeln!(&mut self.output, "    movq xmm0, xmm1")?;
            }
            "=" => {
                writeln!(&mut self.output, "    ucomisd xmm1, xmm0")?;
                writeln!(&mut self.output, "    setnp al")?;
                writeln!(&mut self.output, "    sete cl")?;
                writeln!(&mut self.output, "    and al, cl")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            "<>" => {
                writeln!(&mut self.output, "    ucomisd xmm1, xmm0")?;
                writeln!(&mut self.output, "    setp al")?;
                writeln!(&mut self.output, "    setne cl")?;
                writeln!(&mut self.output, "    or al, cl")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            "<" => {
                writeln!(&mut self.output, "    ucomisd xmm1, xmm0")?;
                writeln!(&mut self.output, "    seta al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            "<=" => {
                writeln!(&mut self.output, "    ucomisd xmm1, xmm0")?;
                writeln!(&mut self.output, "    setae al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            ">" => {
                writeln!(&mut self.output, "    ucomisd xmm0, xmm1")?;
                writeln!(&mut self.output, "    seta al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            ">=" => {
                writeln!(&mut self.output, "    ucomisd xmm0, xmm1")?;
                writeln!(&mut self.output, "    setae al")?;
                writeln!(&mut self.output, "    movzx rax, al")?;
            }
            _ => {
                writeln!(&mut self.output, "    # Unsupported float binary op: {}", op)?;
                writeln!(&mut self.output, "    pxor xmm0, xmm0")?;
            }
        }

        Ok(())
    }

    /// Check if an expression produces a string value
    fn is_string_expression(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(Literal::String(_)) => true,
            Expr::Variable(name) => {
                if let Some(info) = self.variables.get(name) {
                    matches!(info.typ, Type::Simple(SimpleType::String)) || matches!(info.typ, Type::String)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Generate string concatenation
    fn generate_string_concat(&mut self, left: &Expr, right: &Expr) -> Result<()> {
        writeln!(&mut self.output, "    # String concatenation")?;

        // For now, this is a simplified implementation
        // A real implementation would:
        // 1. Calculate lengths of both strings
        // 2. Allocate memory for the result
        // 3. Copy both strings to the result
        // 4. Return the result

        self.generate_expression(left)?;
        writeln!(&mut self.output, "    push rax  # Save left string address")?;

        self.generate_expression(right)?;
        writeln!(&mut self.output, "    mov rdx, rax  # Right string address")?;

        writeln!(&mut self.output, "    pop rcx  # Left string address")?;

        // Call runtime string concatenation function
        writeln!(&mut self.output, ".extern _pascal_string_concat")?;
        writeln!(&mut self.output, "    call _pascal_string_concat")?;

        // Result is in rax
        writeln!(&mut self.output, "    # String concatenation result in rax")?;

        Ok(())
    }

    /// Generate unary operation
    fn generate_unary_op(&mut self, op: &str, expr: &Expr) -> Result<()> {
        // Check if this is a float operation
        let is_float = self.is_float_expression(expr);

        self.generate_expression(expr)?;

        match op {
            "-" => {
                if is_float {
                    // Float negation using SSE
                    writeln!(&mut self.output, "    mov rax, 0x8000000000000000  # Sign bit")?;
                    writeln!(&mut self.output, "    movq xmm1, rax")?;
                    writeln!(&mut self.output, "    xorpd xmm0, xmm1  # Negate float")?;
                } else {
                    writeln!(&mut self.output, "    neg rax")?;
                }
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
