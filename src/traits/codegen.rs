//! Code generation trait definitions

use crate::ast::{Expr, Program, Stmt, Type};
use anyhow::Result;

/// Core code generation capability
pub trait CodeGeneratorCapability {
    /// Generate code for a complete program
    fn generate(&mut self, program: &Program) -> Result<String>;

    /// Generate code for a statement
    fn generate_statement(&mut self, stmt: &Stmt) -> Result<String>;

    /// Generate code for an expression
    fn generate_expression(&mut self, expr: &Expr) -> Result<String>;
}

/// Assembly output generation trait
pub trait AssemblyOutput {
    /// Emit assembly code
    fn emit(&mut self, code: &str);

    /// Emit a label
    fn emit_label(&mut self, label: &str);

    /// Emit a comment
    fn emit_comment(&mut self, comment: &str);

    /// Get the generated output
    fn get_output(&self) -> &str;

    /// Clear the output buffer
    fn clear(&mut self);

    /// Emit formatted line
    fn emit_line(&mut self, indent: u8, code: &str);
}

/// Variable and scope management trait
pub trait VariableManager {
    /// Allocate a variable on the stack
    fn allocate_variable(&mut self, name: &str, typ: &Type) -> Result<i32>;

    /// Get stack offset for a variable
    fn get_variable_offset(&self, name: &str) -> Option<i32>;

    /// Enter a new scope
    fn enter_scope(&mut self, name: &str);

    /// Exit the current scope
    fn exit_scope(&mut self);

    /// Get current scope depth
    fn scope_depth(&self) -> usize;
}

/// Register allocation trait
pub trait RegisterAllocator {
    /// Allocate a register for a variable
    fn allocate_register(&mut self, var_name: &str) -> Result<String>;

    /// Free a register
    fn free_register(&mut self, register: &str);

    /// Get available registers
    fn get_available_registers(&self) -> Vec<String>;

    /// Spill a register to memory
    fn spill_register(&mut self, register: &str) -> Result<String>;

    /// Save register state
    fn save_register_state(&mut self);

    /// Restore register state
    fn restore_register_state(&mut self);
}

/// Target architecture trait
pub trait TargetArchitecture {
    /// Architecture name (e.g., "x86_64")
    fn architecture_name(&self) -> &str;

    /// Register size in bytes
    fn register_size(&self) -> usize;

    /// Stack alignment requirement
    fn stack_alignment(&self) -> usize;

    /// Calling convention (e.g., "System V AMD64 ABI")
    fn calling_convention(&self) -> &str;

    /// Get caller-saved registers
    fn caller_saved_registers(&self) -> Vec<String>;

    /// Get callee-saved registers
    fn callee_saved_registers(&self) -> Vec<String>;

    /// Get parameter registers
    fn parameter_registers(&self) -> Vec<String>;

    /// Get return register
    fn return_register(&self) -> &str;
}

/// Function calling trait
pub trait FunctionCalling {
    /// Generate function prologue
    fn generate_prologue(&mut self, name: &str, stack_size: i32) -> Result<()>;

    /// Generate function epilogue
    fn generate_epilogue(&mut self) -> Result<()>;

    /// Generate function call
    fn generate_call(&mut self, name: &str, args: &[Expr]) -> Result<()>;

    /// Generate return instruction
    fn generate_return(&mut self, value: Option<&Expr>) -> Result<()>;
}

/// Control flow generation trait
pub trait ControlFlowGenerator {
    /// Generate if-then-else structure
    fn generate_if_then_else(
        &mut self,
        condition: &Expr,
        then_block: &[Stmt],
        else_block: Option<&[Stmt]>,
    ) -> Result<()>;

    /// Generate while loop
    fn generate_while_loop(&mut self, condition: &Expr, body: &[Stmt]) -> Result<()>;

    /// Generate for loop
    fn generate_for_loop(
        &mut self,
        variable: &str,
        start: &Expr,
        end: &Expr,
        step: Option<&Expr>,
        body: &[Stmt],
        direction: crate::ast::ForDirection,
    ) -> Result<()>;

    /// Generate repeat-until loop
    fn generate_repeat_loop(&mut self, body: &[Stmt], condition: &Expr) -> Result<()>;
}

/// Debug information generation trait
pub trait DebugInfo {
    /// Generate debug information for program
    fn generate_debug_info(&mut self, program: &Program) -> Result<String>;

    /// Add debug symbol
    fn add_debug_symbol(&mut self, name: &str, address: usize);

    /// Generate source line mapping
    fn generate_line_mapping(&mut self, source: &str) -> Result<String>;

    /// Emit debug location directive
    fn emit_debug_location(&mut self, line: usize, column: usize);
}
