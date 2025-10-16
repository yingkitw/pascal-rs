use minipas_ast::Program;
use anyhow::Result;

/// Trait for code generation capabilities
pub trait CodeGeneratorCapability {
    /// Creates a new code generator instance
    fn new() -> Self where Self: Sized;
    
    /// Generates code for a complete program
    fn generate(&mut self, program: &Program) -> Result<String>;
    
    /// Generates code for a single statement
    fn generate_statement(&mut self, stmt: &minipas_ast::Stmt) -> Result<String>;
    
    /// Generates code for an expression
    fn generate_expression(&mut self, expr: &minipas_ast::Expr) -> Result<String>;
}

/// Trait for assembly output capabilities
pub trait AssemblyOutput {
    /// Emits assembly code
    fn emit(&mut self, code: &str);
    
    /// Emits a label
    fn emit_label(&mut self, label: &str);
    
    /// Emits a comment
    fn emit_comment(&mut self, comment: &str);
    
    /// Returns the generated assembly
    fn get_output(&self) -> &str;
    
    /// Clears the output buffer
    fn clear(&mut self);
}

/// Trait for variable management capabilities
pub trait VariableManager {
    /// Allocates a variable on the stack
    fn allocate_variable(&mut self, name: &str, typ: &minipas_ast::Type) -> Result<i32>;
    
    /// Gets the stack offset for a variable
    fn get_variable_offset(&self, name: &str) -> Option<i32>;
    
    /// Enters a new scope
    fn enter_scope(&mut self, name: &str);
    
    /// Exits the current scope
    fn exit_scope(&mut self);
}

/// Trait for register allocation capabilities
pub trait RegisterAllocator {
    /// Allocates a register for a variable
    fn allocate_register(&mut self, var_name: &str) -> Result<String>;
    
    /// Frees a register
    fn free_register(&mut self, register: &str);
    
    /// Gets available registers
    fn get_available_registers(&self) -> Vec<String>;
    
    /// Spills a register to memory
    fn spill_register(&mut self, register: &str) -> Result<String>;
}

/// Trait for optimization capabilities
pub trait Optimizer {
    /// Optimizes the generated assembly
    fn optimize(&mut self, assembly: &str) -> Result<String>;
    
    /// Removes dead code
    fn remove_dead_code(&mut self, assembly: &str) -> Result<String>;
    
    /// Optimizes register usage
    fn optimize_registers(&mut self, assembly: &str) -> Result<String>;
    
    /// Inlines simple functions
    fn inline_functions(&mut self, assembly: &str) -> Result<String>;
}

/// Trait for target architecture capabilities
pub trait TargetArchitecture {
    /// Returns the target architecture name
    fn architecture_name(&self) -> &str;
    
    /// Returns the register size in bytes
    fn register_size(&self) -> usize;
    
    /// Returns the stack alignment requirement
    fn stack_alignment(&self) -> usize;
    
    /// Returns the calling convention
    fn calling_convention(&self) -> &str;
}

/// Trait for debugging capabilities
pub trait DebugInfo {
    /// Generates debug information
    fn generate_debug_info(&mut self, program: &Program) -> Result<String>;
    
    /// Adds a debug symbol
    fn add_debug_symbol(&mut self, name: &str, address: usize);
    
    /// Generates source line mapping
    fn generate_line_mapping(&mut self, source: &str) -> Result<String>;
}
