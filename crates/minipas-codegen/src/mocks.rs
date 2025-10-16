use super::traits::*;
use minipas_ast::*;
use anyhow::Result;
use std::collections::HashMap;

/// Mock code generator for testing purposes
pub struct MockCodeGenerator {
    output: String,
    variables: HashMap<String, i32>,
    current_scope: Vec<String>,
    label_counter: u32,
}

impl MockCodeGenerator {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            variables: HashMap::new(),
            current_scope: Vec::new(),
            label_counter: 0,
        }
    }
    
    pub fn with_output(output: &str) -> Self {
        Self {
            output: output.to_string(),
            variables: HashMap::new(),
            current_scope: Vec::new(),
            label_counter: 0,
        }
    }
}

impl CodeGeneratorCapability for MockCodeGenerator {
    fn new() -> Self {
        MockCodeGenerator::new()
    }
    
    fn generate(&mut self, program: &Program) -> Result<String> {
        self.output.clear();
        self.output.push_str(&format!("; Generated code for program: {}\n", program.name));
        self.output.push_str("; Mock implementation\n");
        Ok(self.output.clone())
    }
    
    fn generate_statement(&mut self, stmt: &Stmt) -> Result<String> {
        self.output.clear();
        self.output.push_str(&format!("; Mock statement: {:?}\n", stmt));
        Ok(self.output.clone())
    }
    
    fn generate_expression(&mut self, expr: &Expr) -> Result<String> {
        self.output.clear();
        self.output.push_str(&format!("; Mock expression: {:?}\n", expr));
        Ok(self.output.clone())
    }
}

impl AssemblyOutput for MockCodeGenerator {
    fn emit(&mut self, code: &str) {
        self.output.push_str(code);
        self.output.push('\n');
    }
    
    fn emit_label(&mut self, label: &str) {
        self.output.push_str(&format!("{}:\n", label));
    }
    
    fn emit_comment(&mut self, comment: &str) {
        self.output.push_str(&format!("; {}\n", comment));
    }
    
    fn get_output(&self) -> &str {
        &self.output
    }
    
    fn clear(&mut self) {
        self.output.clear();
    }
}

impl VariableManager for MockCodeGenerator {
    fn allocate_variable(&mut self, name: &str, _typ: &Type) -> Result<i32> {
        let offset = -(self.variables.len() as i32 + 1) * 8; // Mock stack allocation
        self.variables.insert(name.to_string(), offset);
        Ok(offset)
    }
    
    fn get_variable_offset(&self, name: &str) -> Option<i32> {
        self.variables.get(name).copied()
    }
    
    fn enter_scope(&mut self, name: &str) {
        self.current_scope.push(name.to_string());
    }
    
    fn exit_scope(&mut self) {
        self.current_scope.pop();
    }
}

impl RegisterAllocator for MockCodeGenerator {
    fn allocate_register(&mut self, _var_name: &str) -> Result<String> {
        // Mock register allocation
        let register = format!("r{}", self.label_counter % 16);
        self.label_counter += 1;
        Ok(register)
    }
    
    fn free_register(&mut self, _register: &str) {
        // Mock register freeing - no-op
    }
    
    fn get_available_registers(&self) -> Vec<String> {
        vec!["rax".to_string(), "rbx".to_string(), "rcx".to_string(), "rdx".to_string()]
    }
    
    fn spill_register(&mut self, register: &str) -> Result<String> {
        Ok(format!("; Spilled {}", register))
    }
}

impl Optimizer for MockCodeGenerator {
    fn optimize(&mut self, assembly: &str) -> Result<String> {
        // Mock optimization - just add a comment
        Ok(format!("; Optimized:\n{}", assembly))
    }
    
    fn remove_dead_code(&mut self, assembly: &str) -> Result<String> {
        // Mock dead code removal
        Ok(format!("; Dead code removed:\n{}", assembly))
    }
    
    fn optimize_registers(&mut self, assembly: &str) -> Result<String> {
        // Mock register optimization
        Ok(format!("; Registers optimized:\n{}", assembly))
    }
    
    fn inline_functions(&mut self, assembly: &str) -> Result<String> {
        // Mock function inlining
        Ok(format!("; Functions inlined:\n{}", assembly))
    }
}

impl TargetArchitecture for MockCodeGenerator {
    fn architecture_name(&self) -> &str {
        "x86-64-mock"
    }
    
    fn register_size(&self) -> usize {
        8
    }
    
    fn stack_alignment(&self) -> usize {
        16
    }
    
    fn calling_convention(&self) -> &str {
        "System V AMD64 ABI (Mock)"
    }
}

impl DebugInfo for MockCodeGenerator {
    fn generate_debug_info(&mut self, program: &Program) -> Result<String> {
        Ok(format!("; Debug info for program: {}\n", program.name))
    }
    
    fn add_debug_symbol(&mut self, name: &str, address: usize) {
        self.output.push_str(&format!("; Debug symbol: {} at 0x{:x}\n", name, address));
    }
    
    fn generate_line_mapping(&mut self, source: &str) -> Result<String> {
        Ok(format!("; Line mapping for {} lines\n", source.lines().count()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use minipas_ast::*;
    
    #[test]
    fn test_mock_codegen_basic() {
        let mut codegen = MockCodeGenerator::new();
        let program = Program {
            name: "test".to_string(),
            block: Block {
                declarations: vec![],
                statements: vec![],
            },
        };
        
        let result = codegen.generate(&program).unwrap();
        assert!(result.contains("Generated code for program: test"));
        assert!(result.contains("Mock implementation"));
    }
    
    #[test]
    fn test_mock_assembly_output() {
        let mut codegen = MockCodeGenerator::new();
        
        codegen.emit("mov eax, 42");
        codegen.emit_label("start");
        codegen.emit_comment("Test comment");
        
        let output = codegen.get_output();
        assert!(output.contains("mov eax, 42"));
        assert!(output.contains("start:"));
        assert!(output.contains("Test comment"));
    }
    
    #[test]
    fn test_mock_variable_management() {
        let mut codegen = MockCodeGenerator::new();
        
        let offset = codegen.allocate_variable("x", &Type::Integer).unwrap();
        assert_eq!(offset, -8);
        
        let retrieved_offset = codegen.get_variable_offset("x").unwrap();
        assert_eq!(retrieved_offset, -8);
        
        assert!(codegen.get_variable_offset("y").is_none());
    }
    
    #[test]
    fn test_mock_register_allocation() {
        let mut codegen = MockCodeGenerator::new();
        
        let reg1 = codegen.allocate_register("x").unwrap();
        let reg2 = codegen.allocate_register("y").unwrap();
        
        assert_ne!(reg1, reg2);
        assert!(reg1.starts_with("r"));
        
        let available = codegen.get_available_registers();
        assert_eq!(available.len(), 4);
        assert!(available.contains(&"rax".to_string()));
    }
    
    #[test]
    fn test_mock_optimization() {
        let mut codegen = MockCodeGenerator::new();
        
        let original = "mov eax, 42\nadd eax, 1";
        let optimized = codegen.optimize(original).unwrap();
        
        assert!(optimized.contains("Optimized:"));
        assert!(optimized.contains(original));
    }
    
    #[test]
    fn test_mock_target_architecture() {
        let codegen = MockCodeGenerator::new();
        
        assert_eq!(codegen.architecture_name(), "x86-64-mock");
        assert_eq!(codegen.register_size(), 8);
        assert_eq!(codegen.stack_alignment(), 16);
        assert!(codegen.calling_convention().contains("System V"));
    }
}
