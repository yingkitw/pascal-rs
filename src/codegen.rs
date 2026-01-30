//! Basic code generator

use crate::ast::Program;

pub struct CodeGenerator;

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator
    }

    pub fn generate(&mut self, _program: &Program) -> Result<String, String> {
        // TODO: Implement actual code generation
        Ok("generated code".to_string())
    }
}
