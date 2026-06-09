//! Function handling for Pascal interpreter

use crate::ast::{Block, FunctionDecl, ProcedureDecl};
use anyhow::Result;
use std::collections::HashMap;

/// User-defined function/procedure
#[derive(Debug, Clone)]
pub struct UserFunction {
    params: Vec<(String, bool)>, // (name, is_var)
    body: Block,
    is_function: bool,
    return_type_name: String,
}

impl UserFunction {
    /// Create a new user function
    pub fn new(
        params: Vec<(String, bool)>,
        body: Block,
        is_function: bool,
        return_type_name: String,
    ) -> Self {
        Self {
            params,
            body,
            is_function,
            return_type_name,
        }
    }

    /// Get function parameters
    pub fn params(&self) -> &[(String, bool)] {
        &self.params
    }

    /// Get function body
    pub fn body(&self) -> &Block {
        &self.body
    }

    /// Check if this is a function (vs procedure)
    pub fn is_function(&self) -> bool {
        self.is_function
    }

    /// Get return type name
    pub fn return_type_name(&self) -> &str {
        &self.return_type_name
    }

    /// Validate function parameters
    pub fn validate_params(&self, expected_count: usize) -> Result<()> {
        if self.params.len() != expected_count {
            return Err(anyhow::anyhow!(
                "Expected {} parameters, got {}",
                expected_count,
                self.params.len()
            ));
        }
        Ok(())
    }
}

/// Function registry for user-defined functions and procedures
pub struct FunctionRegistry {
    functions: HashMap<String, UserFunction>,
}

impl FunctionRegistry {
    /// Create a new function registry
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    /// Register a user-defined function
    pub fn register_function(&mut self, name: String, function: UserFunction) -> Result<()> {
        if self.functions.contains_key(&name) {
            return Err(anyhow::anyhow!("Function '{}' already defined", name));
        }
        
        self.functions.insert(name, function);
        Ok(())
    }

    /// Get a user-defined function
    pub fn get_function(&self, name: &str) -> Option<&UserFunction> {
        self.functions.get(name)
    }

    /// Check if a function exists
    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Remove a function
    pub fn remove_function(&mut self, name: &str) -> Option<UserFunction> {
        self.functions.remove(name)
    }

    /// Get all function names
    pub fn function_names(&self) -> Vec<String> {
        self.functions.keys().cloned().collect()
    }

    /// Clear all functions
    pub fn clear(&mut self) {
        self.functions.clear();
    }
}

/// Helper to convert AST function declarations to user functions
pub struct FunctionConverter;

impl FunctionConverter {
    /// Convert AST function declaration to user function
    pub fn from_function_decl(decl: &FunctionDecl) -> UserFunction {
        UserFunction::new(
            decl.parameters.iter().map(|p| (p.name.clone(), p.is_var)).collect(),
            decl.block.clone(),
            true,
            format!("{}", decl.return_type),
        )
    }

    /// Convert AST procedure declaration to user function
    pub fn from_procedure_decl(decl: &ProcedureDecl) -> UserFunction {
        UserFunction::new(
            decl.parameters.iter().map(|p| (p.name.clone(), p.is_var)).collect(),
            decl.block.clone(),
            false,
            "void".to_string(),
        )
    }

    /// Register all functions and procedures from a block
    pub fn register_from_block(
        registry: &mut FunctionRegistry,
        functions: &[FunctionDecl],
        procedures: &[ProcedureDecl],
    ) -> Result<()> {
        for func in functions {
            let user_func = Self::from_function_decl(func);
            registry.register_function(func.name.clone(), user_func)?;
        }

        for proc in procedures {
            let user_func = Self::from_procedure_decl(proc);
            registry.register_function(proc.name.clone(), user_func)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FunctionDecl, Parameter, SimpleType, Type, ProcedureDecl, Statement, Expression, Expr, FieldVisibility};

    #[test]
    fn test_user_function_creation() {
        let block = Block { 
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![],
            classes: vec![],
            statements: Vec::new() 
        };
        let params = vec![
            ("x".to_string(), false),
            ("y".to_string(), true),
        ];
        
        let func = UserFunction::new(
            params.clone(),
            block,
            true,
            "integer".to_string(),
        );

        assert_eq!(func.params(), &params);
        assert!(func.is_function());
        assert_eq!(func.return_type_name(), "integer");
    }

    #[test]
    fn test_function_registry() {
        let mut registry = FunctionRegistry::new();
        
        let block = Block { 
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![],
            classes: vec![],
            statements: Vec::new() 
        };
        let function = UserFunction::new(
            vec![("x".to_string(), false)],
            block,
            true,
            "integer".to_string(),
        );

        assert!(registry.register_function("test_func".to_string(), function).is_ok());
        assert!(registry.has_function("test_func"));
        
        let retrieved = registry.get_function("test_func");
        assert!(retrieved.is_some());
    }

    #[test]
    fn test_function_converter() {
        let params = vec![Parameter {
            name: "x".to_string(),
            param_type: Type::Simple(SimpleType::Integer),
            is_var: false,
            is_const: false,
            is_out: false,
            default_value: None,
        }];
        
        let block = Block { 
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![],
            classes: vec![],
            statements: Vec::new() 
        };
        let ast_func = FunctionDecl {
            name: "test_func".to_string(),
            parameters: params,
            return_type: Type::Simple(SimpleType::Integer),
            block: block,
            visibility: FieldVisibility::Public,
            is_external: false,
            external_name: None,
            is_inline: false,
            is_forward: false,
            is_class_method: false,
            is_virtual: false,
            is_override: false,
            is_overload: false,
        };

        let user_func = FunctionConverter::from_function_decl(&ast_func);
        assert_eq!(user_func.return_type_name(), "integer");
    }
}