//! User-defined function and procedure support

use crate::ast::Block;
use crate::interpreter_traits::FunctionRegistry;
use std::collections::HashMap;

/// User-defined function/procedure
#[derive(Debug, Clone)]
pub struct UserFunction {
    /// Parameters: (name, is_var)
    pub params: Vec<(String, bool)>,
    /// Function body
    pub body: Block,
    /// Is this a function (vs procedure)?
    pub is_function: bool,
    /// Return type name
    pub return_type_name: String,
}

impl UserFunction {
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

    pub fn param_count(&self) -> usize {
        self.params.len()
    }

    pub fn is_var_param(&self, index: usize) -> bool {
        self.params.get(index).map(|(_, is_var)| *is_var).unwrap_or(false)
    }
}

/// Function registry for storing and retrieving user-defined functions
///
/// Implements the FunctionRegistry trait for better abstraction and testability.
#[derive(Debug, Clone)]
pub struct FunctionRegistryImpl {
    functions: HashMap<String, UserFunction>,
}

impl FunctionRegistryImpl {
    /// Create a new empty function registry
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    /// Get all function names
    pub fn function_names(&self) -> Vec<String> {
        self.functions.keys().cloned().collect()
    }

    /// Register multiple functions at once
    pub fn register_functions(&mut self, funcs: Vec<(String, UserFunction)>) {
        for (name, func) in funcs {
            self.functions.insert(name, func);
        }
    }

    /// Remove a function from the registry
    pub fn unregister(&mut self, name: &str) -> Option<UserFunction> {
        self.functions.remove(name)
    }

    /// Get the number of registered functions
    pub fn len(&self) -> usize {
        self.functions.len()
    }

    /// Check if the registry is empty
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }

    /// Clear all functions from the registry
    pub fn clear(&mut self) {
        self.functions.clear();
    }
}

impl Default for FunctionRegistryImpl {
    fn default() -> Self {
        Self::new()
    }
}

// Implement the FunctionRegistry trait
impl FunctionRegistry for FunctionRegistryImpl {
    fn get_function(&self, name: &str) -> Option<&UserFunction> {
        self.functions.get(name)
    }

    fn register_function(&mut self, name: String, func: UserFunction) {
        self.functions.insert(name, func);
    }

    fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}

// Also implement FunctionRegistry for HashMap<String, UserFunction> directly
impl FunctionRegistry for HashMap<String, UserFunction> {
    fn get_function(&self, name: &str) -> Option<&UserFunction> {
        self.get(name)
    }

    fn register_function(&mut self, name: String, func: UserFunction) {
        self.insert(name, func);
    }

    fn has_function(&self, name: &str) -> bool {
        self.contains_key(name)
    }
}
