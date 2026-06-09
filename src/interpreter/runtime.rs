//! Runtime environment for Pascal interpreter

use crate::ast::Block;
use crate::interpreter::{value::Scope, value::Value};
use anyhow::Result;

/// Runtime environment that manages execution state
pub struct RuntimeEnvironment {
    scopes: Vec<Scope>,
    current_line: usize,
    current_column: usize,
    verbose: bool,
}

impl RuntimeEnvironment {
    /// Create a new runtime environment
    pub fn new(verbose: bool) -> Self {
        let mut global = Scope::new();
        // Pre-define some common constants
        global.insert("maxint".to_string(), Value::Integer(i64::MAX));
        global.insert("true".to_string(), Value::Boolean(true));
        global.insert("false".to_string(), Value::Boolean(false));

        Self {
            scopes: vec![global],
            current_line: 1,
            current_column: 1,
            verbose,
        }
    }

    /// Get the current (innermost) scope
    pub fn current_scope(&self) -> &Scope {
        self.scopes
            .last()
            .expect("Scope stack should never be empty")
    }

    /// Get a variable value for testing purposes
    pub fn get_variable_value(&self, name: &str) -> Option<Value> {
        let name_lower = name.to_lowercase();
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(&name_lower) {
                return Some(val.clone());
            }
        }
        None
    }

    /// Enter a new scope (e.g., for block statements)
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Declare a single variable with a given initial value
    pub fn declare_variable(&mut self, name: &str, value: Value) -> Result<()> {
        let name_lower = name.to_lowercase();
        let scope = self.scopes.last_mut()
            .ok_or_else(|| anyhow::anyhow!("No scope available for variable declaration"))?;
        if scope.contains_key(&name_lower) {
            return Err(anyhow::anyhow!("Variable '{}' already declared", name));
        }
        scope.insert(name_lower, value);
        Ok(())
    }

    /// Declare variables in the current scope (legacy, initializes to Nil)
    pub fn declare_variables(&mut self, variables: &[&str]) -> Result<()> {
        for var_name in variables {
            self.declare_variable(var_name, Value::Nil)?;
        }
        Ok(())
    }

    /// Update current source location (for debugging)
    pub fn set_location(&mut self, line: usize, column: usize) {
        self.current_line = line;
        self.current_column = column;
    }

    /// Get current location
    pub fn get_location(&self) -> (usize, usize) {
        (self.current_line, self.current_column)
    }

    /// Enable/disable verbose logging
    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    /// Check if verbose mode is enabled
    pub fn is_verbose(&self) -> bool {
        self.verbose
    }

    /// Set a variable value in the current scope
    pub fn set_variable(&mut self, name: String, value: Value) {
        let name_lower = name.to_lowercase();
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name_lower, value);
        }
    }

    /// Set a nested element in an array or string variable by name and index path
    pub fn set_nested_element(&mut self, name: &str, indices: &[i64], value: Value) -> Result<()> {
        let name_lower = name.to_lowercase();
        for scope in self.scopes.iter_mut().rev() {
            if let Some(existing) = scope.get_mut(&name_lower) {
                Self::set_element_at(existing, indices, value)?;
                return Ok(());
            }
        }
        Err(anyhow::anyhow!("Undefined variable: {}", name))
    }

    fn set_element_at(val: &mut Value, indices: &[i64], value: Value) -> Result<()> {
        if indices.is_empty() {
            *val = value;
            return Ok(());
        }
        match val {
            Value::Array { elements, lower_bound } => {
                let adjusted = (indices[0] - *lower_bound) as usize;
                if adjusted >= elements.len() {
                    return Err(anyhow::anyhow!(
                        "Array index out of bounds: index {} (bounds: {}..{})",
                        indices[0], *lower_bound, *lower_bound + elements.len() as i64 - 1
                    ));
                }
                Self::set_element_at(&mut elements[adjusted], &indices[1..], value)
            }
            Value::String(s) => {
                if indices.len() > 1 {
                    return Err(anyhow::anyhow!("Strings only support single index"));
                }
                if let Value::Char(c) = value {
                    let idx = (indices[0] - 1) as usize; // Pascal strings are 1-indexed
                    if idx >= s.len() {
                        return Err(anyhow::anyhow!(
                            "String index out of bounds: {} (length: {})", indices[0], s.len()
                        ));
                    }
                    s.replace_range(idx..idx + 1, &c.to_string());
                    Ok(())
                } else {
                    Err(anyhow::anyhow!("Can only assign char to string index, got {:?}", value))
                }
            }
            _ => Err(anyhow::anyhow!("Cannot index into value of type {:?}", val)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_management() {
        let mut runtime = RuntimeEnvironment::new(false);
        
        // Test initial global scope
        assert_eq!(runtime.scopes.len(), 1);
        
        // Test entering scope
        runtime.enter_scope();
        assert_eq!(runtime.scopes.len(), 2);
        
        // Test exiting scope
        runtime.exit_scope();
        assert_eq!(runtime.scopes.len(), 1);
    }

    #[test]
    fn test_variable_declaration() {
        let mut runtime = RuntimeEnvironment::new(false);
        
        let vars = ["x", "y", "z"];
        runtime.declare_variables(&vars).unwrap();
        
        assert!(runtime.get_variable_value("x").is_some());
        assert!(runtime.get_variable_value("y").is_some());
        assert!(runtime.get_variable_value("z").is_some());
    }
}