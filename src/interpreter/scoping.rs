//! Scope management for Pascal interpreter

use crate::interpreter::value::{Scope, Value};
use anyhow::Result;
use std::collections::HashMap;

/// Stack frame for function/procedure calls
#[derive(Debug, Clone)]
pub struct StackFrame {
    name: String,
    parent_scope_index: Option<usize>,
    variables: HashMap<String, Value>,
}

impl StackFrame {
    /// Create a new stack frame
    pub fn new(name: String, parent_scope_index: Option<usize>) -> Self {
        Self {
            name,
            parent_scope_index,
            variables: HashMap::new(),
        }
    }

    /// Get frame name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get parent scope index
    pub fn parent_scope_index(&self) -> Option<usize> {
        self.parent_scope_index
    }

    /// Get variables map
    pub fn variables(&self) -> &HashMap<String, Value> {
        &self.variables
    }

    /// Get variables map mutably
    pub fn variables_mut(&mut self) -> &mut HashMap<String, Value> {
        &mut self.variables
    }

    /// Check if variable exists in this frame
    pub fn has_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Get variable value
    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    /// Set variable value
    pub fn set_variable(&mut self, name: String, value: Value) -> Option<Value> {
        self.variables.insert(name, value)
    }

    /// Remove variable
    pub fn remove_variable(&mut self, name: &str) -> Option<Value> {
        self.variables.remove(name)
    }

    /// Get all variable names
    pub fn variable_names(&self) -> Vec<String> {
        self.variables.keys().cloned().collect()
    }
}

/// Scope manager that handles scope hierarchy and stack frames
pub struct ScopeManager {
    scopes: Vec<Scope>,
    call_stack: Vec<StackFrame>,
    debug_mode: bool,
}

impl ScopeManager {
    /// Create a new scope manager
    pub fn new(debug_mode: bool) -> Self {
        let mut global = Scope::new();
        // Pre-define some common constants
        global.insert("maxint".to_string(), Value::Integer(i64::MAX));
        global.insert("true".to_string(), Value::Boolean(true));
        global.insert("false".to_string(), Value::Boolean(false));

        Self {
            scopes: vec![global],
            call_stack: Vec::new(),
            debug_mode,
        }
    }

    /// Get the current (innermost) scope
    pub fn current_scope(&self) -> &Scope {
        self.scopes
            .last()
            .expect("Scope stack should never be empty")
    }

    /// Get the current (innermost) scope mutably
    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .last_mut()
            .expect("Scope stack should never be empty")
    }

    /// Enter a new scope (e.g., for block statements)
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
        
        if self.debug_mode {
            eprintln!("[scope] Entered new scope (total scopes: {})", self.scopes.len());
        }
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            let scope_count = self.scopes.len();
            self.scopes.pop();
            
            if self.debug_mode {
                eprintln!("[scope] Exited scope (total scopes: {})", scope_count - 1);
            }
        }
    }

    /// Get a variable value from any scope (scoping rules)
    pub fn get_variable(&self, name: &str) -> Option<Value> {
        let name_lower = name.to_lowercase();
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(&name_lower) {
                return Some(val.clone());
            }
        }
        None
    }

    /// Set a variable value in the current scope
    pub fn set_variable(&mut self, name: String, value: Value) -> Option<Value> {
        let name_lower = name.to_lowercase();
        let scope = self.scopes.last_mut()
            .unwrap_or_else(|| panic!("No scope available for variable assignment"));
        
        // Get old value if it exists
        let old_value = scope.get(&name_lower).cloned();
        
        // Set the new value
        scope.insert(name_lower, value);
        
        old_value
    }

    /// Declare variables in the current scope
    pub fn declare_variables(&mut self, variables: &[String]) -> Result<()> {
        for var_name in variables {
            let name_lower = var_name.to_lowercase();
            if !self.scopes.last()
                .unwrap_or_else(|| panic!("No scope available for variable declaration"))
                .contains_key(&name_lower)
            {
                self.scopes.last_mut()
                    .unwrap_or_else(|| panic!("No scope available for variable declaration"))
                    .insert(name_lower, Value::Nil);
            } else {
                return Err(anyhow::anyhow!("Variable '{}' already declared", var_name));
            }
        }
        Ok(())
    }

    /// Push a new stack frame for function/procedure calls
    pub fn push_frame(&mut self, frame: StackFrame) {
        if self.debug_mode {
            eprintln!("[call] Entered frame: {}", frame.name);
        }
        self.call_stack.push(frame);
    }

    /// Pop the current stack frame
    pub fn pop_frame(&mut self) -> Option<StackFrame> {
        let frame = self.call_stack.pop();
        if let Some(ref f) = frame {
            if self.debug_mode {
                eprintln!("[call] Exited frame: {}", f.name);
            }
        }
        frame
    }

    /// Get the current stack frame
    pub fn current_frame(&self) -> Option<&StackFrame> {
        self.call_stack.last()
    }

    /// Get current stack frame mutably
    pub fn current_frame_mut(&mut self) -> Option<&mut StackFrame> {
        self.call_stack.last_mut()
    }

    /// Get all stack frames
    pub fn call_stack(&self) -> &[StackFrame] {
        &self.call_stack
    }

    /// Check if we're in a particular function context
    pub fn in_function(&self, function_name: &str) -> bool {
        self.call_stack.iter().any(|frame| frame.name() == function_name)
    }

    /// Get stack depth
    pub fn stack_depth(&self) -> usize {
        self.call_stack.len()
    }

    /// Enable/disable debug mode
    pub fn set_debug_mode(&mut self, debug_mode: bool) {
        self.debug_mode = debug_mode;
    }

    /// Check if debug mode is enabled
    pub fn debug_mode(&self) -> bool {
        self.debug_mode
    }
}

/// Scope utilities for common operations
pub struct ScopeUtils;

impl ScopeUtils {
    /// Create variables from a list of names
    pub fn create_variable_list(names: &[&str]) -> Vec<String> {
        names.iter().map(|&s| s.to_string()).collect()
    }

    /// Check if a variable name is valid
    pub fn is_valid_variable_name(name: &str) -> bool {
        let name_lower = name.to_lowercase();
        !name_lower.is_empty()
        && name_lower.chars().all(|c| c.is_alphanumeric() || c == '_')
        && !name_lower.chars().next().unwrap_or('0').is_numeric()
        && name_lower != "true"
        && name_lower != "false"
        && name_lower != "maxint"
    }

    /// Resolve scope hierarchy (returns list of scope names from global to local)
    pub fn resolve_scope_hierarchy(scope_manager: &ScopeManager) -> Vec<String> {
        let mut hierarchy = Vec::new();
        for (i, _scope) in scope_manager.scopes.iter().enumerate().rev() {
            hierarchy.push(format!("scope_{}", i));
        }
        hierarchy
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stack_frame_creation() {
        let frame = StackFrame::new("test_func".to_string(), None);
        assert_eq!(frame.name(), "test_func");
        assert!(!frame.has_variable("x"));
    }

    #[test]
    fn test_scope_manager() {
        let mut manager = ScopeManager::new(false);
        
        // Test initial global scope
        assert_eq!(manager.scopes.len(), 1);
        
        // Test scope management
        manager.enter_scope();
        assert_eq!(manager.scopes.len(), 2);
        
        manager.exit_scope();
        assert_eq!(manager.scopes.len(), 1);
    }

    #[test]
    fn test_variable_declaration() {
        let mut manager = ScopeManager::new(false);
        let vars = vec!["x".to_string(), "y".to_string()];
        
        assert!(manager.declare_variables(&vars).is_ok());
        assert!(manager.get_variable("x").is_some());
        assert_eq!(manager.get_variable("x"), Some(Value::Nil));
    }

    #[test]
    fn test_stack_frames() {
        let mut manager = ScopeManager::new(false);
        
        let frame = StackFrame::new("test".to_string(), None);
        manager.push_frame(frame);
        
        assert_eq!(manager.stack_depth(), 1);
        assert!(manager.in_function("test"));
        
        manager.pop_frame();
        assert_eq!(manager.stack_depth(), 0);
    }

    #[test]
    fn test_scope_utilities() {
        let valid_names = ["x", "test_var", "abc123"];
        let invalid_names = ["123var", "", "true", "false", "maxint"];
        
        for name in valid_names {
            assert!(ScopeUtils::is_valid_variable_name(name));
        }
        
        for name in invalid_names {
            assert!(!ScopeUtils::is_valid_variable_name(name));
        }
    }
}