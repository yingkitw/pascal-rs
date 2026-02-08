//! Variable scope management for the interpreter

use super::interpreter_value::Value;
use super::interpreter_traits::ScopeOperations;
use std::collections::HashMap;

/// Variable scope
#[derive(Debug, Clone)]
pub struct Scope {
    variables: HashMap<String, Value>,
}

/// Stack of scopes for nested variable lookup
///
/// Provides ScopeOperations by searching from innermost to outermost scope.
#[derive(Debug, Clone)]
pub struct ScopeStack {
    scopes: Vec<Scope>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    pub fn set(&mut self, name: impl Into<String>, value: Value) {
        self.variables.insert(name.into(), value);
    }

    pub fn contains(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    pub fn variable_names(&self) -> impl Iterator<Item = &String> {
        self.variables.keys()
    }
}

// Implement the ScopeOperations trait for Scope
impl ScopeOperations for Scope {
    fn get(&self, name: &str) -> Option<Value> {
        self.variables.get(name).cloned()
    }

    fn set(&mut self, name: &str, value: Value) {
        self.variables.insert(name.to_string(), value);
    }

    fn contains(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    fn variable_names(&self) -> Vec<String> {
        self.variables.keys().cloned().collect()
    }
}

impl ScopeStack {
    /// Create a new empty scope stack
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
        }
    }

    /// Create a new scope stack with a global scope
    pub fn with_global(global: Scope) -> Self {
        let mut scopes = Vec::new();
        scopes.push(global);
        Self { scopes }
    }

    /// Push a new scope onto the stack
    pub fn push(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    /// Pop the innermost scope from the stack
    pub fn pop(&mut self) -> Option<Scope> {
        if self.scopes.len() > 1 {
            self.scopes.pop()
        } else {
            None // Never pop the global scope
        }
    }

    /// Get the innermost scope
    pub fn current(&self) -> &Scope {
        self.scopes.last().expect("Scope stack should never be empty")
    }

    /// Get a mutable reference to the innermost scope
    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("Scope stack should never be empty")
    }

    /// Get the global scope
    pub fn global(&self) -> &Scope {
        self.scopes.first().expect("Scope stack should never be empty")
    }

    /// Get a mutable reference to the global scope
    pub fn global_mut(&mut self) -> &mut Scope {
        self.scopes.first_mut().expect("Scope stack should never be empty")
    }

    /// Get all scopes
    pub fn all_scopes(&self) -> &[Scope] {
        &self.scopes
    }
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self::new()
    }
}

// Implement ScopeOperations for ScopeStack
// Searches from innermost to outermost scope
impl ScopeOperations for ScopeStack {
    fn get(&self, name: &str) -> Option<Value> {
        // Search from innermost to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    fn set(&mut self, name: &str, value: Value) {
        // Always set in the innermost scope
        self.current_mut().set(name, value);
    }

    fn contains(&self, name: &str) -> bool {
        // Check all scopes from innermost to outermost
        for scope in self.scopes.iter().rev() {
            if scope.contains(name) {
                return true;
            }
        }
        false
    }

    fn variable_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        for scope in self.scopes.iter().rev() {
            for name in scope.variable_names() {
                if !names.contains(name) {
                    names.push(name.clone());
                }
            }
        }
        names
    }
}
