//! Symbol table for code generation
//!
//! Provides proper variable tracking, type information, and scope management

use crate::ast::Type;
use anyhow::{anyhow, Result};
use std::collections::HashMap;

/// Function signature information
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Vec<(String, Type)>, // (name, type) pairs
    pub return_type: Type,
    pub is_external: bool,
    pub external_name: Option<String>,
}

/// Symbol information
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub typ: Type,
    pub offset: i32,
    pub is_parameter: bool,
    pub is_exported: bool,
    pub is_const: bool,
    pub const_value: Option<ConstValue>,
    pub function_signature: Option<FunctionSignature>,
}

/// Constant value for compile-time evaluation
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Integer(i64),
    Real(f64),
    Boolean(bool),
    String(String),
    Char(char),
}

/// Scope level in the symbol table
#[derive(Debug, Clone)]
pub struct Scope {
    pub symbols: HashMap<String, Symbol>,
    pub parent: Option<usize>,
    pub stack_offset: i32,
}

/// Symbol table with scope management
#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
    current_scope: usize,
    next_offset: i32,
}

impl SymbolTable {
    /// Create a new symbol table
    pub fn new() -> Self {
        let global_scope = Scope {
            symbols: HashMap::new(),
            parent: None,
            stack_offset: 0,
        };

        Self {
            scopes: vec![global_scope],
            current_scope: 0,
            next_offset: 8, // Start after base pointer
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        let parent = self.current_scope;
        let new_scope = Scope {
            symbols: HashMap::new(),
            parent: Some(parent),
            stack_offset: self.next_offset,
        };

        self.scopes.push(new_scope);
        self.current_scope = self.scopes.len() - 1;
    }

    /// Exit current scope
    pub fn exit_scope(&mut self) -> Result<()> {
        if self.current_scope == 0 {
            return Err(anyhow!("Cannot exit global scope"));
        }

        if let Some(parent) = self.scopes[self.current_scope].parent {
            self.current_scope = parent;
            Ok(())
        } else {
            Err(anyhow!("Invalid scope hierarchy"))
        }
    }

    /// Add a symbol to the current scope
    pub fn add_symbol(
        &mut self,
        name: String,
        typ: Type,
        is_parameter: bool,
        is_exported: bool,
    ) -> Result<i32> {
        // Check if symbol already exists in current scope
        if self.scopes[self.current_scope].symbols.contains_key(&name) {
            return Err(anyhow!(
                "Symbol '{}' already defined in current scope",
                name
            ));
        }

        let offset = self.next_offset;
        self.next_offset += self.get_type_size(&typ);

        let symbol = Symbol {
            name: name.clone(),
            typ,
            offset,
            is_parameter,
            is_exported,
            is_const: false,
            const_value: None,
            function_signature: None,
        };

        self.scopes[self.current_scope].symbols.insert(name, symbol);
        Ok(offset)
    }

    /// Add a function symbol with signature
    pub fn add_function(
        &mut self,
        name: String,
        parameters: Vec<(String, Type)>,
        return_type: Type,
        is_external: bool,
        external_name: Option<String>,
        is_exported: bool,
    ) -> Result<()> {
        // Check if symbol already exists in current scope
        if self.scopes[self.current_scope].symbols.contains_key(&name) {
            return Err(anyhow!(
                "Function '{}' already defined in current scope",
                name
            ));
        }

        let signature = FunctionSignature {
            name: name.clone(),
            parameters: parameters.clone(),
            return_type: return_type.clone(),
            is_external,
            external_name,
        };

        let symbol = Symbol {
            name: name.clone(),
            typ: return_type,
            offset: 0, // Functions don't have stack offsets
            is_parameter: false,
            is_exported,
            is_const: false,
            const_value: None,
            function_signature: Some(signature),
        };

        self.scopes[self.current_scope].symbols.insert(name, symbol);
        Ok(())
    }

    /// Look up a function signature
    pub fn lookup_function(&self, name: &str) -> Option<&FunctionSignature> {
        let mut scope_idx = self.current_scope;

        loop {
            if let Some(symbol) = self.scopes[scope_idx].symbols.get(name) {
                if let Some(ref sig) = symbol.function_signature {
                    return Some(sig);
                }
            }

            if let Some(parent) = self.scopes[scope_idx].parent {
                scope_idx = parent;
            } else {
                return None;
            }
        }
    }

    /// Add a constant symbol
    pub fn add_const(&mut self, name: String, typ: Type, value: ConstValue) -> Result<()> {
        let symbol = Symbol {
            name: name.clone(),
            typ,
            offset: 0, // Constants don't need stack space
            is_parameter: false,
            is_exported: false,
            is_const: true,
            const_value: Some(value),
            function_signature: None,
        };

        self.scopes[self.current_scope].symbols.insert(name, symbol);
        Ok(())
    }

    /// Look up a symbol in current and parent scopes
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        let mut scope_idx = self.current_scope;

        loop {
            if let Some(symbol) = self.scopes[scope_idx].symbols.get(name) {
                return Some(symbol);
            }

            if let Some(parent) = self.scopes[scope_idx].parent {
                scope_idx = parent;
            } else {
                return None;
            }
        }
    }

    /// Get type size in bytes
    fn get_type_size(&self, typ: &Type) -> i32 {
        match typ {
            Type::Integer | Type::Simple(_) | Type::Boolean | Type::Char => 8,
            Type::Real => 8,
            Type::String | Type::WideString => 8, // Pointer to string
            Type::Pointer(_) => 8,
            Type::Array { .. } => 8,  // Pointer to array
            Type::Record { .. } => 8, // Pointer to record
            _ => 8, // Default size
        }
    }

    /// Get current stack offset
    pub fn current_offset(&self) -> i32 {
        self.next_offset
    }

    /// Reset offset for new function
    pub fn reset_offset(&mut self) {
        self.next_offset = 8;
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_basic() {
        let mut table = SymbolTable::new();

        // Add a variable
        let offset = table
            .add_symbol("x".to_string(), Type::Integer, false, false)
            .unwrap();
        assert_eq!(offset, 8);

        // Look it up
        let symbol = table.lookup("x").unwrap();
        assert_eq!(symbol.name, "x");
        assert_eq!(symbol.offset, 8);
    }

    #[test]
    fn test_scopes() {
        let mut table = SymbolTable::new();

        // Add to global scope
        table
            .add_symbol("x".to_string(), Type::Integer, false, false)
            .unwrap();

        // Enter new scope
        table.enter_scope();
        table
            .add_symbol("y".to_string(), Type::Integer, false, false)
            .unwrap();

        // Can see both
        assert!(table.lookup("x").is_some());
        assert!(table.lookup("y").is_some());

        // Exit scope
        table.exit_scope().unwrap();

        // Can only see x
        assert!(table.lookup("x").is_some());
        assert!(table.lookup("y").is_none());
    }

    #[test]
    fn test_constants() {
        let mut table = SymbolTable::new();

        table
            .add_const("PI".to_string(), Type::Real, ConstValue::Real(3.14159))
            .unwrap();

        let symbol = table.lookup("PI").unwrap();
        assert!(symbol.is_const);
        assert_eq!(symbol.const_value, Some(ConstValue::Real(3.14159)));
    }
}
