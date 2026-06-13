//! Symbol table for code generation (facade over `OptimizedSymbolTable`).

use crate::ast::Type;
use crate::optimized_symbol_table::OptimizedSymbolTable;
use anyhow::Result;

pub use crate::optimized_symbol_table::{ConstValue, FunctionSignature, Symbol, SymbolFlags};

/// Symbol table with scope management backed by the optimized implementation.
#[derive(Debug)]
pub struct SymbolTable {
    inner: OptimizedSymbolTable,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            inner: OptimizedSymbolTable::new(256),
        }
    }

    pub fn enter_scope(&mut self) {
        self.inner.enter_scope();
    }

    pub fn exit_scope(&mut self) -> Result<()> {
        self.inner.exit_scope()
    }

    pub fn add_symbol(
        &mut self,
        name: String,
        typ: Type,
        is_parameter: bool,
        is_exported: bool,
    ) -> Result<i32> {
        self.inner
            .add_symbol(name, typ, is_parameter, is_exported)
    }

    pub fn add_function(
        &mut self,
        name: String,
        parameters: Vec<(String, Type)>,
        return_type: Type,
        is_external: bool,
        external_name: Option<String>,
        is_exported: bool,
    ) -> Result<()> {
        self.inner.add_function(
            name,
            parameters,
            return_type,
            is_external,
            external_name,
            is_exported,
        )
    }

    pub fn add_const(&mut self, name: String, typ: Type, value: ConstValue) -> Result<()> {
        self.inner.add_const(name, typ, value)
    }

    pub fn lookup(&mut self, name: &str) -> Option<Symbol> {
        self.inner.lookup_symbol(name).map(|s| (*s).clone())
    }

    pub fn lookup_function(&mut self, name: &str) -> Option<FunctionSignature> {
        self.inner.lookup_function(name)
    }

    pub fn current_offset(&self) -> i32 {
        self.inner.current_offset()
    }

    pub fn reset_offset(&mut self) {
        self.inner.reset_offset();
    }

    pub fn insert_symbol(&mut self, symbol: Symbol) -> Result<()> {
        self.inner.insert_symbol(symbol)
    }

    pub fn clear(&mut self) {
        self.inner.reset();
    }

    /// Access the underlying optimized table (for codegen).
    pub fn optimized(&mut self) -> &mut OptimizedSymbolTable {
        &mut self.inner
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl Symbol {
    pub fn is_parameter(&self) -> bool {
        self.flags.is_parameter()
    }

    pub fn is_exported(&self) -> bool {
        self.flags.is_exported()
    }

    pub fn is_const(&self) -> bool {
        self.flags.is_const()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_basic() {
        let mut table = SymbolTable::new();
        let offset = table
            .add_symbol("x".to_string(), Type::Integer, false, false)
            .unwrap();
        assert_eq!(offset, 8);

        let symbol = table.lookup("x").unwrap();
        assert_eq!(symbol.name, "x");
        assert_eq!(symbol.offset, 8);
    }

    #[test]
    fn test_scopes() {
        let mut table = SymbolTable::new();
        table
            .add_symbol("x".to_string(), Type::Integer, false, false)
            .unwrap();
        table.enter_scope();
        table
            .add_symbol("y".to_string(), Type::Integer, false, false)
            .unwrap();
        assert!(table.lookup("x").is_some());
        assert!(table.lookup("y").is_some());
        table.exit_scope().unwrap();
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
        assert!(symbol.is_const());
        assert_eq!(symbol.const_value, Some(ConstValue::Real(3.14159)));
    }
}
