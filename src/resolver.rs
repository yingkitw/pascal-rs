//! Symbol resolution across modules

use crate::{Module, ModuleError, ModuleResult};
use std::collections::HashMap;

/// Resolves symbols across module boundaries
pub struct ModuleResolver {
    /// Symbol table for each module
    symbol_tables: HashMap<String, SymbolTable>,
}

/// Symbol table for a module
#[derive(Debug, Clone)]
pub struct SymbolTable {
    /// Module name
    pub module_name: String,

    /// Exported types
    pub types: HashMap<String, SymbolInfo>,

    /// Exported constants
    pub constants: HashMap<String, SymbolInfo>,

    /// Exported variables
    pub variables: HashMap<String, SymbolInfo>,

    /// Exported procedures
    pub procedures: HashMap<String, SymbolInfo>,

    /// Exported functions
    pub functions: HashMap<String, SymbolInfo>,

    /// Exported classes
    pub classes: HashMap<String, SymbolInfo>,
}

/// Information about a symbol
#[derive(Debug, Clone)]
pub struct SymbolInfo {
    /// Symbol name
    pub name: String,

    /// Module where symbol is defined
    pub module: String,

    /// Whether symbol is exported from interface
    pub is_public: bool,

    /// Symbol kind
    pub kind: SymbolKind,
}

/// Kind of symbol
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Type,
    Constant,
    Variable,
    Procedure,
    Function,
    Class,
    Interface,
}

impl ModuleResolver {
    /// Create a new module resolver
    pub fn new() -> Self {
        Self {
            symbol_tables: HashMap::new(),
        }
    }

    /// Build symbol table for a module
    pub fn build_symbol_table(&mut self, module: &Module) -> ModuleResult<()> {
        let mut table = SymbolTable {
            module_name: module.name.clone(),
            types: HashMap::new(),
            constants: HashMap::new(),
            variables: HashMap::new(),
            procedures: HashMap::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
        };

        // Add types from interface
        for type_decl in &module.unit.interface.types {
            table.types.insert(
                type_decl.name.clone(),
                SymbolInfo {
                    name: type_decl.name.clone(),
                    module: module.name.clone(),
                    is_public: true,
                    kind: SymbolKind::Type,
                },
            );
        }

        // Add constants from interface
        for const_decl in &module.unit.interface.constants {
            table.constants.insert(
                const_decl.name.clone(),
                SymbolInfo {
                    name: const_decl.name.clone(),
                    module: module.name.clone(),
                    is_public: true,
                    kind: SymbolKind::Constant,
                },
            );
        }

        // Add variables from interface
        for var_decl in &module.unit.interface.variables {
            table.variables.insert(
                var_decl.name.clone(),
                SymbolInfo {
                    name: var_decl.name.clone(),
                    module: module.name.clone(),
                    is_public: true,
                    kind: SymbolKind::Variable,
                },
            );
        }

        // Add procedures from interface
        for proc_decl in &module.unit.interface.procedures {
            table.procedures.insert(
                proc_decl.name.clone(),
                SymbolInfo {
                    name: proc_decl.name.clone(),
                    module: module.name.clone(),
                    is_public: true,
                    kind: SymbolKind::Procedure,
                },
            );
        }

        // Add functions from interface
        for func_decl in &module.unit.interface.functions {
            table.functions.insert(
                func_decl.name.clone(),
                SymbolInfo {
                    name: func_decl.name.clone(),
                    module: module.name.clone(),
                    is_public: true,
                    kind: SymbolKind::Function,
                },
            );
        }

        // Add classes from interface
        for class_decl in &module.unit.interface.classes {
            table.classes.insert(
                class_decl.name.clone(),
                SymbolInfo {
                    name: class_decl.name.clone(),
                    module: module.name.clone(),
                    is_public: true,
                    kind: SymbolKind::Class,
                },
            );
        }

        self.symbol_tables.insert(module.name.clone(), table);
        Ok(())
    }

    /// Resolve a symbol in a module's context
    pub fn resolve_symbol(
        &self,
        symbol_name: &str,
        current_module: &str,
        used_modules: &[String],
    ) -> ModuleResult<SymbolInfo> {
        // First check current module
        if let Some(table) = self.symbol_tables.get(current_module) {
            if let Some(info) = self.find_in_table(table, symbol_name) {
                return Ok(info.clone());
            }
        }

        // Then check used modules
        for module_name in used_modules {
            if let Some(table) = self.symbol_tables.get(module_name) {
                if let Some(info) = self.find_in_table(table, symbol_name) {
                    if info.is_public {
                        return Ok(info.clone());
                    }
                }
            }
        }

        Err(ModuleError::ModuleNotFound(format!(
            "Symbol '{}' not found in module '{}' or its dependencies",
            symbol_name, current_module
        )))
    }

    /// Find a symbol in a symbol table
    fn find_in_table<'a>(&self, table: &'a SymbolTable, name: &str) -> Option<&'a SymbolInfo> {
        table
            .types
            .get(name)
            .or_else(|| table.constants.get(name))
            .or_else(|| table.variables.get(name))
            .or_else(|| table.procedures.get(name))
            .or_else(|| table.functions.get(name))
            .or_else(|| table.classes.get(name))
    }

    /// Get all symbols exported by a module
    pub fn get_exported_symbols(&self, module_name: &str) -> Vec<SymbolInfo> {
        if let Some(table) = self.symbol_tables.get(module_name) {
            let mut symbols = Vec::new();
            symbols.extend(table.types.values().cloned());
            symbols.extend(table.constants.values().cloned());
            symbols.extend(table.variables.values().cloned());
            symbols.extend(table.procedures.values().cloned());
            symbols.extend(table.functions.values().cloned());
            symbols.extend(table.classes.values().cloned());
            symbols
        } else {
            Vec::new()
        }
    }

    /// Check if a symbol is exported by a module
    pub fn is_symbol_exported(&self, module_name: &str, symbol_name: &str) -> bool {
        if let Some(table) = self.symbol_tables.get(module_name) {
            self.find_in_table(table, symbol_name)
                .map(|info| info.is_public)
                .unwrap_or(false)
        } else {
            false
        }
    }
}

impl Default for ModuleResolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolver_creation() {
        let resolver = ModuleResolver::new();
        assert_eq!(resolver.symbol_tables.len(), 0);
    }

    #[test]
    fn test_symbol_table_creation() {
        let table = SymbolTable {
            module_name: "Test".to_string(),
            types: HashMap::new(),
            constants: HashMap::new(),
            variables: HashMap::new(),
            procedures: HashMap::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
        };

        assert_eq!(table.module_name, "Test");
        assert_eq!(table.types.len(), 0);
    }

    #[test]
    fn test_symbol_info() {
        let info = SymbolInfo {
            name: "MyType".to_string(),
            module: "TestModule".to_string(),
            is_public: true,
            kind: SymbolKind::Type,
        };

        assert_eq!(info.name, "MyType");
        assert_eq!(info.module, "TestModule");
        assert!(info.is_public);
        assert_eq!(info.kind, SymbolKind::Type);
    }
}
