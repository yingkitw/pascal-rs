//! Module system for minipas Pascal compiler
//! 
//! This crate implements the Pascal unit system, including:
//! - Unit interface/implementation sections
//! - Uses clause dependency management
//! - Module compilation and linking
//! - Symbol resolution across units

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use minipas_ast::Unit;

pub mod error;
pub mod loader;
pub mod resolver;

pub use error::{ModuleError, ModuleResult};
pub use loader::ModuleLoader;
pub use resolver::ModuleResolver;

/// Represents a compiled Pascal unit
#[derive(Debug, Clone)]
pub struct Module {
    /// Module name
    pub name: String,
    
    /// Source file path
    pub source_path: PathBuf,
    
    /// Compiled unit AST
    pub unit: Unit,
    
    /// CRC checksum for interface
    pub interface_crc: u32,
    
    /// CRC checksum for implementation
    pub implementation_crc: u32,
    
    /// Modules used in interface section
    pub interface_uses: Vec<String>,
    
    /// Modules used in implementation section
    pub implementation_uses: Vec<String>,
    
    /// Whether this module has been compiled
    pub is_compiled: bool,
    
    /// Whether interface has been loaded
    pub interface_loaded: bool,
    
    /// Compilation timestamp
    pub compile_time: Option<std::time::SystemTime>,
}

impl Module {
    /// Create a new module from a unit AST
    pub fn new(name: String, source_path: PathBuf, unit: Unit) -> Self {
        let interface_uses = unit.uses.clone();
        let implementation_uses = unit.implementation.uses.clone();
        
        Self {
            name,
            source_path,
            unit,
            interface_crc: 0,
            implementation_crc: 0,
            interface_uses,
            implementation_uses,
            is_compiled: false,
            interface_loaded: false,
            compile_time: None,
        }
    }
    
    /// Get all dependencies (interface + implementation)
    pub fn all_dependencies(&self) -> Vec<String> {
        let mut deps = self.interface_uses.clone();
        deps.extend(self.implementation_uses.clone());
        deps.sort();
        deps.dedup();
        deps
    }
    
    /// Check if this module depends on another
    pub fn depends_on(&self, module_name: &str) -> bool {
        self.interface_uses.contains(&module_name.to_string()) ||
        self.implementation_uses.contains(&module_name.to_string())
    }
    
    /// Calculate interface CRC
    pub fn calculate_interface_crc(&mut self) {
        // Simple CRC calculation based on interface contents
        // In a real implementation, this would be more sophisticated
        let mut crc = 0u32;
        crc = crc.wrapping_add(self.name.len() as u32);
        crc = crc.wrapping_add(self.unit.interface.types.len() as u32);
        crc = crc.wrapping_add(self.unit.interface.constants.len() as u32);
        crc = crc.wrapping_add(self.unit.interface.variables.len() as u32);
        crc = crc.wrapping_add(self.unit.interface.procedures.len() as u32);
        crc = crc.wrapping_add(self.unit.interface.functions.len() as u32);
        self.interface_crc = crc;
    }
}

/// Manages a collection of modules and their dependencies
#[derive(Debug)]
pub struct ModuleManager {
    /// All loaded modules indexed by name
    modules: HashMap<String, Module>,
    
    /// Module search paths
    search_paths: Vec<PathBuf>,
    
    /// Compilation order cache
    compile_order: Vec<String>,
}

impl ModuleManager {
    /// Create a new module manager
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            search_paths: vec![PathBuf::from(".")],
            compile_order: Vec::new(),
        }
    }
    
    /// Add a search path for modules
    pub fn add_search_path(&mut self, path: PathBuf) {
        if !self.search_paths.contains(&path) {
            self.search_paths.push(path);
        }
    }
    
    /// Register a module
    pub fn register_module(&mut self, module: Module) -> ModuleResult<()> {
        let name = module.name.clone();
        if self.modules.contains_key(&name) {
            return Err(ModuleError::DuplicateModule(name));
        }
        self.modules.insert(name, module);
        self.compile_order.clear(); // Invalidate cache
        Ok(())
    }
    
    /// Get a module by name
    pub fn get_module(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }
    
    /// Get a mutable module by name
    pub fn get_module_mut(&mut self, name: &str) -> Option<&mut Module> {
        self.modules.get_mut(name)
    }
    
    /// Check if a module exists
    pub fn has_module(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }
    
    /// Get all module names
    pub fn module_names(&self) -> Vec<String> {
        self.modules.keys().cloned().collect()
    }
    
    /// Compute compilation order using topological sort
    pub fn compute_compile_order(&mut self) -> ModuleResult<Vec<String>> {
        if !self.compile_order.is_empty() {
            return Ok(self.compile_order.clone());
        }
        
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut visiting = HashSet::new();
        
        for module_name in self.modules.keys() {
            self.visit_module(module_name, &mut visited, &mut visiting, &mut order)?;
        }
        
        self.compile_order = order.clone();
        Ok(order)
    }
    
    /// Visit a module during topological sort
    fn visit_module(
        &self,
        name: &str,
        visited: &mut HashSet<String>,
        visiting: &mut HashSet<String>,
        order: &mut Vec<String>,
    ) -> ModuleResult<()> {
        if visited.contains(name) {
            return Ok(());
        }
        
        if visiting.contains(name) {
            return Err(ModuleError::CircularDependency(name.to_string()));
        }
        
        visiting.insert(name.to_string());
        
        if let Some(module) = self.modules.get(name) {
            // Visit dependencies first
            for dep in &module.interface_uses {
                self.visit_module(dep, visited, visiting, order)?;
            }
            for dep in &module.implementation_uses {
                self.visit_module(dep, visited, visiting, order)?;
            }
        }
        
        visiting.remove(name);
        visited.insert(name.to_string());
        order.push(name.to_string());
        
        Ok(())
    }
    
    /// Check for circular dependencies
    pub fn check_circular_dependencies(&self) -> ModuleResult<()> {
        let mut visited = HashSet::new();
        let mut visiting = HashSet::new();
        let mut order = Vec::new();
        
        for module_name in self.modules.keys() {
            self.visit_module(module_name, &mut visited, &mut visiting, &mut order)?;
        }
        
        Ok(())
    }
    
    /// Get modules that depend on a given module
    pub fn get_dependents(&self, module_name: &str) -> Vec<String> {
        self.modules
            .iter()
            .filter(|(_, module)| module.depends_on(module_name))
            .map(|(name, _)| name.clone())
            .collect()
    }
    
    /// Check if recompilation is needed
    pub fn needs_recompilation(&self, module_name: &str) -> bool {
        if let Some(module) = self.modules.get(module_name) {
            if !module.is_compiled {
                return true;
            }
            
            // Check if any dependencies have changed
            for dep_name in module.all_dependencies() {
                if let Some(dep) = self.modules.get(&dep_name) {
                    if let (Some(module_time), Some(dep_time)) = 
                        (module.compile_time, dep.compile_time) {
                        if dep_time > module_time {
                            return true;
                        }
                    }
                }
            }
            
            false
        } else {
            true
        }
    }
}

impl Default for ModuleManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use minipas_ast::{Block, UnitInterface, UnitImplementation};
    
    fn create_test_unit(name: &str, interface_uses: Vec<String>, impl_uses: Vec<String>) -> Unit {
        Unit {
            name: name.to_string(),
            uses: interface_uses,
            interface: UnitInterface {
                types: vec![],
                constants: vec![],
                variables: vec![],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                interfaces: vec![],
            },
            implementation: UnitImplementation {
                uses: impl_uses,
                types: vec![],
                constants: vec![],
                variables: vec![],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                interfaces: vec![],
                initialization: None,
                finalization: None,
            },
        }
    }
    
    #[test]
    fn test_module_creation() {
        let unit = create_test_unit("TestUnit", vec![], vec![]);
        let module = Module::new(
            "TestUnit".to_string(),
            PathBuf::from("test.pas"),
            unit,
        );
        
        assert_eq!(module.name, "TestUnit");
        assert!(!module.is_compiled);
        assert!(!module.interface_loaded);
    }
    
    #[test]
    fn test_module_dependencies() {
        let unit = create_test_unit(
            "TestUnit",
            vec!["System".to_string()],
            vec!["SysUtils".to_string()],
        );
        let module = Module::new(
            "TestUnit".to_string(),
            PathBuf::from("test.pas"),
            unit,
        );
        
        assert!(module.depends_on("System"));
        assert!(module.depends_on("SysUtils"));
        assert!(!module.depends_on("Classes"));
        
        let deps = module.all_dependencies();
        assert_eq!(deps.len(), 2);
        assert!(deps.contains(&"System".to_string()));
        assert!(deps.contains(&"SysUtils".to_string()));
    }
    
    #[test]
    fn test_module_manager() {
        let mut manager = ModuleManager::new();
        
        let unit1 = create_test_unit("Unit1", vec![], vec![]);
        let module1 = Module::new("Unit1".to_string(), PathBuf::from("unit1.pas"), unit1);
        
        manager.register_module(module1).unwrap();
        assert!(manager.has_module("Unit1"));
        assert!(!manager.has_module("Unit2"));
    }
    
    #[test]
    fn test_compile_order() {
        let mut manager = ModuleManager::new();
        
        // Unit1 depends on nothing
        let unit1 = create_test_unit("Unit1", vec![], vec![]);
        let module1 = Module::new("Unit1".to_string(), PathBuf::from("unit1.pas"), unit1);
        manager.register_module(module1).unwrap();
        
        // Unit2 depends on Unit1
        let unit2 = create_test_unit("Unit2", vec!["Unit1".to_string()], vec![]);
        let module2 = Module::new("Unit2".to_string(), PathBuf::from("unit2.pas"), unit2);
        manager.register_module(module2).unwrap();
        
        // Unit3 depends on Unit2
        let unit3 = create_test_unit("Unit3", vec!["Unit2".to_string()], vec![]);
        let module3 = Module::new("Unit3".to_string(), PathBuf::from("unit3.pas"), unit3);
        manager.register_module(module3).unwrap();
        
        let order = manager.compute_compile_order().unwrap();
        
        // Unit1 should come before Unit2, Unit2 before Unit3
        let pos1 = order.iter().position(|x| x == "Unit1").unwrap();
        let pos2 = order.iter().position(|x| x == "Unit2").unwrap();
        let pos3 = order.iter().position(|x| x == "Unit3").unwrap();
        
        assert!(pos1 < pos2);
        assert!(pos2 < pos3);
    }
    
    #[test]
    fn test_circular_dependency_detection() {
        let mut manager = ModuleManager::new();
        
        // Unit1 depends on Unit2
        let unit1 = create_test_unit("Unit1", vec!["Unit2".to_string()], vec![]);
        let module1 = Module::new("Unit1".to_string(), PathBuf::from("unit1.pas"), unit1);
        manager.register_module(module1).unwrap();
        
        // Unit2 depends on Unit1 (circular!)
        let unit2 = create_test_unit("Unit2", vec!["Unit1".to_string()], vec![]);
        let module2 = Module::new("Unit2".to_string(), PathBuf::from("unit2.pas"), unit2);
        manager.register_module(module2).unwrap();
        
        let result = manager.compute_compile_order();
        assert!(result.is_err());
        
        if let Err(ModuleError::CircularDependency(_)) = result {
            // Expected
        } else {
            panic!("Expected circular dependency error");
        }
    }
}
