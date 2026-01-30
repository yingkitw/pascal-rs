//! Module loader for loading and caching Pascal units

use crate::ppu::PpuFile;
use crate::{Module, ModuleError, ModuleResult};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

/// Loads Pascal units from the filesystem
/// Thread-safe with Arc and RwLock for parallel compilation
pub struct ModuleLoader {
    /// Cache of loaded modules (thread-safe)
    cache: Arc<RwLock<HashMap<String, Module>>>,

    /// Search paths for finding units
    search_paths: Vec<PathBuf>,

    /// File extension for Pascal units
    unit_extension: String,
}

impl ModuleLoader {
    /// Create a new module loader
    pub fn new() -> Self {
        Self {
            cache: Arc::new(RwLock::new(HashMap::new())),
            search_paths: vec![
                PathBuf::from("."),
                PathBuf::from("./stdlib"),
            ],
            unit_extension: "pas".to_string(),
        }
    }

    /// Clone the loader for use in multiple threads
    pub fn clone_for_thread(&self) -> Self {
        Self {
            cache: Arc::clone(&self.cache),
            search_paths: self.search_paths.clone(),
            unit_extension: self.unit_extension.clone(),
        }
    }

    /// Add a search path
    pub fn add_search_path<P: AsRef<Path>>(&mut self, path: P) {
        let path = path.as_ref().to_path_buf();
        if !self.search_paths.contains(&path) {
            self.search_paths.push(path);
        }
    }

    /// Set the unit file extension
    pub fn set_unit_extension(&mut self, ext: String) {
        self.unit_extension = ext;
    }

    /// Find a unit file in the search paths
    pub fn find_unit_file(&self, unit_name: &str) -> ModuleResult<PathBuf> {
        let filename = format!("{}.{}", unit_name.to_lowercase(), self.unit_extension);

        for search_path in &self.search_paths {
            let full_path = search_path.join(&filename);
            if full_path.exists() {
                return Ok(full_path);
            }
        }

        Err(ModuleError::ModuleNotFound(unit_name.to_string()))
    }

    /// Load a unit from file (without parsing - that's done by the parser)
    pub fn load_unit_source(&self, unit_name: &str) -> ModuleResult<(PathBuf, String)> {
        let path = self.find_unit_file(unit_name)?;
        let source = fs::read_to_string(&path)
            .map_err(|e| ModuleError::LoadError(unit_name.to_string(), e.to_string()))?;
        Ok((path, source))
    }

    /// Check if a unit is in the cache
    pub fn is_cached(&self, unit_name: &str) -> bool {
        self.cache.read().unwrap().contains_key(unit_name)
    }

    /// Get a cached module
    pub fn get_cached(&self, unit_name: &str) -> Option<Module> {
        self.cache.read().unwrap().get(unit_name).cloned()
    }

    /// Add a module to the cache
    pub fn cache_module(&mut self, module: Module) {
        self.cache.write().unwrap().insert(module.name.clone(), module);
    }

    /// Clear the cache
    pub fn clear_cache(&mut self) {
        self.cache.write().unwrap().clear();
    }

    /// Get all cached module names
    pub fn cached_modules(&self) -> Vec<String> {
        self.cache.read().unwrap().keys().cloned().collect()
    }

    /// Check if a unit file exists
    pub fn unit_exists(&self, unit_name: &str) -> bool {
        self.find_unit_file(unit_name).is_ok()
    }

    /// Get the modification time of a unit file
    pub fn get_unit_mtime(&self, unit_name: &str) -> ModuleResult<std::time::SystemTime> {
        let path = self.find_unit_file(unit_name)?;
        let metadata = fs::metadata(&path)
            .map_err(|e| ModuleError::LoadError(unit_name.to_string(), e.to_string()))?;
        metadata
            .modified()
            .map_err(|e| ModuleError::LoadError(unit_name.to_string(), e.to_string()))
    }

    /// Check if cached module is still valid
    pub fn is_cache_valid(&self, unit_name: &str) -> bool {
        if let Some(_cached) = self.cache.read().unwrap().get(unit_name) {
            if let Ok(_file_time) = self.get_unit_mtime(unit_name) {
                // TODO: Add timestamp tracking to Module structure
                return true;
            }
        }
        false
    }

    /// Find a PPU file for a unit
    pub fn find_ppu_file(&self, unit_name: &str) -> ModuleResult<PathBuf> {
        let filename = format!("{}.ppu", unit_name.to_lowercase());

        for search_path in &self.search_paths {
            let full_path = search_path.join(&filename);
            if full_path.exists() {
                return Ok(full_path);
            }
        }

        Err(ModuleError::ModuleNotFound(unit_name.to_string()))
    }

    /// Load a unit from a PPU file
    pub fn load_from_ppu(&self, unit_name: &str) -> ModuleResult<crate::ast::Unit> {
        let ppu_path = self.find_ppu_file(unit_name)?;
        let ppu = PpuFile::load(&ppu_path)?;

        // Verify checksum
        if !ppu.verify_checksum() {
            return Err(ModuleError::LoadError(
                unit_name.to_string(),
                "PPU checksum verification failed".to_string(),
            ));
        }

        Ok(ppu.module.unit)
    }

    /// Save a unit to a PPU file
    pub fn save_to_ppu(
        &self,
        unit: &crate::ast::Unit,
        output_dir: Option<&Path>,
    ) -> ModuleResult<PathBuf> {
        // Create a Module from the Unit
        let module = Module {
            name: unit.name.clone(),
            unit: unit.clone(),
            dependencies: unit.interface.uses.clone(),
        };
        
        let ppu = PpuFile::new(module);

        // Determine output path
        let dir = output_dir.unwrap_or_else(|| Path::new("."));
        let filename = format!("{}.ppu", unit.name.to_lowercase());
        let ppu_path = dir.join(filename);

        // Write PPU file
        ppu.save(&ppu_path)?;

        Ok(ppu_path)
    }

    /// Check if a PPU file exists and is newer than the source
    pub fn is_ppu_up_to_date(&self, unit_name: &str) -> bool {
        if let (Ok(ppu_path), Ok(src_path)) = (
            self.find_ppu_file(unit_name),
            self.find_unit_file(unit_name),
        ) {
            if let (Ok(ppu_meta), Ok(src_meta)) = (fs::metadata(&ppu_path), fs::metadata(&src_path))
            {
                if let (Ok(ppu_time), Ok(src_time)) = (ppu_meta.modified(), src_meta.modified()) {
                    return ppu_time >= src_time;
                }
            }
        }
        false
    }
}

impl Default for ModuleLoader {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    
    // Helper function to create a test module
    fn create_test_module(name: &str) -> Module {
        Module {
            name: name.to_string(),
            unit: crate::ast::Unit {
                name: name.to_string(),
                uses: vec![],
                interface: crate::ast::UnitInterface {
                    uses: vec![],
                    types: std::collections::HashMap::new(),
                    constants: std::collections::HashMap::new(),
                    variables: std::collections::HashMap::new(),
                    procedures: vec![],
                    functions: vec![],
                    classes: vec![],
                    interfaces: vec![],
                },
                implementation: crate::ast::UnitImplementation {
                    uses: vec![],
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
            },
            dependencies: vec![],
        }
    }

    #[test]
    fn test_loader_creation() {
        let loader = ModuleLoader::new();
        assert_eq!(loader.search_paths.len(), 2);
        assert_eq!(loader.unit_extension, "pas");
    }

    #[test]
    fn test_add_search_path() {
        let mut loader = ModuleLoader::new();
        loader.add_search_path("/usr/lib/pascal");
        assert_eq!(loader.search_paths.len(), 3);

        // Adding same path again should not duplicate
        loader.add_search_path("/usr/lib/pascal");
        assert_eq!(loader.search_paths.len(), 3);
    }

    #[test]
    fn test_cache_operations() {
        let loader = ModuleLoader::new();
        assert!(!loader.is_cached("TestUnit"));
        assert_eq!(loader.cached_modules().len(), 0);
    }
    
    #[test]
    fn test_clone_for_thread() {
        let loader = ModuleLoader::new();
        let cloned = loader.clone_for_thread();
        
        // Both should share the same cache
        assert_eq!(loader.cached_modules().len(), cloned.cached_modules().len());
    }
    
    #[test]
    fn test_cache_module_and_retrieve() {
        let mut loader = ModuleLoader::new();
        
        let module = create_test_module("TestModule");
        
        loader.cache_module(module.clone());
        assert!(loader.is_cached("TestModule"));
        
        let cached = loader.get_cached("TestModule");
        assert!(cached.is_some());
        assert_eq!(cached.unwrap().name, "TestModule");
    }
    
    #[test]
    fn test_clear_cache() {
        let mut loader = ModuleLoader::new();
        
        let module = create_test_module("TestModule");
        
        loader.cache_module(module);
        assert!(loader.is_cached("TestModule"));
        
        loader.clear_cache();
        assert!(!loader.is_cached("TestModule"));
        assert_eq!(loader.cached_modules().len(), 0);
    }
    
    #[test]
    fn test_concurrent_cache_access() {
        let loader = ModuleLoader::new();
        let mut handles = vec![];
        
        // Spawn multiple threads reading from cache
        for i in 0..10 {
            let loader_clone = loader.clone_for_thread();
            let handle = thread::spawn(move || {
                // Read operations should not block each other
                for _ in 0..100 {
                    let _ = loader_clone.is_cached(&format!("Module{}", i));
                    let _ = loader_clone.cached_modules();
                }
            });
            handles.push(handle);
        }
        
        // Wait for all threads
        for handle in handles {
            handle.join().unwrap();
        }
    }
    
    #[test]
    fn test_concurrent_cache_write() {
        let mut loader = ModuleLoader::new();
        let mut handles = vec![];
        
        // Spawn multiple threads writing to cache
        for i in 0..5 {
            let mut loader_clone = loader.clone_for_thread();
            let handle = thread::spawn(move || {
                let module = create_test_module(&format!("Module{}", i));
                loader_clone.cache_module(module);
            });
            handles.push(handle);
        }
        
        // Wait for all threads
        for handle in handles {
            handle.join().unwrap();
        }
        
        // Verify all modules were cached
        assert_eq!(loader.cached_modules().len(), 5);
    }
    
    #[test]
    fn test_cached_modules_list() {
        let mut loader = ModuleLoader::new();
        
        for i in 0..3 {
            let module = create_test_module(&format!("Module{}", i));
            loader.cache_module(module);
        }
        
        let modules = loader.cached_modules();
        assert_eq!(modules.len(), 3);
        assert!(modules.contains(&"Module0".to_string()));
        assert!(modules.contains(&"Module1".to_string()));
        assert!(modules.contains(&"Module2".to_string()));
    }
}
