//! Module loader for loading and caching Pascal units

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use crate::{Module, ModuleError, ModuleResult};

/// Loads Pascal units from the filesystem
pub struct ModuleLoader {
    /// Cache of loaded modules
    cache: HashMap<String, Module>,
    
    /// Search paths for finding units
    search_paths: Vec<PathBuf>,
    
    /// File extension for Pascal units
    unit_extension: String,
}

impl ModuleLoader {
    /// Create a new module loader
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            search_paths: vec![PathBuf::from(".")],
            unit_extension: "pas".to_string(),
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
        self.cache.contains_key(unit_name)
    }
    
    /// Get a cached module
    pub fn get_cached(&self, unit_name: &str) -> Option<&Module> {
        self.cache.get(unit_name)
    }
    
    /// Add a module to the cache
    pub fn cache_module(&mut self, module: Module) {
        self.cache.insert(module.name.clone(), module);
    }
    
    /// Clear the cache
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }
    
    /// Get all cached module names
    pub fn cached_modules(&self) -> Vec<String> {
        self.cache.keys().cloned().collect()
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
        metadata.modified()
            .map_err(|e| ModuleError::LoadError(unit_name.to_string(), e.to_string()))
    }
    
    /// Check if a cached module is up to date
    pub fn is_cache_valid(&self, unit_name: &str) -> bool {
        if let Some(cached) = self.cache.get(unit_name) {
            if let (Some(cached_time), Ok(file_time)) = 
                (cached.compile_time, self.get_unit_mtime(unit_name)) {
                return cached_time >= file_time;
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
    
    #[test]
    fn test_loader_creation() {
        let loader = ModuleLoader::new();
        assert_eq!(loader.search_paths.len(), 1);
        assert_eq!(loader.unit_extension, "pas");
    }
    
    #[test]
    fn test_add_search_path() {
        let mut loader = ModuleLoader::new();
        loader.add_search_path("/usr/lib/pascal");
        assert_eq!(loader.search_paths.len(), 2);
        
        // Adding same path again should not duplicate
        loader.add_search_path("/usr/lib/pascal");
        assert_eq!(loader.search_paths.len(), 2);
    }
    
    #[test]
    fn test_cache_operations() {
        let mut loader = ModuleLoader::new();
        assert!(!loader.is_cached("TestUnit"));
        assert_eq!(loader.cached_modules().len(), 0);
    }
}
