//! Module loader for loading and caching Pascal units

use crate::ppu::PpuFile;
use crate::{Module, ModuleError, ModuleResult};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

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
            search_paths: vec![
                PathBuf::from("."),
                PathBuf::from("/Users/yingkitw/Desktop/productivity project/poscal-rs/stdlib"),
            ],
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
        metadata
            .modified()
            .map_err(|e| ModuleError::LoadError(unit_name.to_string(), e.to_string()))
    }

    /// Check if a cached module is up to date
    pub fn is_cache_valid(&self, unit_name: &str) -> bool {
        if let Some(cached) = self.cache.get(unit_name) {
            if let (Some(cached_time), Ok(file_time)) =
                (cached.compile_time, self.get_unit_mtime(unit_name))
            {
                return cached_time >= file_time;
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
        let ppu = PpuFile::read_from_file(&ppu_path)
            .map_err(|e| ModuleError::LoadError(unit_name.to_string(), e.to_string()))?;

        // Verify checksums
        if !ppu.verify_checksums() {
            return Err(ModuleError::LoadError(
                unit_name.to_string(),
                "PPU checksum verification failed".to_string(),
            ));
        }

        Ok(ppu.unit)
    }

    /// Save a unit to a PPU file
    pub fn save_to_ppu(
        &self,
        unit: &crate::ast::Unit,
        output_dir: Option<&Path>,
    ) -> ModuleResult<PathBuf> {
        let mut ppu = PpuFile::new(unit.clone());

        // Determine output path
        let dir = output_dir.unwrap_or_else(|| Path::new("."));
        let filename = format!("{}.ppu", unit.name.to_lowercase());
        let ppu_path = dir.join(filename);

        // Write PPU file
        ppu.write_to_file(&ppu_path)
            .map_err(|e| ModuleError::LoadError(unit.name.clone(), e.to_string()))?;

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
}
