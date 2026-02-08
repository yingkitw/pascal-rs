//! PPU (Precompiled Pascal Unit) file format support

use crate::ast::{Module, ModuleError, ModuleResult};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

/// PPU file format for precompiled units
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PpuFile {
    pub version: u32,
    pub module: Module,
    pub checksum: u64,
}

impl PpuFile {
    /// Create a new PPU file
    pub fn new(module: Module) -> Self {
        let checksum = Self::calculate_checksum(&module);
        Self {
            version: 1,
            module,
            checksum,
        }
    }

    /// Load a PPU file from disk
    pub fn load<P: AsRef<Path>>(path: P) -> ModuleResult<Self> {
        let data = fs::read(path.as_ref()).map_err(|e| {
            ModuleError::LoadError(path.as_ref().display().to_string(), e.to_string())
        })?;

        bincode::deserialize(&data)
            .map_err(|e| ModuleError::LoadError(path.as_ref().display().to_string(), e.to_string()))
    }

    /// Save a PPU file to disk
    pub fn save<P: AsRef<Path>>(&self, path: P) -> ModuleResult<()> {
        let data = bincode::serialize(self).map_err(|e| {
            ModuleError::LoadError(path.as_ref().display().to_string(), e.to_string())
        })?;

        fs::write(path.as_ref(), data)
            .map_err(|e| ModuleError::LoadError(path.as_ref().display().to_string(), e.to_string()))
    }

    /// Calculate checksum for a module
    fn calculate_checksum(module: &Module) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        module.name.hash(&mut hasher);
        hasher.finish()
    }

    /// Verify checksum
    pub fn verify_checksum(&self) -> bool {
        self.checksum == Self::calculate_checksum(&self.module)
    }
}
