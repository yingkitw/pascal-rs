//! Incremental compilation cache — skip unchanged units on rebuild.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct IncrementalCache {
    pub entries: HashMap<String, CacheEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheEntry {
    pub source_checksum: String,
    pub compiled_at: u64,
}

impl IncrementalCache {
    pub fn path(project_root: &Path) -> PathBuf {
        project_root.join(".pascal").join("build-cache.json")
    }

    pub fn load(project_root: &Path) -> Self {
        let path = Self::path(project_root);
        if path.exists()
            && let Ok(content) = std::fs::read_to_string(&path)
                && let Ok(cache) = serde_json::from_str(&content) {
                    return cache;
                }
        Self::default()
    }

    pub fn save(&self, project_root: &Path) -> Result<()> {
        let path = Self::path(project_root);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let content = serde_json::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }

    pub fn needs_recompile(&self, unit_name: &str, source: &str) -> bool {
        let checksum = checksum(source);
        match self.entries.get(unit_name) {
            Some(entry) => entry.source_checksum != checksum,
            None => true,
        }
    }

    pub fn mark_compiled(&mut self, unit_name: &str, source: &str) {
        self.entries.insert(
            unit_name.to_string(),
            CacheEntry {
                source_checksum: checksum(source),
                compiled_at: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .map(|d| d.as_secs())
                    .unwrap_or(0),
            },
        );
    }
}

fn checksum(source: &str) -> String {
    format!("{:x}", Sha256::digest(source.as_bytes()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_incremental_cache() {
        let dir = tempfile::tempdir().unwrap();
        let mut cache = IncrementalCache::default();
        assert!(cache.needs_recompile("unit", "program X; begin end."));
        cache.mark_compiled("unit", "program X; begin end.");
        assert!(!cache.needs_recompile("unit", "program X; begin end."));
        assert!(cache.needs_recompile("unit", "program Y; begin end."));
        cache.save(dir.path()).unwrap();
        let loaded = IncrementalCache::load(dir.path());
        assert!(!loaded.needs_recompile("unit", "program X; begin end."));
    }
}
