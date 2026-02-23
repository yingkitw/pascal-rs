//! Source maps for debugging generated code
//!
//! Maps positions in generated output (e.g. assembly) back to source locations.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// A single mapping from generated output to source
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceMapping {
    /// Line (1-based) in generated output
    pub generated_line: u32,
    /// Source file path
    pub source_file: PathBuf,
    /// Line (1-based) in source
    pub source_line: u32,
    /// Column (1-based) in source, if known
    pub source_column: Option<u32>,
}

/// Source map for a compilation unit
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceMap {
    /// Ordered list of mappings
    pub mappings: Vec<SourceMapping>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            mappings: Vec::new(),
        }
    }

    /// Add a mapping
    pub fn add(&mut self, generated_line: u32, source_file: PathBuf, source_line: u32, source_column: Option<u32>) {
        self.mappings.push(SourceMapping {
            generated_line,
            source_file,
            source_line,
            source_column,
        });
    }

    /// Look up source location for a generated line
    pub fn lookup(&self, generated_line: u32) -> Option<&SourceMapping> {
        self.mappings
            .iter()
            .filter(|m| m.generated_line <= generated_line)
            .max_by_key(|m| m.generated_line)
    }

    /// Write to JSON file (Source Map v3 format can be extended later)
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Load from JSON
    pub fn from_json(s: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_map_add_lookup() {
        let mut map = SourceMap::new();
        map.add(10, PathBuf::from("main.pas"), 1, Some(1));
        map.add(20, PathBuf::from("main.pas"), 5, None);

        let m = map.lookup(15).unwrap();
        assert_eq!(m.generated_line, 10);
        assert_eq!(m.source_line, 1);

        let m = map.lookup(25).unwrap();
        assert_eq!(m.generated_line, 20);
        assert_eq!(m.source_line, 5);

        assert!(map.lookup(5).is_none());
    }

    #[test]
    fn test_source_map_json_roundtrip() {
        let mut map = SourceMap::new();
        map.add(1, PathBuf::from("x.pas"), 10, Some(5));
        let json = map.to_json().unwrap();
        let loaded = SourceMap::from_json(&json).unwrap();
        assert_eq!(loaded.mappings.len(), 1);
        assert_eq!(loaded.mappings[0].source_line, 10);
    }
}

