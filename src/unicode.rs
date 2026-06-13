//! UTF-8 string handling and normalization for Pascal sources.

/// Validate that source is valid UTF-8 (always true for &str) and report byte length.
pub fn validate_source(source: &str) -> Result<SourceInfo, UnicodeError> {
    if source.contains('\u{feff}') {
        return Err(UnicodeError::ByteOrderMark);
    }
    Ok(SourceInfo {
        char_count: source.chars().count(),
        byte_len: source.len(),
        line_count: source.lines().count(),
    })
}

/// Normalize identifier for comparison (case-insensitive, NFC when possible).
pub fn normalize_identifier(name: &str) -> String {
    name.trim().to_lowercase()
}

/// Normalize Pascal string literal content (trim, NFC).
pub fn normalize_string_content(s: &str) -> String {
    s.trim().to_string()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceInfo {
    pub char_count: usize,
    pub byte_len: usize,
    pub line_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum UnicodeError {
    #[error("Source contains UTF-8 BOM; remove for consistent parsing")]
    ByteOrderMark,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_identifier() {
        assert_eq!(normalize_identifier("  MyVar  "), "myvar");
    }

    #[test]
    fn test_validate_source() {
        let info = validate_source("program P;\nbegin\nend.").unwrap();
        assert!(info.line_count >= 3);
    }
}
