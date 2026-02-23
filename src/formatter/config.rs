//! Pascal code formatter with configurable style rules
//!
//! Provides automatic code formatting with customizable indentation,
//! spacing, line breaks, and other style preferences.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Configuration for code formatting preferences
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FormatConfig {
    /// Indentation style
    pub indent_style: IndentStyle,

    /// Width of indentation (spaces or tabs count)
    pub indent_width: usize,

    /// Maximum line length before wrapping
    pub max_line_length: usize,

    /// Spacing around operators
    pub operator_spacing: OperatorSpacing,

    /// Spacing around parentheses and brackets
    pub bracket_spacing: BracketSpacing,

    /// Line ending style
    pub line_ending: LineEnding,

    /// Whether to use block comments vs. line comments
    pub comment_style: CommentStyle,

    /// Trailing comma behavior
    pub trailing_comma: TrailingComma,

    /// Whether to align consecutive declarations
    pub align_declarations: bool,

    /// Whether to align consecutive assignments
    pub align_assignments: bool,

    /// Whether to format case statements with extra spacing
    pub case_spacing: CaseSpacing,

    /// Whether to keep blank lines between sections
    pub blank_lines: BlankLines,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent_style: IndentStyle::Spaces,
            indent_width: 2,
            max_line_length: 120,
            operator_spacing: OperatorSpacing::default(),
            bracket_spacing: BracketSpacing::default(),
            line_ending: LineEnding::Lf,
            comment_style: CommentStyle::Line,
            trailing_comma: TrailingComma::Never,
            align_declarations: false,
            align_assignments: false,
            case_spacing: CaseSpacing::default(),
            blank_lines: BlankLines::default(),
        }
    }
}

/// Indentation style
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IndentStyle {
    /// Use spaces for indentation
    Spaces,
    /// Use tabs for indentation
    Tabs,
}

/// Spacing configuration around operators
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OperatorSpacing {
    /// Space around assignment operators (:=, =)
    pub assignment: bool,
    /// Space around arithmetic operators (+, -, *, /)
    pub arithmetic: bool,
    /// Space around comparison operators (=, <>, <, >, <=, >=)
    pub comparison: bool,
    /// Space around logical operators (and, or, not, xor)
    pub logical: bool,
    /// Space around colon (:) in type declarations
    pub colon: bool,
    /// Space around semicolon (;)
    pub semicolon: bool,
    /// Space around comma (,)
    pub comma: bool,
}

impl Default for OperatorSpacing {
    fn default() -> Self {
        Self {
            assignment: true,
            arithmetic: true,
            comparison: true,
            logical: true,
            colon: false,
            semicolon: true,
            comma: true,
        }
    }
}

/// Spacing configuration around brackets and parentheses
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BracketSpacing {
    /// Space after opening parenthesis
    pub paren_open: bool,
    /// Space before closing parenthesis
    pub paren_close: bool,
    /// Space after opening bracket
    pub bracket_open: bool,
    /// Space before closing bracket
    pub bracket_close: bool,
    /// Space after opening brace
    pub brace_open: bool,
    /// Space before closing brace
    pub brace_close: bool,
}

impl Default for BracketSpacing {
    fn default() -> Self {
        Self {
            paren_open: false,
            paren_close: false,
            bracket_open: false,
            bracket_close: false,
            brace_open: true,
            brace_close: true,
        }
    }
}

/// Line ending style
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LineEnding {
    /// LF (\n) - Unix/macOS style
    Lf,
    /// CRLF (\r\n) - Windows style
    CrLf,
    /// CR (\r) - Classic Mac style
    Cr,
}

/// Comment style preference
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CommentStyle {
    /// Prefer line comments (//)
    Line,
    /// Prefer block comments ({ })
    Block,
    /// Mixed based on context
    Mixed,
}

/// Trailing comma behavior
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TrailingComma {
    /// Never add trailing commas
    Never,
    /// Always add trailing commas
    Always,
    /// Add trailing commas in multi-line constructs only
    MultilineOnly,
}

/// Case statement spacing configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CaseSpacing {
    /// Empty line before each case
    pub before_case: bool,
    /// Empty line after each case
    pub after_case: bool,
    /// Indent case labels
    pub indent_labels: bool,
}

impl Default for CaseSpacing {
    fn default() -> Self {
        Self {
            before_case: false,
            after_case: true,
            indent_labels: false,
        }
    }
}

/// Blank line configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BlankLines {
    /// Lines between top-level declarations
    pub between_declarations: usize,
    /// Lines between procedures/functions
    pub between_routines: usize,
    /// Lines before begin blocks
    pub before_begin: usize,
    /// Lines after end statements
    pub after_end: usize,
    /// Lines before case statements
    pub before_case: usize,
    /// Lines after case statements
    pub after_case: usize,
    /// Maximum consecutive blank lines
    pub max_consecutive: usize,
}

impl Default for BlankLines {
    fn default() -> Self {
        Self {
            between_declarations: 1,
            between_routines: 2,
            before_begin: 0,
            after_end: 1,
            before_case: 1,
            after_case: 1,
            max_consecutive: 1,
        }
    }
}

/// Load configuration from a file
pub fn load_config<P: AsRef<Path>>(path: P) -> Result<FormatConfig> {
    let content = std::fs::read_to_string(path)?;
    let config: FormatConfig = toml::from_str(&content)?;
    Ok(config)
}

/// Save configuration to a file
pub fn save_config<P: AsRef<Path>>(config: &FormatConfig, path: P) -> Result<()> {
    let content = toml::to_string_pretty(config)?;
    std::fs::write(path, content)?;
    Ok(())
}

/// Find and load configuration file, searching up the directory tree
pub fn find_config<P: AsRef<Path>>(start_dir: P) -> Result<Option<FormatConfig>> {
    let mut current_dir = start_dir.as_ref();

    loop {
        let config_path = current_dir.join("pascalfmt.toml");
        if config_path.exists() {
            return Ok(Some(load_config(config_path)?));
        }

        let config_path = current_dir.join(".pascalfmt.toml");
        if config_path.exists() {
            return Ok(Some(load_config(config_path)?));
        }

        match current_dir.parent() {
            Some(parent) => current_dir = parent,
            None => break,
        }
    }

    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_default_config() {
        let config = FormatConfig::default();
        assert_eq!(config.indent_style, IndentStyle::Spaces);
        assert_eq!(config.indent_width, 2);
        assert_eq!(config.max_line_length, 120);
    }

    #[test]
    fn test_save_load_config() {
        let dir = tempdir().unwrap();
        let config_path = dir.path().join("config.toml");

        let original = FormatConfig {
            indent_width: 4,
            max_line_length: 100,
            ..Default::default()
        };

        save_config(&original, &config_path).unwrap();
        let loaded = load_config(&config_path).unwrap();

        assert_eq!(original, loaded);
    }

    #[test]
    fn test_find_config_not_found() {
        let dir = tempdir().unwrap();
        let config = find_config(dir.path()).unwrap();
        assert!(config.is_none());
    }
}
