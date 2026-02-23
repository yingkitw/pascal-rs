//! Public API for the Pascal code formatter
//!
//! Provides high-level functions for formatting Pascal source code
//! with automatic configuration discovery and error handling.

use crate::ast::*;
use crate::formatter::config::*;
use crate::formatter::core::Formatter;
use crate::parser::Parser;
use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

pub mod config;
pub mod core;

// Re-export key types for convenience
pub use config::*;

/// Format Pascal source code to a string
///
/// # Arguments
///
/// * `source` - The Pascal source code to format
/// * `config` - Optional formatting configuration, will use defaults if None
///
/// # Examples
///
/// ```
/// use pascal::formatter::format_string;
///
/// let source = "program test;begin writeln('Hello');end.";
/// let formatted = format_string(source, None)?;
/// # Ok::<(), anyhow::Error>(())
/// ```
pub fn format_string(source: &str, config: Option<FormatConfig>) -> Result<String> {
    let config = config.unwrap_or_default();

    // Parse the source code
    let mut parser = Parser::new(source);
    let ast = parser
        .parse_program()
        .context("Failed to parse Pascal source code")?;

    // Format the AST
    let mut formatter = Formatter::new(config);
    formatter.format_program(&ast)
}

/// Format a Pascal source file
///
/// # Arguments
///
/// * `input_path` - Path to the input Pascal file
/// * `output_path` - Path to write the formatted output (None for stdout)
/// * `config` - Optional formatting configuration
///
/// # Examples
///
/// ```no_run
/// use pascal::formatter::format_file;
///
/// format_file("input.pas", Some("output.pas"), None).unwrap();
/// ```
pub fn format_file<P1, P2>(
    input_path: P1,
    output_path: Option<P2>,
    config: Option<FormatConfig>,
) -> Result<()>
where
    P1: AsRef<Path>,
    P2: AsRef<Path>,
{
    let input_path = input_path.as_ref();

    // Read the input file
    let source = std::fs::read_to_string(input_path)
        .with_context(|| format!("Failed to read file: {}", input_path.display()))?;

    // Find configuration file if none provided
    let config = match config {
        Some(c) => c,
        None => {
            let parent_dir = input_path.parent().unwrap_or_else(|| Path::new("."));
            find_config(parent_dir)?.unwrap_or_default()
        }
    };

    // Format the source
    let formatted = format_string(&source, Some(config))?;

    // Write output
    match output_path {
        Some(output_path) => {
            let output_path = output_path.as_ref();
            std::fs::write(output_path, formatted)
                .with_context(|| format!("Failed to write to file: {}", output_path.display()))?;

            println!("Formatted file written to: {}", output_path.display());
        }
        None => {
            println!("{}", formatted);
        }
    }

    Ok(())
}

/// Format a unit file specifically
///
/// Similar to `format_file` but parses as a unit rather than a program
pub fn format_unit_file<P1, P2>(
    input_path: P1,
    output_path: Option<P2>,
    config: Option<FormatConfig>,
) -> Result<()>
where
    P1: AsRef<Path>,
    P2: AsRef<Path>,
{
    let input_path = input_path.as_ref();

    // Read the input file
    let source = std::fs::read_to_string(input_path)
        .with_context(|| format!("Failed to read file: {}", input_path.display()))?;

    // Find configuration file if none provided
    let config = match config {
        Some(c) => c,
        None => {
            let parent_dir = input_path.parent().unwrap_or_else(|| Path::new("."));
            find_config(parent_dir)?.unwrap_or_default()
        }
    };

    // Parse as unit
    let mut parser = Parser::new(&source);
    let ast = parser.parse_unit().context("Failed to parse Pascal unit")?;

    // Format the AST
    let mut formatter = Formatter::new(config);
    let formatted = formatter.format_unit(&ast)?;

    // Write output
    match output_path {
        Some(output_path) => {
            let output_path = output_path.as_ref();
            std::fs::write(output_path, formatted)
                .with_context(|| format!("Failed to write to file: {}", output_path.display()))?;

            println!("Formatted unit written to: {}", output_path.display());
        }
        None => {
            println!("{}", formatted);
        }
    }

    Ok(())
}

/// Check if a file needs formatting
///
/// Returns true if the formatted version differs from the original
pub fn needs_formatting<P>(input_path: P, config: Option<FormatConfig>) -> Result<bool>
where
    P: AsRef<Path>,
{
    let input_path = input_path.as_ref();

    // Read the original file
    let original = std::fs::read_to_string(input_path)
        .with_context(|| format!("Failed to read file: {}", input_path.display()))?;

    // Format it
    let formatted = format_string(&original, config)?;

    // Compare (normalize line endings)
    let normalized_original = original.replace("\r\n", "\n").replace('\r', "\n");
    let normalized_formatted = formatted.replace("\r\n", "\n").replace('\r', "\n");

    Ok(normalized_original != normalized_formatted)
}

/// Create a default configuration file
///
/// # Arguments
///
/// * `path` - Path where to create the configuration file
///
/// # Examples
///
/// ```
/// use pascal::formatter::create_default_config;
///
/// create_default_config("pascalfmt.toml")?;
/// # Ok::<(), anyhow::Error>(())
/// ```
pub fn create_default_config<P>(path: P) -> Result<()>
where
    P: AsRef<Path>,
{
    let path_ref = path.as_ref();
    let config = FormatConfig::default();
    save_config(&config, path_ref).context("Failed to save default configuration")?;

    println!("Created default configuration file: {}", path_ref.display());
    Ok(())
}

/// Batch format multiple files or directories
///
/// # Arguments
///
/// * `paths` - List of files or directories to format
/// * `recursive` - Whether to search directories recursively
/// * `config` - Optional formatting configuration
/// * `check_only` - If true, only check if formatting is needed
///
/// # Returns
///
/// Number of files that needed formatting (0 if all were already formatted)
pub fn format_batch(
    paths: &[impl AsRef<Path>],
    recursive: bool,
    config: Option<FormatConfig>,
    check_only: bool,
) -> Result<usize> {
    let mut files_needing_formatting = 0;

    // Collect all Pascal files
    let mut pascal_files = Vec::new();

    for path in paths {
        let path = path.as_ref();

        if path.is_file() {
            if is_pascal_file(path) {
                pascal_files.push(path.to_path_buf());
            }
        } else if path.is_dir() {
            if recursive {
                collect_pascal_files_recursive(path, &mut pascal_files)?;
            } else {
                collect_pascal_files(path, &mut pascal_files)?;
            }
        }
    }

    if pascal_files.is_empty() {
        println!("No Pascal files found to format.");
        return Ok(0);
    }

    // Process each file
    for file in &pascal_files {
        if check_only {
            if needs_formatting(file, config.clone())? {
                files_needing_formatting += 1;
                println!("Would format: {}", file.display());
            }
        } else {
            if needs_formatting(file, config.clone())? {
                files_needing_formatting += 1;
                format_file(file, None as Option<&Path>, config.clone())?;
                println!("Formatted: {}", file.display());
            } else {
                println!("Already formatted: {}", file.display());
            }
        }
    }

    Ok(files_needing_formatting)
}

/// Check if a file is a Pascal source file
fn is_pascal_file(path: &Path) -> bool {
    if let Some(extension) = path.extension() {
        if let Some(ext_str) = extension.to_str() {
            matches!(ext_str.to_lowercase().as_str(), "pas" | "pp" | "p")
        } else {
            false
        }
    } else {
        false
    }
}

/// Collect Pascal files from a directory (non-recursive)
fn collect_pascal_files(dir: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    let entries = std::fs::read_dir(dir)
        .with_context(|| format!("Failed to read directory: {}", dir.display()))?;

    for entry in entries {
        let entry = entry.with_context(|| "Failed to read directory entry")?;
        let path = entry.path();

        if path.is_file() && is_pascal_file(&path) {
            files.push(path);
        }
    }

    Ok(())
}

/// Collect Pascal files from a directory (recursive)
fn collect_pascal_files_recursive(dir: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    collect_pascal_files(dir, files)?;

    let entries = std::fs::read_dir(dir)
        .with_context(|| format!("Failed to read directory: {}", dir.display()))?;

    for entry in entries {
        let entry = entry.with_context(|| "Failed to read directory entry")?;
        let path = entry.path();

        if path.is_dir() {
            collect_pascal_files_recursive(&path, files)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_format_string() {
        let source = "program Test;begin writeln('Hello');end.";
        let result = format_string(source, None);
        assert!(result.is_ok());

        let formatted = result.unwrap();
        assert!(formatted.contains("program Test"));
        assert!(formatted.contains("begin"));
        assert!(formatted.contains("writeln"));
        assert!(formatted.contains("end."));
    }

    #[test]
    fn test_needs_formatting() {
        // Unformatted source
        let source = "program Test;begin writeln('Hello');end.";

        let mut temp_file = NamedTempFile::new().unwrap();
        temp_file.write_all(source.as_bytes()).unwrap();

        // This should need formatting
        let needs_fmt = needs_formatting(temp_file.path(), None).unwrap();
        assert!(needs_fmt);
    }

    #[test]
    fn test_is_pascal_file() {
        assert!(is_pascal_file(Path::new("test.pas")));
        assert!(is_pascal_file(Path::new("test.pp")));
        assert!(is_pascal_file(Path::new("test.p")));
        assert!(!is_pascal_file(Path::new("test.txt")));
        assert!(!is_pascal_file(Path::new("test")));
    }

    #[test]
    fn test_create_default_config() {
        let temp_file = NamedTempFile::new().unwrap();
        create_default_config(temp_file.path()).unwrap();

        assert!(temp_file.path().exists());

        let loaded_config = load_config(temp_file.path()).unwrap();
        assert_eq!(loaded_config.indent_width, 2);
        assert_eq!(loaded_config.max_line_length, 120);
    }
}
