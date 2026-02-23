//! Integration tests for the Pascal code formatter

use pascal::formatter;
use std::fs;
use std::path::PathBuf;
use tempfile::tempdir;

#[test]
fn test_format_simple_program() {
    let source = r#"program Test;begin writeln('Hello');end."#;
    let result = formatter::format_string(source, None);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("program Test"));
    assert!(formatted.contains("begin"));
    assert!(formatted.contains("writeln"));
    assert!(formatted.contains("end."));
}

#[test]
fn test_format_program_with_uses() {
    let source = r#"program Test;uses SysUtils,Classes;begin writeln('Hello');end."#;
    let result = formatter::format_string(source, None);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("program Test"));
    assert!(formatted.contains("uses SysUtils, Classes;"));
    assert!(formatted.contains("begin"));
    assert!(formatted.contains("writeln"));
    assert!(formatted.contains("end."));
}

#[test]
fn test_format_with_variables() {
    let source = r#"program Test;var x:Integer; s:String;begin x:=42; s:='Hello'; writeln(s);end."#;
    let result = formatter::format_string(source, None);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("var"));
    assert!(formatted.contains("x: Integer;"));
    assert!(formatted.contains("s: String;"));
    assert!(formatted.contains("x := 42;"));
    assert!(formatted.contains("s := 'Hello';"));
}

#[test]
fn test_format_procedure() {
    let source = r#"program Test;procedure TestProc;begin writeln('Test');end;begin TestProc;end."#;
    let result = formatter::format_string(source, None);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("procedure TestProc;"));
    assert!(formatted.contains("begin"));
    assert!(formatted.contains("writeln"));
    assert!(formatted.contains("end;"));
}

#[test]
fn test_format_function() {
    let source = r#"program Test;function Add(a,b:Integer):Integer;begin Result:=a+b;end;begin writeln(Add(2,3));end."#;
    let result = formatter::format_string(source, None);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("function Add(a, b: Integer): Integer;"));
    assert!(formatted.contains("Result := a + b;"));
}

#[test]
fn test_format_if_statement() {
    let source =
        r#"program Test;begin if x>0 then writeln('positive') else writeln('non-positive');end."#;
    let result = formatter::format_string(source, None);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("if x > 0 then"));
    assert!(formatted.contains("writeln('positive')"));
    assert!(formatted.contains("else"));
    assert!(formatted.contains("writeln('non-positive')"));
}

#[test]
fn test_format_while_loop() {
    let source = r#"program Test;begin while i<10 do begin writeln(i); i:=i+1;end;end."#;
    let result = formatter::format_string(source, None);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("while i < 10 do"));
    assert!(formatted.contains("writeln(i)"));
    assert!(formatted.contains("i := i + 1;"));
}

#[test]
fn test_format_for_loop() {
    let source = r#"program Test;begin for i:=1 to 10 do writeln(i);end."#;
    let result = formatter::format_string(source, None);
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("for i := 1 to 10 do"));
    assert!(formatted.contains("writeln(i)"));
}

#[test]
fn test_format_with_config() {
    let source = r#"program Test;begin writeln('Hello');end."#;

    let config = pascal::formatter::config::FormatConfig {
        indent_style: pascal::formatter::config::IndentStyle::Tabs,
        indent_width: 4,
        max_line_length: 80,
        ..Default::default()
    };

    let result = formatter::format_string(source, Some(config));
    assert!(result.is_ok());

    let formatted = result.unwrap();
    assert!(formatted.contains("\t")); // Should have tabs
}

#[test]
fn test_needs_formatting() {
    let source = r#"program Test;begin writeln('Hello');end."#;

    // Create a temporary file
    let temp_dir = tempdir().unwrap();
    let file_path = temp_dir.path().join("test.pas");
    fs::write(&file_path, source).unwrap();

    // The file should need formatting
    let needs_fmt = formatter::needs_formatting(&file_path, None).unwrap();
    assert!(needs_fmt);
}

#[test]
fn test_create_default_config() {
    let temp_dir = tempdir().unwrap();
    let config_path = temp_dir.path().join("pascalfmt.toml");

    formatter::create_default_config(&config_path).unwrap();

    assert!(config_path.exists());

    let loaded_config = pascal::formatter::config::load_config(&config_path).unwrap();
    assert_eq!(loaded_config.indent_width, 2);
    assert_eq!(loaded_config.max_line_length, 120);
}

#[test]
fn test_format_batch() {
    let temp_dir = tempdir().unwrap();

    // Create test files
    let file1 = temp_dir.path().join("test1.pas");
    let file2 = temp_dir.path().join("test2.pas");

    fs::write(&file1, "program Test1;begin writeln('Hello1');end.").unwrap();
    fs::write(&file2, "program Test2;begin writeln('Hello2');end.").unwrap();

    let files_formatted = formatter::format_batch(&[file1, file2], false, None, false).unwrap();

    assert_eq!(files_formatted, 2);
}

#[test]
fn test_format_batch_check_only() {
    let temp_dir = tempdir().unwrap();

    // Create test file
    let file1 = temp_dir.path().join("test1.pas");
    fs::write(&file1, "program Test1;begin writeln('Hello1');end.").unwrap();

    let files_formatted = formatter::format_batch(
        &[&file1],
        false,
        None,
        true, // check_only
    )
    .unwrap();

    assert_eq!(files_formatted, 1);
}

#[test]
fn test_format_unit() {
    let source = r#"unit TestUnit;interface procedure TestProc;implementation procedure TestProc;begin writeln('Test');end;end."#;

    let temp_dir = tempdir().unwrap();
    let unit_file = temp_dir.path().join("testunit.pas");
    fs::write(&unit_file, source).unwrap();

    let result = formatter::format_unit_file(&unit_file, None as Option<PathBuf>, None);
    assert!(result.is_ok());
}

#[test]
fn test_is_pascal_file() {
    use pascal::formatter::is_pascal_file;
    use std::path::Path;

    assert!(is_pascal_file(Path::new("test.pas")));
    assert!(is_pascal_file(Path::new("test.pp")));
    assert!(is_pascal_file(Path::new("test.p")));
    assert!(!is_pascal_file(Path::new("test.txt")));
    assert!(!is_pascal_file(Path::new("test")));
}

#[test]
fn test_find_config_not_found() {
    let temp_dir = tempdir().unwrap();
    let config = formatter::find_config(temp_dir.path()).unwrap();
    assert!(config.is_none());
}

#[test]
fn test_find_config_found() {
    let temp_dir = tempdir().unwrap();
    let config_path = temp_dir.path().join("pascalfmt.toml");

    // Create config file
    formatter::create_default_config(&config_path).unwrap();

    let config = formatter::find_config(temp_dir.path()).unwrap();
    assert!(config.is_some());
}

#[test]
fn test_format_with_different_indentation() {
    let source = r#"program Test;var x:Integer;begin x:=1;end."#;

    // Test 2-space indentation
    let config = pascal::formatter::config::FormatConfig {
        indent_style: pascal::formatter::config::IndentStyle::Spaces,
        indent_width: 2,
        ..Default::default()
    };

    let result = formatter::format_string(source, Some(config));
    assert!(result.is_ok());

    // Test 4-space indentation
    let config = pascal::formatter::config::FormatConfig {
        indent_style: pascal::formatter::config::IndentStyle::Spaces,
        indent_width: 4,
        ..Default::default()
    };

    let result = formatter::format_string(source, Some(config));
    assert!(result.is_ok());

    // Test tab indentation
    let config = pascal::formatter::config::FormatConfig {
        indent_style: pascal::formatter::config::IndentStyle::Tabs,
        indent_width: 1,
        ..Default::default()
    };

    let result = formatter::format_string(source, Some(config));
    assert!(result.is_ok());
}
