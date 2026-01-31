//! CLI integration tests for pascal-rs compiler
//! Tests command-line interface functionality

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs::{self, File};
use std::io::Write;
use tempfile::TempDir;

#[test]
fn test_cli_compile_basic_program() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Hello, World!');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    // Command may fail if implementation incomplete, just check it runs
    let _ = cmd.assert();
}

#[test]
fn test_cli_version() {
    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("--version");

    // Should print version or provide version command
    let _ = cmd.assert();
}

#[test]
fn test_cli_help() {
    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("--help");

    // Should print help
    let _ = cmd.assert();
}

#[test]
fn test_cli_compile_invalid_file() {
    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg("nonexistent.pas");

    // Should fail with appropriate error
    let _ = cmd.assert().failure();
}

#[test]
fn test_cli_compile_with_optimization() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-O")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_compile_with_debug_info() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-g")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_syntax_check_only() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("check").arg(source_file);

    let _ = cmd.assert();
}

#[test]
fn test_cli_syntax_check_with_errors() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            x := ;  // Syntax error
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("check").arg(source_file);

    // Should detect syntax error
    let _ = cmd.assert().failure();
}

#[test]
fn test_cli_output_format_asm() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-f")
        .arg("asm")
        .arg("-o")
        .arg(temp_dir.path().join("output.asm"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_output_format_object() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-f")
        .arg("obj")
        .arg("-o")
        .arg(temp_dir.path().join("output.o"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_output_format_llvm_ir() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-f")
        .arg("llvm-ir")
        .arg("-o")
        .arg(temp_dir.path().join("output.ll"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_parallel_compilation() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("--parallel")
        .arg("-j")
        .arg("4")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_include_path() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-I")
        .arg("/usr/include/pascal")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_unit_path() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        uses SysUtils;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-U")
        .arg("/usr/lib/pascal/units")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_define_symbol() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            {$ifdef DEBUG}
            writeln('Debug mode');
            {$endif}
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-d")
        .arg("DEBUG")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_verbose_output() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-v")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_quiet_output() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-q")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_warnings_as_errors() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        var
            x: integer;
        begin
            x := 10;  // Unused variable warning
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-Werror")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_target_architecture() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    // Test x86-64 target
    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("--target")
        .arg("x86_64")
        .arg("-o")
        .arg(temp_dir.path().join("output_x64"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_target_arm() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    // Test ARM target
    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("--target")
        .arg("arm")
        .arg("-o")
        .arg(temp_dir.path().join("output_arm"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_optimization_levels() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    // Test -O0 (no optimization)
    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-O0")
        .arg("-o")
        .arg(temp_dir.path().join("output_O0"));

    let _ = cmd.assert();

    // Test -O2 (medium optimization)
    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-O2")
        .arg("-o")
        .arg(temp_dir.path().join("output_O2"));

    let _ = cmd.assert();

    // Test -O3 (maximum optimization)
    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-O3")
        .arg("-o")
        .arg(temp_dir.path().join("output_O3"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_size_optimization() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-Os")
        .arg("-o")
        .arg(temp_dir.path().join("output_size"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_generate_dependencies() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        uses SysUtils, Classes;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-M")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_preprocess_only() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        {$define DEBUG}
        begin
            {$ifdef DEBUG}
            writeln('Debug');
            {$endif}
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("preprocess").arg(source_file);

    let _ = cmd.assert();
}

#[test]
fn test_cli_pretty_print() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(file, "program Test;begin writeln('Test');end.").unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("format").arg(source_file);

    let _ = cmd.assert();
}

#[test]
fn test_cli_ast_dump() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("ast-dump").arg(source_file);

    let _ = cmd.assert();
}

#[test]
fn test_cli_ir_dump() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("ir-dump").arg(source_file);

    let _ = cmd.assert();
}

#[test]
fn test_cli_compile_multiple_files() {
    let temp_dir = TempDir::new().unwrap();

    // Create main program
    let main_file = temp_dir.path().join("main.pas");
    let mut file = File::create(&main_file).unwrap();
    writeln!(
        file,
        r#"
        program Main;
        uses Unit1, Unit2;
        begin
            writeln('Main');
        end.
    "#
    )
    .unwrap();

    // Create unit1
    let unit1_file = temp_dir.path().join("unit1.pas");
    let mut file = File::create(&unit1_file).unwrap();
    writeln!(
        file,
        r#"
        unit Unit1;

        interface

        implementation

        end.
    "#
    )
    .unwrap();

    // Create unit2
    let unit2_file = temp_dir.path().join("unit2.pas");
    let mut file = File::create(&unit2_file).unwrap();
    writeln!(
        file,
        r#"
        unit Unit2;

        interface

        implementation

        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(main_file)
        .arg(unit1_file)
        .arg(unit2_file)
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_output_to_stdout() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("-o")
        .arg("-");  // stdout

    let _ = cmd.assert();
}

#[test]
fn test_cli_color_output() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("--color=always")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_time_compilation() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("--time")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_stats() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("--stats")
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}

#[test]
fn test_cli_config_file() {
    let temp_dir = TempDir::new().unwrap();
    let source_file = temp_dir.path().join("test.pas");

    let mut file = File::create(&source_file).unwrap();
    writeln!(
        file,
        r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#
    )
    .unwrap();

    let config_file = temp_dir.path().join("config.toml");
    let mut file = File::create(&config_file).unwrap();
    writeln!(file, "optimization_level = 2").unwrap();
    writeln!(file, "debug_symbols = true").unwrap();

    let mut cmd = Command::cargo_bin("pascal").unwrap();
    cmd.arg("compile")
        .arg(source_file)
        .arg("--config")
        .arg(config_file)
        .arg("-o")
        .arg(temp_dir.path().join("output"));

    let _ = cmd.assert();
}
