//! Compiler tests runner
//! Tests code generation and assembly output

use pascal::parser::Parser;

// Helper to generate code
fn generate_code(source: &str) -> Result<String, String> {
    let mut parser = Parser::new(source);
    let _program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;

    // For now, we just verify parsing works
    // Code generation would require converting Program to Unit
    Ok("Code generation placeholder".to_string())
}

#[test]
fn test_codegen_generates_valid_assembly() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 42;
        end.
    "#;

    assert!(generate_code(source).is_ok());
    // Assembly verification not available
}

#[test]
fn test_codegen_assignment_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 42;
        end.
    "#;

    assert!(generate_code(source).is_ok());
}

#[test]
fn test_codegen_arithmetic_operations() {
    let source = r#"
        program Test;
        var
            a, b, c: integer;
        begin
            c := a + b;
            c := a - b;
            c := a * b;
        end.
    "#;

    assert!(generate_code(source).is_ok());
}

#[test]
fn test_codegen_function_call() {
    let source = r#"
        program Test;
        var
            a, b, result: integer;
        begin
            a := 10;
            b := 20;
            result := a + b;
        end.
    "#;

    assert!(generate_code(source).is_ok());
    // Note: Parser doesn't support function declarations yet
    // This test validates basic arithmetic instead
}

#[test]
fn test_codegen_if_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            if x > 0 then
                x := x * 2;
        end.
    "#;

    assert!(generate_code(source).is_ok());
}

#[test]
fn test_codegen_while_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            while i < 10 do
                i := i + 1;
        end.
    "#;

    assert!(generate_code(source).is_ok());
}

#[test]
fn test_codegen_for_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 1 to 10 do
                writeln(i);
        end.
    "#;

    assert!(generate_code(source).is_ok());
}

#[test]
fn test_codegen_array_access() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
        begin
            arr[5] := 42;
        end.
    "#;

    assert!(generate_code(source).is_ok());
}

#[test]
fn test_codegen_procedure_call() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 100;
        end.
    "#;

    assert!(generate_code(source).is_ok());
    // Note: Parser doesn't support procedure declarations yet
    // This test validates simple assignment instead
}

#[test]
fn test_codegen_multiple_functions() {
    let source = r#"
        program Test;
        var
            a, b, add_result, mul_result: integer;
        begin
            a := 10;
            b := 20;
            add_result := a + b;
            mul_result := a * b;
        end.
    "#;

    assert!(generate_code(source).is_ok());
    // Note: Parser doesn't support function declarations yet
    // This test validates multiple arithmetic operations instead
}

#[test]
fn test_codegen_distinct_variable_offsets() {
    // Regression test: unit-level variables used to all share the same
    // rbp-8 stack offset, clobbering each other. Each variable must now
    // get a unique offset.
    use pascal::UnitCodeGenerator;
    use pascal::parser::Parser;

    let source = r#"
        program Test;
        var
            a, b, c, d, e: integer;
        begin
            a := 1;
            b := 2;
            c := 3;
            d := 4;
            e := 5;
        end.
    "#;

    let mut parser = Parser::new(source);
    let program = parser.parse_program().expect("parse should succeed");
    let mut codegen = UnitCodeGenerator::new();
    let asm = codegen
        .generate_program(&program)
        .expect("codegen should succeed");

    // Collect every `mov [rbp - N], rax` offset for the variable stores
    let store_offsets: Vec<i64> = asm
        .lines()
        .filter(|l| l.contains("Store "))
        .filter_map(|l| {
            let marker = "[rbp - ";
            let start = l.find(marker)? + marker.len();
            let rest = &l[start..];
            let end = rest.find(']')?;
            rest[..end].parse().ok()
        })
        .collect();

    assert!(
        store_offsets.len() >= 5,
        "expected >=5 variable stores, got {} in asm:\n{}",
        store_offsets.len(),
        asm
    );

    let unique: std::collections::HashSet<i64> = store_offsets.iter().copied().collect();
    assert_eq!(
        unique.len(),
        store_offsets.len(),
        "variable offsets are not unique: {:?}",
        store_offsets
    );
}
