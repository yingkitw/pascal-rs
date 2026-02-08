//! Simple interpreter tests that avoid I/O hanging

use pascal::parser::Parser;
use pascal::interpreter::Interpreter;

// Helper to parse and execute a program
fn execute_program(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    let program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;

    let mut interpreter = Interpreter::new(false);
    interpreter.run_program(&program).map_err(|e| format!("Runtime error: {:?}", e))?;

    Ok(())
}

#[test]
fn test_simple_empty_program() {
    let source = r#"
        program Test;
        begin
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_assignment() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 42;
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_arithmetic() {
    let source = r#"
        program Test;
        var
            a, b, c: integer;
        begin
            a := 10;
            b := 20;
            c := a + b;
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_if() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 10;
            if x > 5 then
                x := 20;
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_while() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            i := 1;
            while i <= 5 do
                i := i + 1;
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_for() {
    let source = r#"
        program Test;
        var
            i, sum: integer;
        begin
            sum := 0;
            for i := 1 to 10 do
                sum := sum + i;
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_function() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;
        var
            result: integer;
        begin
            result := Add(10, 20);
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_procedure() {
    let source = r#"
        program Test;
        procedure Double(var x: integer);
        begin
            x := x * 2;
        end;
        var
            y: integer;
        begin
            y := 5;
            Double(y);
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_array() {
    let source = r#"
        program Test;
        var
            arr: array[1..5] of integer;
            i: integer;
        begin
            for i := 1 to 5 do
                arr[i] := i * 10;
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_string_ops() {
    let source = r#"
        program Test;
        var
            s1, s2: string;
        begin
            s1 := 'Hello';
            s2 := s1 + ' World';
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_record() {
    let source = r#"
        program Test;
        type
            Point = record
                x, y: integer;
            end;
        var
            p: Point;
        begin
            p.x := 10;
            p.y := 20;
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_comparison() {
    let source = r#"
        program Test;
        var
            a, b: integer;
            result: boolean;
        begin
            a := 10;
            b := 20;
            result := a < b;
        end.
    "#;

    assert!(execute_program(source).is_ok());
}

#[test]
fn test_simple_logical() {
    let source = r#"
        program Test;
        var
            a, b, c: boolean;
        begin
            a := true;
            b := false;
            c := a and not b;
        end.
    "#;

    assert!(execute_program(source).is_ok());
}
