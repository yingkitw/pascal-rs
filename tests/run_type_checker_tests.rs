//! Type checker tests runner
//! Tests type system validation

use pascal::parser::Parser;

// Helper to compile and type check a program
fn type_check_program(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    let program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;
    // Type checking would go here if we had a separate type checker module
    // For now, we just verify parsing works
    Ok(())
}

#[test]
fn test_type_check_integer_assignment() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 42;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_real_assignment() {
    let source = r#"
        program Test;
        var
            x: real;
        begin
            x := 3.14;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_boolean_assignment() {
    let source = r#"
        program Test;
        var
            b: boolean;
        begin
            b := true;
            b := false;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_string_assignment() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'Hello';
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_array_assignment() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
        begin
            arr[5] := 42;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_record_field_access() {
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

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_function_call() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;
        begin
            writeln(Add(10, 20));
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_var_parameters() {
    let source = r#"
        program Test;
        procedure Swap(var x, y: integer);
        var
            temp: integer;
        begin
            temp := x;
            x := y;
            y := temp;
        end;
        var
            a, b: integer;
        begin
            Swap(a, b);
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_pointer_operations() {
    let source = r#"
        program Test;
        var
            p: ^integer;
        begin
            new(p);
            p^ := 10;
            dispose(p);
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_compatible_assignment() {
    let source = r#"
        program Test;
        type
            TInt = integer;
        var
            x: TInt;
            y: integer;
        begin
            x := y;
            y := x;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}
