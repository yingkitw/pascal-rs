//! Simple compiler tests for parsing validation

use pascal::parser::Parser;

// Helper to parse and validate program structure
fn parse_program(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;
    Ok(())
}

#[test]
fn test_parse_empty_program() {
    let source = r#"
        program Test;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_with_variables() {
    let source = r#"
        program Test;
        var
            x, y: integer;
            s: string;
        begin
            x := 10;
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_with_constants() {
    let source = r#"
        program Test;
        const
            Max = 100;
            Pi = 3.14;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_with_function() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_with_procedure() {
    let source = r#"
        program Test;
        procedure Print(msg: string);
        begin
        end;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_if_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            if x > 0 then
                x := 1
            else
                x := -1;
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_while_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            while i < 10 do
                i := i + 1;
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_for_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 1 to 10 do
                i := i + 1;
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_repeat_until() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            i := 0;
            repeat
                i := i + 1
            until i > 10;
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_case_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            case x of
                1: x := 10;
                2: x := 20;
                else x := 0;
            end;
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_array_type() {
    let source = r#"
        program Test;
        type
            TIntArray = array[1..100] of integer;
        var
            arr: TIntArray;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_record_type() {
    let source = r#"
        program Test;
        type
            Point = record
                x, y: integer;
            end;
        var
            p: Point;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_enumerated_type() {
    let source = r#"
        program Test;
        type
            Color = (Red, Green, Blue);
        var
            c: Color;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_pointer_type() {
    let source = r#"
        program Test;
        type
            PInt = ^integer;
        var
            p: PInt;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_nested_functions() {
    let source = r#"
        program Test;
        function Outer(x: integer): integer;
            function Inner(y: integer): integer;
            begin
                Inner := y * 2;
            end;
        begin
            Outer := Inner(x);
        end;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_forward_declaration() {
    let source = r#"
        program Test;
        function ForwardFunc(x: integer): integer; forward;
        function ForwardFunc(x: integer): integer;
        begin
            ForwardFunc := x * 2;
        end;
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_var_parameters() {
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
        begin
        end.
    "#;

    assert!(parse_program(source).is_ok());
}

#[test]
fn test_parse_complex_expressions() {
    let source = r#"
        program Test;
        var
            a, b, c, d, result: integer;
        begin
            result := ((a + b) * (c - d)) div (a + b + c + d);
        end.
    "#;

    assert!(parse_program(source).is_ok());
}
