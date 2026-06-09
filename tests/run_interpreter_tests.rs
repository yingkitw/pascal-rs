//! Interpreter tests runner
//! Tests program execution validation
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
fn test_interpret_empty_program() {
    let source = r#"
        program Test;
        begin
        end.
    "#;
    assert!(execute_program(source).is_ok());
}
#[test]
fn test_interpret_simple_assignment() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 42;
            writeln(x);
        end.
    "#;
    assert!(execute_program(source).is_ok());
    // Output verification not available
}
#[test]
fn test_interpret_arithmetic_operations() {
    let source = r#"
        program Test;
        var
            a, b: integer;
        begin
            a := 10;
            b := 3;
            writeln(a + b);
            writeln(a - b);
            writeln(a * b);
        end.
    "#;
    assert!(execute_program(source).is_ok());
    // Output verification not available
}
#[test]
fn test_interpret_if_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 10;
            if x > 5 then
                writeln('Greater');
        end.
    "#;
    assert!(execute_program(source).is_ok());
    // Output verification not available
}
#[test]
fn test_interpret_if_else_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 3;
            if x > 5 then
                writeln('Greater')
            else
                writeln('Lesser');
        end.
    "#;
    assert!(execute_program(source).is_ok());
    // Output verification not available
}
#[test]
fn test_interpret_while_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            i := 1;
            while i <= 3 do
            begin
                writeln(i);
                i := i + 1;
            end;
        end.
    "#;
    assert!(execute_program(source).is_ok());
    // Output verification not available
    // Output verification not available
}
#[test]
fn test_interpret_for_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 1 to 5 do
                writeln(i);
        end.
    "#;
    assert!(execute_program(source).is_ok());
    // Output verification not available
    // Output verification not available
}
#[test]
fn test_interpret_function_call() {
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
    assert!(execute_program(source).is_ok());
}
#[test]
fn test_interpret_array_access() {
    let source = r#"
        program Test;
        var
            arr: array[1..5] of integer;
        begin
            arr[1] := 10;
            arr[2] := 20;
            writeln(arr[1]);
            writeln(arr[2]);
        end.
    "#;
    assert!(execute_program(source).is_ok());
    // Output verification not available
    // Output verification not available
}
#[test]
fn test_interpret_string_concatenation() {
    let source = r#"
        program Test;
        var
            s1, s2, s3: string;
        begin
            s1 := 'Hello';
            s2 := ' World';
            s3 := s1 + s2;
            writeln(s3);
        end.
    "#;
    assert!(execute_program(source).is_ok());
    // Output verification not available
}
#[test]
fn test_interpret_recursive_function() {
    let source = r#"
        program Test;
        function Factorial(n: integer): integer;
        begin
            if n <= 1 then
                Factorial := 1
            else
                Factorial := n * Factorial(n - 1);
        end;
        begin
            writeln(Factorial(5));
        end.
    "#;
    assert!(execute_program(source).is_ok());
    // Output verification not available
}

#[test]
fn test_interpret_div_mod_operators() {
    let source = r#"
        program Test;
        var
            a, b, c: integer;
        begin
            a := 17;
            b := 5;
            c := a div b;
            writeln(c);
            c := a mod b;
            writeln(c);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_interpret_logical_operators() {
    let source = r#"
        program Test;
        var
            a, b, c: boolean;
        begin
            a := true;
            b := false;
            c := a and b;
            writeln(c);
            c := a or b;
            writeln(c);
            c := a xor b;
            writeln(c);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_interpret_comparison_operators() {
    let source = r#"
        program Test;
        var
            x, y: integer;
            r: boolean;
        begin
            x := 5;
            y := 10;
            r := x <= y;
            writeln(r);
            r := x >= y;
            writeln(r);
            r := x < y;
            writeln(r);
            r := x > y;
            writeln(r);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_interpret_string_index() {
    let source = r#"
        program Test;
        var
            s: string;
            c: char;
        begin
            s := 'Hello';
            c := s[1];
            writeln(c);
            s[2] := 'a';
            writeln(s[2]);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_interpret_array_iteration() {
    let source = r#"
        program Test;
        var
            arr: array[1..5] of integer;
            i: integer;
        begin
            for i := 1 to 5 do
                arr[i] := i * 10;
            for i := 1 to 5 do
                writeln(arr[i]);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_interpret_enum_types() {
    let source = r#"
        program Test;
        type
            Color = (Red, Green, Blue);
        var
            c: Color;
        begin
            c := Red;
            writeln(0);
            c := Green;
            writeln(1);
            c := Blue;
            writeln(2);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_interpret_enum_comparison() {
    let source = r#"
        program Test;
        type
            Size = (Small, Medium, Large);
        var
            s: Size;
            r: boolean;
        begin
            s := Small;
            r := s < Large;
            writeln(r);
            r := s = Small;
            writeln(r);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_interpret_set_operations() {
    let source = r#"
        program Test;
        var
            a, b, c: set of integer;
            r: boolean;
        begin
            a := [1, 2, 3];
            b := [2, 3, 4];
            c := a + b;
            writeln(0);
            c := a * b;
            writeln(0);
            c := a - b;
            writeln(0);
            r := 2 in a;
            writeln(r);
            r := 5 in a;
            writeln(r);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_interpret_set_with_enum() {
    let source = r#"
        program Test;
        type
            Color = (Red, Green, Blue);
        var
            colors: set of Color;
            r: boolean;
        begin
            colors := [Red, Blue];
            r := Red in colors;
            writeln(r);
            r := Green in colors;
            writeln(r);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}
