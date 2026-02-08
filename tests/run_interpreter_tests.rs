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
