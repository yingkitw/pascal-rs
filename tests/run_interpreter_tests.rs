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

// Multi-argument writeln regression tests.
// These previously silently dropped every argument after the first, so the
// tests below must at minimum succeed without error.

#[test]
fn test_writeln_multi_arg_strings() {
    let source = r#"
        program Test;
        begin
            writeln('hello, ', 'world');
            writeln('a', 'b', 'c', 'd');
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_writeln_multi_arg_mixed() {
    let source = r#"
        program Test;
        var
            n: integer;
            s: string;
        begin
            n := 42;
            s := 'the answer';
            writeln('n = ', n, ', s = ', s);
            writeln('sum: ', 1 + 2 + 3);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_write_no_newline_multi_arg() {
    let source = r#"
        program Test;
        begin
            write('a', 'b', 'c');
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_writeln_array_assignment_and_print() {
    let source = r#"
        program Test;
        var
            arr: array[1..3] of integer;
            i: integer;
        begin
            for i := 1 to 3 do
                arr[i] := i * 10;
            for i := 1 to 3 do
                writeln('arr[', i, '] = ', arr[i]);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_builtin_string_conversions() {
    let source = r#"
        program Test;
        var
            s: string;
            n: integer;
        begin
            s := inttostr(42);
            writeln('inttostr: ', s);
            n := strtoint('123');
            writeln('strtoint: ', n);
            s := upcase('hello');
            writeln('upcase: ', s);
            s := lowercase('WORLD');
            writeln('lowercase: ', s);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_builtin_math_extensions() {
    let source = r#"
        program Test;
        var
            n: integer;
            r: real;
        begin
            n := sqr(5);
            writeln('sqr(5) = ', n);
            n := round(3.7);
            writeln('round(3.7) = ', n);
            n := trunc(3.9);
            writeln('trunc(3.9) = ', n);
            n := power(2, 10);
            writeln('power(2, 10) = ', n);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_builtin_ordinal_ops() {
    let source = r#"
        program Test;
        var
            b: boolean;
            c: char;
        begin
            b := odd(7);
            writeln('odd(7) = ', b);
            b := odd(8);
            writeln('odd(8) = ', b);
            c := succ('a');
            writeln('succ(a) = ', c);
            c := pred('b');
            writeln('pred(b) = ', c);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}

#[test]
fn test_builtin_setlength() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'Hello, World!';
            setlength(s, 5);
            writeln('short: ', s);
        end.
    "#;
    assert!(execute_program(source).is_ok());
}
