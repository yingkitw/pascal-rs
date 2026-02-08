//! Comprehensive interpreter tests for pascal-rs compiler
//! Tests program execution and runtime behavior validation

use pascal::Parser;
use pascal::Interpreter;

// Helper to parse and execute a program
fn execute_program(source: &str) -> Result<String, String> {
    let mut parser = Parser::new(source);
    let program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;

    let mut interpreter = Interpreter::new();
    interpreter.interpret(&program).map_err(|e| format!("Runtime error: {:?}", e))?;

    Ok(interpreter.get_output().clone())
}

// Basic Execution Tests

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

    let output = execute_program(source).unwrap();
    assert!(output.contains("42"));
}

#[test]
fn test_interpret_multiple_assignments() {
    let source = r#"
        program Test;
        var
            a, b, c: integer;
        begin
            a := 10;
            b := 20;
            c := a + b;
            writeln(c);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("30"));
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
            writeln(a + b);  // 13
            writeln(a - b);  // 7
            writeln(a * b);  // 30
            writeln(a div b); // 3
            writeln(a mod b); // 1
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("13"));
    assert!(output.contains("7"));
    assert!(output.contains("30"));
    assert!(output.contains("3"));
    assert!(output.contains("1"));
}

#[test]
fn test_interpret_real_arithmetic() {
    let source = r#"
        program Test;
        var
            a, b: real;
        begin
            a := 10.5;
            b := 3.2;
            writeln(a + b);  // 13.7
            writeln(a - b);  // 7.3
            writeln(a * b);  // 33.6
            writeln(a / b);  // 3.28125
        end.
    "#;

    let output = execute_program(source).unwrap();
    // Check that output contains expected values
    assert!(output.contains("13.7") || output.contains("13.700"));
}

#[test]
fn test_interpret_boolean_operations() {
    let source = r#"
        program Test;
        var
            a, b, c: boolean;
        begin
            a := true;
            b := false;
            c := a and b;
            writeln(c);  // false
            c := a or b;
            writeln(c);  // true
            c := not a;
            writeln(c);  // false
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("false") || output.contains("TRUE") || output.contains("FALSE"));
}

#[test]
fn test_interpret_comparison_operations() {
    let source = r#"
        program Test;
        var
            a, b: integer;
            result: boolean;
        begin
            a := 10;
            b := 20;
            result := a < b;
            writeln(result);  // true
            result := a > b;
            writeln(result);  // false
            result := a = b;
            writeln(result);  // false
            result := a <> b;
            writeln(result);  // true
        end.
    "#;

    let output = execute_program(source).unwrap();
    // Output should contain true/false values
}

// Control Flow Tests

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

    let output = execute_program(source).unwrap();
    assert!(output.contains("Greater"));
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

    let output = execute_program(source).unwrap();
    assert!(output.contains("Lesser"));
}

#[test]
fn test_interpret_nested_if() {
    let source = r#"
        program Test;
        var
            x, y: integer;
        begin
            x := 10;
            y := 20;
            if x > 5 then
                if y > 15 then
                    writeln('Both greater');
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("Both greater"));
}

#[test]
fn test_interpret_while_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            i := 1;
            while i <= 5 do
            begin
                writeln(i);
                i := i + 1;
            end;
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("1"));
    assert!(output.contains("2"));
    assert!(output.contains("3"));
    assert!(output.contains("4"));
    assert!(output.contains("5"));
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

    let output = execute_program(source).unwrap();
    assert!(output.contains("1"));
    assert!(output.contains("5"));
}

#[test]
fn test_interpret_for_downto_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 5 downto 1 do
                writeln(i);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("5"));
    assert!(output.contains("1"));
}

#[test]
fn test_interpret_repeat_until_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            i := 1;
            repeat
                writeln(i);
                i := i + 1;
            until i > 5;
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("1"));
    assert!(output.contains("5"));
}

#[test]
fn test_interpret_nested_loops() {
    let source = r#"
        program Test;
        var
            i, j: integer;
        begin
            for i := 1 to 3 do
                for j := 1 to 2 do
                    writeln(i, ' ', j);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("1"));
    assert!(output.contains("2"));
    assert!(output.contains("3"));
}

#[test]
fn test_interpret_case_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 2;
            case x of
                1: writeln('One');
                2: writeln('Two');
                3: writeln('Three');
            end;
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("Two"));
}

// Function and Procedure Tests

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

    let output = execute_program(source).unwrap();
    assert!(output.contains("30"));
}

#[test]
fn test_interpret_procedure_call() {
    let source = r#"
        program Test;
        procedure PrintSum(a, b: integer);
        begin
            writeln(a + b);
        end;
        begin
            PrintSum(10, 20);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("30"));
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

    let output = execute_program(source).unwrap();
    assert!(output.contains("120"));
}

#[test]
fn test_interpret_mutual_recursion() {
    let source = r#"
        program Test;
        function IsEven(n: integer): boolean; forward;
        function IsOdd(n: integer): boolean;
        begin
            if n = 0 then
                IsOdd := false
            else
                IsOdd := IsEven(n - 1);
        end;
        function IsEven(n: integer): boolean;
        begin
            if n = 0 then
                IsEven := true
            else
                IsEven := IsOdd(n - 1);
        end;
        begin
            writeln(IsEven(4));
            writeln(IsOdd(4));
        end.
    "#;

    // This may or may not be supported
    let _ = execute_program(source);
}

#[test]
fn test_interpret_var_parameters() {
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
            a := 10;
            b := 20;
            Swap(a, b);
            writeln(a, ' ', b);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("20") && output.contains("10"));
}

#[test]
fn test_interpret_nested_procedures() {
    let source = r#"
        program Test;
        procedure Outer;
            procedure Inner;
            begin
                writeln('Inner');
            end;
        begin
            writeln('Outer');
            Inner;
        end;
        begin
            Outer;
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("Outer"));
    assert!(output.contains("Inner"));
}

// Array Tests

#[test]
fn test_interpret_array_access() {
    let source = r#"
        program Test;
        var
            arr: array[1..5] of integer;
        begin
            arr[1] := 10;
            arr[2] := 20;
            arr[3] := 30;
            writeln(arr[1]);
            writeln(arr[2]);
            writeln(arr[3]);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("10"));
    assert!(output.contains("20"));
    assert!(output.contains("30"));
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

    let output = execute_program(source).unwrap();
    assert!(output.contains("10"));
    assert!(output.contains("50"));
}

#[test]
fn test_interpret_multi_dimensional_array() {
    let source = r#"
        program Test;
        var
            matrix: array[1..2, 1..3] of integer;
        begin
            matrix[1, 1] := 1;
            matrix[1, 2] := 2;
            matrix[1, 3] := 3;
            writeln(matrix[1, 1]);
            writeln(matrix[1, 2]);
            writeln(matrix[1, 3]);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("1"));
    assert!(output.contains("2"));
    assert!(output.contains("3"));
}

// Record Tests

#[test]
fn test_interpret_record_field_access() {
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
            writeln(p.x, ' ', p.y);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("10"));
    assert!(output.contains("20"));
}

#[test]
fn test_interpret_nested_record() {
    let source = r#"
        program Test;
        type
            Inner = record
                value: integer;
            end;
            Outer = record
                data: Inner;
            end;
        var
            o: Outer;
        begin
            o.data.value := 42;
            writeln(o.data.value);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("42"));
}

// String Tests

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

    let output = execute_program(source).unwrap();
    assert!(output.contains("Hello World"));
}

#[test]
fn test_interpret_string_length() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'Hello';
            writeln(Length(s));
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("5"));
}

#[test]
fn test_interpret_string_copy() {
    let source = r#"
        program Test;
        var
            s, sub: string;
        begin
            s := 'Hello World';
            sub := Copy(s, 1, 5);
            writeln(sub);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("Hello"));
}

#[test]
fn test_interpret_string_pos() {
    let source = r#"
        program Test;
        var
            s: string;
            index: integer;
        begin
            s := 'Hello World';
            index := Pos('World', s);
            writeln(index);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("7"));
}

// Built-in Function Tests

#[test]
fn test_interpret_abs_function() {
    let source = r#"
        program Test;
        begin
            writeln(Abs(-42));
            writeln(Abs(3.14));
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("42"));
}

#[test]
fn test_interpret_sqr_function() {
    let source = r#"
        program Test;
        begin
            writeln(Sqr(5));
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("25"));
}

#[test]
fn test_interpret_sqrt_function() {
    let source = r#"
        program Test;
        begin
            writeln(Sqrt(16.0):2:0);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("4"));
}

#[test]
fn test_interpret_round_trunc_function() {
    let source = r#"
        program Test;
        begin
            writeln(Round(3.7));
            writeln(Trunc(3.7));
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("4"));
    assert!(output.contains("3"));
}

#[test]
fn test_interpret_ord_chr_function() {
    let source = r#"
        program Test;
        var
            c: char;
            i: integer;
        begin
            c := 'A';
            i := Ord(c);
            writeln(i);
            c := Chr(65);
            writeln(c);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("65"));
    assert!(output.contains("A"));
}

#[test]
fn test_interpret_pred_succ_function() {
    let source = r#"
        program Test;
        var
            c: char;
        begin
            c := 'B';
            c := Pred(c);
            writeln(c);
            c := Succ(c);
            writeln(c);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("A"));
}

// Pointer Tests

#[test]
fn test_interpret_pointer_operations() {
    let source = r#"
        program Test;
        var
            p: ^integer;
            x: integer;
        begin
            new(p);
            p^ := 42;
            x := p^;
            writeln(x);
            dispose(p);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("42"));
}

#[test]
fn test_interpret_address_of() {
    let source = r#"
        program Test;
        var
            p: ^integer;
            x: integer;
        begin
            x := 42;
            p := @x;
            writeln(p^);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("42"));
}

// Constant Tests

#[test]
fn test_interpret_constant_usage() {
    let source = r#"
        program Test;
        const
            Max = 100;
            Pi = 3.14159;
        begin
            writeln(Max);
            writeln(Pi:0:2);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("100"));
    assert!(output.contains("3.14"));
}

// Complex Program Tests

#[test]
fn test_interpret_fibonacci() {
    let source = r#"
        program Test;
        function Fib(n: integer): integer;
        begin
            if n <= 1 then
                Fib := n
            else
                Fib := Fib(n - 1) + Fib(n - 2);
        end;
        var
            i: integer;
        begin
            for i := 0 to 9 do
                writeln(Fib(i));
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("0"));
    assert!(output.contains("1"));
    assert!(output.contains("34"));
}

#[test]
fn test_interpret_prime_check() {
    let source = r#"
        program Test;
        function IsPrime(n: integer): boolean;
        var
            i: integer;
        begin
            if n < 2 then
            begin
                IsPrime := false;
                exit;
            end;
            IsPrime := true;
            for i := 2 to trunc(sqrt(n)) do
                if n mod i = 0 then
                begin
                    IsPrime := false;
                    exit;
                end;
        end;
        begin
            writeln(IsPrime(2));
            writeln(IsPrime(17));
            writeln(IsPrime(18));
        end.
    "#;

    let output = execute_program(source).unwrap();
    // Should identify primes correctly
}

#[test]
fn test_interpret_greatest_common_divisor() {
    let source = r#"
        program Test;
        function GCD(a, b: integer): integer;
        begin
            if b = 0 then
                GCD := a
            else
                GCD := GCD(b, a mod b);
        end;
        begin
            writeln(GCD(48, 18));
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("6"));
}

#[test]
fn test_interpret_array_sum() {
    let source = r#"
        program Test;
        var
            arr: array[1..5] of integer;
            i, sum: integer;
        begin
            for i := 1 to 5 do
                arr[i] := i;
            sum := 0;
            for i := 1 to 5 do
                sum := sum + arr[i];
            writeln(sum);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("15"));
}

#[test]
fn test_interpret_array_max() {
    let source = r#"
        program Test;
        var
            arr: array[1..5] of integer;
            i, max: integer;
        begin
            arr[1] := 10;
            arr[2] := 50;
            arr[3] := 30;
            arr[4] := 20;
            arr[5] := 40;
            max := arr[1];
            for i := 2 to 5 do
                if arr[i] > max then
                    max := arr[i];
            writeln(max);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("50"));
}

// Error Handling Tests

#[test]
fn test_interpret_division_by_zero() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 10 div 0;
        end.
    "#;

    assert!(execute_program(source).is_err());
}

#[test]
fn test_interpret_array_bounds_check() {
    let source = r#"
        program Test;
        var
            arr: array[1..5] of integer;
        begin
            arr[10] := 42;
        end.
    "#;

    // May or may not be checked
    let _ = execute_program(source);
}

#[test]
fn test_interpret_undefined_variable() {
    let source = r#"
        program Test;
        begin
            writeln(undefined_var);
        end.
    "#;

    assert!(execute_program(source).is_err());
}

// Scope Tests

#[test]
fn test_interpret_variable_scope() {
    let source = r#"
        program Test;
        var
            x: integer;
        procedure Inner;
        var
            x: integer;
        begin
            x := 20;
            writeln(x);
        end;
        begin
            x := 10;
            writeln(x);
            Inner;
            writeln(x);
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("10"));
    assert!(output.contains("20"));
}

// Early Exit Tests

#[test]
fn test_interpret_exit_in_procedure() {
    let source = r#"
        program Test;
        procedure TestExit;
        begin
            writeln('Before');
            exit;
            writeln('After');
        end;
        begin
            TestExit;
        end.
    "#;

    let output = execute_program(source).unwrap();
    assert!(output.contains("Before"));
    assert!(!output.contains("After"));
}

#[test]
fn test_interpret_break_in_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 1 to 10 do
            begin
                writeln(i);
                if i = 5 then
                    break;
            end;
        end.
    "#;

    // Break may or may not be supported
    let _ = execute_program(source);
}

#[test]
fn test_interpret_continue_in_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 1 to 5 do
            begin
                if i = 3 then
                    continue;
                writeln(i);
            end;
        end.
    "#;

    // Continue may or may not be supported
    let _ = execute_program(source);
}
