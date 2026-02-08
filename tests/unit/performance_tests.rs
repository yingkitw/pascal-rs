//! Performance and stress tests for pascal-rs compiler
//! Tests compilation speed, execution performance, and resource limits

use pascal::Parser;
use pascal::Interpreter;
use std::time::Instant;

// Helper to measure compilation time
fn measure_compilation(source: &str) -> Result<(std::time::Duration, std::time::Duration), String> {
    let parse_start = Instant::now();
    let mut parser = Parser::new(source);
    let program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;
    let parse_duration = parse_start.elapsed();

    let interp_start = Instant::now();
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&program).map_err(|e| format!("Runtime error: {:?}", e))?;
    let interp_duration = interp_start.elapsed();

    Ok((parse_duration, interp_duration))
}

// Large Program Tests

#[test]
fn test_large_variable_count() {
    let mut source = String::from(
        "program Test;\nvar\n"
    );

    // Generate 1000 variables
    for i in 0..1000 {
        source.push_str(&format!("    var{}: integer;\n", i));
    }

    source.push_str(
        "begin\n    var0 := 42;\n    writeln(var0);\nend.\n"
    );

    let result = measure_compilation(&source);
    assert!(result.is_ok(), "Should handle 1000 variables");

    let (parse_time, interp_time) = result.unwrap();
    println!("1000 variables - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);

    // Should complete within reasonable time (adjust thresholds as needed)
    assert!(parse_time.as_secs() < 10, "Parsing should be fast");
    assert!(interp_time.as_secs() < 10, "Interpretation should be fast");
}

#[test]
fn test_large_array_size() {
    let source = r#"
        program Test;
        var
            arr: array[1..10000] of integer;
            i, sum: integer;
        begin
            for i := 1 to 10000 do
                arr[i] := i;
            sum := 0;
            for i := 1 to 10000 do
                sum := sum + arr[i];
            writeln(sum);
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle large arrays");

    let (parse_time, interp_time) = result.unwrap();
    println!("Large array - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);

    // Should complete
    assert!(interp_time.as_secs() < 30, "Large array operations should complete");
}

#[test]
fn test_deep_nesting() {
    let mut source = String::from(
        "program Test;\nvar\n    x: integer;\nbegin\n    x := 0;\n"
    );

    // Create 100 levels of nested if statements
    for i in 0..100 {
        source.push_str(&format!("    if x = {} then\n", i));
        source.push_str("    begin\n");
    }

    source.push_str("        x := 100;\n");

    // Close all the blocks
    for _ in 0..100 {
        source.push_str("    end;\n");
    }

    source.push_str("    writeln(x);\nend.\n");

    let result = measure_compilation(&source);
    assert!(result.is_ok(), "Should handle deep nesting");

    let (parse_time, interp_time) = result.unwrap();
    println!("Deep nesting - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_many_functions() {
    let mut source = String::from("program Test;\n");

    // Generate 500 functions
    for i in 0..500 {
        source.push_str(&format!(
            "function Func{}(x: integer): integer;\nbegin\n    Func{} := x * 2;\nend;\n",
            i, i
        ));
    }

    source.push_str("begin\n    writeln(Func0(21));\nend.\n");

    let result = measure_compilation(&source);
    assert!(result.is_ok(), "Should handle many functions");

    let (parse_time, interp_time) = result.unwrap();
    println!("500 functions - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);

    assert!(parse_time.as_secs() < 10, "Parsing many functions should be fast");
}

#[test]
fn test_complex_expression() {
    let mut source = String::from("program Test;\nvar\n    x: integer;\nbegin\n    x := ");

    // Create a very complex expression with many operations
    let mut ops = Vec::new();
    for i in 0..100 {
        ops.push(format!("{}", i));
    }

    source.push_str(&ops.join(" + "));
    source.push_str(";\n    writeln(x);\nend.\n");

    let result = measure_compilation(&source);
    assert!(result.is_ok(), "Should handle complex expressions");

    let (parse_time, interp_time) = result.unwrap();
    println!("Complex expression - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_many_nested_loops() {
    let source = r#"
        program Test;
        var
            i, j, k, l, m: integer;
            count: integer;
        begin
            count := 0;
            for i := 1 to 5 do
                for j := 1 to 5 do
                    for k := 1 to 5 do
                        for l := 1 to 5 do
                            for m := 1 to 5 do
                                count := count + 1;
            writeln(count);
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle nested loops");

    let (parse_time, interp_time) = result.unwrap();
    println!("Nested loops - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);

    // 5^5 = 3125 iterations
    assert!(interp_time.as_secs() < 10, "Nested loops should complete");
}

#[test]
fn test_deep_recursion() {
    let source = r#"
        program Test;
        function RecursiveDepth(n: integer): integer;
        begin
            if n <= 0 then
                RecursiveDepth := 0
            else
                RecursiveDepth := 1 + RecursiveDepth(n - 1);
        end;
        begin
            writeln(RecursiveDepth(100));
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle deep recursion");

    let (parse_time, interp_time) = result.unwrap();
    println!("Deep recursion - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_long_string_operations() {
    let source = r#"
        program Test;
        var
            s1, s2, s3: string;
            i: integer;
        begin
            s1 := '';
            for i := 1 to 10000 do
                s1 := s1 + 'a';
            writeln(Length(s1));
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle long string operations");

    let (parse_time, interp_time) = result.unwrap();
    println!("Long string - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);

    assert!(interp_time.as_secs() < 30, "String operations should complete");
}

#[test]
fn test_many_function_calls() {
    let source = r#"
        program Test;
        function SimpleAdd(a, b: integer): integer;
        begin
            SimpleAdd := a + b;
        end;
        var
            i, result: integer;
        begin
            result := 0;
            for i := 1 to 10000 do
                result := SimpleAdd(result, i);
            writeln(result);
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle many function calls");

    let (parse_time, interp_time) = result.unwrap();
    println!("Many function calls - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);

    assert!(interp_time.as_secs() < 30, "Function calls should be efficient");
}

#[test]
fn test_large_record() {
    let mut source = String::from("program Test;\ntype\n    LargeRecord = record\n");

    // Generate a record with 100 fields
    for i in 0..100 {
        source.push_str(&format!("        field{}: integer;\n", i));
    }

    source.push_str("    end;\nvar\n    rec: LargeRecord;\nbegin\n");

    // Assign to some fields
    for i in 0..10 {
        source.push_str(&format!("    rec.field{} := {};\n", i, i));
    }

    source.push_str("    writeln(rec.field0);\nend.\n");

    let result = measure_compilation(&source);
    assert!(result.is_ok(), "Should handle large records");

    let (parse_time, interp_time) = result.unwrap();
    println!("Large record - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_memory_allocation() {
    let source = r#"
        program Test;
        var
            p1, p2, p3, p4, p5: ^integer;
        begin
            new(p1);
            new(p2);
            new(p3);
            new(p4);
            new(p5);
            p1^ := 1;
            p2^ := 2;
            p3^ := 3;
            p4^ := 4;
            p5^ := 5;
            writeln(p1^ + p2^ + p3^ + p4^ + p5^);
            dispose(p1);
            dispose(p2);
            dispose(p3);
            dispose(p4);
            dispose(p5);
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle memory allocation");

    let (parse_time, interp_time) = result.unwrap();
    println!("Memory allocation - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_set_operations_performance() {
    let source = r#"
        program Test;
        type
            CharSet = set of char;
        var
            s1, s2, s3: CharSet;
            i: integer;
            c: char;
        begin
            s1 := [];
            for c := 'a' to 'z' do
                s1 := s1 + [c];
            s2 := ['A', 'B', 'C'];
            s3 := s1 * s2;
            if 'a' in s1 then
                writeln('Found');
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle set operations");

    let (parse_time, interp_time) = result.unwrap();
    println!("Set operations - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_case_statement_many_cases() {
    let mut source = String::from("program Test;\nvar\n    x: integer;\nbegin\n    x := 50;\n    case x of\n");

    // Generate 1000 case labels
    for i in 0..1000 {
        source.push_str(&format!("        {}: writeln({});\n", i, i));
    }

    source.push_str("    end;\nend.\n");

    let result = measure_compilation(&source);
    assert!(result.is_ok(), "Should handle case statement with many cases");

    let (parse_time, interp_time) = result.unwrap();
    println!("Case statement (1000 cases) - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_many_mathematical_operations() {
    let source = r#"
        program Test;
        var
            i: integer;
            r: real;
        begin
            r := 0.0;
            for i := 1 to 10000 do
                r := r + sqrt(i) + sin(i) + cos(i);
            writeln(r:0:2);
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle many mathematical operations");

    let (parse_time, interp_time) = result.unwrap();
    println!("Many math operations - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);

    assert!(interp_time.as_secs() < 30, "Math operations should complete");
}

#[test]
fn test_large_source_file() {
    let mut source = String::from("program Test;\nvar\n    i: integer;\nbegin\n");

    // Generate 10000 statements
    for i in 0..10000 {
        source.push_str(&format!("    i := {};\n", i));
    }

    source.push_str("    writeln(i);\nend.\n");

    let parse_start = Instant::now();
    let mut parser = Parser::new(&source);
    let result = parser.parse_program();
    let parse_duration = parse_start.elapsed();

    assert!(result.is_ok(), "Should handle large source files");

    println!("Large source file (10000 lines) - Parse: {:?}", parse_duration);
    assert!(parse_duration.as_secs() < 10, "Parsing large files should be fast");
}

#[test]
fn test_string_comparison_performance() {
    let source = r#"
        program Test;
        var
            s1, s2: string;
            i: integer;
        begin
            s1 := 'TestString';
            s2 := 'TestString';
            for i := 1 to 10000 do
                if s1 = s2 then
                    s1 := s1;
            writeln('Done');
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle string comparisons");

    let (parse_time, interp_time) = result.unwrap();
    println!("String comparisons - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_nested_procedures_deep() {
    let mut source = String::from("program Test;\n");

    // Create 50 levels of nested procedures
    for i in 0..50 {
        let indent = "    ".repeat(i + 1);
        source.push_str(&format!("{}procedure Proc{};\n", indent, i));
        source.push_str(&format!("{}begin\n", indent));
    }

    let innermost = "    ".repeat(51);
    source.push_str(&format!("{}writeln('Deep');\n", innermost));

    // Close all procedures
    for i in (0..50).rev() {
        let indent = "    ".repeat(i + 1);
        source.push_str(&format!("{}end;\n", indent));
    }

    source.push_str("begin\n    Proc0;\nend.\n");

    let result = measure_compilation(&source);
    assert!(result.is_ok(), "Should handle deeply nested procedures");

    let (parse_time, interp_time) = result.unwrap();
    println!("Deeply nested procedures - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_fibonacci_performance() {
    let source = r#"
        program Test;
        function Fib(n: integer): integer;
        begin
            if n <= 1 then
                Fib := n
            else
                Fib := Fib(n - 1) + Fib(n - 2);
        end;
        begin
            writeln(Fib(30));
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should compute Fibonacci(30)");

    let (parse_time, interp_time) = result.unwrap();
    println!("Fibonacci(30) - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);

    // This is intentionally slow (exponential), but should still complete
    assert!(interp_time.as_secs() < 60, "Fibonacci(30) should complete");
}

#[test]
fn test_array_sort_simulation() {
    let source = r#"
        program Test;
        var
            arr: array[1..100] of integer;
            i, j, temp: integer;
        begin
            for i := 1 to 100 do
                arr[i] := 101 - i;
            for i := 1 to 99 do
                for j := i + 1 to 100 do
                    if arr[i] > arr[j] then
                    begin
                        temp := arr[i];
                        arr[i] := arr[j];
                        arr[j] := temp;
                    end;
            writeln(arr[1]);
            writeln(arr[100]);
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle bubble sort");

    let (parse_time, interp_time) = result.unwrap();
    println!("Bubble sort (100 elements) - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);

    // Bubble sort is O(n²), should still be reasonable
    assert!(interp_time.as_secs() < 30, "Bubble sort should complete");
}

#[test]
fn test_multiple_output_operations() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 1 to 1000 do
                writeln(i);
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle many output operations");

    let (parse_time, interp_time) = result.unwrap();
    println!("1000 writeln calls - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

// Stress Tests

#[test]
fn test_stress_multiple_modules() {
    // This test would check if multiple modules/units can be loaded
    // For now, just verify compilation of a large program
    let source = r#"
        program Test;
        function Func1: integer; forward;
        function Func2: integer; forward;
        function Func3: integer; forward;
        function Func1: integer;
        begin
            Func1 := Func2 + Func3;
        end;
        function Func2: integer;
        begin
            Func2 := 10;
        end;
        function Func3: integer;
        begin
            Func3 := 20;
        end;
        begin
            writeln(Func1);
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle forward declarations");
}

#[test]
fn test_stress_complex_control_flow() {
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
            if a > 0 then
                if b > 0 then
                    if c > 0 then
                        if d > 0 then
                            if e > 0 then
                                writeln('All positive');
            while a < 10 do
            begin
                a := a + 1;
                if a = 5 then
                    break;
            end;
            for b := 1 to 10 do
            begin
                case b of
                    1: writeln('One');
                    2: writeln('Two');
                    3: writeln('Three');
                    else writeln('Other');
                end;
            end;
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle complex control flow");

    let (parse_time, interp_time) = result.unwrap();
    println!("Complex control flow - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}

#[test]
fn test_stress_type_conversions() {
    let source = r#"
        program Test;
        var
            i: integer;
            r: real;
            c: char;
            s: string;
        begin
            i := 65;
            c := chr(i);
            i := ord(c);
            r := i;
            i := trunc(r);
            str(i, s);
            val(s, i, i);
            writeln(c, ' ', i, ' ', r:0:2);
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle type conversions");
}

// Memory Usage Stress Tests

#[test]
fn test_stress_dynamic_memory() {
    let source = r#"
        program Test;
        var
            i: integer;
            arr: array[1..100] of ^integer;
        begin
            for i := 1 to 100 do
            begin
                new(arr[i]);
                arr[i]^ := i * i;
            end;
            for i := 1 to 100 do
            begin
                writeln(arr[i]^);
                dispose(arr[i]);
            end;
        end.
    "#;

    let result = measure_compilation(source);
    assert!(result.is_ok(), "Should handle dynamic memory allocation");

    let (parse_time, interp_time) = result.unwrap();
    println!("Dynamic memory - Parse: {:?}, Interpret: {:?}", parse_time, interp_time);
}
