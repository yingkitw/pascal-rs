//! Comprehensive type checker tests for pascal-rs compiler
//! Tests type inference, type checking, and type compatibility

use pascal::Parser;
use pascal::TypeChecker;

// Helper to compile and type check a program
fn type_check_program(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    let program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;

    let mut type_checker = TypeChecker::new();
    type_checker.check_program(&program).map_err(|e| format!("Type error: {:?}", e))?;
    Ok(())
}

// Basic Type Tests

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
fn test_type_check_char_assignment() {
    let source = r#"
        program Test;
        var
            c: char;
        begin
            c := 'A';
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// Type Mismatch Tests

#[test]
fn test_type_mismatch_integer_to_boolean() {
    let source = r#"
        program Test;
        var
            x: integer;
            b: boolean;
        begin
            b := x;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

#[test]
fn test_type_mismatch_string_to_integer() {
    let source = r#"
        program Test;
        var
            x: integer;
            s: string;
        begin
            x := s;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

#[test]
fn test_type_mismatch_array_to_scalar() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
            x: integer;
        begin
            x := arr;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

// Array Type Tests

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
fn test_type_check_array_index_mismatch() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
        begin
            arr['a'] := 42;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

#[test]
fn test_type_check_array_element_type_mismatch() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
        begin
            arr[5] := 'string';
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

#[test]
fn test_type_check_multi_dimensional_array() {
    let source = r#"
        program Test;
        var
            matrix: array[1..10, 1..20] of real;
        begin
            matrix[5, 10] := 3.14;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// Record Type Tests

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
fn test_type_check_record_field_mismatch() {
    let source = r#"
        program Test;
        type
            Point = record
                x, y: integer;
            end;
        var
            p: Point;
        begin
            p.x := 'string';
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

#[test]
fn test_type_check_nested_record_access() {
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
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_invalid_field_access() {
    let source = r#"
        program Test;
        type
            Point = record
                x, y: integer;
            end;
        var
            p: Point;
        begin
            p.z := 30;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

// Function/Procedure Type Tests

#[test]
fn test_type_check_function_return_type() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;
        begin
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_function_return_type_mismatch() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            Add := 'string';
        end;
        begin
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

#[test]
fn test_type_check_parameter_type_mismatch() {
    let source = r#"
        program Test;
        procedure Display(x: integer);
        begin
            writeln(x);
        end;
        begin
            Display('string');
        end.
    "#;

    assert!(type_check_program(source).is_err());
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
fn test_type_check_var_parameter_with_literal() {
    let source = r#"
        program Test;
        procedure Increment(var x: integer);
        begin
            x := x + 1;
        end;
        begin
            Increment(42);
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

// Expression Type Tests

#[test]
fn test_type_check_arithmetic_operations() {
    let source = r#"
        program Test;
        var
            a, b, c: integer;
            x, y, z: real;
        begin
            c := a + b;
            c := a - b;
            c := a * b;
            c := a div b;
            c := a mod b;
            z := x / y;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_mixed_arithmetic() {
    let source = r#"
        program Test;
        var
            i: integer;
            r: real;
        begin
            r := i + 3.14;
        end.
    "#;

    // This should either pass (with implicit conversion) or fail (without)
    // Just verify it doesn't panic
    let _ = type_check_program(source);
}

#[test]
fn test_type_check_comparison_operations() {
    let source = r#"
        program Test;
        var
            a, b: integer;
            result: boolean;
        begin
            result := a = b;
            result := a <> b;
            result := a < b;
            result := a <= b;
            result := a > b;
            result := a >= b;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_logical_operations() {
    let source = r#"
        program Test;
        var
            a, b, c: boolean;
        begin
            c := a and b;
            c := a or b;
            c := not a;
            c := a xor b;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_bitwise_operations() {
    let source = r#"
        program Test;
        var
            a, b, c: integer;
        begin
            c := a and b;
            c := a or b;
            c := a xor b;
            c := a shl b;
            c := a shr b;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_logical_with_non_boolean() {
    let source = r#"
        program Test;
        var
            a: integer;
            b: boolean;
        begin
            b := a and 5;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

// Pointer Type Tests

#[test]
fn test_type_check_pointer_dereference() {
    let source = r#"
        program Test;
        var
            p: ^integer;
            x: integer;
        begin
            x := p^;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_pointer_assignment() {
    let source = r#"
        program Test;
        var
            p: ^integer;
        begin
            new(p);
            p^ := 42;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_pointer_type_mismatch() {
    let source = r#"
        program Test;
        var
            p: ^integer;
        begin
            p^ := 'string';
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

#[test]
fn test_type_check_address_of() {
    let source = r#"
        program Test;
        var
            p: ^integer;
            x: integer;
        begin
            p := @x;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// Type Compatibility Tests

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

#[test]
fn test_type_check_incompatible_assignment() {
    let source = r#"
        program Test;
        var
            x: integer;
            s: string;
        begin
            x := s;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

// Subrange Type Tests

#[test]
fn test_type_check_subrange_valid() {
    let source = r#"
        program Test;
        type
            Digit = 0..9;
            Letter = 'A'..'Z';
        var
            d: Digit;
            l: Letter;
        begin
            d := 5;
            l := 'M';
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_subrange_assignment() {
    let source = r#"
        program Test;
        type
            Digit = 0..9;
        var
            d: Digit;
            i: integer;
        begin
            d := i;
        end.
    "#;

    // This should check if i's value is in range
    // For now, just verify it doesn't panic
    let _ = type_check_program(source);
}

// Enumerated Type Tests

#[test]
fn test_type_check_enumerated_type() {
    let source = r#"
        program Test;
        type
            Color = (Red, Green, Blue);
        var
            c: Color;
        begin
            c := Red;
            c := Green;
            c := Blue;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_enumerated_assignment() {
    let source = r#"
        program Test;
        type
            Color = (Red, Green, Blue);
            Shape = (Circle, Square, Triangle);
        var
            c: Color;
        begin
            c := Circle;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

// Set Type Tests

#[test]
fn test_type_check_set_operations() {
    let source = r#"
        program Test;
        type
            CharSet = set of char;
        var
            s1, s2, s3: CharSet;
            b: boolean;
        begin
            s3 := s1 + s2;
            s3 := s1 * s2;
            s3 := s1 - s2;
            b := 'a' in s1;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// String Type Tests

#[test]
fn test_type_check_string_concatenation() {
    let source = r#"
        program Test;
        var
            s1, s2, s3: string;
        begin
            s3 := s1 + s2;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_string_comparison() {
    let source = r#"
        program Test;
        var
            s1, s2: string;
            b: boolean;
        begin
            b := s1 = s2;
            b := s1 < s2;
            b := s1 > s2;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// Type Conversion Tests

#[test]
fn test_type_check_explicit_conversion() {
    let source = r#"
        program Test;
        var
            i: integer;
            r: real;
            c: char;
        begin
            i := integer(r);
            r := real(i);
            i := ord(c);
            c := chr(i);
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// Function Overloading Tests

#[test]
fn test_type_check_function_overloading() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer; overload;
        begin
            Add := a + b;
        end;
        function Add(a, b: real): real; overload;
        begin
            Add := a + b;
        end;
        var
            x: integer;
            y: real;
        begin
            x := Add(1, 2);
            y := Add(1.0, 2.0);
        end.
    "#;

    // This may or may not be supported
    // Just verify it doesn't panic
    let _ = type_check_program(source);
}

// Complex Expression Tests

#[test]
fn test_type_check_complex_expression() {
    let source = r#"
        program Test;
        var
            a, b, c, d, result: integer;
        begin
            result := ((a + b) * (c - d)) div (a + b + c + d);
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_mixed_complex_expression() {
    let source = r#"
        program Test;
        var
            a, b: integer;
            x, y: real;
            cond: boolean;
            result: real;
        begin
            if cond then
                result := a + x
            else
                result := b / y;
        end.
    "#;

    // This might succeed or fail depending on implicit conversion
    let _ = type_check_program(source);
}

// Scope and Visibility Tests

#[test]
fn test_type_check_local_variable_shadowing() {
    let source = r#"
        program Test;
        var
            x: integer;
        procedure Test;
        var
            x: string;
        begin
            x := 'local';
        end;
        begin
            x := 42;
        end.
    "#;

    // Local variables should shadow outer ones
    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_undefined_variable() {
    let source = r#"
        program Test;
        begin
            x := 42;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

// Forward Declaration Tests

#[test]
fn test_type_check_forward_declaration() {
    let source = r#"
        program Test;
        function ForwardFunc(x: integer): integer; forward;
        procedure UseForward;
        begin
            writeln(ForwardFunc(42));
        end;
        function ForwardFunc(x: integer): integer;
        begin
            ForwardFunc := x * 2;
        end;
        begin
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// Recursive Function Tests

#[test]
fn test_type_check_recursive_function() {
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
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// Constant Type Tests

#[test]
fn test_type_check_constant_usage() {
    let source = r#"
        program Test;
        const
            MaxInt = 100;
            Pi = 3.14159;
            Name = 'Test';
        var
            i: integer;
            r: real;
            s: string;
        begin
            i := MaxInt;
            r := Pi;
            s := Name;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_constant_type_mismatch() {
    let source = r#"
        program Test;
        const
            MaxInt = 100;
        var
            s: string;
        begin
            s := MaxInt;
        end.
    "#;

    assert!(type_check_program(source).is_err());
}

// Type Inference Tests

#[test]
fn test_type_check_inferred_integer() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 42;
            x := x + 10;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_inferred_real() {
    let source = r#"
        program Test;
        var
            x: real;
        begin
            x := 3.14;
            x := x * 2.0;
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// File Type Tests

#[test]
fn test_type_check_text_file() {
    let source = r#"
        program Test;
        var
            f: text;
        begin
            assign(f, 'test.txt');
            rewrite(f);
            writeln(f, 'Hello');
            close(f);
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

#[test]
fn test_type_check_typed_file() {
    let source = r#"
        program Test;
        var
            f: file of integer;
        begin
            assign(f, 'test.dat');
            rewrite(f);
            write(f, 42);
            close(f);
        end.
    "#;

    assert!(type_check_program(source).is_ok());
}

// Generic Type Tests (if supported)

#[test]
fn test_type_check_generic_function() {
    let source = r#"
        program Test;
        type
            TGenericArray = array of T;
        function Max<T>(arr: array of T): T;
        var
            i: integer;
            result: T;
        begin
            result := arr[0];
            for i := 1 to Length(arr) - 1 do
                if arr[i] > result then
                    result := arr[i];
            Max := result;
        end;
        begin
        end.
    "#;

    // Generics may or may not be supported
    // Just verify it doesn't panic
    let _ = type_check_program(source);
}

// Class Type Tests

#[test]
fn test_type_check_class_instantiation() {
    let source = r#"
        program Test;
        type
            TMyClass = class
                FValue: integer;
                procedure SetValue(val: integer);
                function GetValue: integer;
            end;
        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            obj.SetValue(42);
            writeln(obj.GetValue);
            obj.Free;
        end.
    "#;

    // Classes may or may not be fully supported
    // Just verify it doesn't panic
    let _ = type_check_program(source);
}

// Exception Handling Tests

#[test]
fn test_type_check_try_except() {
    let source = r#"
        program Test;
        begin
            try
                // Some code
            except
                on E: Exception do
                    writeln(E.Message);
            end;
        end.
    "#;

    // Exception handling may or may not be supported
    // Just verify it doesn't panic
    let _ = type_check_program(source);
}

// Array of Const Tests

#[test]
fn test_type_check_array_of_const() {
    let source = r#"
        program Test;
        procedure Format(const fmt: string; const args: array of const);
        begin
            // Implementation
        end;
        begin
            Format('Value: %d', [42]);
            Format('Values: %d %f %s', [1, 2.0, 'test']);
        end.
    "#;

    // Array of const may or may not be supported
    // Just verify it doesn't panic
    let _ = type_check_program(source);
}
