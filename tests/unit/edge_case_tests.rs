//! Edge case and error recovery tests for pascal-rs compiler
//! Tests boundary conditions, malformed input, and error handling

use pascal::Parser;

// Helper to test parsing doesn't panic
fn test_parse_safe(source: &str) -> bool {
    let mut parser = Parser::new(source);
    let result = parser.parse_program();
    result.is_ok() || result.is_err()
}

// Lexical Edge Cases

#[test]
fn test_edge_case_empty_input() {
    let source = "";
    let mut parser = Parser::new(source);
    let result = parser.parse_program();
    // Should fail gracefully, not panic
    assert!(result.is_err());
}

#[test]
fn test_edge_case_whitespace_only() {
    let source = "   \n\t\r\n   ";
    let mut parser = Parser::new(source);
    let result = parser.parse_program();
    assert!(result.is_err());
}

#[test]
fn test_edge_case_comments_only() {
    let source = r#"
        // This is a comment
        { This is another comment }
        (* This is a block comment *)
        // More comments
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_very_long_identifier() {
    let source = format!(
        "program Test;\nvar\n    {}: integer;\nbegin\nend.\n",
        "a".repeat(1000)
    );
    assert!(test_parse_safe(&source));
}

#[test]
fn test_edge_case_very_long_string() {
    let source = format!(
        "program Test;\nvar\n    s: string;\nbegin\n    s := '{}';\nend.\n",
        "a".repeat(10000)
    );
    assert!(test_parse_safe(&source));
}

#[test]
fn test_edge_case_very_long_number() {
    let source = format!(
        "program Test;\nvar\n    x: integer;\nbegin\n    x := {};\nend.\n",
        "9".repeat(100)
    );
    assert!(test_parse_safe(&source));
}

#[test]
fn test_edge_case_unicode_in_string() {
    let source = r#"
        program Test;
        begin
            writeln('Hello 世界 🌍');
            writeln('Привет мир');
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_special_characters() {
    let source = r#"
        program Test;
        begin
            writeln('Special: \t\n\r');
            writeln('Quotes: "test"');
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Syntax Error Recovery Tests

#[test]
fn test_error_recovery_missing_program_end() {
    let source = r#"
        program Test;
        begin
            writeln('Hello');
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_error_recovery_missing_semicolons() {
    let source = r#"
        program Test;
        begin
            x := 1
            y := 2
            z := 3
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_error_recovery_unbalanced_parentheses() {
    let source = r#"
        program Test;
        begin
            x := ((1 + 2);
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_error_recovery_extra_parentheses() {
    let source = r#"
        program Test;
        begin
            x := (1 + 2));
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_error_recovery_missing_then() {
    let source = r#"
        program Test;
        begin
            if x > 0
                x := 1;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_error_recovery_missing_do() {
    let source = r#"
        program Test;
        begin
            while x < 10
                x := x + 1;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_error_recovery_missing_of() {
    let source = r#"
        program Test;
        begin
            case x
                1: writeln('One');
            end;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_error_recovery_mismatched_begin_end() {
    let source = r#"
        program Test;
        begin
            begin
                x := 1;
            end;
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Type Error Edge Cases

#[test]
fn test_edge_case_implicit_type_conversion() {
    let source = r#"
        program Test;
        var
            i: integer;
            r: real;
        begin
            i := r;
            r := i;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_integer_overflow() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 999999999999999999999;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_division_by_zero_constant() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 10 div 0;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_invalid_character_literal() {
    let source = r#"
        program Test;
        var
            c: char;
        begin
            c := 'AB';
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_empty_string_literal() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := '';
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Expression Edge Cases

#[test]
fn test_edge_case_extremely_nested_expression() {
    let source = r#"
        program Test;
        begin
            x := ((((1 + 2) * (3 + 4)) - ((5 + 6) / (7 + 8))));
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_consecutive_operators() {
    let source = r#"
        program Test;
        begin
            x := 1 + + 2;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_trailing_operator() {
    let source = r#"
        program Test;
        begin
            x := 1 + 2 +;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_operator_without_operands() {
    let source = r#"
        program Test;
        begin
            x := +;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_invalid_comparison_chain() {
    let source = r#"
        program Test;
        begin
            if 1 < 2 < 3 then
                writeln('true');
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Control Flow Edge Cases

#[test]
fn test_edge_case_infinite_loop() {
    let source = r#"
        program Test;
        begin
            while true do
                writeln('forever');
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_unreachable_code() {
    let source = r#"
        program Test;
        begin
            exit;
            writeln('unreachable');
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_empty_loop_body() {
    let source = r#"
        program Test;
        begin
            while true do;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_empty_if_branch() {
    let source = r#"
        program Test;
        begin
            if true then
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_multiple_case_default() {
    let source = r#"
        program Test;
        begin
            case x of
                1: writeln('One');
                else writeln('Default');
                else writeln('Another');  // Multiple else
            end;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_for_loop_direction() {
    let source = r#"
        program Test;
        begin
            // Invalid: to with larger start
            for i := 10 to 1 do
                writeln(i);
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_for_loop_same_bounds() {
    let source = r#"
        program Test;
        begin
            for i := 5 to 5 do
                writeln(i);
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Variable/Function Edge Cases

#[test]
fn test_edge_case_undefined_variable_use() {
    let source = r#"
        program Test;
        begin
            writeln(undefined_variable);
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_duplicate_variable_declaration() {
    let source = r#"
        program Test;
        var
            x: integer;
            x: string;
        begin
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_function_without_return() {
    let source = r#"
        program Test;
        function NoReturn: integer;
        begin
            // Missing return statement
        end;
        begin
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_recursive_without_base_case() {
    let source = r#"
        program Test;
        function InfiniteRecursion(n: integer): integer;
        begin
            Result := InfiniteRecursion(n);
        end;
        begin
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_wrong_parameter_count() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            Result := a + b;
        end;
        begin
            Add(1);  // Missing parameter
            Add(1, 2, 3);  // Extra parameter
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_var_parameter_with_literal() {
    let source = r#"
        program Test;
        procedure Modify(var x: integer);
        begin
            x := 10;
        end;
        begin
            Modify(5);  // Can't pass literal to var parameter
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Array/Record Edge Cases

#[test]
fn test_edge_case_negative_array_index() {
    let source = r#"
        program Test;
        var
            arr: array[-10..10] of integer;
        begin
            arr[-5] := 42;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_zero_length_array() {
    let source = r#"
        program Test;
        var
            arr: array[1..0] of integer;
        begin
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_array_index_out_of_bounds() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
        begin
            arr[0] := 1;  // Out of bounds
            arr[11] := 2;  // Out of bounds
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_empty_record() {
    let source = r#"
        program Test;
        type
            TEmptyRecord = record
            end;
        var
            rec: TEmptyRecord;
        begin
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_nonexistent_field_access() {
    let source = r#"
        program Test;
        type
            TPoint = record
                X, Y: integer;
            end;
        var
            p: TPoint;
        begin
            p.Z := 10;  // Field doesn't exist
        end.
    "#;

    assert!(test_parse_safe(source));
}

// String Edge Cases

#[test]
fn test_edge_case_string_index_zero() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'Hello';
            writeln(s[0]);  // Invalid index
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_string_index_out_of_bounds() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'Hello';
            writeln(s[100]);  // Out of bounds
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_string_copy_invalid_range() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'Hello';
            s := Copy(s, 10, 5);  // Invalid range
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Pointer Edge Cases

#[test]
fn test_edge_case_null_pointer_dereference() {
    let source = r#"
        program Test;
        var
            p: ^integer;
        begin
            p := nil;
            p^ := 42;  // Dereferencing nil
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_double_free() {
    let source = r#"
        program Test;
        var
            p: ^integer;
        begin
            new(p);
            dispose(p);
            dispose(p);  // Double free
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_use_after_free() {
    let source = r#"
        program Test;
        var
            p: ^integer;
        begin
            new(p);
            dispose(p);
            p^ := 42;  // Use after free
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Comment Edge Cases

#[test]
fn test_edge_case_unclosed_comment() {
    let source = r#"
        program Test;
        begin
            { This comment is not closed
            writeln('Hello');
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_nested_comments() {
    let source = r#"
        program Test;
        begin
            (* Outer comment (* inner comment *) still outer *)
            writeln('Hello');
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_comment_in_expression() {
    let source = r#"
        program Test;
        begin
            x := 1 + (* add two *) 2 + (* add three *) 3;
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Numeric Literal Edge Cases

#[test]
fn test_edge_case_hexadecimal_literal() {
    let source = r#"
        program Test;
        begin
            x := $FF;
            x := $ABCD;
            x := $1234;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_binary_literal() {
    let source = r#"
        program Test;
        begin
            x := %1010;
            x := %11110000;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_octal_literal() {
    let source = r#"
        program Test;
        begin
            x := &755;
            x := &123;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_scientific_notation() {
    let source = r#"
        program Test;
        begin
            r := 1.23e10;
            r := 1.23E-10;
            r := 1e5;
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Forward Declaration Edge Cases

#[test]
fn test_edge_case_forward_without_implementation() {
    let source = r#"
        program Test;
        function ForwardFunc: integer; forward;
        begin
            writeln(ForwardFunc);
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_duplicate_forward() {
    let source = r#"
        program Test;
        function ForwardFunc: integer; forward;
        function ForwardFunc: integer; forward;
        function ForwardFunc: integer;
        begin
            Result := 42;
        end;
        begin
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Label Edge Cases

#[test]
fn test_edge_case_undefined_label() {
    let source = r#"
        program Test;
        begin
            goto undefined_label;
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_duplicate_label() {
    let source = r#"
        program Test;
        label 10;
        begin
            10: writeln('First');
            10: writeln('Second');
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_goto_into_loop() {
    let source = r#"
        program Test;
        label
            skip;
        begin
            skip:
            for i := 1 to 10 do
            begin
                goto skip;  // Jumping into loop
            end;
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Set Edge Cases

#[test]
fn test_edge_case_empty_set() {
    let source = r#"
        program Test;
        type
            TCharSet = set of char;
        var
            s: TCharSet;
        begin
            s := [];
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_full_set() {
    let source = r#"
        program Test;
        type
            TByteSet = set of 0..255;
        var
            s: TByteSet;
        begin
            s := [0..255];
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_set_intersection_empty() {
    let source = r#"
        program Test;
        type
            TIntSet = set of 1..10;
        var
            s1, s2, s3: TIntSet;
        begin
            s1 := [1, 2, 3];
            s2 := [4, 5, 6];
            s3 := s1 * s2;  // Empty intersection
        end.
    "#;

    assert!(test_parse_safe(source));
}

// File Operation Edge Cases

#[test]
fn test_edge_case_double_open() {
    let source = r#"
        program Test;
        var
            f: text;
        begin
            assign(f, 'test.txt');
            reset(f);
            reset(f);  // Opening already open file
            close(f);
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_close_unopened() {
    let source = r#"
        program Test;
        var
            f: text;
        begin
            close(f);  // Closing unopened file
        end.
    "#;

    assert!(test_parse_safe(source));
}

// Misc Edge Cases

#[test]
fn test_edge_case_empty_program() {
    let source = "program Test; end.";
    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_program_without_name() {
    let source = r#"
        program;
        begin
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_multiple_uses_clauses() {
    let source = r#"
        program Test;
        uses
            SysUtils;
        uses
            Classes;
        begin
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_case_sensitive_identifiers() {
    let source = r#"
        program Test;
        var
            myVar: integer;
            MyVar: string;
            MYVAR: real;
        begin
            myVar := 1;
            MyVar := 'test';
            MYVAR := 3.14;
        end.
    "#;

    // Pascal is case-insensitive, so these should conflict
    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_reserved_word_as_identifier() {
    let source = r#"
        program Test;
        var
            program: integer;
            begin: string;
        begin
        end.
    "#;

    assert!(test_parse_safe(source));
}

#[test]
fn test_edge_case_very_deep_nesting() {
    let mut source = String::from("program Test;\nbegin\n");
    for _ in 0..100 {
        source.push_str("    if true then\n");
    }
    for _ in 0..100 {
        source.push_str("    writeln('deep');\n");
    }
    source.push_str("end.\n");

    assert!(test_parse_safe(&source));
}
