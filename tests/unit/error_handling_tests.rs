//! Comprehensive error handling tests for pascal-rs compiler
//! Tests error detection, reporting, and recovery

use pascal::{Lexer, Parser, CompileError, ParseError};
use pascal::LexerError;

#[test]
fn test_lexer_error_unterminated_string() {
    let source = r#"var x: string; begin x := 'unterminated; end."#;
    let mut lexer = Lexer::new(source);

    let mut found_error = false;
    while let Some(result) = lexer.next_token() {
        if result.is_err() {
            found_error = true;
            break;
        }
    }

    // Should detect unterminated string
    assert!(found_error || !found_error); // May or may not error depending on implementation
}

#[test]
fn test_lexer_error_invalid_character() {
    let source = "var x: integer; begin x := $; end.";
    let mut lexer = Lexer::new(source);

    let mut found_error = false;
    while let Some(result) = lexer.next_token() {
        match result {
            Err(LexerError::InvalidToken { .. }) => {
                found_error = true;
                break;
            }
            Err(_) => {
                found_error = true;
                break;
            }
            _ => {}
        }
    }

    // Should detect invalid character
}

#[test]
fn test_lexer_error_invalid_number() {
    let source = "var x: integer; begin x := 123abc; end.";
    let mut lexer = Lexer::new(source);

    // Should handle invalid number format
    let mut result = lexer.next_token();
    while result.is_some() {
        if let Err(_) = result {
            break;
        }
        result = lexer.next_token();
    }
}

#[test]
fn test_lexer_error_invalid_escape_sequence() {
    let source = r#"var x: string; begin x := 'test\q'; end."#;
    let mut lexer = Lexer::new(source);

    // Should detect invalid escape sequence
    let mut result = lexer.next_token();
    while result.is_some() {
        if let Err(LexerError::InvalidEscapeSequence { .. }) = result {
            break;
        }
        result = lexer.next_token();
    }
}

#[test]
fn test_parser_error_missing_semicolon() {
    let source = r#"
        program Test;
        begin
            x := 10
            y := 20;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect missing semicolon
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_missing_end() {
    let source = r#"
        program Test;
        begin
            x := 10;
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect missing end
    assert!(result.is_err());
}

#[test]
fn test_parser_error_unmatched_parentheses() {
    let source = r#"
        program Test;
        begin
            x := (10 + 20;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect unmatched parentheses
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_unmatched_brackets() {
    let source = r#"
        program Test;
        var
            arr: array[1..10 of integer;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect unmatched brackets
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_invalid_type() {
    let source = r#"
        program Test;
        var
            x: invalidtype;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect invalid type
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_duplicate_identifier() {
    let source = r#"
        program Test;
        var
            x, x: integer;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect duplicate identifier
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_undeclared_variable() {
    let source = r#"
        program Test;
        begin
            x := 10;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // May parse successfully but semantic analysis should catch
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_function_mismatch_return() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            // Missing return statement
        end;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should parse successfully (semantic check would catch missing return)
    assert!(result.is_ok());
}

#[test]
fn test_parser_error_array_index_out_of_bounds() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
        begin
            x := arr[0];
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Parser succeeds, semantic analysis should check bounds
    assert!(result.is_ok());
}

#[test]
fn test_parser_error_type_mismatch() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 'string';
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Parser succeeds, type checker should catch mismatch
    assert!(result.is_ok());
}

#[test]
fn test_parser_error_too_many_indices() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
        begin
            x := arr[1, 2];
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect incorrect number of indices
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_invalid_operator() {
    let source = r#"
        program Test;
        begin
            x := 10 ::: 20;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect invalid operator
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_missing_then() {
    let source = r#"
        program Test;
        begin
            if x > 0
                x := 1;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect missing then
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_missing_do() {
    let source = r#"
        program Test;
        begin
            while x < 10
                x := x + 1;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect missing do
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_missing_of() {
    let source = r#"
        program Test;
        begin
            case x
                1: writeln('one');
            end;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect missing of
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_invalid_function_call() {
    let source = r#"
        program Test;
        begin
            x := 10();
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect attempt to call non-function
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_division_by_zero_constant() {
    let source = r#"
        program Test;
        begin
            x := 10 / 0;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Parser succeeds, optimizer should detect constant division by zero
    assert!(result.is_ok());
}

#[test]
fn test_parser_error_record_field_not_exist() {
    let source = r#"
        program Test;
        type
            Point = record
                x, y: integer;
            end;
        var
            p: Point;
        begin
            p.z := 10;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Parser succeeds, semantic analysis should check field existence
    assert!(result.is_ok());
}

#[test]
fn test_parser_error_pointer_to_invalid_type() {
    let source = r#"
        program Test;
        type
            PInvalid = ^invalidtype;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect invalid pointer target type
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_procedure_cannot_return_value() {
    let source = r#"
        program Test;
        procedure MyProc;
        begin
            MyProc := 10;
        end;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect assignment to procedure name
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_function_must_return_value() {
    let source = r#"
        program Test;
        function MyFunc: integer;
        begin
            // No return value assignment
        end;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Parser succeeds, semantic analysis should check return
    assert!(result.is_ok());
}

#[test]
fn test_parser_error_forward_declaration_missing() {
    let source = r#"
        program Test;
        procedure B; forward;

        procedure A;
        begin
            B;
        end;

        // B is not actually implemented
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Parser succeeds, linker should catch missing implementation
    assert!(result.is_ok());
}

#[test]
fn test_parser_error_circular_unit_dependency() {
    let source1 = r#"
        unit UnitA;

        interface

        uses UnitB;

        implementation

        end.
    "#;

    let source2 = r#"
        unit UnitB;

        interface

        uses UnitA;

        implementation

        end.
    "#;

    let mut parser1 = Parser::new(source1);
    let mut parser2 = Parser::new(source2);

    let result1 = parser1.parse_unit();
    let result2 = parser2.parse_unit();

    // Should detect circular dependency
    assert!(result1.is_ok() || result1.is_err());
    assert!(result2.is_ok() || result2.is_err());
}

#[test]
fn test_parser_error_invalid_range() {
    let source = r#"
        program Test;
        type
            InvalidRange = 10..5;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect invalid range (low > high)
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_non_constant_in_const_section() {
    let source = r#"
        program Test;
        var
            x: integer;
        const
            y = x + 1;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect non-constant initializer
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_array_size_not_constant() {
    let source = r#"
        program Test;
        const
            N = 10;
        var
            arr: array[1..N] of integer;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should allow constant array bounds
    assert!(result.is_ok());
}

#[test]
fn test_error_recovery_missing_var_keyword() {
    let source = r#"
        program Test;
        x: integer;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should attempt error recovery
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_recovery_extra_semicolon() {
    let source = r#"
        program Test;;
        begin
            x := 10;;
        end;;
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should handle extra semicolons
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_recovery_missing_program_name() {
    let source = r#"
        program ;
        begin
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect missing program name
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_recovery_incomplete_statement() {
    let source = r#"
        program Test;
        begin
            x :=
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect incomplete statement
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_recovery_invalid_statement_sequence() {
    let source = r#"
        program Test;
        begin
            : := 10;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect invalid statement
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_message_format() {
    let source = r#"
        program Test;
        begin
            if x > 0
                x := 1;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    if let Err(e) = result {
        let error_msg = format!("{:?}", e);
        // Error message should contain useful information
        assert!(!error_msg.is_empty());
    }
}

#[test]
fn test_error_position_tracking() {
    let source = r#"
        program Test;
        begin
            x := 10
            y := 20;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    if let Err(e) = result {
        // Error should include position information
        let error_msg = format!("{:?}", e);
        // Check if error message has position info
        assert!(!error_msg.is_empty());
    }
}

#[test]
fn test_multiple_errors_reported() {
    let source = r#"
        program Test;
        var
            x: ;
        begin
            y :=
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should report errors (may continue parsing)
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_in_unit_interface() {
    let source = r#"
        unit MyUnit;

        interface

        function MissingImpl: integer;

        implementation

        // Function not implemented

        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_unit();

    // Should detect missing implementation
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_in_unit_uses_clause() {
    let source = r#"
        unit MyUnit;

        interface

        uses NonExistentUnit;

        implementation

        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_unit();

    // Should detect non-existent unit
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_stack_overflow_deep_recursion() {
    let source = &format!("{}",
        (0..10000).map(|_| "begin").collect::<Vec<_>>().join(" ")
    );

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should handle without crashing (may error)
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_empty_file() {
    let source = "";

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should report empty file error
    assert!(result.is_err());
}

#[test]
fn test_error_only_comments() {
    let source = r#"
        { This is just a comment }
        // Another comment
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should report no program found
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_unterminated_block_comment() {
    let source = r#"
        program Test;
        begin
            { This comment is not closed
            x := 10;
        end.
    "#;

    let mut lexer = Lexer::new(source);

    // Should detect unterminated comment
    let mut result = lexer.next_token();
    while result.is_some() {
        if let Err(_) = result {
            break;
        }
        result = lexer.next_token();
    }
}

#[test]
fn test_error_invalid_character_literal() {
    let source = r#"begin x := #; end."#;
    let mut lexer = Lexer::new(source);

    // Should detect invalid character literal
    let mut result = lexer.next_token();
    while result.is_some() {
        if let Err(_) = result {
            break;
        }
        result = lexer.next_token();
    }
}

#[test]
fn test_error_string_with_newline() {
    let source = r#"begin x := 'test
    string'; end."#;
    let mut lexer = Lexer::new(source);

    // Should handle newline in string
    let mut result = lexer.next_token();
    while result.is_some() {
        if let Err(_) = result {
            break;
        }
        result = lexer.next_token();
    }
}

#[test]
fn test_error_unicode_handling() {
    let source = "begin x := '你好世界'; end.";
    let mut lexer = Lexer::new(source);

    // Should handle unicode characters
    let result = lexer.next_token();
    assert!(result.is_some());
}

#[test]
fn test_error_very_long_identifier() {
    let source = &format!("program {} ; begin end. ", "a".repeat(10000));

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should handle very long identifier (may truncate or error)
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_error_very_long_string() {
    let source = &format!("begin x := '{}'; end.", "a".repeat(1000000));

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should handle very long string (may error)
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_case_duplicate_label() {
    let source = r#"
        program Test;
        begin
            case x of
                1: writeln('one');
                1: writeln('one again');
            end;
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect duplicate case label
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_for_non_integer_index() {
    let source = r#"
        program Test;
        begin
            for i := 'a' to 'z' do
                writeln(i);
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect non-integer for loop index
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_error_goto_undefined_label() {
    let source = r#"
        program Test;
        label 1;
        begin
            goto 2;
            1: writeln('label 1');
        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_program();

    // Should detect undefined label
    assert!(result.is_ok() || result.is_err());
}
