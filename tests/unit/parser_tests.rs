//! Comprehensive parser tests for pascal-rs compiler
//! Tests AST generation, operator precedence, and error handling

use pascal::parser::Parser;
use pascal::{Program, ParseError, Expr, Stmt, Type, SimpleType};

// Helper function to parse a program
fn parse_program(source: &str) -> Result<Program, ParseError> {
    let mut parser = Parser::new(source);
    parser.parse_program()
}

// Helper function to parse an expression
fn parse_expression(source: &str) -> Result<Expr, ParseError> {
    let mut parser = Parser::new(source);
    parser.parse_expression()
}

// Helper function to parse a statement
fn parse_statement(source: &str) -> Result<Stmt, ParseError> {
    let mut parser = Parser::new(source);
    parser.parse_statement()
}

#[test]
fn test_empty_program() {
    let result = parse_program("program Test; begin end.");
    assert!(result.is_ok(), "Empty program should parse successfully");

    let program = result.unwrap();
    assert_eq!(program.name, "Test");
    assert!(program.statements.is_empty());
}

#[test]
fn test_program_with_simple_statement() {
    let result = parse_program(
        r#"
        program Test;
        begin
            x := 10;
        end.
    "#,
    );
    assert!(result.is_ok());

    let program = result.unwrap();
    assert_eq!(program.name, "Test");
    assert_eq!(program.statements.len(), 1);
}

#[test]
fn test_program_with_var_declaration() {
    let result = parse_program(
        r#"
        program Test;
        var
            x, y: integer;
            name: string;
        begin
        end.
    "#,
    );
    assert!(result.is_ok());

    let program = result.unwrap();
    assert_eq!(program.name, "Test");
}

#[test]
fn test_program_with_const_declaration() {
    let result = parse_program(
        r#"
        program Test;
        const
            Max = 100;
            Pi = 3.14159;
        begin
        end.
    "#,
    );
    assert!(result.is_ok());

    let program = result.unwrap();
    assert_eq!(program.name, "Test");
}

#[test]
fn test_identifier_expression() {
    let result = parse_expression("myVariable");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "myVariable"),
        _ => panic!("Expected Variable expression"),
    }
}

#[test]
fn test_integer_literal_expression() {
    let result = parse_expression("42");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => assert_eq!(n, 42),
        _ => panic!("Expected Integer literal"),
    }
}

#[test]
fn test_real_literal_expression() {
    let result = parse_expression("3.14");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Real(f)) => {
            assert!((f - 3.14).abs() < 0.001);
        }
        _ => panic!("Expected Real literal"),
    }
}

#[test]
fn test_string_literal_expression() {
    let result = parse_expression(r#""Hello World""#);
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::String(s)) => assert_eq!(s, "Hello World"),
        _ => panic!("Expected String literal"),
    }
}

#[test]
fn test_character_literal_expression() {
    let result = parse_expression("#'A'");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Char(c)) => assert_eq!(c, 'A'),
        _ => panic!("Expected Character literal"),
    }
}

#[test]
fn test_boolean_literals() {
    let result_true = parse_expression("true");
    let result_false = parse_expression("false");

    assert!(result_true.is_ok());
    assert!(result_false.is_ok());

    match result_true.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(b),
        _ => panic!("Expected Boolean literal"),
    }

    match result_false.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(!b),
        _ => panic!("Expected Boolean literal"),
    }
}

#[test]
fn test_binary_expression_addition() {
    let result = parse_expression("x + y");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::BinaryOp { op, left, right } => {
            assert_eq!(op, "+");
            match *left {
                Expr::Variable(name) => assert_eq!(name, "x"),
                _ => panic!("Expected x as left operand"),
            }
            match *right {
                Expr::Variable(name) => assert_eq!(name, "y"),
                _ => panic!("Expected y as right operand"),
            }
        }
        _ => panic!("Expected BinaryOp expression"),
    }
}

#[test]
fn test_binary_expression_subtraction() {
    let result = parse_expression("x - y");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::BinaryOp { op, .. } => {
            assert_eq!(op, "-");
        }
        _ => panic!("Expected BinaryOp expression"),
    }
}

#[test]
fn test_binary_expression_multiplication() {
    let result = parse_expression("x * y");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::BinaryOp { op, .. } => assert_eq!(op, "*"),
        _ => panic!("Expected BinaryOp expression"),
    }
}

#[test]
fn test_binary_expression_division() {
    let result = parse_expression("x / y");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::BinaryOp { op, .. } => assert_eq!(op, "/"),
        _ => panic!("Expected BinaryOp expression"),
    }
}

#[test]
fn test_operator_precedence() {
    let result = parse_expression("x + y * z");
    assert!(result.is_ok());

    // Multiplication should have higher precedence than addition
    // Should be parsed as: x + (y * z)
    match result.unwrap() {
        Expr::BinaryOp { op, left, right } => {
            assert_eq!(op, "+");
            match *left {
                Expr::Variable(name) => assert_eq!(name, "x"),
                _ => panic!("Expected x as left operand"),
            }
            // Right should be a multiplication
            match *right {
                Expr::BinaryOp { op: op2, .. } => assert_eq!(op2, "*"),
                _ => panic!("Expected multiplication in right operand"),
            }
        }
        _ => panic!("Expected BinaryOp expression"),
    }
}

#[test]
fn test_operator_precedence_with_parentheses() {
    let result = parse_expression("(x + y) * z");
    assert!(result.is_ok());

    // Parentheses should change precedence
    // Should be parsed as: (x + y) * z
    match result.unwrap() {
        Expr::BinaryOp { op, left, right } => {
            assert_eq!(op, "*");
            // Left should be addition in parentheses
            match *left {
                Expr::BinaryOp { op: op2, .. } => assert_eq!(op2, "+"),
                _ => panic!("Expected addition in left operand"),
            }
            match *right {
                Expr::Variable(name) => assert_eq!(name, "z"),
                _ => panic!("Expected z as right operand"),
            }
        }
        _ => panic!("Expected BinaryOp expression"),
    }
}

#[test]
fn test_complex_expression_precedence() {
    let result = parse_expression("a + b * c - d / e");
    assert!(result.is_ok());

    // Should be parsed as: a + (b * c) - (d / e)
    // Or left associative: ((a + (b * c)) - (d / e))
    match result.unwrap() {
        Expr::BinaryOp { op, left, right } => {
            assert_eq!(op, "-");
            match *right {
                Expr::BinaryOp { op: op2, .. } => assert_eq!(op2, "/"),
                _ => panic!("Expected division in right operand"),
            }
        }
        _ => panic!("Expected BinaryOp expression"),
    }
}

#[test]
fn test_comparison_operators() {
    let operators = vec!["=", "<>", "<", "<=", ">", ">="];

    for op in operators {
        let source = format!("x {} y", op);
        let result = parse_expression(&source);
        assert!(result.is_ok(), "Failed to parse operator {}", op);

        match result.unwrap() {
            Expr::BinaryOp { op: parsed_op, .. } => {
                assert_eq!(parsed_op, op);
            }
            _ => panic!("Expected BinaryOp expression for {}", op),
        }
    }
}

#[test]
fn test_logical_operators() {
    let result_and = parse_expression("x and y");
    let result_or = parse_expression("x or y");
    let result_not = parse_expression("not x");

    assert!(result_and.is_ok());
    assert!(result_or.is_ok());
    assert!(result_not.is_ok());

    match result_and.unwrap() {
        Expr::BinaryOp { op, .. } => assert_eq!(op, "and"),
        _ => panic!("Expected and operator"),
    }

    match result_or.unwrap() {
        Expr::BinaryOp { op, .. } => assert_eq!(op, "or"),
        _ => panic!("Expected or operator"),
    }

    match result_not.unwrap() {
        Expr::UnaryOp { op, .. } => assert_eq!(op, "not"),
        _ => panic!("Expected not operator"),
    }
}

#[test]
fn test_unary_operators() {
    let result_minus = parse_expression("-x");
    let result_plus = parse_expression("+x");

    assert!(result_minus.is_ok());
    assert!(result_plus.is_ok());

    match result_minus.unwrap() {
        Expr::UnaryOp { op, .. } => assert_eq!(op, "-"),
        _ => panic!("Expected unary minus"),
    }

    match result_plus.unwrap() {
        Expr::UnaryOp { op, .. } => assert_eq!(op, "+"),
        _ => panic!("Expected unary plus"),
    }
}

#[test]
fn test_function_call() {
    let result = parse_expression("foo()");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::FunctionCall { name, args } => {
            assert_eq!(name, "foo");
            assert!(args.is_empty());
        }
        _ => panic!("Expected FunctionCall expression"),
    }
}

#[test]
fn test_function_call_with_args() {
    let result = parse_expression("foo(x, y, z)");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::FunctionCall { name, args } => {
            assert_eq!(name, "foo");
            assert_eq!(args.len(), 3);
        }
        _ => panic!("Expected FunctionCall expression"),
    }
}

#[test]
fn test_array_access() {
    let result = parse_expression("arr[0]");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::ArrayAccess { name, index } => {
            assert_eq!(name, "arr");
            match *index {
                Expr::Literal(pascal::Literal::Integer(n)) => assert_eq!(n, 0),
                _ => panic!("Expected integer index"),
            }
        }
        _ => panic!("Expected ArrayAccess expression"),
    }
}

#[test]
fn test_array_access_with_expression() {
    let result = parse_expression("arr[i + 1]");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::ArrayAccess { name, index } => {
            assert_eq!(name, "arr");
            match *index {
                Expr::BinaryOp { .. } => {
                    // Should be a binary expression
                }
                _ => panic!("Expected binary expression as index"),
            }
        }
        _ => panic!("Expected ArrayAccess expression"),
    }
}

#[test]
fn test_record_field_access() {
    let result = parse_expression("point.x");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::FieldAccess { object, field } => {
            match *object {
                Expr::Variable(name) => assert_eq!(name, "point"),
                _ => panic!("Expected variable as object"),
            }
            assert_eq!(field, "x");
        }
        _ => panic!("Expected FieldAccess expression"),
    }
}

#[test]
fn test_pointer_dereference() {
    let result = parse_expression("ptr^");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Dereference { expr } => {
            match *expr {
                Expr::Variable(name) => assert_eq!(name, "ptr"),
                _ => panic!("Expected variable as pointer"),
            }
        }
        _ => panic!("Expected Dereference expression"),
    }
}

#[test]
fn test_address_of() {
    let result = parse_expression("@var");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::AddressOf { expr } => {
            match *expr {
                Expr::Variable(name) => assert_eq!(name, "var"),
                _ => panic!("Expected variable as target"),
            }
        }
        _ => panic!("Expected AddressOf expression"),
    }
}

#[test]
fn test_assignment_statement() {
    let result = parse_statement("x := 10;");
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::Assignment { var, expr } => {
            match var {
                Expr::Variable(name) => assert_eq!(name, "x"),
                _ => panic!("Expected variable name"),
            }
            match *expr {
                Expr::Literal(pascal::Literal::Integer(n)) => assert_eq!(n, 10),
                _ => panic!("Expected integer literal"),
            }
        }
        _ => panic!("Expected Assignment statement"),
    }
}

#[test]
fn test_if_statement() {
    let result = parse_statement("if x > 0 then x := 1;");
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::If { condition, then_stmt, else_stmt } => {
            match *condition {
                Expr::BinaryOp { .. } => {
                    // Should be a comparison
                }
                _ => panic!("Expected binary expression as condition"),
            }
            assert!(else_stmt.is_none());
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn test_if_else_statement() {
    let result = parse_statement("if x > 0 then x := 1 else x := -1;");
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::If { else_stmt, .. } => {
            assert!(else_stmt.is_some());
        }
        _ => panic!("Expected If-Else statement"),
    }
}

#[test]
fn test_while_statement() {
    let result = parse_statement("while x < 10 do x := x + 1;");
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::While { condition, body } => {
            match *condition {
                Expr::BinaryOp { .. } => {
                    // Should be a comparison
                }
                _ => panic!("Expected binary expression as condition"),
            }
            match *body {
                Stmt::Assignment { .. } => {
                    // Should be an assignment
                }
                _ => panic!("Expected assignment as body"),
            }
        }
        _ => panic!("Expected While statement"),
    }
}

#[test]
fn test_for_statement() {
    let result = parse_statement("for i := 1 to 10 do x := x + i;");
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::For { var, start, end, body, .. } => {
            assert_eq!(var, "i");
            match *start {
                Expr::Literal(pascal::Literal::Integer(n)) => assert_eq!(n, 1),
                _ => panic!("Expected integer literal as start"),
            }
            match *end {
                Expr::Literal(pascal::Literal::Integer(n)) => assert_eq!(n, 10),
                _ => panic!("Expected integer literal as end"),
            }
        }
        _ => panic!("Expected For statement"),
    }
}

#[test]
fn test_for_downto_statement() {
    let result = parse_statement("for i := 10 downto 1 do x := x + i;");
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::For { direction, .. } => {
            assert_eq!(direction, pascal::ForDirection::DownTo);
        }
        _ => panic!("Expected For downto statement"),
    }
}

#[test]
fn test_repeat_until_statement() {
    let result = parse_statement("repeat x := x + 1 until x > 10;");
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::RepeatUntil { body, condition } => {
            assert!(!body.is_empty());
            match *condition {
                Expr::BinaryOp { .. } => {
                    // Should be a comparison
                }
                _ => panic!("Expected binary expression as condition"),
            }
        }
        _ => panic!("Expected RepeatUntil statement"),
    }
}

#[test]
fn test_case_statement() {
    let result = parse_statement(
        r#"case x of
            1: y := 10;
            2: y := 20;
          end;"#,
    );
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::Case { expr, cases } => {
            match *expr {
                Expr::Variable(name) => assert_eq!(name, "x"),
                _ => panic!("Expected variable as case expression"),
            }
            assert!(!cases.is_empty());
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn test_compound_statement() {
    let result = parse_statement("begin x := 1; y := 2; end;");
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::Compound { statements } => {
            assert_eq!(statements.len(), 2);
        }
        _ => panic!("Expected Compound statement"),
    }
}

#[test]
fn test_nested_compound_statement() {
    let result = parse_statement("begin begin x := 1; end; y := 2; end;");
    assert!(result.is_ok());

    match result.unwrap() {
        Stmt::Compound { statements } => {
            assert_eq!(statements.len(), 2);
        }
        _ => panic!("Expected Compound statement"),
    }
}

#[test]
fn test_function_declaration() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());

    let program = result.unwrap();
    assert!(!program.functions.is_empty());
}

#[test]
fn test_procedure_declaration() {
    let source = r#"
        program Test;
        procedure Print(msg: string);
        begin
            writeln(msg);
        end;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());

    let program = result.unwrap();
    assert!(!program.procedures.is_empty());
}

#[test]
fn test_parameterless_function() {
    let source = r#"
        program Test;
        function GetX: integer;
        begin
            GetX := 10;
        end;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());
}

#[test]
fn test_multiple_parameters() {
    let source = r#"
        program Test;
        function Sum(a, b, c: integer): integer;
        begin
            Sum := a + b + c;
        end;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());
}

#[test]
fn test_var_parameters() {
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

    let result = parse_program(source);
    assert!(result.is_ok());
}

#[test]
fn test_array_type_declaration() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());
}

#[test]
fn test_record_type_declaration() {
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

    let result = parse_program(source);
    assert!(result.is_ok());
}

#[test]
fn test_pointer_type_declaration() {
    let source = r#"
        program Test;
        type
            PInteger = ^integer;
        var
            ptr: PInteger;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());
}

#[test]
fn test_enumerated_type() {
    let source = r#"
        program Test;
        type
            Color = (Red, Green, Blue);
        var
            c: Color;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());
}

#[test]
fn test_subrange_type() {
    let source = r#"
        program Test;
        type
            Digit = 0..9;
            Letter = 'A'..'Z';
        var
            d: Digit;
            l: Letter;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());
}

#[test]
fn test_error_unexpected_end() {
    let result = parse_program("program Test; begin");
    assert!(result.is_err());

    match result.unwrap_err() {
        ParseError::UnexpectedToken(_) => {
            // Expected error
        }
        _ => panic!("Expected UnexpectedToken error"),
    }
}

#[test]
fn test_error_missing_semicolon() {
    let result = parse_program(
        r#"
        program Test;
        begin
            x := 10
            y := 20;
        end.
    "#,
    );
    // This might succeed or fail depending on error recovery
    // Just check it doesn't panic
    let _ = result;
}

#[test]
fn test_error_mismatched_types_in_declaration() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := "string";  // Type mismatch
        end.
    "#;

    let result = parse_program(source);
    // Parser should succeed (type checker would catch this)
    assert!(result.is_ok(), "Parser should accept syntax errors");
}

#[test]
fn test_complex_nested_expressions() {
    let result = parse_expression("((a + b) * (c - d)) / e");
    assert!(result.is_ok());

    // Just verify it parses without error
    result.unwrap();
}

#[test]
fn test_chained_comparisons() {
    let result = parse_expression("a < b and b < c");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::BinaryOp { op, left, right } => {
            assert_eq!(op, "and");
            match *left {
                Expr::BinaryOp { op: op2, .. } => assert_eq!(op2, "<"),
                _ => panic!("Expected comparison in left operand"),
            }
            match *right {
                Expr::BinaryOp { op: op3, .. } => assert_eq!(op3, "<"),
                _ => panic!("Expected comparison in right operand"),
            }
        }
        _ => panic!("Expected and operator"),
    }
}

#[test]
fn test_string_concatenation() {
    let result = parse_expression(r#""Hello" + "World""#);
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::BinaryOp { op, .. } => assert_eq!(op, "+"),
        _ => panic!("Expected + operator"),
    }
}

#[test]
fn test_nested_function_calls() {
    let result = parse_expression("foo(bar(x))");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::FunctionCall { name, args } => {
            assert_eq!(name, "foo");
            assert_eq!(args.len(), 1);
            match &args[0] {
                Expr::FunctionCall { name: inner_name, .. } => {
                    assert_eq!(inner_name, "bar");
                }
                _ => panic!("Expected function call as argument"),
            }
        }
        _ => panic!("Expected FunctionCall expression"),
    }
}

#[test]
fn test_empty_parentheses() {
    // This might be an error or valid depending on the grammar
    let result = parse_expression("()");
    // Just verify it doesn't panic
    let _ = result;
}

#[test]
fn test_multiple_statements() {
    let source = r#"
        program Test;
        begin
            x := 1;
            y := 2;
            z := x + y;
            writeln(z);
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());

    let program = result.unwrap();
    assert!(!program.statements.is_empty());
}

#[test]
fn test_unit_structure() {
    let source = r#"
        unit MyUnit;

        interface

        uses
            SysUtils;

        type
            TMyClass = class
            end;

        procedure MyProcedure;
        function MyFunction: integer;

        implementation

        procedure MyProcedure;
        begin
        end;

        function MyFunction: integer;
        begin
            MyFunction := 0;
        end;

        end.
    "#;

    // Parse as unit (might need a different parse function)
    // For now, just verify it doesn't crash
    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

#[test]
fn test_program_with_uses_clause() {
    let source = r#"
        program Test;
        uses
            SysUtils, Classes;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());
}

#[test]
fn test_label_declaration() {
    let source = r#"
        program Test;
        label
            1, 2, 3;
        begin
            goto 1;
            1: writeln('Label 1');
        end.
    "#;

    let result = parse_program(source);
    // Parser should accept this
    let _ = result;
}

#[test]
fn test_type_aliases() {
    let source = r#"
        program Test;
        type
            TInt = integer;
            TStr = string;
            TReal = real;
        var
            x: TInt;
            s: TStr;
            r: TReal;
        begin
        end.
    "#;

    let result = parse_program(source);
    assert!(result.is_ok());
}
