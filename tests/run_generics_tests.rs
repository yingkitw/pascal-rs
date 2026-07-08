//! Generic type parameter tests for pascal-rs compiler
//! Tests parsing and interpreter wiring for generic types.

use pascal::parser::Parser;
use pascal::interpreter::Interpreter;

fn parse_program(source: &str) -> Result<pascal::ast::Program, String> {
    let mut parser = Parser::new(source);
    parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))
}

fn run_program(source: &str) -> Result<(), String> {
    let program = parse_program(source)?;
    let mut interpreter = Interpreter::new(false);
    interpreter.run_program(&program).map_err(|e| format!("Runtime error: {:?}", e))
}

#[test]
fn test_generic_array_type_parse() {
    let source = r#"
        program Test;
        type
            TList<T> = array of T;
        var
            x: TList<Integer>;
        begin
        end.
    "#;

    let program = parse_program(source).expect("parse should succeed");

    // The generic type declaration should be registered.
    let type_decl = program
        .block
        .types
        .iter()
        .find(|t| t.name == "TList")
        .expect("TList type declaration not found");
    assert_eq!(type_decl.type_parameters, vec!["T"]);
    assert!(
        matches!(&type_decl.type_definition, pascal::ast::Type::Array { element_type, .. } if matches!(element_type.as_ref(), pascal::ast::Type::Generic { name, .. } if name == "T")),
        "expected array of generic T"
    );

    // The variable should use a generic instantiation.
    let var_decl = program
        .block
        .vars
        .iter()
        .find(|v| v.name == "x")
        .expect("x variable not found");
    assert!(
        matches!(&var_decl.variable_type, pascal::ast::Type::GenericInstance { base_type, type_arguments } if base_type == "TList" && type_arguments.len() == 1),
        "expected TList<Integer> generic instance"
    );
}

#[test]
fn test_generic_array_instantiation_runs() {
    let source = r#"
        program Test;
        type
            TList<T> = array of T;
        var
            x: TList<Integer>;
        begin
        end.
    "#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_generic_record_instantiation_runs() {
    let source = r#"
        program Test;
        type
            TPair<T, U> = record
                First: T;
                Second: U;
            end;
        var
            p: TPair<Integer, String>;
        begin
            p.First := 42;
            p.Second := 'hello';
        end.
    "#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_generic_wrong_argument_count_errors() {
    let source = r#"
        program Test;
        type
            TList<T> = array of T;
        var
            x: TList<Integer, String>;
        begin
        end.
    "#;

    let result = run_program(source);
    assert!(
        result.is_err(),
        "expected runtime error for wrong number of generic arguments"
    );
    let err = result.unwrap_err();
    assert!(
        err.contains("expected 1 type arguments, got 2") || err.contains("Expected 1 type parameters"),
        "unexpected error message: {}",
        err
    );
}
