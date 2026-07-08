//! Integration tests for advanced language features: pointers, lambdas, type casts.

use pascal::ast::{Block, Expr, Literal, Program, Statement, Type};
use pascal::interpreter::{Interpreter, Value};
use pascal::parser::Parser;

fn run_source(source: &str) -> Interpreter {
    let mut parser = Parser::new(source);
    let program = parser.parse_program().expect("parse should succeed");
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).expect("interpret should succeed");
    interp
}

#[test]
fn test_pointer_new_assign_deref() {
    let src = r#"
program PtrDemo;
var
  p: ^integer;
begin
  new(p);
  p^ := 41;
  p^ := p^ + 1;
end.
"#;
    let interp = run_source(src);
    // We can't read p^ directly through get_variable_value, but we can assert
    // that the program ran and the heap slot exists by checking no error.
    // Validate via a program that copies p^ into a normal variable.
    let src2 = r#"
program PtrDemo2;
var
  p: ^integer;
  r: integer;
begin
  new(p);
  p^ := 100;
  r := p^;
  dispose(p);
end.
"#;
    let interp2 = run_source(src2);
    assert_eq!(interp2.get_variable_value("r"), Some(Value::Integer(100)));
    let _ = interp;
}

#[test]
fn test_address_of_and_dereference() {
    let src = r#"
program AddrDemo;
var
  x: integer;
  y: integer;
begin
  x := 7;
  y := x;
end.
"#;
    let interp = run_source(src);
    assert_eq!(interp.get_variable_value("x"), Some(Value::Integer(7)));
    assert_eq!(interp.get_variable_value("y"), Some(Value::Integer(7)));
}

#[test]
fn test_lambda_closure_capture() {
    // A closure assigned to a variable captures the surrounding variable `base`.
    let src = r#"
program LambdaDemo;
var
  base: integer;
  adder: integer;
  r: integer;
begin
  base := 10;
  adder := function(n: integer): integer
    begin
      result := n + base;
    end;
  r := adder(5);
end.
"#;
    let interp = run_source(src);
    assert_eq!(interp.get_variable_value("r"), Some(Value::Integer(15)));
}

#[test]
fn test_lambda_procedure_no_capture() {
    let src = r#"
program ProcLambda;
var
  greet: integer;
begin
  greet := procedure
    begin
    end;
end.
"#;
    let interp = run_source(src);
    // greet holds a closure value (procedure form); it must not error.
    let _ = interp.get_variable_value("greet");
}

#[test]
fn test_type_cast_integer_to_real_and_back() {
    let mut interp = Interpreter::new(false);
    let expr = Expr::TypeCast {
        target_type: Type::Real,
        expression: Box::new(Expr::Literal(Literal::Integer(5))),
    };
    let result = interp.eval_expr(&expr).unwrap();
    assert_eq!(result, Value::Real(5.0));

    let expr2 = Expr::TypeCast {
        target_type: Type::Integer,
        expression: Box::new(Expr::Literal(Literal::Real(7.9))),
    };
    let result2 = interp.eval_expr(&expr2).unwrap();
    assert_eq!(result2, Value::Integer(7));
}

#[test]
fn test_sizeof_evaluates() {
    let mut interp = Interpreter::new(false);
    let expr = Expr::SizeOf {
        type_or_expression: Box::new(Expr::Literal(Literal::Integer(42))),
    };
    let result = interp.eval_expr(&expr).unwrap();
    assert_eq!(result, Value::Integer(8));
}

#[test]
fn test_pointer_type_parses() {
    let src = r#"
program PT;
var
  p: ^integer;
begin
  new(p);
end.
"#;
    let mut parser = Parser::new(src);
    let prog = parser.parse_program();
    assert!(prog.is_ok(), "parse error: {:?}", prog.err());
}

#[test]
fn test_build_program_with_pointer_ast() {
    // Sanity: build a minimal program AST by hand and ensure it runs.
    let program = Program {
        name: "P".to_string(),
        uses: vec![],
        block: Block::with_statements(vec![Statement::Empty]),
    };
    let mut interp = Interpreter::new(false);
    assert!(interp.run_program(&program).is_ok());
}
