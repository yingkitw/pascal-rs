//! Interpreter edge-case integration tests.

use pascal::ast::{Block, Expr, Literal, Program, SimpleType, Statement, Type, VariableDecl};
use pascal::interpreter::Interpreter;

fn run_program(program: &Program) -> Result<(), anyhow::Error> {
    let mut interp = Interpreter::new(false);
    interp.run_program(program)
}

#[test]
fn test_empty_program_body() {
    let program = Program {
        name: "Empty".to_string(),
        uses: vec![],
        block: Block::with_statements(vec![]),
    };
    assert!(run_program(&program).is_ok());
}

#[test]
fn test_nested_if_else() {
    let program = Program {
        name: "Nested".to_string(),
        uses: vec![],
        block: Block {
            vars: vec![VariableDecl {
                name: "x".to_string(),
                variable_type: Type::Simple(SimpleType::Integer),
                initial_value: None,
                visibility: pascal::ast::FieldVisibility::Public,
                is_absolute: false,
                absolute_address: None,
            }],
            statements: vec![
                Statement::Assignment {
                    target: Expr::Variable("x".to_string()),
                    value: Expr::Literal(Literal::Integer(5)),
                },
                Statement::If {
                    condition: Expr::BinaryOp {
                        operator: ">".to_string(),
                        left: Box::new(Expr::Variable("x".to_string())),
                        right: Box::new(Expr::Literal(Literal::Integer(0))),
                    },
                    then_branch: vec![Statement::Assignment {
                        target: Expr::Variable("x".to_string()),
                        value: Expr::Literal(Literal::Integer(1)),
                    }],
                    else_branch: Some(vec![Statement::Assignment {
                        target: Expr::Variable("x".to_string()),
                        value: Expr::Literal(Literal::Integer(0)),
                    }]),
                },
            ],
            ..Block::empty()
        },
    };
    assert!(run_program(&program).is_ok());
}

#[test]
fn test_division_by_zero_runtime_error() {
    let program = Program {
        name: "DivZero".to_string(),
        uses: vec![],
        block: Block::with_statements(vec![Statement::Assignment {
            target: Expr::Variable("x".to_string()),
            value: Expr::BinaryOp {
                operator: "/".to_string(),
                left: Box::new(Expr::Literal(Literal::Integer(1))),
                right: Box::new(Expr::Literal(Literal::Integer(0))),
            },
        }]),
    };
    assert!(run_program(&program).is_err());
}
