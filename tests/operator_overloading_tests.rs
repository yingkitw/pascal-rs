use minipas::ast::*;

#[cfg(test)]
mod operator_overloading_tests {
    use super::*;

    #[test]
    fn test_operator_declaration() {
        let operator_decl = OperatorDecl {
            operator: BinaryOp::Add,
            left_type: Type::Record {
                fields: {
                    let mut fields = std::collections::HashMap::new();
                    fields.insert("x".to_string(), Type::Integer);
                    fields.insert("y".to_string(), Type::Integer);
                    fields
                },
                is_packed: false,
                variant_part: None,
            },
            right_type: Some(Type::Record {
                fields: {
                    let mut fields = std::collections::HashMap::new();
                    fields.insert("x".to_string(), Type::Integer);
                    fields.insert("y".to_string(), Type::Integer);
                    fields
                },
                is_packed: false,
                variant_part: None,
            }),
            return_type: Type::Record {
                fields: {
                    let mut fields = std::collections::HashMap::new();
                    fields.insert("x".to_string(), Type::Integer);
                    fields.insert("y".to_string(), Type::Integer);
                    fields
                },
                is_packed: false,
                variant_part: None,
            },
            params: vec![
                Parameter {
                    name: "left".to_string(),
                    param_type: Type::Record {
                        fields: {
                            let mut fields = std::collections::HashMap::new();
                            fields.insert("x".to_string(), Type::Integer);
                            fields.insert("y".to_string(), Type::Integer);
                            fields
                        },
                        is_packed: false,
                        variant_part: None,
                    },
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                },
                Parameter {
                    name: "right".to_string(),
                    param_type: Type::Record {
                        fields: {
                            let mut fields = std::collections::HashMap::new();
                            fields.insert("x".to_string(), Type::Integer);
                            fields.insert("y".to_string(), Type::Integer);
                            fields
                        },
                        is_packed: false,
                        variant_part: None,
                    },
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                }
            ],
            body: Some(vec![
                Stmt::Assignment {
                    target: Expr::RecordAccess {
                        record: Box::new(Expr::Identifier(vec!["result".to_string()])),
                        field: "x".to_string(),
                    },
                    value: Expr::BinaryOp {
                        left: Box::new(Expr::RecordAccess {
                            record: Box::new(Expr::Identifier(vec!["left".to_string()])),
                            field: "x".to_string(),
                        }),
                        op: BinaryOp::Add,
                        right: Box::new(Expr::RecordAccess {
                            record: Box::new(Expr::Identifier(vec!["right".to_string()])),
                            field: "x".to_string(),
                        }),
                    },
                },
                Stmt::Assignment {
                    target: Expr::RecordAccess {
                        record: Box::new(Expr::Identifier(vec!["result".to_string()])),
                        field: "y".to_string(),
                    },
                    value: Expr::BinaryOp {
                        left: Box::new(Expr::RecordAccess {
                            record: Box::new(Expr::Identifier(vec!["left".to_string()])),
                            field: "y".to_string(),
                        }),
                        op: BinaryOp::Add,
                        right: Box::new(Expr::RecordAccess {
                            record: Box::new(Expr::Identifier(vec!["right".to_string()])),
                            field: "y".to_string(),
                        }),
                    },
                },
            ]),
            is_class: false,
        };
        
        assert_eq!(operator_decl.operator, BinaryOp::Add);
        assert_eq!(operator_decl.params.len(), 2);
        assert!(operator_decl.body.is_some());
        assert_eq!(operator_decl.body.unwrap().len(), 2);
    }

    #[test]
    fn test_unary_operator_declaration() {
        let unary_operator_decl = UnaryOperatorDecl {
            operator: UnaryOp::Not,
            operand_type: Type::Boolean,
            return_type: Type::Boolean,
            params: vec![
                Parameter {
                    name: "operand".to_string(),
                    param_type: Type::Boolean,
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                }
            ],
            body: Some(vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["result".to_string()]),
                    value: Expr::UnaryOp {
                        op: UnaryOp::Not,
                        expr: Box::new(Expr::Identifier(vec!["operand".to_string()])),
                    },
                },
            ]),
            is_class: false,
        };
        
        assert_eq!(unary_operator_decl.operator, UnaryOp::Not);
        assert_eq!(unary_operator_decl.params.len(), 1);
        assert!(unary_operator_decl.body.is_some());
        assert_eq!(unary_operator_decl.body.unwrap().len(), 1);
    }

    #[test]
    fn test_custom_operator() {
        let custom_operator = BinaryOp::Custom("**".to_string());
        
        match custom_operator {
            BinaryOp::Custom(op) => {
                assert_eq!(op, "**");
            },
            _ => panic!("Expected Custom operator"),
        }
    }

    #[test]
    fn test_operator_overloading_usage() {
        let vector_add = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["v1".to_string()])),
            op: BinaryOp::Add,
            right: Box::new(Expr::Identifier(vec!["v2".to_string()])),
        };
        
        match vector_add {
            Expr::BinaryOp { left, op, right } => {
                assert!(matches!(*left, Expr::Identifier(_)));
                assert_eq!(op, BinaryOp::Add);
                assert!(matches!(*right, Expr::Identifier(_)));
            },
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_complex_number_operators() {
        let complex_add = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["c1".to_string()])),
            op: BinaryOp::Add,
            right: Box::new(Expr::Identifier(vec!["c2".to_string()])),
        };
        
        let complex_multiply = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["c1".to_string()])),
            op: BinaryOp::Multiply,
            right: Box::new(Expr::Identifier(vec!["c2".to_string()])),
        };
        
        let complex_divide = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["c1".to_string()])),
            op: BinaryOp::Divide,
            right: Box::new(Expr::Identifier(vec!["c2".to_string()])),
        };
        
        assert!(matches!(complex_add, Expr::BinaryOp { op: BinaryOp::Add, .. }));
        assert!(matches!(complex_multiply, Expr::BinaryOp { op: BinaryOp::Multiply, .. }));
        assert!(matches!(complex_divide, Expr::BinaryOp { op: BinaryOp::Divide, .. }));
    }

    #[test]
    fn test_string_operators() {
        let string_concat = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Literal::String("Hello".to_string()))),
            op: BinaryOp::Add,
            right: Box::new(Expr::Literal(Literal::String(" World".to_string()))),
        };
        
        let string_compare = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["str1".to_string()])),
            op: BinaryOp::Equal,
            right: Box::new(Expr::Identifier(vec!["str2".to_string()])),
        };
        
        assert!(matches!(string_concat, Expr::BinaryOp { op: BinaryOp::Add, .. }));
        assert!(matches!(string_compare, Expr::BinaryOp { op: BinaryOp::Equal, .. }));
    }

    #[test]
    fn test_matrix_operators() {
        let matrix_multiply = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["m1".to_string()])),
            op: BinaryOp::Multiply,
            right: Box::new(Expr::Identifier(vec!["m2".to_string()])),
        };
        
        let matrix_add = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["m1".to_string()])),
            op: BinaryOp::Add,
            right: Box::new(Expr::Identifier(vec!["m2".to_string()])),
        };
        
        assert!(matches!(matrix_multiply, Expr::BinaryOp { op: BinaryOp::Multiply, .. }));
        assert!(matches!(matrix_add, Expr::BinaryOp { op: BinaryOp::Add, .. }));
    }

    #[test]
    fn test_operator_precedence() {
        let complex_expression = Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Identifier(vec!["a".to_string()])),
                op: BinaryOp::Add,
                right: Box::new(Expr::Identifier(vec!["b".to_string()])),
            }),
            op: BinaryOp::Multiply,
            right: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Identifier(vec!["c".to_string()])),
                op: BinaryOp::Subtract,
                right: Box::new(Expr::Identifier(vec!["d".to_string()])),
            }),
        };
        
        match complex_expression {
            Expr::BinaryOp { left, op, right } => {
                assert_eq!(op, BinaryOp::Multiply);
                assert!(matches!(*left, Expr::BinaryOp { op: BinaryOp::Add, .. }));
                assert!(matches!(*right, Expr::BinaryOp { op: BinaryOp::Subtract, .. }));
            },
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_operator_overloading_with_different_types() {
        let mixed_operation = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["vector".to_string()])),
            op: BinaryOp::Multiply,
            right: Box::new(Expr::Literal(Literal::Real(2.5))),
        };
        
        match mixed_operation {
            Expr::BinaryOp { left, op, right } => {
                assert!(matches!(*left, Expr::Identifier(_)));
                assert_eq!(op, BinaryOp::Multiply);
                assert!(matches!(*right, Expr::Literal(Literal::Real(_))));
            },
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_operator_overloading_with_arrays() {
        let array_operation = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["arr1".to_string()])),
            op: BinaryOp::Add,
            right: Box::new(Expr::Identifier(vec!["arr2".to_string()])),
        };
        
        match array_operation {
            Expr::BinaryOp { left, op, right } => {
                assert!(matches!(*left, Expr::Identifier(_)));
                assert_eq!(op, BinaryOp::Add);
                assert!(matches!(*right, Expr::Identifier(_)));
            },
            _ => panic!("Expected binary expression"),
        }
    }
}