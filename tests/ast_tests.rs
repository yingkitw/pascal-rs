use minipas::ast::*;
use std::collections::HashMap;

#[cfg(test)]
mod ast_tests {
    use super::*;

    #[test]
    fn test_parameter_struct() {
        let param = Parameter {
            name: "x".to_string(),
            param_type: Type::Integer,
            is_var: true,
            is_const: false,
            is_out: false,
            default_value: Some(Expr::Literal(Literal::Integer(42))),
        };
        
        assert_eq!(param.name, "x");
        assert_eq!(param.param_type, Type::Integer);
        assert!(param.is_var);
        assert!(!param.is_const);
        assert!(!param.is_out);
        assert!(param.default_value.is_some());
    }

    #[test]
    fn test_record_type() {
        let mut fields = HashMap::new();
        fields.insert("name".to_string(), Type::String(Some(255)));
        fields.insert("age".to_string(), Type::Integer);
        fields.insert("is_student".to_string(), Type::Boolean);
        
        let record_type = Type::Record {
            fields: fields.clone(),
            is_packed: false,
            variant_part: None,
        };
        
        let packed_record_type = Type::Record {
            fields,
            is_packed: true,
            variant_part: None,
        };
        
        match record_type {
            Type::Record { fields: f, is_packed, variant_part: _ } => {
                assert_eq!(f.len(), 3);
                assert!(!is_packed);
            }
            _ => panic!("Expected Record type"),
        }
        
        match packed_record_type {
            Type::Record { fields: f, is_packed, variant_part: _ } => {
                assert_eq!(f.len(), 3);
                assert!(is_packed);
            }
            _ => panic!("Expected Record type"),
        }
    }

    #[test]
    fn test_enum_type() {
        let enum_type = Type::Enum {
            values: vec!["Red".to_string(), "Green".to_string(), "Blue".to_string()],
            custom_values: None,
        };
        
        let mut custom_values = HashMap::new();
        custom_values.insert("Red".to_string(), 1);
        custom_values.insert("Green".to_string(), 2);
        custom_values.insert("Blue".to_string(), 4);
        
        let custom_enum_type = Type::Enum {
            values: vec!["Red".to_string(), "Green".to_string(), "Blue".to_string()],
            custom_values: Some(custom_values),
        };
        
        match enum_type {
            Type::Enum { values, custom_values } => {
                assert_eq!(values.len(), 3);
                assert!(custom_values.is_none());
            }
            _ => panic!("Expected Enum type"),
        }
        
        match custom_enum_type {
            Type::Enum { values, custom_values } => {
                assert_eq!(values.len(), 3);
                assert!(custom_values.is_some());
            }
            _ => panic!("Expected Enum type"),
        }
    }

    #[test]
    fn test_set_type() {
        let set_type = Type::Set {
            base_type: Box::new(Type::Enum {
                values: vec!["Red".to_string(), "Green".to_string(), "Blue".to_string()],
                custom_values: None,
            }),
        };
        
        match set_type {
            Type::Set { base_type } => {
                match *base_type {
                    Type::Enum { values, .. } => {
                        assert_eq!(values.len(), 3);
                    }
                    _ => panic!("Expected Enum base type"),
                }
            }
            _ => panic!("Expected Set type"),
        }
    }

    #[test]
    fn test_range_type() {
        let range_type = Type::Range {
            base_type: Box::new(Type::Integer),
            min: 0,
            max: 100,
        };
        
        match range_type {
            Type::Range { base_type, min, max } => {
                assert_eq!(*base_type, Type::Integer);
                assert_eq!(min, 0);
                assert_eq!(max, 100);
            }
            _ => panic!("Expected Range type"),
        }
    }

    #[test]
    fn test_procedure_type() {
        let params = vec![
            Parameter {
                name: "x".to_string(),
                param_type: Type::Integer,
                is_var: true,
                is_const: false,
                is_out: false,
                default_value: None,
            },
            Parameter {
                name: "y".to_string(),
                param_type: Type::String(None),
                is_var: false,
                is_const: true,
                is_out: false,
                default_value: Some(Expr::Literal(Literal::String("default".to_string()))),
            },
        ];
        
        let proc_type = Type::Procedure {
            params: params.clone(),
            is_forward: false,
        };
        
        let forward_proc_type = Type::Procedure {
            params,
            is_forward: true,
        };
        
        match proc_type {
            Type::Procedure { params: p, is_forward } => {
                assert_eq!(p.len(), 2);
                assert!(!is_forward);
            }
            _ => panic!("Expected Procedure type"),
        }
        
        match forward_proc_type {
            Type::Procedure { params: p, is_forward } => {
                assert_eq!(p.len(), 2);
                assert!(is_forward);
            }
            _ => panic!("Expected Procedure type"),
        }
    }

    #[test]
    fn test_function_type() {
        let params = vec![
            Parameter {
                name: "x".to_string(),
                param_type: Type::Integer,
                is_var: false,
                is_const: false,
                is_out: false,
                default_value: None,
            },
        ];
        
        let func_type = Type::Function {
            return_type: Box::new(Type::Integer),
            params,
            is_forward: false,
        };
        
        match func_type {
            Type::Function { return_type, params: p, is_forward } => {
                assert_eq!(*return_type, Type::Integer);
                assert_eq!(p.len(), 1);
                assert!(!is_forward);
            }
            _ => panic!("Expected Function type"),
        }
    }

    #[test]
    fn test_class_type() {
        let mut fields = HashMap::new();
        fields.insert("name".to_string(), Type::String(None));
        fields.insert("age".to_string(), Type::Integer);
        
        let mut methods = HashMap::new();
        methods.insert("getName".to_string(), Type::Function {
            return_type: Box::new(Type::String(None)),
            params: vec![],
            is_forward: false,
        });
        
        let class_type = Type::Class {
            name: "Person".to_string(),
            parent: Some("Object".to_string()),
            fields,
            methods,
        };
        
        match class_type {
            Type::Class { name, parent, fields: f, methods: m } => {
                assert_eq!(name, "Person");
                assert_eq!(parent, Some("Object".to_string()));
                assert_eq!(f.len(), 2);
                assert_eq!(m.len(), 1);
            }
            _ => panic!("Expected Class type"),
        }
    }

    #[test]
    fn test_enhanced_literals() {
        // Set literal
        let set_literal = Literal::Set(vec![
            Expr::Literal(Literal::Enum("Red".to_string(), 0)),
            Expr::Literal(Literal::Enum("Blue".to_string(), 2)),
        ]);
        
        // Enum literal
        let enum_literal = Literal::Enum("Red".to_string(), 0);
        
        // Range literal
        let range_literal = Literal::Range(1, 10);
        
        match set_literal {
            Literal::Set(exprs) => {
                assert_eq!(exprs.len(), 2);
            }
            _ => panic!("Expected Set literal"),
        }
        
        match enum_literal {
            Literal::Enum(name, value) => {
                assert_eq!(name, "Red");
                assert_eq!(value, 0);
            }
            _ => panic!("Expected Enum literal"),
        }
        
        match range_literal {
            Literal::Range(min, max) => {
                assert_eq!(min, 1);
                assert_eq!(max, 10);
            }
            _ => panic!("Expected Range literal"),
        }
    }

    #[test]
    fn test_enhanced_expressions() {
        // In expression
        let in_expr = Expr::In {
            expr: Box::new(Expr::Literal(Literal::Enum("Red".to_string(), 0))),
            set_expr: Box::new(Expr::Literal(Literal::Set(vec![]))),
        };
        
        // Is expression
        let is_expr = Expr::Is {
            expr: Box::new(Expr::Variable("x".to_string())),
            type_name: "Integer".to_string(),
        };
        
        // As expression
        let as_expr = Expr::As {
            expr: Box::new(Expr::Variable("x".to_string())),
            type_name: "String".to_string(),
        };
        
        // Range expression
        let range_expr = Expr::Range {
            start: Box::new(Expr::Literal(Literal::Integer(1))),
            end: Box::new(Expr::Literal(Literal::Integer(10))),
        };
        
        // With expression
        let with_expr = Expr::With {
            expr: Box::new(Expr::Variable("person".to_string())),
            body: Box::new(Expr::Variable("name".to_string())),
        };
        
        match in_expr {
            Expr::In { expr, set_expr } => {
                assert!(matches!(expr.as_ref(), Expr::Literal(Literal::Enum(_, _))));
                assert!(matches!(set_expr.as_ref(), Expr::Literal(Literal::Set(_))));
            }
            _ => panic!("Expected In expression"),
        }
        
        match is_expr {
            Expr::Is { expr, type_name } => {
                assert!(matches!(expr.as_ref(), Expr::Variable(_)));
                assert_eq!(type_name, "Integer");
            }
            _ => panic!("Expected Is expression"),
        }
        
        match as_expr {
            Expr::As { expr, type_name } => {
                assert!(matches!(expr.as_ref(), Expr::Variable(_)));
                assert_eq!(type_name, "String");
            }
            _ => panic!("Expected As expression"),
        }
        
        match range_expr {
            Expr::Range { start, end } => {
                assert!(matches!(start.as_ref(), Expr::Literal(Literal::Integer(1))));
                assert!(matches!(end.as_ref(), Expr::Literal(Literal::Integer(10))));
            }
            _ => panic!("Expected Range expression"),
        }
        
        match with_expr {
            Expr::With { expr, body } => {
                assert!(matches!(expr.as_ref(), Expr::Variable(_)));
                assert!(matches!(body.as_ref(), Expr::Variable(_)));
            }
            _ => panic!("Expected With expression"),
        }
    }

    #[test]
    fn test_enhanced_statements() {
        // Try-except-finally statement
        let try_stmt = Stmt::Try {
            body: vec![
                Stmt::Assignment {
                    target: Expr::Variable("x".to_string()),
                    value: Expr::Literal(Literal::Integer(42)),
                },
            ],
            except_arms: vec![
                ExceptArm {
                    exception_type: Some("EDivByZero".to_string()),
                    variable: Some("e".to_string()),
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Variable("x".to_string()),
                            value: Expr::Literal(Literal::Integer(0)),
                        },
                    ],
                },
            ],
            finally_body: Some(vec![
                Stmt::Assignment {
                    target: Expr::Variable("cleanup".to_string()),
                    value: Expr::Literal(Literal::Boolean(true)),
                },
            ]),
        };
        
        // Raise statement
        let raise_stmt = Stmt::Raise {
            exception_type: Some("Exception".to_string()),
            message: Some(Expr::Variable("e".to_string())),
        };
        
        // Break and Continue statements
        let break_stmt = Stmt::Break;
        let continue_stmt = Stmt::Continue;
        
        // Exit statement
        let exit_stmt = Stmt::Exit {
            expr: Some(Expr::Literal(Literal::Integer(1))),
        };
        
        // With statement
        let with_stmt = Stmt::With {
            expr: Expr::Variable("person".to_string()),
            body: vec![
                Stmt::Assignment {
                    target: Expr::RecordAccess {
                        record: Box::new(Expr::Variable("person".to_string())),
                        field: "name".to_string(),
                    },
                    value: Expr::Literal(Literal::String("John".to_string())),
                },
            ],
        };
        
        // Label and Goto statements
        let label_stmt = Stmt::Label {
            name: "start".to_string(),
            stmt: Box::new(Stmt::Assignment {
                target: Expr::Variable("x".to_string()),
                value: Expr::Literal(Literal::Integer(1)),
            }),
        };
        
        let goto_stmt = Stmt::Goto {
            label: "start".to_string(),
        };
        
        // Test Try statement
        match try_stmt {
            Stmt::Try { body, except_arms, finally_body } => {
                assert_eq!(body.len(), 1);
                assert_eq!(except_arms.len(), 1);
                assert!(finally_body.is_some());
            }
            _ => panic!("Expected Try statement"),
        }
        
        // Test Raise statement
        match raise_stmt {
            Stmt::Raise { exception_type, message } => {
                assert!(message.is_some());
            }
            _ => panic!("Expected Raise statement"),
        }
        
        // Test Break and Continue
        assert!(matches!(break_stmt, Stmt::Break));
        assert!(matches!(continue_stmt, Stmt::Continue));
        
        // Test Exit statement
        match exit_stmt {
            Stmt::Exit { expr } => {
                assert!(expr.is_some());
            }
            _ => panic!("Expected Exit statement"),
        }
        
        // Test With statement
        match with_stmt {
            Stmt::With { expr, body } => {
                assert!(matches!(expr, Expr::Variable(_)));
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected With statement"),
        }
        
        // Test Label and Goto statements
        match label_stmt {
            Stmt::Label { name, stmt } => {
                assert_eq!(name, "start");
                assert!(matches!(*stmt, Stmt::Assignment { .. }));
            }
            _ => panic!("Expected Label statement"),
        }
        
        match goto_stmt {
            Stmt::Goto { label } => {
                assert_eq!(label, "start");
            }
            _ => panic!("Expected Goto statement"),
        }
    }

    #[test]
    fn test_except_arm_struct() {
        let except_arm = ExceptArm {
            exception_type: Some("EDivByZero".to_string()),
            variable: Some("e".to_string()),
            body: vec![
                Stmt::Assignment {
                    target: Expr::Variable("x".to_string()),
                    value: Expr::Literal(Literal::Integer(0)),
                },
            ],
        };
        
        assert_eq!(except_arm.exception_type, Some("EDivByZero".to_string()));
        assert_eq!(except_arm.variable, Some("e".to_string()));
        assert_eq!(except_arm.body.len(), 1);
    }

    #[test]
    fn test_complex_type_combinations() {
        // Array of records
        let record_type = Type::Record {
            fields: {
                let mut fields = HashMap::new();
                fields.insert("name".to_string(), Type::String(None));
                fields.insert("age".to_string(), Type::Integer);
                fields
            },
            is_packed: false,
            variant_part: None,
        };
        
        let array_type = Type::Array {
            index_type: Box::new(Type::Range {
                base_type: Box::new(Type::Integer),
                min: 1,
                max: 100,
            }),
            element_type: Box::new(record_type),
            range: Some((1, 100)),
        };
        
        // Set of enums
        let enum_type = Type::Enum {
            values: vec!["Red".to_string(), "Green".to_string(), "Blue".to_string()],
            custom_values: None,
        };
        
        let set_type = Type::Set {
            base_type: Box::new(enum_type),
        };
        
        // Pointer to function
        let func_type = Type::Function {
            return_type: Box::new(Type::Integer),
            params: vec![
                Parameter {
                    name: "x".to_string(),
                    param_type: Type::Integer,
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                },
            ],
            is_forward: false,
        };
        
        let pointer_type = Type::Pointer(Box::new(func_type));
        
        // Test array type
        match array_type {
            Type::Array { index_type, element_type, range } => {
                assert!(matches!(*index_type, Type::Range { .. }));
                assert!(matches!(*element_type, Type::Record { .. }));
                assert_eq!(range, Some((1, 100)));
            }
            _ => panic!("Expected Array type"),
        }
        
        // Test set type
        match set_type {
            Type::Set { base_type } => {
                assert!(matches!(*base_type, Type::Enum { .. }));
            }
            _ => panic!("Expected Set type"),
        }
        
        // Test pointer type
        match pointer_type {
            Type::Pointer(inner_type) => {
                assert!(matches!(inner_type.as_ref(), Type::Function { .. }));
            }
            _ => panic!("Expected Pointer type"),
        }
    }
}
