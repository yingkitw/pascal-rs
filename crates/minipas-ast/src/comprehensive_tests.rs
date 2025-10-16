#[cfg(test)]
mod comprehensive_tests {
    use crate::*;
    use std::collections::HashMap;

    // ============================================================================
    // BASIC AST TESTS
    // ============================================================================

    #[test]
    fn test_program_creation() {
        let program = Program {
            name: "TestProgram".to_string(),
            uses: vec!["SysUtils".to_string()],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![],
            },
        };

        assert_eq!(program.name, "TestProgram");
        assert_eq!(program.uses.len(), 1);
        assert_eq!(program.uses[0], "SysUtils");
    }

    #[test]
    fn test_block_creation() {
        let block = Block {
            consts: vec![ConstDecl {
                name: "MAX_SIZE".to_string(),
                value: Expr::Literal(Literal::Integer(100)),
            }],
            types: vec![TypeDecl {
                name: "TMyType".to_string(),
                typ: Type::Integer,
            }],
            vars: vec![VariableDecl {
                name: "x".to_string(),
                var_type: Type::Integer,
                initializer: Some(Expr::Literal(Literal::Integer(42))),
                is_threadvar: false,
                absolute_address: None,
                external_name: None,
            }],
            procedures: vec![],
            functions: vec![],
            statements: vec![Stmt::Assignment {
                target: Expr::Variable("x".to_string()),
                value: Expr::Literal(Literal::Integer(10)),
            }],
        };

        assert_eq!(block.consts.len(), 1);
        assert_eq!(block.types.len(), 1);
        assert_eq!(block.vars.len(), 1);
        assert_eq!(block.statements.len(), 1);
    }

    #[test]
    fn test_expression_creation() {
        // Test literal expressions
        let int_lit = Expr::Literal(Literal::Integer(42));
        let real_lit = Expr::Literal(Literal::Real(3.14));
        let bool_lit = Expr::Literal(Literal::Boolean(true));
        let str_lit = Expr::Literal(Literal::String("hello".to_string()));

        assert!(matches!(int_lit, Expr::Literal(Literal::Integer(42))));
        assert!(matches!(real_lit, Expr::Literal(Literal::Real(3.14))));
        assert!(matches!(bool_lit, Expr::Literal(Literal::Boolean(true))));
        assert!(matches!(str_lit, Expr::Literal(Literal::String(_))));

        // Test variable expressions
        let var_expr = Expr::Variable("x".to_string());
        assert!(matches!(var_expr, Expr::Variable(_)));

        // Test binary operations
        let bin_op = Expr::BinaryOp {
            op: BinaryOp::Add,
            left: Box::new(Expr::Literal(Literal::Integer(1))),
            right: Box::new(Expr::Literal(Literal::Integer(2))),
        };
        assert!(matches!(bin_op, Expr::BinaryOp { .. }));

        // Test unary operations
        let unary_op = Expr::UnaryOp {
            op: UnaryOp::Negate,
            expr: Box::new(Expr::Literal(Literal::Integer(5))),
        };
        assert!(matches!(unary_op, Expr::UnaryOp { .. }));
    }

    #[test]
    fn test_statement_creation() {
        // Test assignment statement
        let assign_stmt = Stmt::Assignment {
            target: Expr::Variable("x".to_string()),
            value: Expr::Literal(Literal::Integer(42)),
        };
        assert!(matches!(assign_stmt, Stmt::Assignment { .. }));

        // Test if statement
        let if_stmt = Stmt::If {
            condition: Expr::BinaryOp {
                op: BinaryOp::Greater,
                left: Box::new(Expr::Variable("x".to_string())),
                right: Box::new(Expr::Literal(Literal::Integer(0))),
            },
            then_branch: vec![Stmt::Assignment {
                target: Expr::Variable("y".to_string()),
                value: Expr::Literal(Literal::Integer(1)),
            }],
            else_branch: Some(vec![Stmt::Assignment {
                target: Expr::Variable("y".to_string()),
                value: Expr::Literal(Literal::Integer(-1)),
            }]),
        };
        assert!(matches!(if_stmt, Stmt::If { .. }));

        // Test while statement
        let while_stmt = Stmt::While {
            condition: Expr::BinaryOp {
                op: BinaryOp::Less,
                left: Box::new(Expr::Variable("i".to_string())),
                right: Box::new(Expr::Literal(Literal::Integer(10))),
            },
            body: vec![Stmt::Assignment {
                target: Expr::Variable("i".to_string()),
                value: Expr::BinaryOp {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Variable("i".to_string())),
                    right: Box::new(Expr::Literal(Literal::Integer(1))),
                },
            }],
        };
        assert!(matches!(while_stmt, Stmt::While { .. }));
    }

    #[test]
    fn test_type_creation() {
        // Test basic types
        assert!(matches!(Type::Integer, Type::Integer));
        assert!(matches!(Type::Real, Type::Real));
        assert!(matches!(Type::Boolean, Type::Boolean));
        assert!(matches!(Type::Char, Type::Char));

        // Test string type
        let str_type = Type::String(Some(100));
        assert!(matches!(str_type, Type::String(Some(100))));

        // Test array type
        let array_type = Type::Array {
            index_type: Box::new(Type::Integer),
            element_type: Box::new(Type::Integer),
            range: Some((1, 10)),
        };
        assert!(matches!(array_type, Type::Array { .. }));

        // Test record type
        let mut fields = HashMap::new();
        fields.insert("field1".to_string(), Type::Integer);
        fields.insert("field2".to_string(), Type::String(None));
        let record_type = Type::Record {
            fields,
            is_packed: false,
            variant_part: None,
        };
        assert!(matches!(record_type, Type::Record { .. }));

        // Test pointer type
        let pointer_type = Type::Pointer(Box::new(Type::Integer));
        assert!(matches!(pointer_type, Type::Pointer(_)));
    }

    // ============================================================================
    // ENHANCED AST TESTS
    // ============================================================================

    #[test]
    fn test_enhanced_ast_creation() {
        // Test enhanced program with complex features
        let program = Program {
            name: "EnhancedTest".to_string(),
            uses: vec!["SysUtils".to_string(), "Classes".to_string()],
            block: Block {
                consts: vec![
                    ConstDecl {
                        name: "PI".to_string(),
                        value: Expr::Literal(Literal::Real(3.14159)),
                    },
                    ConstDecl {
                        name: "MAX_SIZE".to_string(),
                        value: Expr::Literal(Literal::Integer(1000)),
                    },
                ],
                types: vec![
                    TypeDecl {
                        name: "TMyRecord".to_string(),
                        typ: Type::Record {
                            fields: {
                                let mut fields = HashMap::new();
                                fields.insert("field1".to_string(), Type::Integer);
                                fields.insert("field2".to_string(), Type::String(None));
                                fields
                            },
                            is_packed: false,
                            variant_part: None,
                        },
                    },
                    TypeDecl {
                        name: "TMyArray".to_string(),
                        typ: Type::Array {
                            index_type: Box::new(Type::Integer),
                            element_type: Box::new(Type::Integer),
                            range: Some((0, 99)),
                        },
                    },
                ],
                vars: vec![
                    VariableDecl {
                        name: "x".to_string(),
                        var_type: Type::Integer,
                        initializer: Some(Expr::Literal(Literal::Integer(0))),
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                    VariableDecl {
                        name: "y".to_string(),
                        var_type: Type::Real,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                ],
                procedures: vec![],
                functions: vec![],
                statements: vec![
                    Stmt::Assignment {
                        target: Expr::Variable("x".to_string()),
                        value: Expr::Literal(Literal::Integer(42)),
                    },
                    Stmt::Assignment {
                        target: Expr::Variable("y".to_string()),
                        value: Expr::BinaryOp {
                            op: BinaryOp::Multiply,
                            left: Box::new(Expr::Variable("x".to_string())),
                            right: Box::new(Expr::Call {
                                name: "PI".to_string(),
                                args: vec![],
                            }),
                        },
                    },
                ],
            },
        };

        assert_eq!(program.name, "EnhancedTest");
        assert_eq!(program.uses.len(), 2);
        assert_eq!(program.block.consts.len(), 2);
        assert_eq!(program.block.types.len(), 2);
        assert_eq!(program.block.vars.len(), 2);
        assert_eq!(program.block.statements.len(), 2);
    }

    #[test]
    fn test_complex_expressions() {
        // Test complex nested expressions
        let complex_expr = Expr::BinaryOp {
            op: BinaryOp::Add,
            left: Box::new(Expr::UnaryOp {
                op: UnaryOp::Negate,
                expr: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Multiply,
                    left: Box::new(Expr::Variable("a".to_string())),
                    right: Box::new(Expr::Literal(Literal::Integer(2))),
                }),
            }),
            right: Box::new(Expr::Call {
                name: "sqrt".to_string(),
                args: vec![Expr::BinaryOp {
                    op: BinaryOp::Subtract,
                    left: Box::new(Expr::BinaryOp {
                        op: BinaryOp::Multiply,
                        left: Box::new(Expr::Variable("b".to_string())),
                        right: Box::new(Expr::Variable("b".to_string())),
                    }),
                    right: Box::new(Expr::BinaryOp {
                        op: BinaryOp::Multiply,
                        left: Box::new(Expr::Literal(Literal::Integer(4))),
                        right: Box::new(Expr::BinaryOp {
                            op: BinaryOp::Multiply,
                            left: Box::new(Expr::Variable("a".to_string())),
                            right: Box::new(Expr::Variable("c".to_string())),
                        }),
                    }),
                }],
            }),
        };

        assert!(matches!(complex_expr, Expr::BinaryOp { .. }));
    }

    #[test]
    fn test_control_flow_statements() {
        // Test for loop
        let for_stmt = Stmt::For {
            var_name: "i".to_string(),
            start: Expr::Literal(Literal::Integer(1)),
            direction: ForDirection::To,
            end: Expr::Literal(Literal::Integer(10)),
            body: vec![Stmt::Assignment {
                target: Expr::Variable("sum".to_string()),
                value: Expr::BinaryOp {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Variable("sum".to_string())),
                    right: Box::new(Expr::Variable("i".to_string())),
                },
            }],
        };
        assert!(matches!(for_stmt, Stmt::For { .. }));

        // Test repeat-until loop
        let repeat_stmt = Stmt::RepeatUntil {
            condition: Expr::BinaryOp {
                op: BinaryOp::Equal,
                left: Box::new(Expr::Variable("x".to_string())),
                right: Box::new(Expr::Literal(Literal::Integer(0))),
            },
            body: vec![
                Stmt::Assignment {
                    target: Expr::Variable("x".to_string()),
                    value: Expr::BinaryOp {
                        op: BinaryOp::Subtract,
                        left: Box::new(Expr::Variable("x".to_string())),
                        right: Box::new(Expr::Literal(Literal::Integer(1))),
                    },
                },
            ],
        };
        assert!(matches!(repeat_stmt, Stmt::RepeatUntil { .. }));

        // Test case statement
        let case_stmt = Stmt::Case {
            expr: Expr::Variable("choice".to_string()),
            arms: vec![
                CaseArm {
                    constants: vec![Literal::Integer(1)],
                    stmts: vec![Stmt::Assignment {
                        target: Expr::Variable("result".to_string()),
                        value: Expr::Literal(Literal::String("One".to_string())),
                    }],
                },
                CaseArm {
                    constants: vec![Literal::Integer(2)],
                    stmts: vec![Stmt::Assignment {
                        target: Expr::Variable("result".to_string()),
                        value: Expr::Literal(Literal::String("Two".to_string())),
                    }],
                },
            ],
            else_arm: Some(vec![Stmt::Assignment {
                target: Expr::Variable("result".to_string()),
                value: Expr::Literal(Literal::String("Other".to_string())),
            }]),
        };
        assert!(matches!(case_stmt, Stmt::Case { .. }));
    }

    #[test]
    fn test_procedure_and_function_declarations() {
        // Test procedure declaration
        let proc_decl = ProcedureDecl {
            name: "DoSomething".to_string(),
            params: vec![
                Parameter {
                    name: "x".to_string(),
                    param_type: Type::Integer,
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                },
                Parameter {
                    name: "y".to_string(),
                    param_type: Type::String(None),
                    is_var: true,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                },
            ],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![],
            },
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: false,
            calling_convention: None,
        };
        assert_eq!(proc_decl.name, "DoSomething");
        assert_eq!(proc_decl.params.len(), 2);

        // Test function declaration
        let func_decl = FunctionDecl {
            name: "Add".to_string(),
            params: vec![
                Parameter {
                    name: "a".to_string(),
                    param_type: Type::Integer,
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                },
                Parameter {
                    name: "b".to_string(),
                    param_type: Type::Integer,
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                },
            ],
            return_type: Type::Integer,
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![Stmt::Assignment {
                    target: Expr::Identifier(vec!["Add".to_string()]),
                    value: Expr::BinaryOp {
                        op: BinaryOp::Add,
                        left: Box::new(Expr::Variable("a".to_string())),
                        right: Box::new(Expr::Variable("b".to_string())),
                    },
                }],
            },
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: false,
            calling_convention: None,
        };
        assert_eq!(func_decl.name, "Add");
        assert_eq!(func_decl.params.len(), 2);
        assert!(matches!(func_decl.return_type, Type::Integer));
    }

    // ============================================================================
    // TYPE SYSTEM TESTS
    // ============================================================================

    #[test]
    fn test_type_compatibility() {
        // Test that identical types are compatible
        assert!(Type::Integer == Type::Integer);
        assert!(Type::Real == Type::Real);
        assert!(Type::Boolean == Type::Boolean);

        // Test that different types are not compatible
        assert!(Type::Integer != Type::Real);
        assert!(Type::Boolean != Type::Integer);
        assert!(Type::String(None) != Type::Char);
    }

    #[test]
    fn test_array_type_creation() {
        let array_type = Type::Array {
            index_type: Box::new(Type::Integer),
            element_type: Box::new(Type::String(Some(50))),
            range: Some((1, 100)),
        };

        assert!(matches!(array_type, Type::Array { .. }));
        if let Type::Array { index_type, element_type, range } = array_type {
            assert!(matches!(*index_type, Type::Integer));
            assert!(matches!(*element_type, Type::String(Some(50))));
            assert_eq!(range, Some((1, 100)));
        }
    }

    #[test]
    fn test_record_type_creation() {
        let mut fields = HashMap::new();
        fields.insert("name".to_string(), Type::String(None));
        fields.insert("age".to_string(), Type::Integer);
        fields.insert("salary".to_string(), Type::Real);

        let record_type = Type::Record {
            fields,
            is_packed: true,
            variant_part: None,
        };

        assert!(matches!(record_type, Type::Record { .. }));
        if let Type::Record { fields, is_packed, .. } = record_type {
            assert_eq!(fields.len(), 3);
            assert!(is_packed);
            assert!(fields.contains_key("name"));
            assert!(fields.contains_key("age"));
            assert!(fields.contains_key("salary"));
        }
    }

    // ============================================================================
    // LITERAL TESTS
    // ============================================================================

    #[test]
    fn test_literal_creation() {
        // Test integer literal
        let int_lit = Literal::Integer(42);
        assert!(matches!(int_lit, Literal::Integer(42)));

        // Test real literal
        let real_lit = Literal::Real(3.14159);
        assert!(matches!(real_lit, Literal::Real(3.14159)));

        // Test boolean literal
        let bool_lit = Literal::Boolean(true);
        assert!(matches!(bool_lit, Literal::Boolean(true)));

        // Test string literal
        let str_lit = Literal::String("Hello, World!".to_string());
        assert!(matches!(str_lit, Literal::String(_)));

        // Test character literal
        let char_lit = Literal::Char('A');
        assert!(matches!(char_lit, Literal::Char('A')));

        // Test nil literal
        let nil_lit = Literal::Nil;
        assert!(matches!(nil_lit, Literal::Nil));
    }

    // ============================================================================
    // OPERATOR TESTS
    // ============================================================================

    #[test]
    fn test_binary_operators() {
        // Test arithmetic operators
        assert!(matches!(BinaryOp::Add, BinaryOp::Add));
        assert!(matches!(BinaryOp::Subtract, BinaryOp::Subtract));
        assert!(matches!(BinaryOp::Multiply, BinaryOp::Multiply));
        assert!(matches!(BinaryOp::Divide, BinaryOp::Divide));
        assert!(matches!(BinaryOp::IntDivide, BinaryOp::IntDivide));
        assert!(matches!(BinaryOp::Modulo, BinaryOp::Modulo));

        // Test comparison operators
        assert!(matches!(BinaryOp::Equal, BinaryOp::Equal));
        assert!(matches!(BinaryOp::NotEqual, BinaryOp::NotEqual));
        assert!(matches!(BinaryOp::Less, BinaryOp::Less));
        assert!(matches!(BinaryOp::LessOrEqual, BinaryOp::LessOrEqual));
        assert!(matches!(BinaryOp::Greater, BinaryOp::Greater));
        assert!(matches!(BinaryOp::GreaterOrEqual, BinaryOp::GreaterOrEqual));

        // Test logical operators
        assert!(matches!(BinaryOp::And, BinaryOp::And));
        assert!(matches!(BinaryOp::Or, BinaryOp::Or));
        assert!(matches!(BinaryOp::Xor, BinaryOp::Xor));

        // Test bitwise operators
        assert!(matches!(BinaryOp::BitwiseAnd, BinaryOp::BitwiseAnd));
        assert!(matches!(BinaryOp::BitwiseOr, BinaryOp::BitwiseOr));
        assert!(matches!(BinaryOp::BitwiseXor, BinaryOp::BitwiseXor));
        assert!(matches!(BinaryOp::ShiftLeft, BinaryOp::ShiftLeft));
        assert!(matches!(BinaryOp::ShiftRight, BinaryOp::ShiftRight));
    }

    #[test]
    fn test_unary_operators() {
        // Test unary operators
        assert!(matches!(UnaryOp::Negate, UnaryOp::Negate));
        assert!(matches!(UnaryOp::Not, UnaryOp::Not));
        assert!(matches!(UnaryOp::AddressOf, UnaryOp::AddressOf));
        assert!(matches!(UnaryOp::Dereference, UnaryOp::Dereference));
    }

    // ============================================================================
    // EDGE CASE TESTS
    // ============================================================================

    #[test]
    fn test_empty_structures() {
        // Test empty program
        let empty_program = Program {
            name: "Empty".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![],
            },
        };
        assert_eq!(empty_program.name, "Empty");
        assert!(empty_program.uses.is_empty());
        assert!(empty_program.block.statements.is_empty());

        // Test empty block
        let empty_block = Block {
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![],
            statements: vec![],
        };
        assert!(empty_block.consts.is_empty());
        assert!(empty_block.types.is_empty());
        assert!(empty_block.vars.is_empty());
        assert!(empty_block.procedures.is_empty());
        assert!(empty_block.functions.is_empty());
        assert!(empty_block.statements.is_empty());
    }

    #[test]
    fn test_nested_structures() {
        // Test nested expressions
        let nested_expr = Expr::BinaryOp {
            op: BinaryOp::Add,
            left: Box::new(Expr::BinaryOp {
                op: BinaryOp::Multiply,
                left: Box::new(Expr::Literal(Literal::Integer(2))),
                right: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Variable("x".to_string())),
                    right: Box::new(Expr::Literal(Literal::Integer(1))),
                }),
            }),
            right: Box::new(Expr::UnaryOp {
                op: UnaryOp::Negate,
                expr: Box::new(Expr::Variable("y".to_string())),
            }),
        };

        assert!(matches!(nested_expr, Expr::BinaryOp { .. }));

        // Test nested statements
        let nested_stmt = Stmt::If {
            condition: Expr::BinaryOp {
                op: BinaryOp::Greater,
                left: Box::new(Expr::Variable("x".to_string())),
                right: Box::new(Expr::Literal(Literal::Integer(0))),
            },
            then_branch: vec![
                Stmt::If {
                    condition: Expr::BinaryOp {
                        op: BinaryOp::Less,
                        left: Box::new(Expr::Variable("x".to_string())),
                        right: Box::new(Expr::Literal(Literal::Integer(10))),
                    },
                    then_branch: vec![Stmt::Assignment {
                        target: Expr::Variable("y".to_string()),
                        value: Expr::Literal(Literal::Integer(1)),
                    }],
                    else_branch: Some(vec![Stmt::Assignment {
                        target: Expr::Variable("y".to_string()),
                        value: Expr::Literal(Literal::Integer(2)),
                    }]),
                },
            ],
            else_branch: Some(vec![Stmt::Assignment {
                target: Expr::Variable("y".to_string()),
                value: Expr::Literal(Literal::Integer(0)),
            }]),
        };

        assert!(matches!(nested_stmt, Stmt::If { .. }));
    }

    // ============================================================================
    // PERFORMANCE TESTS
    // ============================================================================

    #[test]
    fn test_large_ast_creation() {
        // Test creating a large AST structure
        let mut statements = Vec::new();
        for i in 0..1000 {
            statements.push(Stmt::Assignment {
                target: Expr::Variable(format!("var_{}", i)),
                value: Expr::Literal(Literal::Integer(i as i64)),
            });
        }

        let large_block = Block {
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![],
            statements,
        };

        assert_eq!(large_block.statements.len(), 1000);
    }

    #[test]
    fn test_deep_nesting() {
        // Test deeply nested expressions
        let mut expr = Expr::Literal(Literal::Integer(0));
        for i in 1..100 {
            expr = Expr::BinaryOp {
                op: BinaryOp::Add,
                left: Box::new(expr),
                right: Box::new(Expr::Literal(Literal::Integer(i))),
            };
        }

        // Should not panic or cause stack overflow
        assert!(matches!(expr, Expr::BinaryOp { .. }));
    }
}
