use minipas::ast::*;

#[cfg(test)]
mod memory_management_tests {
    use super::*;

    #[test]
    fn test_new_expression() {
        let new_expr = Expr::New {
            type_name: "TMyObject".to_string(),
            args: vec![
                Expr::Literal(Literal::String("test".to_string())),
                Expr::Literal(Literal::Integer(42)),
            ],
        };
        
        match new_expr {
            Expr::New { type_name, args } => {
                assert_eq!(type_name, "TMyObject");
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expr::Literal(Literal::String(_))));
                assert!(matches!(args[1], Expr::Literal(Literal::Integer(42))));
            },
            _ => panic!("Expected New expression"),
        }
    }

    #[test]
    fn test_dispose_expression() {
        let dispose_expr = Expr::Dispose {
            expr: Box::new(Expr::Identifier(vec!["obj".to_string()])),
        };
        
        match dispose_expr {
            Expr::Dispose { expr } => {
                assert!(matches!(*expr, Expr::Identifier(_)));
            },
            _ => panic!("Expected Dispose expression"),
        }
    }

    #[test]
    fn test_getmem_expression() {
        let getmem_expr = Expr::GetMem {
            size: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Literal(Literal::Integer(10))),
                op: BinaryOp::Multiply,
                right: Box::new(Expr::Literal(Literal::Integer(4))),
            }),
        };
        
        match getmem_expr {
            Expr::GetMem { size } => {
                match *size {
                    Expr::BinaryOp { left, op, right } => {
                        assert!(matches!(*left, Expr::Literal(Literal::Integer(10))));
                        assert_eq!(op, BinaryOp::Multiply);
                        assert!(matches!(*right, Expr::Literal(Literal::Integer(4))));
                    },
                    _ => panic!("Expected binary expression"),
                }
            },
            _ => panic!("Expected GetMem expression"),
        }
    }

    #[test]
    fn test_freemem_expression() {
        let freemem_expr = Expr::FreeMem {
            expr: Box::new(Expr::Identifier(vec!["ptr".to_string()])),
        };
        
        match freemem_expr {
            Expr::FreeMem { expr } => {
                assert!(matches!(*expr, Expr::Identifier(_)));
            },
            _ => panic!("Expected FreeMem expression"),
        }
    }

    #[test]
    fn test_pointer_types() {
        let int_pointer = Type::Pointer(Box::new(Type::Integer));
        let string_pointer = Type::Pointer(Box::new(Type::String(None)));
        let record_pointer = Type::Pointer(Box::new(Type::Record {
            fields: {
                let mut fields = std::collections::HashMap::new();
                fields.insert("value".to_string(), Type::Integer);
                fields
            },
            is_packed: false,
            variant_part: None,
        }));
        
        match int_pointer {
            Type::Pointer(inner_type) => {
                assert!(matches!(*inner_type, Type::Integer));
            },
            _ => panic!("Expected Pointer type"),
        }
        
        match string_pointer {
            Type::Pointer(inner_type) => {
                assert!(matches!(*inner_type, Type::String(None)));
            },
            _ => panic!("Expected Pointer type"),
        }
        
        match record_pointer {
            Type::Pointer(inner_type) => {
                assert!(matches!(*inner_type, Type::Record { .. }));
            },
            _ => panic!("Expected Pointer type"),
        }
    }

    #[test]
    fn test_address_of_expression() {
        let address_of_expr = Expr::AddressOf(Box::new(Expr::Identifier(vec!["variable".to_string()])));
        
        match address_of_expr {
            Expr::AddressOf(expr) => {
                assert!(matches!(*expr, Expr::Identifier(_)));
            },
            _ => panic!("Expected AddressOf expression"),
        }
    }

    #[test]
    fn test_dereference_expression() {
        let deref_expr = Expr::Dereference(Box::new(Expr::Identifier(vec!["ptr".to_string()])));
        
        match deref_expr {
            Expr::Dereference(expr) => {
                assert!(matches!(*expr, Expr::Identifier(_)));
            },
            _ => panic!("Expected Dereference expression"),
        }
    }

    #[test]
    fn test_pointer_arithmetic() {
        let pointer_arithmetic = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["ptr".to_string()])),
            op: BinaryOp::Add,
            right: Box::new(Expr::Literal(Literal::Integer(4))),
        };
        
        match pointer_arithmetic {
            Expr::BinaryOp { left, op, right } => {
                assert!(matches!(*left, Expr::Identifier(_)));
                assert_eq!(op, BinaryOp::Add);
                assert!(matches!(*right, Expr::Literal(Literal::Integer(4))));
            },
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_memory_allocation_procedure() {
        let alloc_procedure = ProcedureDecl {
            name: "AllocateMemory".to_string(),
            params: vec![
                Parameter {
                    name: "size".to_string(),
                    param_type: Type::Integer,
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                }
            ],
            block: Block {
                consts: Vec::new(),
                types: Vec::new(),
                vars: vec![
                    VariableDecl {
                        name: "ptr".to_string(),
                        var_type: Type::Pointer(Box::new(Type::Char)),
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    }
                ],
                procedures: Vec::new(),
                functions: Vec::new(),
                statements: vec![
                    Stmt::Assignment {
                        target: Expr::Identifier(vec!["ptr".to_string()]),
                        value: Expr::GetMem {
                            size: Box::new(Expr::Identifier(vec!["size".to_string()])),
                        },
                    },
                    Stmt::If {
                        condition: Expr::BinaryOp {
                            left: Box::new(Expr::Identifier(vec!["ptr".to_string()])),
                            op: BinaryOp::Equal,
                            right: Box::new(Expr::Literal(Literal::Integer(0))),
                        },
                        then_branch: vec![
                            Stmt::Raise {
                                exception_type: Some("EOutOfMemory".to_string()),
                                message: Some(Expr::Literal(Literal::String("Memory allocation failed".to_string()))),
                            }
                        ],
                        else_branch: None,
                    }
                ],
            },
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: false,
            calling_convention: None,
        };
        
        assert_eq!(alloc_procedure.name, "AllocateMemory");
        assert_eq!(alloc_procedure.params.len(), 1);
        assert_eq!(alloc_procedure.block.vars.len(), 1);
        assert_eq!(alloc_procedure.block.statements.len(), 2);
    }

    #[test]
    fn test_memory_deallocation_procedure() {
        let dealloc_procedure = ProcedureDecl {
            name: "DeallocateMemory".to_string(),
            params: vec![
                Parameter {
                    name: "ptr".to_string(),
                    param_type: Type::Pointer(Box::new(Type::Char)),
                    is_var: true,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                }
            ],
            block: Block {
                consts: Vec::new(),
                types: Vec::new(),
                vars: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                statements: vec![
                    Stmt::If {
                        condition: Expr::BinaryOp {
                            left: Box::new(Expr::Identifier(vec!["ptr".to_string()])),
                            op: BinaryOp::NotEqual,
                            right: Box::new(Expr::Literal(Literal::Integer(0))),
                        },
                        then_branch: vec![
                            Stmt::Assignment {
                                target: Expr::Identifier(vec!["ptr".to_string()]),
                                value: Expr::FreeMem {
                                    expr: Box::new(Expr::Identifier(vec!["ptr".to_string()])),
                                },
                            }
                        ],
                        else_branch: None,
                    }
                ],
            },
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: false,
            calling_convention: None,
        };
        
        assert_eq!(dealloc_procedure.name, "DeallocateMemory");
        assert_eq!(dealloc_procedure.params.len(), 1);
        assert!(dealloc_procedure.params[0].is_var);
        assert_eq!(dealloc_procedure.block.statements.len(), 1);
    }

    #[test]
    fn test_object_lifecycle() {
        let object_lifecycle = vec![
            // Create object
            Stmt::Assignment {
                target: Expr::Identifier(vec!["obj".to_string()]),
                value: Expr::New {
                    type_name: "TMyObject".to_string(),
                    args: vec![
                        Expr::Literal(Literal::String("test".to_string())),
                    ],
                },
            },
            // Use object
            Stmt::Assignment {
                target: Expr::Identifier(vec!["result".to_string()]),
                value: Expr::MethodCall {
                    object: Box::new(Expr::Identifier(vec!["obj".to_string()])),
                    method: "DoSomething".to_string(),
                    args: vec![
                        Expr::Literal(Literal::Integer(42)),
                    ],
                },
            },
            // Dispose object
            Stmt::Assignment {
                target: Expr::Identifier(vec!["obj".to_string()]),
                value: Expr::Dispose {
                    expr: Box::new(Expr::Identifier(vec!["obj".to_string()])),
                },
            },
        ];
        
        assert_eq!(object_lifecycle.len(), 3);
        
        // Check object creation
        match &object_lifecycle[0] {
            Stmt::Assignment { target, value } => {
                assert!(matches!(target, Expr::Identifier(_)));
                assert!(matches!(value, Expr::New { .. }));
            },
            _ => panic!("Expected assignment with New expression"),
        }
        
        // Check object usage
        match &object_lifecycle[1] {
            Stmt::Assignment { target, value } => {
                assert!(matches!(target, Expr::Identifier(_)));
                assert!(matches!(value, Expr::MethodCall { .. }));
            },
            _ => panic!("Expected assignment with MethodCall expression"),
        }
        
        // Check object disposal
        match &object_lifecycle[2] {
            Stmt::Assignment { target, value } => {
                assert!(matches!(target, Expr::Identifier(_)));
                assert!(matches!(value, Expr::Dispose { .. }));
            },
            _ => panic!("Expected assignment with Dispose expression"),
        }
    }

    #[test]
    fn test_memory_management_with_exception_handling() {
        let safe_allocation = Stmt::Try {
            body: vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["ptr".to_string()]),
                    value: Expr::GetMem {
                        size: Box::new(Expr::Literal(Literal::Integer(1024))),
                    },
                },
                Stmt::Assignment {
                    target: Expr::Dereference(Box::new(Expr::Identifier(vec!["ptr".to_string()]))),
                    value: Expr::Literal(Literal::Integer(42)),
                }
            ],
            except_arms: vec![
                ExceptArm {
                    exception_type: Some("EOutOfMemory".to_string()),
                    variable: Some("e".to_string()),
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["ptr".to_string()]),
                            value: Expr::Literal(Literal::Integer(0)),
                        }
                    ],
                }
            ],
            finally_body: Some(vec![
                Stmt::If {
                    condition: Expr::BinaryOp {
                        left: Box::new(Expr::Identifier(vec!["ptr".to_string()])),
                        op: BinaryOp::NotEqual,
                        right: Box::new(Expr::Literal(Literal::Integer(0))),
                    },
                    then_branch: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["ptr".to_string()]),
                            value: Expr::FreeMem {
                                expr: Box::new(Expr::Identifier(vec!["ptr".to_string()])),
                            },
                        }
                    ],
                    else_branch: None,
                }
            ]),
        };
        
        match safe_allocation {
            Stmt::Try { body, except_arms, finally_body } => {
                assert_eq!(body.len(), 2);
                assert_eq!(except_arms.len(), 1);
                assert!(finally_body.is_some());
                assert_eq!(finally_body.unwrap().len(), 1);
            },
            _ => panic!("Expected Try statement"),
        }
    }

    #[test]
    fn test_sizeof_expression() {
        let sizeof_expr = Expr::SizeOf {
            type_name: Type::Record {
                fields: {
                    let mut fields = std::collections::HashMap::new();
                    fields.insert("field1".to_string(), Type::Integer);
                    fields.insert("field2".to_string(), Type::String(None));
                    fields
                },
                is_packed: false,
                variant_part: None,
            },
        };
        
        match sizeof_expr {
            Expr::SizeOf { type_name } => {
                assert!(matches!(type_name, Type::Record { .. }));
            },
            _ => panic!("Expected SizeOf expression"),
        }
    }

    #[test]
    fn test_typeof_expression() {
        let typeof_expr = Expr::TypeOf {
            expr: Box::new(Expr::Identifier(vec!["variable".to_string()])),
        };
        
        match typeof_expr {
            Expr::TypeOf { expr } => {
                assert!(matches!(*expr, Expr::Identifier(_)));
            },
            _ => panic!("Expected TypeOf expression"),
        }
    }

    #[test]
    fn test_pointer_comparison() {
        let pointer_comparison = Expr::BinaryOp {
            left: Box::new(Expr::Identifier(vec!["ptr1".to_string()])),
            op: BinaryOp::Equal,
            right: Box::new(Expr::Identifier(vec!["ptr2".to_string()])),
        };
        
        match pointer_comparison {
            Expr::BinaryOp { left, op, right } => {
                assert!(matches!(*left, Expr::Identifier(_)));
                assert_eq!(op, BinaryOp::Equal);
                assert!(matches!(*right, Expr::Identifier(_)));
            },
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_nil_pointer() {
        let nil_pointer = Expr::Literal(Literal::Integer(0));
        
        match nil_pointer {
            Expr::Literal(Literal::Integer(0)) => {
                // Nil pointer represented as integer 0
            },
            _ => panic!("Expected integer literal 0 for nil pointer"),
        }
    }

    #[test]
    fn test_pointer_to_pointer() {
        let pointer_to_pointer = Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Integer))));
        
        match pointer_to_pointer {
            Type::Pointer(inner_type) => {
                match *inner_type {
                    Type::Pointer(inner_inner_type) => {
                        assert!(matches!(*inner_inner_type, Type::Integer));
                    },
                    _ => panic!("Expected Pointer type"),
                }
            },
            _ => panic!("Expected Pointer type"),
        }
    }
}
