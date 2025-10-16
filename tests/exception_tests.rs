use minipas::ast::*;

#[cfg(test)]
mod exception_tests {
    use super::*;

    #[test]
    fn test_try_except_statement() {
        let try_except_stmt = Stmt::TryExcept {
            body: vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["x".to_string()]),
                    value: Expr::BinaryOp {
                        left: Box::new(Expr::Literal(Literal::Integer(10))),
                        op: BinaryOp::Divide,
                        right: Box::new(Expr::Literal(Literal::Integer(0))),
                    },
                }
            ],
            except_arms: vec![
                ExceptArm {
                    exception_type: Some("EDivByZero".to_string()),
                    variable: Some("e".to_string()),
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["x".to_string()]),
                            value: Expr::Literal(Literal::Integer(0)),
                        }
                    ],
                },
                ExceptArm {
                    exception_type: None, // catch all
                    variable: None,
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["x".to_string()]),
                            value: Expr::Literal(Literal::Integer(-1)),
                        }
                    ],
                }
            ],
        };
        
        match try_except_stmt {
            Stmt::TryExcept { body, except_arms } => {
                assert_eq!(body.len(), 1);
                assert_eq!(except_arms.len(), 2);
                assert_eq!(except_arms[0].exception_type, Some("EDivByZero".to_string()));
                assert_eq!(except_arms[0].variable, Some("e".to_string()));
                assert!(except_arms[1].exception_type.is_none());
                assert!(except_arms[1].variable.is_none());
            },
            _ => panic!("Expected TryExcept statement"),
        }
    }

    #[test]
    fn test_try_finally_statement() {
        let try_finally_stmt = Stmt::TryFinally {
            body: vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["file".to_string()]),
                    value: Expr::Literal(Literal::String("test.txt".to_string())),
                },
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["data".to_string()]),
                    value: Expr::Literal(Literal::String("content".to_string())),
                }
            ],
            finally_body: vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["file".to_string()]),
                    value: Expr::Literal(Literal::String("".to_string())),
                }
            ],
        };
        
        match try_finally_stmt {
            Stmt::TryFinally { body, finally_body } => {
                assert_eq!(body.len(), 2);
                assert_eq!(finally_body.len(), 1);
            },
            _ => panic!("Expected TryFinally statement"),
        }
    }

    #[test]
    fn test_try_except_finally_statement() {
        let try_except_finally_stmt = Stmt::Try {
            body: vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["result".to_string()]),
                    value: Expr::BinaryOp {
                        left: Box::new(Expr::Identifier(vec!["a".to_string()])),
                        op: BinaryOp::Divide,
                        right: Box::new(Expr::Identifier(vec!["b".to_string()])),
                    },
                }
            ],
            except_arms: vec![
                ExceptArm {
                    exception_type: Some("EDivByZero".to_string()),
                    variable: Some("e".to_string()),
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["result".to_string()]),
                            value: Expr::Literal(Literal::Integer(0)),
                        }
                    ],
                }
            ],
            finally_body: Some(vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["a".to_string()]),
                    value: Expr::Literal(Literal::Integer(0)),
                },
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["b".to_string()]),
                    value: Expr::Literal(Literal::Integer(1)),
                }
            ]),
        };
        
        match try_except_finally_stmt {
            Stmt::Try { body, except_arms, finally_body } => {
                assert_eq!(body.len(), 1);
                assert_eq!(except_arms.len(), 1);
                assert!(finally_body.is_some());
                assert_eq!(finally_body.unwrap().len(), 2);
            },
            _ => panic!("Expected Try statement"),
        }
    }

    #[test]
    fn test_raise_statement_with_exception_type() {
        let raise_stmt = Stmt::Raise {
            exception_type: Some("EInvalidOperation".to_string()),
            message: Some(Expr::Literal(Literal::String("Invalid operation".to_string()))),
        };
        
        match raise_stmt {
            Stmt::Raise { exception_type, message } => {
                assert_eq!(exception_type, Some("EInvalidOperation".to_string()));
                assert!(message.is_some());
                match message.unwrap() {
                    Expr::Literal(Literal::String(msg)) => assert_eq!(msg, "Invalid operation"),
                    _ => panic!("Expected string literal"),
                }
            },
            _ => panic!("Expected Raise statement"),
        }
    }

    #[test]
    fn test_raise_statement_without_exception_type() {
        let raise_stmt = Stmt::Raise {
            exception_type: None,
            message: Some(Expr::Literal(Literal::String("Generic error".to_string()))),
        };
        
        match raise_stmt {
            Stmt::Raise { exception_type, message } => {
                assert!(exception_type.is_none());
                assert!(message.is_some());
            },
            _ => panic!("Expected Raise statement"),
        }
    }

    #[test]
    fn test_raise_statement_without_message() {
        let raise_stmt = Stmt::Raise {
            exception_type: Some("EAbort".to_string()),
            message: None,
        };
        
        match raise_stmt {
            Stmt::Raise { exception_type, message } => {
                assert_eq!(exception_type, Some("EAbort".to_string()));
                assert!(message.is_none());
            },
            _ => panic!("Expected Raise statement"),
        }
    }

    #[test]
    fn test_except_arm_with_specific_exception() {
        let except_arm = ExceptArm {
            exception_type: Some("EOutOfMemory".to_string()),
            variable: Some("memError".to_string()),
            body: vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["errorCode".to_string()]),
                    value: Expr::Literal(Literal::Integer(1)),
                },
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["errorMessage".to_string()]),
                    value: Expr::Literal(Literal::String("Out of memory".to_string())),
                }
            ],
        };
        
        assert_eq!(except_arm.exception_type, Some("EOutOfMemory".to_string()));
        assert_eq!(except_arm.variable, Some("memError".to_string()));
        assert_eq!(except_arm.body.len(), 2);
    }

    #[test]
    fn test_except_arm_catch_all() {
        let except_arm = ExceptArm {
            exception_type: None,
            variable: None,
            body: vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["errorCode".to_string()]),
                    value: Expr::Literal(Literal::Integer(-1)),
                }
            ],
        };
        
        assert!(except_arm.exception_type.is_none());
        assert!(except_arm.variable.is_none());
        assert_eq!(except_arm.body.len(), 1);
    }

    #[test]
    fn test_nested_try_blocks() {
        let nested_try = Stmt::Try {
            body: vec![
                Stmt::Try {
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["inner".to_string()]),
                            value: Expr::Literal(Literal::Integer(1)),
                        }
                    ],
                    except_arms: vec![
                        ExceptArm {
                            exception_type: Some("EInner".to_string()),
                            variable: Some("innerError".to_string()),
                            body: vec![
                                Stmt::Assignment {
                                    target: Expr::Identifier(vec!["inner".to_string()]),
                                    value: Expr::Literal(Literal::Integer(0)),
                                }
                            ],
                        }
                    ],
                    finally_body: None,
                }
            ],
            except_arms: vec![
                ExceptArm {
                    exception_type: Some("EOuter".to_string()),
                    variable: Some("outerError".to_string()),
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["outer".to_string()]),
                            value: Expr::Literal(Literal::Integer(0)),
                        }
                    ],
                }
            ],
            finally_body: Some(vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["cleanup".to_string()]),
                    value: Expr::Literal(Literal::Boolean(true)),
                }
            ]),
        };
        
        match nested_try {
            Stmt::Try { body, except_arms, finally_body } => {
                assert_eq!(body.len(), 1);
                assert_eq!(except_arms.len(), 1);
                assert!(finally_body.is_some());
                
                // Check nested try
                match &body[0] {
                    Stmt::Try { body: inner_body, except_arms: inner_except, finally_body: inner_finally } => {
                        assert_eq!(inner_body.len(), 1);
                        assert_eq!(inner_except.len(), 1);
                        assert!(inner_finally.is_none());
                    },
                    _ => panic!("Expected nested Try statement"),
                }
            },
            _ => panic!("Expected Try statement"),
        }
    }

    #[test]
    fn test_exception_handling_in_procedures() {
        let procedure_with_exception = ProcedureDecl {
            name: "RiskyProcedure".to_string(),
            params: vec![
                Parameter {
                    name: "value".to_string(),
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
                vars: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                statements: vec![
                    Stmt::Try {
                        body: vec![
                            Stmt::If {
                                condition: Expr::BinaryOp {
                                    left: Box::new(Expr::Identifier(vec!["value".to_string()])),
                                    op: BinaryOp::Less,
                                    right: Box::new(Expr::Literal(Literal::Integer(0))),
                                },
                                then_branch: vec![
                                    Stmt::Raise {
                                        exception_type: Some("EInvalidValue".to_string()),
                                        message: Some(Expr::Literal(Literal::String("Value must be positive".to_string()))),
                                    }
                                ],
                                else_branch: None,
                            }
                        ],
                        except_arms: vec![
                            ExceptArm {
                                exception_type: Some("EInvalidValue".to_string()),
                                variable: Some("e".to_string()),
                                body: vec![
                                    Stmt::Assignment {
                                        target: Expr::Identifier(vec!["result".to_string()]),
                                        value: Expr::Literal(Literal::Integer(0)),
                                    }
                                ],
                            }
                        ],
                        finally_body: None,
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
        
        assert_eq!(procedure_with_exception.name, "RiskyProcedure");
        assert_eq!(procedure_with_exception.params.len(), 1);
        assert_eq!(procedure_with_exception.block.statements.len(), 1);
    }

    #[test]
    fn test_exception_handling_in_functions() {
        let function_with_exception = FunctionDecl {
            name: "SafeDivide".to_string(),
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
                }
            ],
            return_type: Type::Integer,
            block: Block {
                consts: Vec::new(),
                types: Vec::new(),
                vars: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                statements: vec![
                    Stmt::Try {
                        body: vec![
                            Stmt::If {
                                condition: Expr::BinaryOp {
                                    left: Box::new(Expr::Identifier(vec!["b".to_string()])),
                                    op: BinaryOp::Equal,
                                    right: Box::new(Expr::Literal(Literal::Integer(0))),
                                },
                                then_branch: vec![
                                    Stmt::Raise {
                                        exception_type: Some("EDivByZero".to_string()),
                                        message: Some(Expr::Literal(Literal::String("Division by zero".to_string()))),
                                    }
                                ],
                                else_branch: None,
                            },
                            Stmt::Assignment {
                                target: Expr::Identifier(vec!["result".to_string()]),
                                value: Expr::BinaryOp {
                                    left: Box::new(Expr::Identifier(vec!["a".to_string()])),
                                    op: BinaryOp::Divide,
                                    right: Box::new(Expr::Identifier(vec!["b".to_string()])),
                                },
                            }
                        ],
                        except_arms: vec![
                            ExceptArm {
                                exception_type: Some("EDivByZero".to_string()),
                                variable: Some("e".to_string()),
                                body: vec![
                                    Stmt::Assignment {
                                        target: Expr::Identifier(vec!["result".to_string()]),
                                        value: Expr::Literal(Literal::Integer(0)),
                                    }
                                ],
                            }
                        ],
                        finally_body: None,
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
        
        assert_eq!(function_with_exception.name, "SafeDivide");
        assert_eq!(function_with_exception.params.len(), 2);
        assert_eq!(function_with_exception.return_type, Type::Integer);
    }

    #[test]
    fn test_multiple_exception_types() {
        let try_with_multiple_exceptions = Stmt::Try {
            body: vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["result".to_string()]),
                    value: Expr::BinaryOp {
                        left: Box::new(Expr::Identifier(vec!["a".to_string()])),
                        op: BinaryOp::Divide,
                        right: Box::new(Expr::Identifier(vec!["b".to_string()])),
                    },
                }
            ],
            except_arms: vec![
                ExceptArm {
                    exception_type: Some("EDivByZero".to_string()),
                    variable: Some("divError".to_string()),
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["result".to_string()]),
                            value: Expr::Literal(Literal::Integer(0)),
                        }
                    ],
                },
                ExceptArm {
                    exception_type: Some("EOverflow".to_string()),
                    variable: Some("overflowError".to_string()),
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["result".to_string()]),
                            value: Expr::Literal(Literal::Integer(-1)),
                        }
                    ],
                },
                ExceptArm {
                    exception_type: Some("EOutOfMemory".to_string()),
                    variable: Some("memError".to_string()),
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["result".to_string()]),
                            value: Expr::Literal(Literal::Integer(-2)),
                        }
                    ],
                },
                ExceptArm {
                    exception_type: None, // catch all
                    variable: None,
                    body: vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["result".to_string()]),
                            value: Expr::Literal(Literal::Integer(-999)),
                        }
                    ],
                }
            ],
            finally_body: None,
        };
        
        match try_with_multiple_exceptions {
            Stmt::Try { body, except_arms, finally_body } => {
                assert_eq!(body.len(), 1);
                assert_eq!(except_arms.len(), 4);
                assert!(finally_body.is_none());
                
                // Check specific exception types
                assert_eq!(except_arms[0].exception_type, Some("EDivByZero".to_string()));
                assert_eq!(except_arms[1].exception_type, Some("EOverflow".to_string()));
                assert_eq!(except_arms[2].exception_type, Some("EOutOfMemory".to_string()));
                assert!(except_arms[3].exception_type.is_none()); // catch all
            },
            _ => panic!("Expected Try statement"),
        }
    }
}
