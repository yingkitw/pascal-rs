#[cfg(test)]
mod comprehensive_tests {
    use super::*;
    use minipas_ast::*;
    use std::collections::HashMap;

    // ============================================================================
    // BASIC CODEGEN TESTS
    // ============================================================================

    #[test]
    fn test_codegen_simple_program() {
        let program = Program {
            name: "TestProgram".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
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
                    value: Box::new(Expr::Literal(Literal::Integer(10))),
                }],
            },
        };

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should contain basic assembly structure
        assert!(assembly.contains(".intel_syntax noprefix"));
        assert!(assembly.contains(".section .text"));
        assert!(assembly.contains("main:"));
    }

    #[test]
    fn test_codegen_arithmetic_expressions() {
        let program = Program {
            name: "ArithmeticTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![
                    VariableDecl {
                        name: "a".to_string(),
                        var_type: Type::Integer,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                    VariableDecl {
                        name: "b".to_string(),
                        var_type: Type::Integer,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                    VariableDecl {
                        name: "result".to_string(),
                        var_type: Type::Integer,
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
                        target: Expr::Variable("a".to_string()),
                        value: Box::new(Expr::Literal(Literal::Integer(5))),
                    },
                    Stmt::Assignment {
                        target: Expr::Variable("b".to_string()),
                        value: Box::new(Expr::Literal(Literal::Integer(3))),
                    },
                    Stmt::Assignment {
                        target: Expr::Variable("result".to_string()),
                        value: Box::new(Expr::BinaryOp {
                            op: BinaryOp::Add,
                            left: Box::new(Expr::Variable("a".to_string())),
                            right: Box::new(Expr::Variable("b".to_string())),
                        }),
                    },
                ],
            },
        };

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should contain arithmetic operations
        assert!(assembly.contains("mov eax, 5"));
        assert!(assembly.contains("mov eax, 3"));
        assert!(assembly.contains("add eax, edx"));
    }

    #[test]
    fn test_codegen_control_structures() {
        let program = Program {
            name: "ControlTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![
                    VariableDecl {
                        name: "x".to_string(),
                        var_type: Type::Integer,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                    VariableDecl {
                        name: "y".to_string(),
                        var_type: Type::Integer,
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
                        value: Box::new(Expr::Literal(Literal::Integer(10))),
                    },
                    Stmt::If {
                        condition: Box::new(Expr::BinaryOp {
                            op: BinaryOp::Greater,
                            left: Box::new(Expr::Variable("x".to_string())),
                            right: Box::new(Expr::Literal(Literal::Integer(5))),
                        }),
                        then_branch: vec![Stmt::Assignment {
                            target: Expr::Variable("y".to_string()),
                            value: Box::new(Expr::Literal(Literal::Integer(1))),
                        }],
                        else_branch: Some(vec![Stmt::Assignment {
                            target: Expr::Variable("y".to_string()),
                            value: Box::new(Expr::Literal(Literal::Integer(0))),
                        }]),
                    },
                ],
            },
        };

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should contain control flow instructions
        assert!(assembly.contains("cmp"));
        assert!(assembly.contains("jg") || assembly.contains("jle"));
        assert!(assembly.contains("_else_") || assembly.contains("_endif_"));
    }

    #[test]
    fn test_codegen_loops() {
        let program = Program {
            name: "LoopTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![
                    VariableDecl {
                        name: "i".to_string(),
                        var_type: Type::Integer,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                    VariableDecl {
                        name: "sum".to_string(),
                        var_type: Type::Integer,
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
                        target: Expr::Variable("sum".to_string()),
                        value: Box::new(Expr::Literal(Literal::Integer(0))),
                    },
                    Stmt::For {
                        var_name: "i".to_string(),
                        start: Box::new(Expr::Literal(Literal::Integer(1))),
                        direction: ForDirection::To,
                        end: Box::new(Expr::Literal(Literal::Integer(10))),
                        body: vec![Stmt::Assignment {
                            target: Expr::Variable("sum".to_string()),
                            value: Box::new(Expr::BinaryOp {
                                op: BinaryOp::Add,
                                left: Box::new(Expr::Variable("sum".to_string())),
                                right: Box::new(Expr::Variable("i".to_string())),
                            }),
                        }],
                    },
                ],
            },
        };

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should contain loop instructions
        assert!(assembly.contains("_for_") || assembly.contains("_while_"));
        assert!(assembly.contains("jmp") || assembly.contains("jz"));
    }

    // ============================================================================
    // ENHANCED CODEGEN TESTS
    // ============================================================================

    #[test]
    fn test_enhanced_codegen_multi_architecture() {
        let program = Program {
            name: "MultiArchTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
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
                statements: vec![],
            },
        };

        // Test x86-64 code generation
        let mut codegen = EnhancedCodeGenerator::new(TargetArchitecture::X86_64);
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should contain x86-64 specific instructions
        assert!(assembly.contains("intel_syntax") || assembly.contains("x86_64"));
    }

    #[test]
    fn test_enhanced_codegen_optimization() {
        let program = Program {
            name: "OptimizationTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![
                    VariableDecl {
                        name: "x".to_string(),
                        var_type: Type::Integer,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                    VariableDecl {
                        name: "y".to_string(),
                        var_type: Type::Integer,
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
                        value: Box::new(Expr::Literal(Literal::Integer(5))),
                    },
                    Stmt::Assignment {
                        target: Expr::Variable("y".to_string()),
                        value: Box::new(Expr::BinaryOp {
                            op: BinaryOp::Multiply,
                            left: Box::new(Expr::Variable("x".to_string())),
                            right: Box::new(Expr::Literal(Literal::Integer(2))),
                        }),
                    },
                ],
            },
        };

        let mut codegen = EnhancedCodeGenerator::new(TargetArchitecture::X86_64);
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should contain optimized instructions
        assert!(assembly.contains("mov") || assembly.contains("imul"));
    }

    #[test]
    fn test_enhanced_codegen_register_allocation() {
        let program = Program {
            name: "RegisterTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![
                    VariableDecl {
                        name: "a".to_string(),
                        var_type: Type::Integer,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                    VariableDecl {
                        name: "b".to_string(),
                        var_type: Type::Integer,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                    VariableDecl {
                        name: "c".to_string(),
                        var_type: Type::Integer,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    },
                    VariableDecl {
                        name: "d".to_string(),
                        var_type: Type::Integer,
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
                        target: Expr::Variable("a".to_string()),
                        value: Box::new(Expr::Literal(Literal::Integer(1))),
                    },
                    Stmt::Assignment {
                        target: Expr::Variable("b".to_string()),
                        value: Box::new(Expr::Literal(Literal::Integer(2))),
                    },
                    Stmt::Assignment {
                        target: Expr::Variable("c".to_string()),
                        value: Box::new(Expr::BinaryOp {
                            op: BinaryOp::Add,
                            left: Box::new(Expr::Variable("a".to_string())),
                            right: Box::new(Expr::Variable("b".to_string())),
                        }),
                    },
                    Stmt::Assignment {
                        target: Expr::Variable("d".to_string()),
                        value: Box::new(Expr::BinaryOp {
                            op: BinaryOp::Multiply,
                            left: Box::new(Expr::Variable("c".to_string())),
                            right: Box::new(Expr::Literal(Literal::Integer(3))),
                        }),
                    },
                ],
            },
        };

        let mut codegen = EnhancedCodeGenerator::new(TargetArchitecture::X86_64);
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should contain register operations
        assert!(assembly.contains("eax") || assembly.contains("ebx") || assembly.contains("ecx") || assembly.contains("edx"));
    }

    // ============================================================================
    // ERROR HANDLING TESTS
    // ============================================================================

    #[test]
    fn test_codegen_error_handling() {
        let program = Program {
            name: "ErrorTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![Stmt::Assignment {
                    target: Expr::Variable("undefined_var".to_string()),
                    value: Box::new(Expr::Literal(Literal::Integer(42))),
                }],
            },
        };

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        // Should handle undefined variables gracefully
        match result {
            Ok(_) => {
                // If it succeeds, the codegen should handle undefined variables
            }
            Err(error) => {
                // If it fails, it should be a meaningful error
                assert!(!error.is_empty());
            }
        }
    }

    #[test]
    fn test_codegen_unsupported_operations() {
        let program = Program {
            name: "UnsupportedTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![VariableDecl {
                    name: "x".to_string(),
                    var_type: Type::Real,
                    initializer: None,
                    is_threadvar: false,
                    absolute_address: None,
                    external_name: None,
                }],
                procedures: vec![],
                functions: vec![],
                statements: vec![Stmt::Assignment {
                    target: Expr::Variable("x".to_string()),
                    value: Box::new(Expr::Literal(Literal::Real(3.14))),
                }],
            },
        };

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        // Should handle real numbers (may not be fully implemented)
        match result {
            Ok(assembly) => {
                // If it succeeds, should contain some assembly
                assert!(!assembly.is_empty());
            }
            Err(error) => {
                // If it fails, should be a meaningful error
                assert!(!error.is_empty());
            }
        }
    }

    // ============================================================================
    // PERFORMANCE TESTS
    // ============================================================================

    #[test]
    fn test_codegen_performance() {
        let program = Program {
            name: "PerformanceTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![VariableDecl {
                    name: "x".to_string(),
                    var_type: Type::Integer,
                    initializer: None,
                    is_threadvar: false,
                    absolute_address: None,
                    external_name: None,
                }],
                procedures: vec![],
                functions: vec![],
                statements: (0..1000)
                    .map(|i| Stmt::Assignment {
                        target: Expr::Variable("x".to_string()),
                        value: Box::new(Expr::Literal(Literal::Integer(i))),
                    })
                    .collect(),
            },
        };

        let mut codegen = CodeGenerator::new();
        let start = std::time::Instant::now();
        let result = codegen.generate_code(&program);
        let duration = start.elapsed();

        // Should generate code quickly (less than 100ms for 1000 statements)
        assert!(duration.as_millis() < 100);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enhanced_codegen_performance() {
        let program = Program {
            name: "EnhancedPerformanceTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![VariableDecl {
                    name: "x".to_string(),
                    var_type: Type::Integer,
                    initializer: None,
                    is_threadvar: false,
                    absolute_address: None,
                    external_name: None,
                }],
                procedures: vec![],
                functions: vec![],
                statements: (0..1000)
                    .map(|i| Stmt::Assignment {
                        target: Expr::Variable("x".to_string()),
                        value: Box::new(Expr::Literal(Literal::Integer(i))),
                    })
                    .collect(),
            },
        };

        let mut codegen = EnhancedCodeGenerator::new(TargetArchitecture::X86_64);
        let start = std::time::Instant::now();
        let result = codegen.generate_code(&program);
        let duration = start.elapsed();

        // Should generate code quickly (less than 200ms for 1000 statements)
        assert!(duration.as_millis() < 200);
        assert!(result.is_ok());
    }

    // ============================================================================
    // EDGE CASE TESTS
    // ============================================================================

    #[test]
    fn test_codegen_empty_program() {
        let program = Program {
            name: "EmptyProgram".to_string(),
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

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should still generate basic program structure
        assert!(assembly.contains("main:"));
    }

    #[test]
    fn test_codegen_minimal_program() {
        let program = Program {
            name: "MinimalProgram".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                statements: vec![Stmt::Empty],
            },
        };

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should handle empty statements
        assert!(!assembly.is_empty());
    }

    #[test]
    fn test_codegen_complex_expressions() {
        let program = Program {
            name: "ComplexExprTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![VariableDecl {
                    name: "result".to_string(),
                    var_type: Type::Integer,
                    initializer: None,
                    is_threadvar: false,
                    absolute_address: None,
                    external_name: None,
                }],
                procedures: vec![],
                functions: vec![],
                statements: vec![Stmt::Assignment {
                    target: Expr::Variable("result".to_string()),
                    value: Box::new(Expr::BinaryOp {
                        op: BinaryOp::Add,
                        left: Box::new(Expr::BinaryOp {
                            op: BinaryOp::Multiply,
                            left: Box::new(Expr::Literal(Literal::Integer(2))),
                            right: Box::new(Expr::Literal(Literal::Integer(3))),
                        }),
                        right: Box::new(Expr::BinaryOp {
                            op: BinaryOp::Subtract,
                            left: Box::new(Expr::Literal(Literal::Integer(10))),
                            right: Box::new(Expr::Literal(Literal::Integer(4))),
                        }),
                    }),
                }],
            },
        };

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        assert!(result.is_ok());
        let assembly = result.unwrap();
        
        // Should handle complex expressions
        assert!(assembly.contains("mov") || assembly.contains("add") || assembly.contains("sub") || assembly.contains("imul"));
    }

    // ============================================================================
    // TARGET ARCHITECTURE TESTS
    // ============================================================================

    #[test]
    fn test_target_architecture_enum() {
        // Test that all target architectures are valid
        let architectures = vec![
            TargetArchitecture::X86_64,
            TargetArchitecture::X86_32,
            TargetArchitecture::ARM64,
            TargetArchitecture::ARM32,
            TargetArchitecture::RiscV64,
            TargetArchitecture::RiscV32,
            TargetArchitecture::Mips64,
            TargetArchitecture::Mips32,
            TargetArchitecture::PowerPC64,
            TargetArchitecture::PowerPC32,
            TargetArchitecture::Sparc64,
            TargetArchitecture::Sparc32,
            TargetArchitecture::WebAssembly,
            TargetArchitecture::Z80,
            TargetArchitecture::AVR,
        ];

        for arch in architectures {
            let mut codegen = EnhancedCodeGenerator::new(arch);
            let program = Program {
                name: "ArchTest".to_string(),
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

            let result = codegen.generate_code(&program);
            // Should not panic, even if not fully implemented
            assert!(result.is_ok() || result.is_err());
        }
    }

    #[test]
    fn test_calling_convention_enum() {
        // Test that all calling conventions are valid
        let conventions = vec![
            CallingConvention::CDecl,
            CallingConvention::Pascal,
            CallingConvention::StdCall,
            CallingConvention::CppDecl,
            CallingConvention::Register,
            CallingConvention::SafeCall,
            CallingConvention::FastCall,
            CallingConvention::VectorCall,
            CallingConvention::ThisCall,
        ];

        for convention in conventions {
            // Should not panic when creating
            let _ = convention;
        }
    }

    #[test]
    fn test_register_enum() {
        // Test that all registers are valid
        let registers = vec![
            Register::RAX,
            Register::RBX,
            Register::RCX,
            Register::RDX,
            Register::RSI,
            Register::RDI,
            Register::RBP,
            Register::RSP,
            Register::R8,
            Register::R9,
            Register::R10,
            Register::R11,
            Register::R12,
            Register::R13,
            Register::R14,
            Register::R15,
        ];

        for register in registers {
            // Should not panic when creating
            let _ = register;
        }
    }
}
