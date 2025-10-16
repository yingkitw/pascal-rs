use minipas::ast::*;

#[cfg(test)]
mod inline_assembly_tests {
    use super::*;

    #[test]
    fn test_inline_assembly_expression() {
        let asm_expr = Expr::InlineAssembly {
            code: "mov eax, 42\nadd eax, ebx".to_string(),
            inputs: vec![
                ("ebx".to_string(), Expr::Literal(Literal::Integer(10))),
            ],
            outputs: vec![
                ("eax".to_string(), "result".to_string()),
            ],
            clobbers: vec!["eax".to_string(), "ebx".to_string()],
        };
        
        match asm_expr {
            Expr::InlineAssembly { code, inputs, outputs, clobbers } => {
                assert_eq!(code, "mov eax, 42\nadd eax, ebx");
                assert_eq!(inputs.len(), 1);
                assert_eq!(inputs[0].0, "ebx");
                assert!(matches!(inputs[0].1, Expr::Literal(Literal::Integer(10))));
                assert_eq!(outputs.len(), 1);
                assert_eq!(outputs[0].0, "eax");
                assert_eq!(outputs[0].1, "result");
                assert_eq!(clobbers.len(), 2);
                assert!(clobbers.contains(&"eax".to_string()));
                assert!(clobbers.contains(&"ebx".to_string()));
            },
            _ => panic!("Expected InlineAssembly expression"),
        }
    }

    #[test]
    fn test_assembler_procedure() {
        let asm_procedure = ProcedureDecl {
            name: "FastAdd".to_string(),
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
            block: Block {
                consts: Vec::new(),
                types: Vec::new(),
                vars: vec![
                    VariableDecl {
                        name: "result".to_string(),
                        var_type: Type::Integer,
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
                        target: Expr::Identifier(vec!["result".to_string()]),
                        value: Expr::InlineAssembly {
                            code: "mov eax, [a]\nadd eax, [b]\nmov [result], eax".to_string(),
                            inputs: vec![
                                ("a".to_string(), Expr::Identifier(vec!["a".to_string()])),
                                ("b".to_string(), Expr::Identifier(vec!["b".to_string()])),
                            ],
                            outputs: vec![
                                ("result".to_string(), "result".to_string()),
                            ],
                            clobbers: vec!["eax".to_string()],
                        },
                    }
                ],
            },
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: true,
            calling_convention: Some(CallingConvention::Register),
        };
        
        assert_eq!(asm_procedure.name, "FastAdd");
        assert!(asm_procedure.is_assembler);
        assert_eq!(asm_procedure.calling_convention, Some(CallingConvention::Register));
        assert_eq!(asm_procedure.params.len(), 2);
        assert_eq!(asm_procedure.block.statements.len(), 1);
    }

    #[test]
    fn test_assembler_function() {
        let asm_function = FunctionDecl {
            name: "MultiplyByTwo".to_string(),
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
            return_type: Type::Integer,
            block: Block {
                consts: Vec::new(),
                types: Vec::new(),
                vars: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                statements: vec![
                    Stmt::Assignment {
                        target: Expr::Identifier(vec!["result".to_string()]),
                        value: Expr::InlineAssembly {
                        code: "mov eax, [value]\nshl eax, 1".to_string(),
                        inputs: vec![
                            ("value".to_string(), Expr::Identifier(vec!["value".to_string()])),
                        ],
                        outputs: vec![
                            ("eax".to_string(), "return_value".to_string()),
                        ],
                        clobbers: vec!["eax".to_string()],
                    },
                },
                ],
            },
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: true,
            calling_convention: Some(CallingConvention::Register),
        };
        
        assert_eq!(asm_function.name, "MultiplyByTwo");
        assert!(asm_function.is_assembler);
        assert_eq!(asm_function.calling_convention, Some(CallingConvention::Register));
        assert_eq!(asm_function.params.len(), 1);
        assert_eq!(asm_function.block.statements.len(), 1);
    }

    #[test]
    fn test_assembly_with_multiple_inputs() {
        let complex_asm = Expr::InlineAssembly {
            code: "mov eax, [a]\nmov ebx, [b]\nmul ebx\nmov [result], eax".to_string(),
            inputs: vec![
                ("a".to_string(), Expr::Literal(Literal::Integer(5))),
                ("b".to_string(), Expr::Literal(Literal::Integer(3))),
            ],
            outputs: vec![
                ("result".to_string(), "product".to_string()),
            ],
            clobbers: vec!["eax".to_string(), "ebx".to_string()],
        };
        
        match complex_asm {
            Expr::InlineAssembly { code, inputs, outputs, clobbers } => {
                assert_eq!(code, "mov eax, [a]\nmov ebx, [b]\nmul ebx\nmov [result], eax");
                assert_eq!(inputs.len(), 2);
                assert_eq!(inputs[0].0, "a");
                assert!(matches!(inputs[0].1, Expr::Literal(Literal::Integer(5))));
                assert_eq!(inputs[1].0, "b");
                assert!(matches!(inputs[1].1, Expr::Literal(Literal::Integer(3))));
                assert_eq!(outputs.len(), 1);
                assert_eq!(outputs[0].0, "result");
                assert_eq!(outputs[0].1, "product");
                assert_eq!(clobbers.len(), 2);
            },
            _ => panic!("Expected InlineAssembly expression"),
        }
    }

    #[test]
    fn test_assembly_with_string_operations() {
        let string_asm = Expr::InlineAssembly {
            code: "mov esi, [src]\nmov edi, [dest]\nmov ecx, [len]\nrep movsb".to_string(),
            inputs: vec![
                ("src".to_string(), Expr::Identifier(vec!["source".to_string()])),
                ("dest".to_string(), Expr::Identifier(vec!["destination".to_string()])),
                ("len".to_string(), Expr::Identifier(vec!["length".to_string()])),
            ],
            outputs: vec![],
            clobbers: vec!["esi".to_string(), "edi".to_string(), "ecx".to_string()],
        };
        
        match string_asm {
            Expr::InlineAssembly { code, inputs, outputs, clobbers } => {
                assert_eq!(code, "mov esi, [src]\nmov edi, [dest]\nmov ecx, [len]\nrep movsb");
                assert_eq!(inputs.len(), 3);
                assert_eq!(outputs.len(), 0);
                assert_eq!(clobbers.len(), 3);
            },
            _ => panic!("Expected InlineAssembly expression"),
        }
    }

    #[test]
    fn test_assembly_with_conditional_jumps() {
        let conditional_asm = Expr::InlineAssembly {
            code: "mov eax, [value]\ncmp eax, 0\njz zero_case\nmov [result], eax\njmp end\nzero_case:\nmov [result], 1\nend:".to_string(),
            inputs: vec![
                ("value".to_string(), Expr::Identifier(vec!["input".to_string()])),
            ],
            outputs: vec![
                ("result".to_string(), "output".to_string()),
            ],
            clobbers: vec!["eax".to_string()],
        };
        
        match conditional_asm {
            Expr::InlineAssembly { code, inputs, outputs, clobbers } => {
                assert!(code.contains("cmp eax, 0"));
                assert!(code.contains("jz zero_case"));
                assert!(code.contains("jmp end"));
                assert_eq!(inputs.len(), 1);
                assert_eq!(outputs.len(), 1);
                assert_eq!(clobbers.len(), 1);
            },
            _ => panic!("Expected InlineAssembly expression"),
        }
    }

    #[test]
    fn test_assembly_with_floating_point() {
        let float_asm = Expr::InlineAssembly {
            code: "fld [a]\nfld [b]\nfaddp st(1), st(0)\nfstp [result]".to_string(),
            inputs: vec![
                ("a".to_string(), Expr::Literal(Literal::Real(3.14))),
                ("b".to_string(), Expr::Literal(Literal::Real(2.86))),
            ],
            outputs: vec![
                ("result".to_string(), "sum".to_string()),
            ],
            clobbers: vec!["st(0)".to_string(), "st(1)".to_string()],
        };
        
        match float_asm {
            Expr::InlineAssembly { code, inputs, outputs, clobbers } => {
                assert!(code.contains("fld"));
                assert!(code.contains("faddp"));
                assert!(code.contains("fstp"));
                assert_eq!(inputs.len(), 2);
                assert_eq!(outputs.len(), 1);
                assert_eq!(clobbers.len(), 2);
            },
            _ => panic!("Expected InlineAssembly expression"),
        }
    }

    #[test]
    fn test_assembly_with_memory_operations() {
        let memory_asm = Expr::InlineAssembly {
            code: "mov eax, [ptr]\nmov [eax], 42\nmov eax, [eax]".to_string(),
            inputs: vec![
                ("ptr".to_string(), Expr::Identifier(vec!["pointer".to_string()])),
            ],
            outputs: vec![
                ("eax".to_string(), "value".to_string()),
            ],
            clobbers: vec!["eax".to_string()],
        };
        
        match memory_asm {
            Expr::InlineAssembly { code, inputs, outputs, clobbers } => {
                assert!(code.contains("mov eax, [ptr]"));
                assert!(code.contains("mov [eax], 42"));
                assert!(code.contains("mov eax, [eax]"));
                assert_eq!(inputs.len(), 1);
                assert_eq!(outputs.len(), 1);
                assert_eq!(clobbers.len(), 1);
            },
            _ => panic!("Expected InlineAssembly expression"),
        }
    }

    #[test]
    fn test_assembly_with_system_calls() {
        let syscall_asm = Expr::InlineAssembly {
            code: "mov eax, 4\nmov ebx, 1\nmov ecx, [message]\nmov edx, [length]\nint 0x80".to_string(),
            inputs: vec![
                ("message".to_string(), Expr::Identifier(vec!["msg".to_string()])),
                ("length".to_string(), Expr::Identifier(vec!["len".to_string()])),
            ],
            outputs: vec![],
            clobbers: vec!["eax".to_string(), "ebx".to_string(), "ecx".to_string(), "edx".to_string()],
        };
        
        match syscall_asm {
            Expr::InlineAssembly { code, inputs, outputs, clobbers } => {
                assert!(code.contains("mov eax, 4"));
                assert!(code.contains("int 0x80"));
                assert_eq!(inputs.len(), 2);
                assert_eq!(outputs.len(), 0);
                assert_eq!(clobbers.len(), 4);
            },
            _ => panic!("Expected InlineAssembly expression"),
        }
    }

    #[test]
    fn test_assembly_with_bit_operations() {
        let bit_asm = Expr::InlineAssembly {
            code: "mov eax, [value]\nand eax, 0xFF\nor eax, 0x100\nxor eax, 0x200\nnot eax".to_string(),
            inputs: vec![
                ("value".to_string(), Expr::Identifier(vec!["input".to_string()])),
            ],
            outputs: vec![
                ("eax".to_string(), "result".to_string()),
            ],
            clobbers: vec!["eax".to_string()],
        };
        
        match bit_asm {
            Expr::InlineAssembly { code, inputs, outputs, clobbers } => {
                assert!(code.contains("and eax, 0xFF"));
                assert!(code.contains("or eax, 0x100"));
                assert!(code.contains("xor eax, 0x200"));
                assert!(code.contains("not eax"));
                assert_eq!(inputs.len(), 1);
                assert_eq!(outputs.len(), 1);
                assert_eq!(clobbers.len(), 1);
            },
            _ => panic!("Expected InlineAssembly expression"),
        }
    }

    #[test]
    fn test_assembly_with_loops() {
        let loop_asm = Expr::InlineAssembly {
            code: "mov ecx, [count]\nmov eax, 0\nloop_start:\nadd eax, [value]\nloop loop_start\nmov [result], eax".to_string(),
            inputs: vec![
                ("count".to_string(), Expr::Literal(Literal::Integer(10))),
                ("value".to_string(), Expr::Literal(Literal::Integer(5))),
            ],
            outputs: vec![
                ("result".to_string(), "sum".to_string()),
            ],
            clobbers: vec!["eax".to_string(), "ecx".to_string()],
        };
        
        match loop_asm {
            Expr::InlineAssembly { code, inputs, outputs, clobbers } => {
                assert!(code.contains("mov ecx, [count]"));
                assert!(code.contains("loop_start:"));
                assert!(code.contains("loop loop_start"));
                assert_eq!(inputs.len(), 2);
                assert_eq!(outputs.len(), 1);
                assert_eq!(clobbers.len(), 2);
            },
            _ => panic!("Expected InlineAssembly expression"),
        }
    }

    #[test]
    fn test_assembly_with_different_calling_conventions() {
        let cdecl_asm = ProcedureDecl {
            name: "CDeclFunction".to_string(),
            params: vec![],
            block: Block {
                consts: Vec::new(),
                types: Vec::new(),
                vars: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                statements: vec![],
            },
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: true,
            calling_convention: Some(CallingConvention::CDecl),
        };
        
        let stdcall_asm = ProcedureDecl {
            name: "StdCallFunction".to_string(),
            params: vec![],
            block: Block {
                consts: Vec::new(),
                types: Vec::new(),
                vars: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                statements: vec![],
            },
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: true,
            calling_convention: Some(CallingConvention::StdCall),
        };
        
        assert_eq!(cdecl_asm.calling_convention, Some(CallingConvention::CDecl));
        assert_eq!(stdcall_asm.calling_convention, Some(CallingConvention::StdCall));
    }
}
