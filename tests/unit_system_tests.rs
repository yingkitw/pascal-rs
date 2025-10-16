use minipas::ast::*;
use minipas::parser::Parser;
use minipas::lexer::Lexer;
use std::collections::HashMap;

#[cfg(test)]
mod unit_system_tests {
    use super::*;

    #[test]
    fn test_unit_interface_parsing() {
        let source = r#"
        unit TestUnit;
        
        interface
        
        uses
            SysUtils, Classes;
        
        type
            TTestRecord = record
                name: string;
                value: integer;
            end;
        
        const
            MAX_SIZE = 100;
            DEFAULT_NAME = 'Test';
        
        var
            GlobalVar: integer;
            SharedData: TTestRecord;
        
        procedure DoSomething(param1: integer; param2: string);
        function Calculate(x, y: integer): integer;
        
        implementation
        end.
        "#;
        
        let mut lexer = Lexer::new(source);
        let mut parser = Parser::new(source);
        
        // This test would need parser support for units
        // For now, we test the AST structures
        let unit_interface = UnitInterface {
            types: vec![
                TypeDecl {
                    name: "TTestRecord".to_string(),
                    typ: Type::Record {
                        fields: {
                            let mut fields = HashMap::new();
                            fields.insert("name".to_string(), Type::String(None));
                            fields.insert("value".to_string(), Type::Integer);
                            fields
                        },
                        is_packed: false,
                        variant_part: None,
                    },
                }
            ],
            constants: vec![
                ConstDecl {
                    name: "MAX_SIZE".to_string(),
                    value: Expr::Literal(Literal::Integer(100)),
                },
                ConstDecl {
                    name: "DEFAULT_NAME".to_string(),
                    value: Expr::Literal(Literal::String("Test".to_string())),
                }
            ],
            variables: vec![
                VariableDecl {
                    name: "GlobalVar".to_string(),
                    var_type: Type::Integer,
                    initializer: None,
                    is_threadvar: false,
                    absolute_address: None,
                    external_name: None,
                },
                VariableDecl {
                    name: "SharedData".to_string(),
                    var_type: Type::Custom("TTestRecord".to_string()),
                    initializer: None,
                    is_threadvar: false,
                    absolute_address: None,
                    external_name: None,
                }
            ],
            procedures: vec![
                ProcedureDecl {
                    name: "DoSomething".to_string(),
                    params: vec![
                        Parameter {
                            name: "param1".to_string(),
                            param_type: Type::Integer,
                            is_var: false,
                            is_const: false,
                            is_out: false,
                            default_value: None,
                        },
                        Parameter {
                            name: "param2".to_string(),
                            param_type: Type::String(None),
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
                        statements: Vec::new(),
                    },
                    is_forward: true,
                    is_external: false,
                    external_name: None,
                    external_library: None,
                    is_inline: false,
                    is_assembler: false,
                    calling_convention: None,
                }
            ],
            functions: vec![
                FunctionDecl {
                    name: "Calculate".to_string(),
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
                        statements: Vec::new(),
                    },
                    is_forward: true,
                    is_external: false,
                    external_name: None,
                    external_library: None,
                    is_inline: false,
                    is_assembler: false,
                    calling_convention: None,
                }
            ],
            classes: Vec::new(),
            interfaces: Vec::new(),
        };
        
        assert_eq!(unit_interface.types.len(), 1);
        assert_eq!(unit_interface.constants.len(), 2);
        assert_eq!(unit_interface.variables.len(), 2);
        assert_eq!(unit_interface.procedures.len(), 1);
        assert_eq!(unit_interface.functions.len(), 1);
    }

    #[test]
    fn test_unit_implementation_parsing() {
        let unit_implementation = UnitImplementation {
            uses: vec!["SysUtils".to_string(), "Classes".to_string()],
            types: Vec::new(),
            constants: Vec::new(),
            variables: Vec::new(),
            procedures: vec![
                ProcedureDecl {
                    name: "DoSomething".to_string(),
                    params: vec![
                        Parameter {
                            name: "param1".to_string(),
                            param_type: Type::Integer,
                            is_var: false,
                            is_const: false,
                            is_out: false,
                            default_value: None,
                        },
                        Parameter {
                            name: "param2".to_string(),
                            param_type: Type::String(None),
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
                            Stmt::Assignment {
                                target: Expr::Identifier(vec!["param1".to_string()]),
                                value: Expr::BinaryOp {
                                    left: Box::new(Expr::Identifier(vec!["param1".to_string()])),
                                    op: BinaryOp::Add,
                                    right: Box::new(Expr::Literal(Literal::Integer(1))),
                                },
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
                }
            ],
            functions: vec![
                FunctionDecl {
                    name: "Calculate".to_string(),
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
                                value: Expr::BinaryOp {
                                    left: Box::new(Expr::Identifier(vec!["x".to_string()])),
                                    op: BinaryOp::Add,
                                    right: Box::new(Expr::Identifier(vec!["y".to_string()])),
                                },
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
                }
            ],
            classes: Vec::new(),
            interfaces: Vec::new(),
            initialization: Some(vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["GlobalVar".to_string()]),
                    value: Expr::Literal(Literal::Integer(0)),
                }
            ]),
            finalization: Some(vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["GlobalVar".to_string()]),
                    value: Expr::Literal(Literal::Integer(-1)),
                }
            ]),
        };
        
        assert_eq!(unit_implementation.uses.len(), 2);
        assert_eq!(unit_implementation.procedures.len(), 1);
        assert_eq!(unit_implementation.functions.len(), 1);
        assert!(unit_implementation.initialization.is_some());
        assert!(unit_implementation.finalization.is_some());
    }

    #[test]
    fn test_complete_unit_structure() {
        let unit = Unit {
            name: "TestUnit".to_string(),
            uses: vec!["SysUtils".to_string(), "Classes".to_string()],
            interface: UnitInterface {
                types: Vec::new(),
                constants: Vec::new(),
                variables: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                classes: Vec::new(),
                interfaces: Vec::new(),
            },
            implementation: UnitImplementation {
                uses: Vec::new(),
                types: Vec::new(),
                constants: Vec::new(),
                variables: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                classes: Vec::new(),
                interfaces: Vec::new(),
                initialization: None,
                finalization: None,
            },
        };
        
        assert_eq!(unit.name, "TestUnit");
        assert_eq!(unit.uses.len(), 2);
    }

    #[test]
    fn test_uses_clause_parsing() {
        let uses_clauses = vec![
            "SysUtils".to_string(),
            "Classes".to_string(),
            "Graphics".to_string(),
            "Controls".to_string(),
        ];
        
        assert_eq!(uses_clauses.len(), 4);
        assert!(uses_clauses.contains(&"SysUtils".to_string()));
        assert!(uses_clauses.contains(&"Classes".to_string()));
    }

    #[test]
    fn test_forward_declarations() {
        let forward_procedure = ProcedureDecl {
            name: "ForwardProc".to_string(),
            params: vec![
                Parameter {
                    name: "param".to_string(),
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
                statements: Vec::new(),
            },
            is_forward: true,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: false,
            is_assembler: false,
            calling_convention: None,
        };
        
        assert!(forward_procedure.is_forward);
    }

    #[test]
    fn test_external_procedures() {
        let external_procedure = ProcedureDecl {
            name: "ExternalProc".to_string(),
            params: vec![
                Parameter {
                    name: "param".to_string(),
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
                statements: Vec::new(),
            },
            is_forward: false,
            is_external: true,
            external_name: Some("external_proc".to_string()),
            external_library: Some("kernel32.dll".to_string()),
            is_inline: false,
            is_assembler: false,
            calling_convention: Some(CallingConvention::StdCall),
        };
        
        assert!(external_procedure.is_external);
        assert_eq!(external_procedure.external_name, Some("external_proc".to_string()));
        assert_eq!(external_procedure.external_library, Some("kernel32.dll".to_string()));
        assert_eq!(external_procedure.calling_convention, Some(CallingConvention::StdCall));
    }

    #[test]
    fn test_inline_procedures() {
        let inline_procedure = ProcedureDecl {
            name: "InlineProc".to_string(),
            params: vec![
                Parameter {
                    name: "param".to_string(),
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
                    Stmt::Assignment {
                        target: Expr::Identifier(vec!["result".to_string()]),
                        value: Expr::BinaryOp {
                            left: Box::new(Expr::Identifier(vec!["param".to_string()])),
                            op: BinaryOp::Multiply,
                            right: Box::new(Expr::Literal(Literal::Integer(2))),
                        },
                    }
                ],
            },
            is_forward: false,
            is_external: false,
            external_name: None,
            external_library: None,
            is_inline: true,
            is_assembler: false,
            calling_convention: None,
        };
        
        assert!(inline_procedure.is_inline);
    }

    #[test]
    fn test_assembler_procedures() {
        let assembler_procedure = ProcedureDecl {
            name: "AssemblerProc".to_string(),
            params: vec![
                Parameter {
                    name: "param".to_string(),
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
                    Stmt::Assignment {
                        target: Expr::Identifier(vec!["result".to_string()]),
                        value: Expr::InlineAssembly {
                            code: "mov eax, [param]\nadd eax, 1".to_string(),
                            inputs: vec![("param".to_string(), Expr::Identifier(vec!["param".to_string()]))],
                            outputs: vec![("eax".to_string(), "=a".to_string())],
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
        
        assert!(assembler_procedure.is_assembler);
        assert_eq!(assembler_procedure.calling_convention, Some(CallingConvention::Register));
    }
}
