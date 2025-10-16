use minipas::ast::*;
use std::collections::HashMap;

#[cfg(test)]
mod generic_tests {
    use super::*;

    #[test]
    fn test_generic_type_declaration() {
        let generic_type = GenericType {
            name: "TGenericList".to_string(),
            type_parameters: vec![
                TypeParameter {
                    name: "T".to_string(),
                    constraints: vec![
                        TypeConstraint::Constructor,
                        TypeConstraint::Class("TObject".to_string()),
                    ],
                }
            ],
            constraints: vec![
                TypeConstraint::Constructor,
            ],
            body: Type::Class {
                name: "TGenericList".to_string(),
                parent: Some("TObject".to_string()),
                fields: HashMap::new(),
                methods: HashMap::new(),
            },
        };
        
        assert_eq!(generic_type.name, "TGenericList");
        assert_eq!(generic_type.type_parameters.len(), 1);
        assert_eq!(generic_type.type_parameters[0].name, "T");
        assert_eq!(generic_type.type_parameters[0].constraints.len(), 2);
    }

    #[test]
    fn test_type_parameters() {
        let type_param = TypeParameter {
            name: "TKey".to_string(),
            constraints: vec![
                TypeConstraint::Class("IComparable".to_string()),
                TypeConstraint::Constructor,
            ],
        };
        
        assert_eq!(type_param.name, "TKey");
        assert_eq!(type_param.constraints.len(), 2);
        assert!(type_param.constraints.contains(&TypeConstraint::Class("IComparable".to_string())));
        assert!(type_param.constraints.contains(&TypeConstraint::Constructor));
    }

    #[test]
    fn test_type_constraints() {
        let constraints = vec![
            TypeConstraint::Class("TObject".to_string()),
            TypeConstraint::Constructor,
            TypeConstraint::Record,
            TypeConstraint::Interface("IComparable".to_string()),
        ];
        
        assert_eq!(constraints.len(), 4);
        assert!(constraints.contains(&TypeConstraint::Class("TObject".to_string())));
        assert!(constraints.contains(&TypeConstraint::Constructor));
        assert!(constraints.contains(&TypeConstraint::Record));
        assert!(constraints.contains(&TypeConstraint::Interface("IComparable".to_string())));
    }

    #[test]
    fn test_specialized_type() {
        let specialized_type = SpecializedType {
            generic_type: "TGenericList".to_string(),
            type_arguments: vec![
                Type::Integer,
                Type::String(None),
                Type::Boolean,
            ],
        };
        
        assert_eq!(specialized_type.generic_type, "TGenericList");
        assert_eq!(specialized_type.type_arguments.len(), 3);
        assert!(specialized_type.type_arguments.contains(&Type::Integer));
        assert!(specialized_type.type_arguments.contains(&Type::String(None)));
        assert!(specialized_type.type_arguments.contains(&Type::Boolean));
    }

    #[test]
    fn test_generic_class_with_multiple_parameters() {
        let generic_class = GenericType {
            name: "TGenericDictionary".to_string(),
            type_parameters: vec![
                TypeParameter {
                    name: "TKey".to_string(),
                    constraints: vec![
                        TypeConstraint::Class("IComparable".to_string()),
                        TypeConstraint::Constructor,
                    ],
                },
                TypeParameter {
                    name: "TValue".to_string(),
                    constraints: vec![
                        TypeConstraint::Constructor,
                    ],
                }
            ],
            constraints: vec![],
            body: Type::Class {
                name: "TGenericDictionary".to_string(),
                parent: Some("TObject".to_string()),
                fields: HashMap::new(),
                methods: HashMap::new(),
            },
        };
        
        assert_eq!(generic_class.name, "TGenericDictionary");
        assert_eq!(generic_class.type_parameters.len(), 2);
        assert_eq!(generic_class.type_parameters[0].name, "TKey");
        assert_eq!(generic_class.type_parameters[1].name, "TValue");
    }

    #[test]
    fn test_generic_procedure() {
        let generic_procedure = ProcedureDecl {
            name: "GenericSwap".to_string(),
            params: vec![
                Parameter {
                    name: "a".to_string(),
                    param_type: Type::Custom("T".to_string()),
                    is_var: true,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                },
                Parameter {
                    name: "b".to_string(),
                    param_type: Type::Custom("T".to_string()),
                    is_var: true,
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
                        name: "temp".to_string(),
                        var_type: Type::Custom("T".to_string()),
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
                        target: Expr::Identifier(vec!["temp".to_string()]),
                        value: Expr::Identifier(vec!["a".to_string()]),
                    },
                    Stmt::Assignment {
                        target: Expr::Identifier(vec!["a".to_string()]),
                        value: Expr::Identifier(vec!["b".to_string()]),
                    },
                    Stmt::Assignment {
                        target: Expr::Identifier(vec!["b".to_string()]),
                        value: Expr::Identifier(vec!["temp".to_string()]),
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
        
        assert_eq!(generic_procedure.name, "GenericSwap");
        assert_eq!(generic_procedure.params.len(), 2);
        assert!(generic_procedure.params[0].is_var);
        assert!(generic_procedure.params[1].is_var);
    }

    #[test]
    fn test_generic_function() {
        let generic_function = FunctionDecl {
            name: "GenericMax".to_string(),
            params: vec![
                Parameter {
                    name: "a".to_string(),
                    param_type: Type::Custom("T".to_string()),
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                },
                Parameter {
                    name: "b".to_string(),
                    param_type: Type::Custom("T".to_string()),
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                }
            ],
            return_type: Type::Custom("T".to_string()),
            block: Block {
                consts: Vec::new(),
                types: Vec::new(),
                vars: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                statements: vec![
                    Stmt::If {
                        condition: Expr::BinaryOp {
                            left: Box::new(Expr::Identifier(vec!["a".to_string()])),
                            op: BinaryOp::Greater,
                            right: Box::new(Expr::Identifier(vec!["b".to_string()])),
                        },
                        then_branch: vec![
                            Stmt::Assignment {
                                target: Expr::Identifier(vec!["result".to_string()]),
                                value: Expr::Identifier(vec!["a".to_string()]),
                            }
                        ],
                        else_branch: Some(vec![
                            Stmt::Assignment {
                                target: Expr::Identifier(vec!["result".to_string()]),
                                value: Expr::Identifier(vec!["b".to_string()]),
                            }
                        ]),
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
        
        assert_eq!(generic_function.name, "GenericMax");
        assert_eq!(generic_function.params.len(), 2);
        assert_eq!(generic_function.return_type, Type::Custom("T".to_string()));
    }

    #[test]
    fn test_generic_specialization_expression() {
        let specialization_expr = Expr::GenericSpecialization {
            generic_name: "TGenericList".to_string(),
            type_arguments: vec![
                Type::Integer,
                Type::String(None),
            ],
        };
        
        match specialization_expr {
            Expr::GenericSpecialization { generic_name, type_arguments } => {
                assert_eq!(generic_name, "TGenericList");
                assert_eq!(type_arguments.len(), 2);
                assert!(type_arguments.contains(&Type::Integer));
                assert!(type_arguments.contains(&Type::String(None)));
            },
            _ => panic!("Expected generic specialization expression"),
        }
    }

    #[test]
    fn test_generic_array_type() {
        let generic_array_type = Type::Array {
            index_type: Box::new(Type::Custom("TIndex".to_string())),
            element_type: Box::new(Type::Custom("TElement".to_string())),
            range: None,
        };
        
        match generic_array_type {
            Type::Array { index_type, element_type, range } => {
                assert!(matches!(*index_type, Type::Custom(_)));
                assert!(matches!(*element_type, Type::Custom(_)));
                assert!(range.is_none());
            },
            _ => panic!("Expected array type"),
        }
    }

    #[test]
    fn test_generic_record_type() {
        let generic_record_type = Type::Record {
            fields: {
                let mut fields = HashMap::new();
                fields.insert("key".to_string(), Type::Custom("TKey".to_string()));
                fields.insert("value".to_string(), Type::Custom("TValue".to_string()));
                fields
            },
            is_packed: false,
            variant_part: None,
        };
        
        match generic_record_type {
            Type::Record { fields, is_packed, variant_part } => {
                assert_eq!(fields.len(), 2);
                assert!(!is_packed);
                assert!(variant_part.is_none());
                assert!(fields.contains_key("key"));
                assert!(fields.contains_key("value"));
            },
            _ => panic!("Expected record type"),
        }
    }

    #[test]
    fn test_generic_constraint_combinations() {
        let complex_constraints = vec![
            TypeConstraint::Class("TObject".to_string()),
            TypeConstraint::Interface("IComparable".to_string()),
            TypeConstraint::Constructor,
        ];
        
        let type_param = TypeParameter {
            name: "TComplex".to_string(),
            constraints: complex_constraints.clone(),
        };
        
        assert_eq!(type_param.constraints.len(), 3);
        assert!(type_param.constraints.contains(&TypeConstraint::Class("TObject".to_string())));
        assert!(type_param.constraints.contains(&TypeConstraint::Interface("IComparable".to_string())));
        assert!(type_param.constraints.contains(&TypeConstraint::Constructor));
    }

    #[test]
    fn test_generic_method_in_class() {
        let generic_method = MethodDecl {
            name: "GenericMethod".to_string(),
            params: vec![
                Parameter {
                    name: "param".to_string(),
                    param_type: Type::Custom("T".to_string()),
                    is_var: false,
                    is_const: false,
                    is_out: false,
                    default_value: None,
                }
            ],
            return_type: Some(Type::Custom("T".to_string())),
            visibility: Visibility::Public,
            is_virtual: true,
            is_override: false,
            is_abstract: false,
            is_class: false,
            is_static: false,
            body: Some(vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["result".to_string()]),
                    value: Expr::Identifier(vec!["param".to_string()]),
                }
            ]),
        };
        
        assert_eq!(generic_method.name, "GenericMethod");
        assert!(generic_method.is_virtual);
        assert_eq!(generic_method.return_type, Some(Type::Custom("T".to_string())));
    }

    #[test]
    fn test_generic_type_instantiation() {
        let integer_list = SpecializedType {
            generic_type: "TGenericList".to_string(),
            type_arguments: vec![Type::Integer],
        };
        
        let string_list = SpecializedType {
            generic_type: "TGenericList".to_string(),
            type_arguments: vec![Type::String(None)],
        };
        
        let mixed_dictionary = SpecializedType {
            generic_type: "TGenericDictionary".to_string(),
            type_arguments: vec![Type::Integer, Type::String(None)],
        };
        
        assert_eq!(integer_list.type_arguments.len(), 1);
        assert_eq!(string_list.type_arguments.len(), 1);
        assert_eq!(mixed_dictionary.type_arguments.len(), 2);
        
        assert!(integer_list.type_arguments.contains(&Type::Integer));
        assert!(string_list.type_arguments.contains(&Type::String(None)));
        assert!(mixed_dictionary.type_arguments.contains(&Type::Integer));
        assert!(mixed_dictionary.type_arguments.contains(&Type::String(None)));
    }
}
