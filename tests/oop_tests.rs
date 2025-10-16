use minipas::ast::*;
use std::collections::HashMap;

#[cfg(test)]
mod oop_tests {
    use super::*;

    #[test]
    fn test_class_declaration() {
        let class_decl = ClassDecl {
            name: "TBaseClass".to_string(),
            parent: None,
            visibility: Visibility::Public,
            fields: vec![
                FieldDecl {
                    name: "name".to_string(),
                    field_type: Type::String(None),
                    visibility: Visibility::Private,
                    is_class: false,
                    initializer: Some(Expr::Literal(Literal::String("".to_string()))),
                },
                FieldDecl {
                    name: "value".to_string(),
                    field_type: Type::Integer,
                    visibility: Visibility::Protected,
                    is_class: false,
                    initializer: Some(Expr::Literal(Literal::Integer(0))),
                }
            ],
            methods: vec![
                MethodDecl {
                    name: "GetName".to_string(),
                    params: Vec::new(),
                    return_type: Some(Type::String(None)),
                    visibility: Visibility::Public,
                    is_virtual: false,
                    is_override: false,
                    is_abstract: false,
                    is_class: false,
                    is_static: false,
                    body: Some(vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["result".to_string()]),
                            value: Expr::Identifier(vec!["name".to_string()]),
                        }
                    ]),
                },
                MethodDecl {
                    name: "SetValue".to_string(),
                    params: vec![
                        Parameter {
                            name: "newValue".to_string(),
                            param_type: Type::Integer,
                            is_var: false,
                            is_const: false,
                            is_out: false,
                            default_value: None,
                        }
                    ],
                    return_type: None,
                    visibility: Visibility::Public,
                    is_virtual: false,
                    is_override: false,
                    is_abstract: false,
                    is_class: false,
                    is_static: false,
                    body: Some(vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["value".to_string()]),
                            value: Expr::Identifier(vec!["newValue".to_string()]),
                        }
                    ]),
                }
            ],
            properties: vec![
                PropertyDecl {
                    name: "Name".to_string(),
                    property_type: Type::String(None),
                    visibility: Visibility::Published,
                    read_accessor: PropertyAccessor::Method("GetName".to_string()),
                    write_accessor: Some(PropertyAccessor::Field("name".to_string())),
                    is_class: false,
                    is_virtual: false,
                    is_override: false,
                }
            ],
            constructors: vec![
                ConstructorDecl {
                    name: "Create".to_string(),
                    params: vec![
                        Parameter {
                            name: "aName".to_string(),
                            param_type: Type::String(None),
                            is_var: false,
                            is_const: false,
                            is_out: false,
                            default_value: None,
                        }
                    ],
                    visibility: Visibility::Public,
                    body: Some(vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["name".to_string()]),
                            value: Expr::Identifier(vec!["aName".to_string()]),
                        }
                    ]),
                }
            ],
            destructors: vec![
                DestructorDecl {
                    name: "Destroy".to_string(),
                    visibility: Visibility::Public,
                    body: Some(vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["name".to_string()]),
                            value: Expr::Literal(Literal::String("".to_string())),
                        }
                    ]),
                }
            ],
            is_abstract: false,
            is_sealed: false,
        };
        
        assert_eq!(class_decl.name, "TBaseClass");
        assert_eq!(class_decl.fields.len(), 2);
        assert_eq!(class_decl.methods.len(), 2);
        assert_eq!(class_decl.properties.len(), 1);
        assert_eq!(class_decl.constructors.len(), 1);
        assert_eq!(class_decl.destructors.len(), 1);
        assert!(!class_decl.is_abstract);
        assert!(!class_decl.is_sealed);
    }

    #[test]
    fn test_inheritance() {
        let derived_class = ClassDecl {
            name: "TDerivedClass".to_string(),
            parent: Some("TBaseClass".to_string()),
            visibility: Visibility::Public,
            fields: vec![
                FieldDecl {
                    name: "extraField".to_string(),
                    field_type: Type::Boolean,
                    visibility: Visibility::Private,
                    is_class: false,
                    initializer: Some(Expr::Literal(Literal::Boolean(false))),
                }
            ],
            methods: vec![
                MethodDecl {
                    name: "GetName".to_string(),
                    params: Vec::new(),
                    return_type: Some(Type::String(None)),
                    visibility: Visibility::Public,
                    is_virtual: false,
                    is_override: true,
                    is_abstract: false,
                    is_class: false,
                    is_static: false,
                    body: Some(vec![
                        Stmt::Assignment {
                            target: Expr::Identifier(vec!["result".to_string()]),
                            value: Expr::BinaryOp {
                                left: Box::new(Expr::Identifier(vec!["inherited".to_string()])),
                                op: BinaryOp::Concat,
                                right: Box::new(Expr::Literal(Literal::String(" (derived)".to_string()))),
                            },
                        }
                    ]),
                }
            ],
            properties: Vec::new(),
            constructors: Vec::new(),
            destructors: Vec::new(),
            is_abstract: false,
            is_sealed: false,
        };
        
        assert_eq!(derived_class.parent, Some("TBaseClass".to_string()));
        assert!(derived_class.methods[0].is_override);
    }

    #[test]
    fn test_abstract_class() {
        let abstract_class = ClassDecl {
            name: "TAbstractClass".to_string(),
            parent: None,
            visibility: Visibility::Public,
            fields: Vec::new(),
            methods: vec![
                MethodDecl {
                    name: "AbstractMethod".to_string(),
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
                    return_type: Some(Type::Integer),
                    visibility: Visibility::Public,
                    is_virtual: true,
                    is_override: false,
                    is_abstract: true,
                    is_class: false,
                    is_static: false,
                    body: None,
                }
            ],
            properties: Vec::new(),
            constructors: Vec::new(),
            destructors: Vec::new(),
            is_abstract: true,
            is_sealed: false,
        };
        
        assert!(abstract_class.is_abstract);
        assert!(abstract_class.methods[0].is_abstract);
        assert!(abstract_class.methods[0].is_virtual);
        assert!(abstract_class.methods[0].body.is_none());
    }

    #[test]
    fn test_sealed_class() {
        let sealed_class = ClassDecl {
            name: "TSealedClass".to_string(),
            parent: Some("TBaseClass".to_string()),
            visibility: Visibility::Public,
            fields: Vec::new(),
            methods: Vec::new(),
            properties: Vec::new(),
            constructors: Vec::new(),
            destructors: Vec::new(),
            is_abstract: false,
            is_sealed: true,
        };
        
        assert!(sealed_class.is_sealed);
        assert!(!sealed_class.is_abstract);
    }

    #[test]
    fn test_interface_declaration() {
        let interface_decl = InterfaceDecl {
            name: "ITestInterface".to_string(),
            parent_interfaces: vec!["IUnknown".to_string()],
            methods: vec![
                MethodDecl {
                    name: "DoSomething".to_string(),
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
                    return_type: Some(Type::Integer),
                    visibility: Visibility::Public,
                    is_virtual: true,
                    is_override: false,
                    is_abstract: true,
                    is_class: false,
                    is_static: false,
                    body: None,
                }
            ],
            properties: vec![
                PropertyDecl {
                    name: "Value".to_string(),
                    property_type: Type::Integer,
                    visibility: Visibility::Public,
                    read_accessor: PropertyAccessor::Method("GetValue".to_string()),
                    write_accessor: Some(PropertyAccessor::Method("SetValue".to_string())),
                    is_class: false,
                    is_virtual: true,
                    is_override: false,
                }
            ],
        };
        
        assert_eq!(interface_decl.name, "ITestInterface");
        assert_eq!(interface_decl.parent_interfaces.len(), 1);
        assert_eq!(interface_decl.methods.len(), 1);
        assert_eq!(interface_decl.properties.len(), 1);
        assert!(interface_decl.methods[0].is_abstract);
        assert!(interface_decl.properties[0].is_virtual);
    }

    #[test]
    fn test_class_fields() {
        let class_field = FieldDecl {
            name: "classField".to_string(),
            field_type: Type::Integer,
            visibility: Visibility::Public,
            is_class: true,
            initializer: Some(Expr::Literal(Literal::Integer(42))),
        };
        
        assert!(class_field.is_class);
        assert_eq!(class_field.visibility, Visibility::Public);
        assert!(class_field.initializer.is_some());
    }

    #[test]
    fn test_static_methods() {
        let static_method = MethodDecl {
            name: "StaticMethod".to_string(),
            params: Vec::new(),
            return_type: Some(Type::Integer),
            visibility: Visibility::Public,
            is_virtual: false,
            is_override: false,
            is_abstract: false,
            is_class: true,
            is_static: true,
            body: Some(vec![
                Stmt::Assignment {
                    target: Expr::Identifier(vec!["result".to_string()]),
                    value: Expr::Literal(Literal::Integer(100)),
                }
            ]),
        };
        
        assert!(static_method.is_class);
        assert!(static_method.is_static);
    }

    #[test]
    fn test_property_accessors() {
        let field_accessor = PropertyAccessor::Field("name".to_string());
        let method_accessor = PropertyAccessor::Method("GetName".to_string());
        let indexed_accessor = PropertyAccessor::Indexed(
            Box::new(PropertyAccessor::Field("items".to_string())),
            vec![Expr::Literal(Literal::Integer(0))]
        );
        
        match field_accessor {
            PropertyAccessor::Field(name) => assert_eq!(name, "name"),
            _ => panic!("Expected field accessor"),
        }
        
        match method_accessor {
            PropertyAccessor::Method(name) => assert_eq!(name, "GetName"),
            _ => panic!("Expected method accessor"),
        }
        
        match indexed_accessor {
            PropertyAccessor::Indexed(base, indices) => {
                assert_eq!(indices.len(), 1);
                assert!(matches!(indices[0], Expr::Literal(Literal::Integer(0))));
            },
            _ => panic!("Expected indexed accessor"),
        }
    }

    #[test]
    fn test_visibility_levels() {
        let visibilities = vec![
            Visibility::Private,
            Visibility::Protected,
            Visibility::Public,
            Visibility::Published,
        ];
        
        assert_eq!(visibilities.len(), 4);
        assert!(visibilities.contains(&Visibility::Private));
        assert!(visibilities.contains(&Visibility::Protected));
        assert!(visibilities.contains(&Visibility::Public));
        assert!(visibilities.contains(&Visibility::Published));
    }

    #[test]
    fn test_method_calls() {
        let method_call = Expr::MethodCall {
            object: Box::new(Expr::Identifier(vec!["obj".to_string()])),
            method: "DoSomething".to_string(),
            args: vec![
                Expr::Literal(Literal::Integer(42)),
                Expr::Literal(Literal::String("test".to_string())),
            ],
        };
        
        match method_call {
            Expr::MethodCall { object, method, args } => {
                assert_eq!(method, "DoSomething");
                assert_eq!(args.len(), 2);
            },
            _ => panic!("Expected method call"),
        }
    }

    #[test]
    fn test_property_access() {
        let property_access = Expr::PropertyAccess {
            object: Box::new(Expr::Identifier(vec!["obj".to_string()])),
            property: "Name".to_string(),
        };
        
        match property_access {
            Expr::PropertyAccess { object, property } => {
                assert_eq!(property, "Name");
            },
            _ => panic!("Expected property access"),
        }
    }

    #[test]
    fn test_property_assignment() {
        let property_assignment = Expr::PropertyAssignment {
            object: Box::new(Expr::Identifier(vec!["obj".to_string()])),
            property: "Value".to_string(),
            value: Box::new(Expr::Literal(Literal::Integer(100))),
        };
        
        match property_assignment {
            Expr::PropertyAssignment { object, property, value } => {
                assert_eq!(property, "Value");
                assert!(matches!(*value, Expr::Literal(Literal::Integer(100))));
            },
            _ => panic!("Expected property assignment"),
        }
    }

    #[test]
    fn test_object_creation() {
        let new_expr = Expr::New {
            type_name: "TMyClass".to_string(),
            args: vec![
                Expr::Literal(Literal::String("test".to_string())),
                Expr::Literal(Literal::Integer(42)),
            ],
        };
        
        match new_expr {
            Expr::New { type_name, args } => {
                assert_eq!(type_name, "TMyClass");
                assert_eq!(args.len(), 2);
            },
            _ => panic!("Expected new expression"),
        }
    }

    #[test]
    fn test_object_disposal() {
        let dispose_expr = Expr::Dispose {
            expr: Box::new(Expr::Identifier(vec!["obj".to_string()])),
        };
        
        match dispose_expr {
            Expr::Dispose { expr } => {
                assert!(matches!(*expr, Expr::Identifier(_)));
            },
            _ => panic!("Expected dispose expression"),
        }
    }

    #[test]
    fn test_inherited_keyword() {
        let inherited_call = Expr::MethodCall {
            object: Box::new(Expr::Identifier(vec!["inherited".to_string()])),
            method: "DoSomething".to_string(),
            args: vec![Expr::Literal(Literal::Integer(1))],
        };
        
        match inherited_call {
            Expr::MethodCall { object, method, args } => {
                assert_eq!(method, "DoSomething");
                assert_eq!(args.len(), 1);
            },
            _ => panic!("Expected inherited method call"),
        }
    }
}
