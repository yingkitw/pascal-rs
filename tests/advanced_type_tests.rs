use minipas::ast::*;
use std::collections::HashMap;

#[cfg(test)]
mod advanced_type_tests {
    use super::*;

    #[test]
    fn test_variant_record() {
        let variant_record = Type::VariantRecord {
            common_fields: {
                let mut fields = HashMap::new();
                fields.insert("id".to_string(), Type::Integer);
                fields.insert("name".to_string(), Type::String(None));
                fields
            },
            variant_part: Box::new(VariantPart {
                discriminant: "kind".to_string(),
                discriminant_type: Box::new(Type::Integer),
                variants: vec![
                    VariantCase {
                        case_values: vec![Literal::Integer(1)],
                        fields: {
                            let mut fields = HashMap::new();
                            fields.insert("width".to_string(), Type::Integer);
                            fields.insert("height".to_string(), Type::Integer);
                            fields
                        },
                    },
                    VariantCase {
                        case_values: vec![Literal::Integer(2)],
                        fields: {
                            let mut fields = HashMap::new();
                            fields.insert("radius".to_string(), Type::Integer);
                            fields
                        },
                    }
                ],
            }),
            is_packed: false,
        };
        
        match variant_record {
            Type::VariantRecord { common_fields, variant_part, is_packed } => {
                assert_eq!(common_fields.len(), 2);
                assert_eq!(variant_part.discriminant, "kind");
                assert_eq!(variant_part.variants.len(), 2);
                assert!(!is_packed);
                
                // Check first variant case
                assert_eq!(variant_part.variants[0].case_values.len(), 1);
                assert_eq!(variant_part.variants[0].fields.len(), 2);
                assert!(variant_part.variants[0].fields.contains_key("width"));
                assert!(variant_part.variants[0].fields.contains_key("height"));
                
                // Check second variant case
                assert_eq!(variant_part.variants[1].case_values.len(), 1);
                assert_eq!(variant_part.variants[1].fields.len(), 1);
                assert!(variant_part.variants[1].fields.contains_key("radius"));
            },
            _ => panic!("Expected VariantRecord type"),
        }
    }

    #[test]
    fn test_packed_record() {
        let packed_record = Type::Record {
            fields: {
                let mut fields = HashMap::new();
                fields.insert("flag1".to_string(), Type::Boolean);
                fields.insert("flag2".to_string(), Type::Boolean);
                fields.insert("flag3".to_string(), Type::Boolean);
                fields.insert("flag4".to_string(), Type::Boolean);
                fields
            },
            is_packed: true,
            variant_part: None,
        };
        
        match packed_record {
            Type::Record { fields, is_packed, variant_part } => {
                assert_eq!(fields.len(), 4);
                assert!(is_packed);
                assert!(variant_part.is_none());
            },
            _ => panic!("Expected Record type"),
        }
    }

    #[test]
    fn test_dynamic_array() {
        let dynamic_array = Type::DynamicArray {
            element_type: Box::new(Type::Integer),
        };
        
        match dynamic_array {
            Type::DynamicArray { element_type } => {
                assert!(matches!(*element_type, Type::Integer));
            },
            _ => panic!("Expected DynamicArray type"),
        }
    }

    #[test]
    fn test_open_array() {
        let open_array = Type::OpenArray {
            element_type: Box::new(Type::String(None)),
        };
        
        match open_array {
            Type::OpenArray { element_type } => {
                assert!(matches!(*element_type, Type::String(None)));
            },
            _ => panic!("Expected OpenArray type"),
        }
    }

    #[test]
    fn test_file_type() {
        let text_file = Type::File {
            element_type: None,
        };
        
        let typed_file = Type::File {
            element_type: Some(Box::new(Type::Integer)),
        };
        
        match text_file {
            Type::File { element_type } => {
                assert!(element_type.is_none());
            },
            _ => panic!("Expected File type"),
        }
        
        match typed_file {
            Type::File { element_type } => {
                assert!(element_type.is_some());
                assert!(matches!(*element_type.unwrap(), Type::Integer));
            },
            _ => panic!("Expected File type"),
        }
    }

    #[test]
    fn test_enum_type() {
        let enum_type = Type::Enum {
            values: vec![
                "Red".to_string(),
                "Green".to_string(),
                "Blue".to_string(),
            ],
            custom_values: None,
        };
        
        let enum_with_values = Type::Enum {
            values: vec![
                "Small".to_string(),
                "Medium".to_string(),
                "Large".to_string(),
            ],
            custom_values: Some({
                let mut values = HashMap::new();
                values.insert("Small".to_string(), 1);
                values.insert("Medium".to_string(), 2);
                values.insert("Large".to_string(), 3);
                values
            }),
        };
        
        match enum_type {
            Type::Enum { values, custom_values } => {
                assert_eq!(values.len(), 3);
                assert!(custom_values.is_none());
                assert!(values.contains(&"Red".to_string()));
                assert!(values.contains(&"Green".to_string()));
                assert!(values.contains(&"Blue".to_string()));
            },
            _ => panic!("Expected Enum type"),
        }
        
        match enum_with_values {
            Type::Enum { values, custom_values } => {
                assert_eq!(values.len(), 3);
                assert!(custom_values.is_some());
                let custom_vals = custom_values.unwrap();
                assert_eq!(custom_vals.get("Small"), Some(&1));
                assert_eq!(custom_vals.get("Medium"), Some(&2));
                assert_eq!(custom_vals.get("Large"), Some(&3));
            },
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
                    Type::Enum { values, custom_values } => {
                        assert_eq!(values.len(), 3);
                        assert!(custom_values.is_none());
                    },
                    _ => panic!("Expected Enum type as base"),
                }
            },
            _ => panic!("Expected Set type"),
        }
    }

    #[test]
    fn test_range_type() {
        let range_type = Type::Range {
            base_type: Box::new(Type::Integer),
            min: 1,
            max: 100,
        };
        
        match range_type {
            Type::Range { base_type, min, max } => {
                assert!(matches!(*base_type, Type::Integer));
                assert_eq!(min, 1);
                assert_eq!(max, 100);
            },
            _ => panic!("Expected Range type"),
        }
    }

    #[test]
    fn test_variant_type() {
        let variant_type = Type::Variant;
        
        match variant_type {
            Type::Variant => {
                // Variant type is a special case for dynamic typing
            },
            _ => panic!("Expected Variant type"),
        }
    }

    #[test]
    fn test_threadvar_declaration() {
        let threadvar_decl = VariableDecl {
            name: "threadLocalVar".to_string(),
            var_type: Type::Integer,
            initializer: Some(Expr::Literal(Literal::Integer(0))),
            is_threadvar: true,
            absolute_address: None,
            external_name: None,
        };
        
        assert!(threadvar_decl.is_threadvar);
        assert_eq!(threadvar_decl.name, "threadLocalVar");
        assert!(threadvar_decl.initializer.is_some());
    }

    #[test]
    fn test_absolute_variable() {
        let absolute_var = VariableDecl {
            name: "absoluteVar".to_string(),
            var_type: Type::Integer,
            initializer: None,
            is_threadvar: false,
            absolute_address: Some(Expr::Literal(Literal::Integer(0x1000))),
            external_name: None,
        };
        
        assert!(absolute_var.absolute_address.is_some());
        match absolute_var.absolute_address.unwrap() {
            Expr::Literal(Literal::Integer(addr)) => assert_eq!(addr, 0x1000),
            _ => panic!("Expected integer literal for absolute address"),
        }
    }

    #[test]
    fn test_external_variable() {
        let external_var = VariableDecl {
            name: "externalVar".to_string(),
            var_type: Type::Integer,
            initializer: None,
            is_threadvar: false,
            absolute_address: None,
            external_name: Some("external_variable".to_string()),
        };
        
        assert_eq!(external_var.external_name, Some("external_variable".to_string()));
    }

    #[test]
    fn test_set_literals() {
        let empty_set = Literal::Set(vec![]);
        let single_element_set = Literal::Set(vec![
            Expr::Literal(Literal::Enum("Red".to_string(), 0))
        ]);
        let multi_element_set = Literal::Set(vec![
            Expr::Literal(Literal::Enum("Red".to_string(), 0)),
            Expr::Literal(Literal::Enum("Green".to_string(), 1)),
            Expr::Literal(Literal::Enum("Blue".to_string(), 2)),
        ]);
        
        match empty_set {
            Literal::Set(elements) => assert_eq!(elements.len(), 0),
            _ => panic!("Expected Set literal"),
        }
        
        match single_element_set {
            Literal::Set(elements) => {
                assert_eq!(elements.len(), 1);
                assert!(matches!(elements[0], Expr::Literal(Literal::Enum(_, _))));
            },
            _ => panic!("Expected Set literal"),
        }
        
        match multi_element_set {
            Literal::Set(elements) => {
                assert_eq!(elements.len(), 3);
                for element in elements {
                    assert!(matches!(element, Expr::Literal(Literal::Enum(_, _))));
                }
            },
            _ => panic!("Expected Set literal"),
        }
    }

    #[test]
    fn test_enum_literals() {
        let enum_literal = Literal::Enum("Red".to_string(), 0);
        let enum_literal_with_value = Literal::Enum("Medium".to_string(), 2);
        
        match enum_literal {
            Literal::Enum(name, value) => {
                assert_eq!(name, "Red");
                assert_eq!(value, 0);
            },
            _ => panic!("Expected Enum literal"),
        }
        
        match enum_literal_with_value {
            Literal::Enum(name, value) => {
                assert_eq!(name, "Medium");
                assert_eq!(value, 2);
            },
            _ => panic!("Expected Enum literal"),
        }
    }

    #[test]
    fn test_range_literals() {
        let range_literal = Literal::Range(1, 100);
        
        match range_literal {
            Literal::Range(min, max) => {
                assert_eq!(min, 1);
                assert_eq!(max, 100);
            },
            _ => panic!("Expected Range literal"),
        }
    }

    #[test]
    fn test_array_literals() {
        let array_literal = Literal::Array(vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
            Expr::Literal(Literal::Integer(3)),
        ]);
        
        match array_literal {
            Literal::Array(elements) => {
                assert_eq!(elements.len(), 3);
                for (i, element) in elements.iter().enumerate() {
                    match element {
                        Expr::Literal(Literal::Integer(value)) => assert_eq!(*value, (i + 1) as i64),
                        _ => panic!("Expected integer literal"),
                    }
                }
            },
            _ => panic!("Expected Array literal"),
        }
    }

    #[test]
    fn test_record_literals() {
        let record_literal = Literal::Record({
            let mut fields = HashMap::new();
            fields.insert("name".to_string(), Expr::Literal(Literal::String("Test".to_string())));
            fields.insert("value".to_string(), Expr::Literal(Literal::Integer(42)));
            fields
        });
        
        match record_literal {
            Literal::Record(fields) => {
                assert_eq!(fields.len(), 2);
                assert!(fields.contains_key("name"));
                assert!(fields.contains_key("value"));
                
                match fields.get("name").unwrap() {
                    Expr::Literal(Literal::String(name)) => assert_eq!(name, "Test"),
                    _ => panic!("Expected string literal"),
                }
                
                match fields.get("value").unwrap() {
                    Expr::Literal(Literal::Integer(value)) => assert_eq!(*value, 42),
                    _ => panic!("Expected integer literal"),
                }
            },
            _ => panic!("Expected Record literal"),
        }
    }

    #[test]
    fn test_char_literals() {
        let char_literal = Literal::Char('A');
        let char_literal_escape = Literal::Char('\n');
        let char_literal_unicode = Literal::Char('€');
        
        match char_literal {
            Literal::Char(ch) => assert_eq!(ch, 'A'),
            _ => panic!("Expected Char literal"),
        }
        
        match char_literal_escape {
            Literal::Char(ch) => assert_eq!(ch, '\n'),
            _ => panic!("Expected Char literal"),
        }
        
        match char_literal_unicode {
            Literal::Char(ch) => assert_eq!(ch, '€'),
            _ => panic!("Expected Char literal"),
        }
    }

    #[test]
    fn test_complex_type_combinations() {
        // Array of variant records
        let array_of_variants = Type::Array {
            index_type: Box::new(Type::Range {
                base_type: Box::new(Type::Integer),
                min: 0,
                max: 9,
            }),
            element_type: Box::new(Type::VariantRecord {
                common_fields: {
                    let mut fields = HashMap::new();
                    fields.insert("id".to_string(), Type::Integer);
                    fields
                },
                variant_part: Box::new(VariantPart {
                    discriminant: "type".to_string(),
                    discriminant_type: Box::new(Type::Integer),
                    variants: vec![
                        VariantCase {
                            case_values: vec![Literal::Integer(1)],
                            fields: {
                                let mut fields = HashMap::new();
                                fields.insert("width".to_string(), Type::Integer);
                                fields.insert("height".to_string(), Type::Integer);
                                fields
                            },
                        }
                    ],
                }),
                is_packed: false,
            }),
            range: Some((0, 9)),
        };
        
        match array_of_variants {
            Type::Array { index_type, element_type, range } => {
                assert!(matches!(*index_type, Type::Range { .. }));
                assert!(matches!(*element_type, Type::VariantRecord { .. }));
                assert_eq!(range, Some((0, 9)));
            },
            _ => panic!("Expected Array type"),
        }
    }

    #[test]
    fn test_dynamic_array_operations() {
        let set_length_expr = Expr::SetLength {
            array_expr: Box::new(Expr::Identifier(vec!["dynamicArray".to_string()])),
            length: Box::new(Expr::Literal(Literal::Integer(100))),
        };
        
        let high_expr = Expr::High {
            expr: Box::new(Expr::Identifier(vec!["dynamicArray".to_string()])),
        };
        
        let low_expr = Expr::Low {
            expr: Box::new(Expr::Identifier(vec!["dynamicArray".to_string()])),
        };
        
        match set_length_expr {
            Expr::SetLength { array_expr, length } => {
                assert!(matches!(*array_expr, Expr::Identifier(_)));
                assert!(matches!(*length, Expr::Literal(Literal::Integer(100))));
            },
            _ => panic!("Expected SetLength expression"),
        }
        
        match high_expr {
            Expr::High { expr } => {
                assert!(matches!(*expr, Expr::Identifier(_)));
            },
            _ => panic!("Expected High expression"),
        }
        
        match low_expr {
            Expr::Low { expr } => {
                assert!(matches!(*expr, Expr::Identifier(_)));
            },
            _ => panic!("Expected Low expression"),
        }
    }
}
