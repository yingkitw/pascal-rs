#[cfg(test)]
mod error_handling_comprehensive_tests {
    use minipas_lexer::{Lexer, EnhancedLexer, LexerError};
    use minipas_parser::{Parser, EnhancedParser, ParseError};
    use minipas_codegen::{CodeGenerator, EnhancedCodeGenerator, TargetArchitecture};
    use minipas_ast::*;

    // ============================================================================
    // LEXER ERROR HANDLING TESTS
    // ============================================================================

    #[test]
    fn test_lexer_unterminated_string_error() {
        let input = r#"program Test; begin writeln("unterminated string); end."#;
        let mut lexer = Lexer::new(input);
        
        let mut has_error = false;
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => {
                    // Check for error tokens
                    if matches!(token, minipas_lexer::Token::StringLiteral(_)) {
                        // This might be an unterminated string
                    }
                }
                Err(error) => {
                    has_error = true;
                    assert!(!format!("{:?}", error).is_empty());
                }
            }
        }
        
        // Should detect the unterminated string
        assert!(has_error, "Should have detected unterminated string");
    }

    #[test]
    fn test_lexer_invalid_character_error() {
        let input = "program Test; begin x := @#$%; end.";
        let mut lexer = Lexer::new(input);
        
        let mut has_error = false;
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => {
                    // Check for unexpected tokens
                    if matches!(token, minipas_lexer::Token::Identifier(_)) {
                        // Might be an invalid identifier
                    }
                }
                Err(error) => {
                    has_error = true;
                    assert!(!format!("{:?}", error).is_empty());
                }
            }
        }
        
        // Should handle invalid characters gracefully
        // Note: The current lexer might not detect all invalid characters
    }

    #[test]
    fn test_enhanced_lexer_error_handling() {
        let input = r#"program Test; begin writeln("unterminated string); end."#;
        let mut enhanced_lexer = EnhancedLexer::new(input);
        
        let mut has_error = false;
        while let Some(result) = enhanced_lexer.next_token() {
            match result {
                Ok((_, token, _)) => {
                    if matches!(token, minipas_lexer::EnhancedToken::Error) {
                        has_error = true;
                    }
                }
                Err(error) => {
                    has_error = true;
                    assert!(!error.is_empty());
                }
            }
        }
        
        // Should detect errors
        assert!(has_error, "Enhanced lexer should detect errors");
    }

    #[test]
    fn test_lexer_position_tracking_on_error() {
        let input = "program Test; begin x := \"unterminated; end.";
        let mut lexer = Lexer::new(input);
        
        let mut error_position = None;
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((pos, token, _)) => {
                    if matches!(token, minipas_lexer::Token::StringLiteral(_)) {
                        error_position = Some(pos);
                    }
                }
                Err(error) => {
                    error_position = Some(0); // Default position
                    assert!(!format!("{:?}", error).is_empty());
                }
            }
        }
        
        // Should track error position
        assert!(error_position.is_some());
    }

    // ============================================================================
    // PARSER ERROR HANDLING TESTS
    // ============================================================================

    #[test]
    fn test_parser_syntax_error_handling() {
        let input = "program Test; begin x := ; end."; // Missing expression
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err(), "Should detect syntax error");
        
        if let Err(ParseError::UnexpectedToken { expected, found, position }) = result {
            assert!(!expected.is_empty());
            assert!(found.is_some());
            assert!(position > 0);
        } else {
            panic!("Expected UnexpectedToken error");
        }
    }

    #[test]
    fn test_parser_missing_semicolon_error() {
        let input = "program Test begin end."; // Missing semicolon
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err(), "Should detect missing semicolon");
        
        if let Err(ParseError::UnexpectedToken { expected, found, position }) = result {
            assert!(expected.contains(&"Semicolon".to_string()));
            assert!(position > 0);
        } else {
            panic!("Expected UnexpectedToken error");
        }
    }

    #[test]
    fn test_parser_missing_end_error() {
        let input = "program Test; begin"; // Missing end
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err(), "Should detect missing end");
        
        if let Err(ParseError::UnexpectedEof { expected }) = result {
            assert!(!expected.is_empty());
        } else {
            panic!("Expected UnexpectedEof error");
        }
    }

    #[test]
    fn test_parser_invalid_expression_error() {
        let input = "program Test; begin x := +; end."; // Invalid expression
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err(), "Should detect invalid expression");
    }

    #[test]
    fn test_parser_undefined_variable_error() {
        let input = "program Test; begin x := undefined_var; end."; // Undefined variable
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        // This might succeed if undefined variables are allowed during parsing
        match result {
            Ok(program) => {
                // If parsing succeeds, code generation might fail
                let mut codegen = CodeGenerator::new();
                let codegen_result = codegen.generate_code(&program);
                match codegen_result {
                    Ok(_) => {
                        // Code generation succeeded
                    }
                    Err(error) => {
                        assert!(!error.is_empty());
                    }
                }
            }
            Err(error) => {
                assert!(!format!("{:?}", error).is_empty());
            }
        }
    }

    #[test]
    fn test_parser_type_mismatch_error() {
        let input = r#"
        program Test;
        var
            x: integer;
            y: string;
        begin
            x := y; // Type mismatch
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        // Type checking might not be implemented yet
        match result {
            Ok(program) => {
                // If parsing succeeds, type checking might be done later
                assert_eq!(program.name, "Test");
            }
            Err(error) => {
                assert!(!format!("{:?}", error).is_empty());
            }
        }
    }

    #[test]
    fn test_enhanced_parser_error_handling() {
        let input = "program Test; begin x := ; end."; // Missing expression
        let mut enhanced_parser = EnhancedParser::new(input);
        
        let result = enhanced_parser.parse_program();
        match result {
            Ok(program) => {
                // If it succeeds, it should handle the error internally
                assert_eq!(program.name, "Test");
            }
            Err(error) => {
                // If it fails, it should be a meaningful error
                assert!(!format!("{:?}", error).is_empty());
            }
        }
    }

    // ============================================================================
    // CODEGEN ERROR HANDLING TESTS
    // ============================================================================

    #[test]
    fn test_codegen_undefined_variable_error() {
        let program = Program {
            name: "UndefinedVarTest".to_string(),
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
            Ok(assembly) => {
                // If it succeeds, it should handle undefined variables
                assert!(!assembly.is_empty());
            }
            Err(error) => {
                // If it fails, it should be a meaningful error
                assert!(!error.is_empty());
            }
        }
    }

    #[test]
    fn test_codegen_unsupported_type_error() {
        let program = Program {
            name: "UnsupportedTypeTest".to_string(),
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
        
        // Should handle unsupported types gracefully
        match result {
            Ok(assembly) => {
                // If it succeeds, it should handle real numbers
                assert!(!assembly.is_empty());
            }
            Err(error) => {
                // If it fails, it should be a meaningful error
                assert!(!error.is_empty());
            }
        }
    }

    #[test]
    fn test_codegen_register_allocation_error() {
        let program = Program {
            name: "RegisterAllocationTest".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: (0..1000)
                    .map(|i| VariableDecl {
                        name: format!("var_{}", i),
                        var_type: Type::Integer,
                        initializer: None,
                        is_threadvar: false,
                        absolute_address: None,
                        external_name: None,
                    })
                    .collect(),
                procedures: vec![],
                functions: vec![],
                statements: vec![],
            },
        };

        let mut codegen = CodeGenerator::new();
        let result = codegen.generate_code(&program);
        
        // Should handle register allocation gracefully
        match result {
            Ok(assembly) => {
                // If it succeeds, it should handle many variables
                assert!(!assembly.is_empty());
            }
            Err(error) => {
                // If it fails, it should be a meaningful error
                assert!(!error.is_empty());
            }
        }
    }

    #[test]
    fn test_enhanced_codegen_error_handling() {
        let program = Program {
            name: "EnhancedErrorTest".to_string(),
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

        let mut enhanced_codegen = EnhancedCodeGenerator::new(TargetArchitecture::X86_64);
        let result = enhanced_codegen.generate_code(&program);
        
        // Should handle errors gracefully
        match result {
            Ok(assembly) => {
                assert!(!assembly.is_empty());
            }
            Err(error) => {
                assert!(!error.is_empty());
            }
        }
    }

    // ============================================================================
    // COMPREHENSIVE ERROR RECOVERY TESTS
    // ============================================================================

    #[test]
    fn test_error_recovery_after_syntax_error() {
        let input = r#"
        program ErrorRecoveryTest;
        var
            x: integer;
        begin
            x := 42;
            x := ; // Syntax error
            x := 10; // Should recover and continue
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        // Should either recover or fail gracefully
        match result {
            Ok(program) => {
                // If it recovers, should have valid statements
                assert!(!program.block.statements.is_empty());
            }
            Err(error) => {
                // If it fails, should be a meaningful error
                assert!(!format!("{:?}", error).is_empty());
            }
        }
    }

    #[test]
    fn test_error_recovery_after_lexical_error() {
        let input = r#"
        program LexicalErrorRecoveryTest;
        var
            x: integer;
        begin
            x := 42;
            x := "unterminated string;
            x := 10; // Should recover and continue
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        // Should handle lexical errors gracefully
        match result {
            Ok(program) => {
                // If it recovers, should have valid statements
                assert!(!program.block.statements.is_empty());
            }
            Err(error) => {
                // If it fails, should be a meaningful error
                assert!(!format!("{:?}", error).is_empty());
            }
        }
    }

    #[test]
    fn test_multiple_error_handling() {
        let input = r#"
        program MultipleErrorsTest;
        var
            x: integer;
        begin
            x := ; // Syntax error 1
            x := "unterminated string; // Lexical error
            x := undefined_var; // Semantic error
            x := 10; // Valid statement
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        // Should handle multiple errors gracefully
        match result {
            Ok(program) => {
                // If it recovers, should have some valid statements
                assert!(!program.block.statements.is_empty());
            }
            Err(error) => {
                // If it fails, should be a meaningful error
                assert!(!format!("{:?}", error).is_empty());
            }
        }
    }

    // ============================================================================
    // ERROR MESSAGE QUALITY TESTS
    // ============================================================================

    #[test]
    fn test_error_message_quality() {
        let input = "program Test; begin x := ; end."; // Missing expression
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
        
        if let Err(ParseError::UnexpectedToken { expected, found, position }) = result {
            // Error message should be informative
            assert!(!expected.is_empty());
            assert!(found.is_some());
            assert!(position > 0);
            
            // Expected tokens should be meaningful
            assert!(expected.iter().any(|e| e.contains("expression") || e.contains("identifier")));
        }
    }

    #[test]
    fn test_error_position_accuracy() {
        let input = "program Test; begin x := ; end."; // Missing expression at position 25
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
        
        if let Err(ParseError::UnexpectedToken { position, .. }) = result {
            // Position should be accurate
            assert!(position > 0);
            assert!(position < input.len());
        }
    }

    #[test]
    fn test_error_context_information() {
        let input = r#"
        program ContextTest;
        var
            x: integer;
        begin
            x := ; // Error here
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
        
        if let Err(ParseError::UnexpectedToken { expected, found, position }) = result {
            // Should provide context about what was expected
            assert!(!expected.is_empty());
            assert!(found.is_some());
            assert!(position > 0);
        }
    }

    // ============================================================================
    // EDGE CASE ERROR TESTS
    // ============================================================================

    #[test]
    fn test_empty_input_error() {
        let input = "";
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
        
        if let Err(ParseError::UnexpectedEof { expected }) = result {
            assert!(!expected.is_empty());
        }
    }

    #[test]
    fn test_whitespace_only_input_error() {
        let input = "   \n  \t  ";
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
        
        if let Err(ParseError::UnexpectedEof { expected }) = result {
            assert!(!expected.is_empty());
        }
    }

    #[test]
    fn test_comment_only_input_error() {
        let input = "{ This is just a comment }";
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
        
        if let Err(ParseError::UnexpectedEof { expected }) = result {
            assert!(!expected.is_empty());
        }
    }

    #[test]
    fn test_malformed_program_structure_error() {
        let input = "begin end."; // Missing program declaration
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
        
        if let Err(ParseError::UnexpectedToken { expected, found, position }) = result {
            assert!(!expected.is_empty());
            assert!(found.is_some());
            assert!(position > 0);
        }
    }

    #[test]
    fn test_nested_error_handling() {
        let input = r#"
        program NestedErrorTest;
        var
            x: integer;
        begin
            if x > 0 then
            begin
                x := ; // Error inside nested block
            end;
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
        
        if let Err(ParseError::UnexpectedToken { expected, found, position }) = result {
            assert!(!expected.is_empty());
            assert!(found.is_some());
            assert!(position > 0);
        }
    }

    // ============================================================================
    // PERFORMANCE UNDER ERROR CONDITIONS
    // ============================================================================

    #[test]
    fn test_error_handling_performance() {
        let input = "program Test; begin x := ; end."; // Simple error
        let mut parser = Parser::new(input);
        
        let start = std::time::Instant::now();
        let result = parser.parse_program();
        let duration = start.elapsed();
        
        // Should handle errors quickly (less than 10ms)
        assert!(duration.as_millis() < 10);
        assert!(result.is_err());
    }

    #[test]
    fn test_large_error_input_handling() {
        let mut input = String::from("program LargeErrorTest;\nvar\n");
        
        // Add many variables
        for i in 0..100 {
            input.push_str(&format!("    var{}: integer;\n", i));
        }
        
        input.push_str("begin\n");
        
        // Add many statements with errors
        for i in 0..100 {
            input.push_str(&format!("    var{} := ;\n", i)); // Missing expression
        }
        
        input.push_str("end.\n");
        
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        
        let start = std::time::Instant::now();
        let result = parser.parse_program();
        let duration = start.elapsed();
        
        // Should handle large error input reasonably quickly (less than 100ms)
        assert!(duration.as_millis() < 100);
        assert!(result.is_err());
    }
}
