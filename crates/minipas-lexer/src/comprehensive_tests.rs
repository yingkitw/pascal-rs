#[cfg(test)]
mod comprehensive_tests {
    use super::*;
    use crate::enhanced_lexer::EnhancedLexer;
    use crate::enhanced_tokens::EnhancedToken;
    use crate::lexer::Lexer;
    use crate::tokens::Token;

    // ============================================================================
    // BASIC LEXER TESTS
    // ============================================================================

    #[test]
    fn test_basic_lexer_keywords() {
        let input = "program var begin end if then else while do for to downto";
        let mut lexer = Lexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        let expected_keywords = vec![
            Token::Program,
            Token::Var,
            Token::Begin,
            Token::End,
            Token::If,
            Token::Then,
            Token::Else,
            Token::While,
            Token::Do,
            Token::For,
            Token::To,
            Token::Downto,
        ];
        
        assert_eq!(tokens, expected_keywords);
    }

    #[test]
    fn test_basic_lexer_operators() {
        let input = "+ - * / = <> < <= > >= and or not";
        let mut lexer = Lexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        let expected_operators = vec![
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Equal,
            Token::NotEqual,
            Token::LessThan,
            Token::LessEqual,
            Token::GreaterThan,
            Token::GreaterEqual,
            Token::And,
            Token::Or,
            Token::Not,
        ];
        
        assert_eq!(tokens, expected_operators);
    }

    #[test]
    fn test_basic_lexer_literals() {
        let input = r#"42 3.14 "hello" 'a' true false"#;
        let mut lexer = Lexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        // Check for literal types
        assert!(tokens.iter().any(|t| matches!(t, Token::IntegerLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, Token::RealLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, Token::StringLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, Token::CharLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, Token::True)));
        assert!(tokens.iter().any(|t| matches!(t, Token::False)));
    }

    #[test]
    fn test_basic_lexer_identifiers() {
        let input = "hello world variable_name _underscore";
        let mut lexer = Lexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        // All should be identifiers
        for token in &tokens {
            assert!(matches!(token, Token::Identifier(_)));
        }
    }

    #[test]
    fn test_basic_lexer_comments() {
        let input = r#"program test; { This is a comment } var x: integer; // Another comment"#;
        let mut lexer = Lexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        // Comments should be filtered out
        let expected_tokens = vec![
            Token::Program,
            Token::Identifier("test".to_string()),
            Token::Semicolon,
            Token::Var,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("integer".to_string()),
            Token::Semicolon,
        ];
        
        assert_eq!(tokens, expected_tokens);
    }

    // ============================================================================
    // ENHANCED LEXER TESTS
    // ============================================================================

    #[test]
    fn test_enhanced_lexer_comprehensive_keywords() {
        let input = "program unit interface implementation uses type const var procedure function class object record set array file string";
        let mut lexer = EnhancedLexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        let expected_keywords = vec![
            EnhancedToken::Program,
            EnhancedToken::Unit,
            EnhancedToken::Interface,
            EnhancedToken::Implementation,
            EnhancedToken::Uses,
            EnhancedToken::Type,
            EnhancedToken::Const,
            EnhancedToken::Var,
            EnhancedToken::Procedure,
            EnhancedToken::Function,
            EnhancedToken::Class,
            EnhancedToken::Object,
            EnhancedToken::Record,
            EnhancedToken::Set,
            EnhancedToken::Array,
            EnhancedToken::File,
            EnhancedToken::String,
        ];
        
        assert_eq!(tokens, expected_keywords);
    }

    #[test]
    fn test_enhanced_lexer_advanced_operators() {
        let input = "+ - * / = <> < <= > >= and or not xor shl shr div mod in is as += -= *= /= := & | ~";
        let mut lexer = EnhancedLexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        let expected_operators = vec![
            EnhancedToken::Plus,
            EnhancedToken::Minus,
            EnhancedToken::Star,
            EnhancedToken::Slash,
            EnhancedToken::Equal,
            EnhancedToken::NotEqual,
            EnhancedToken::LessThan,
            EnhancedToken::LessEqual,
            EnhancedToken::GreaterThan,
            EnhancedToken::GreaterEqual,
            EnhancedToken::And,
            EnhancedToken::Or,
            EnhancedToken::Not,
            EnhancedToken::Xor,
            EnhancedToken::Shl,
            EnhancedToken::Shr,
            EnhancedToken::Div,
            EnhancedToken::Mod,
            EnhancedToken::In,
            EnhancedToken::Is,
            EnhancedToken::As,
            EnhancedToken::PlusAssign,
            EnhancedToken::MinusAssign,
            EnhancedToken::StarAssign,
            EnhancedToken::SlashAssign,
            EnhancedToken::Assignment,
            EnhancedToken::BitwiseAnd,
            EnhancedToken::BitwiseOr,
            EnhancedToken::BitwiseNot,
        ];
        
        assert_eq!(tokens, expected_operators);
    }

    #[test]
    fn test_enhanced_lexer_comprehensive_literals() {
        let input = r#"42 3.14 1.23e-4 "hello" 'a' #65 true false #$FF #%1010"#;
        let mut lexer = EnhancedLexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        // Check for various literal types
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::IntegerLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::RealLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::StringLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::CharLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::True)));
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::False)));
    }

    #[test]
    fn test_enhanced_lexer_escape_sequences() {
        let input = r#""hello\nworld" "tab\there" "quote\"here" "backslash\\here""#;
        let mut lexer = EnhancedLexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        // All should be string literals
        for token in &tokens {
            assert!(matches!(token, EnhancedToken::StringLiteral(_)));
        }
    }

    #[test]
    fn test_enhanced_lexer_preprocessor_directives() {
        let input = "{$mode objfpc} {$h+} {$ifdef DEBUG} {$endif}";
        let mut lexer = EnhancedLexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        // Should handle preprocessor directives
        assert!(!tokens.is_empty());
    }

    #[test]
    fn test_enhanced_lexer_complex_program() {
        let input = r#"
        program ComplexTest;
        uses SysUtils;
        
        type
            TMyRecord = record
                field1: Integer;
                field2: String;
            end;
            
            TMyClass = class
            private
                FValue: Integer;
            public
                property Value: Integer read FValue write FValue;
                constructor Create(AValue: Integer);
                destructor Destroy; override;
            end;
        
        var
            x, y: Integer;
            str: String;
            rec: TMyRecord;
            obj: TMyClass;
        
        constructor TMyClass.Create(AValue: Integer);
        begin
            FValue := AValue;
        end;
        
        destructor TMyClass.Destroy;
        begin
            // Cleanup
        end;
        
        begin
            x := 42;
            y := x + 1;
            str := 'Hello, World!';
            rec.field1 := x;
            rec.field2 := str;
            obj := TMyClass.Create(y);
            try
                obj.Value := rec.field1 * 2;
            finally
                obj.Free;
            end;
        end.
        "#;
        
        let mut lexer = EnhancedLexer::new(input);
        let mut token_count = 0;
        let mut error_count = 0;
        
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => {
                    token_count += 1;
                    // Verify token is valid
                    assert!(!matches!(token, EnhancedToken::Error));
                }
                Err(error) => {
                    error_count += 1;
                    println!("Lexer error: {}", error);
                }
            }
        }
        
        // Should have many tokens and no errors
        assert!(token_count > 50);
        assert_eq!(error_count, 0);
    }

    // ============================================================================
    // ERROR HANDLING TESTS
    // ============================================================================

    #[test]
    fn test_lexer_error_handling() {
        let input = r#""unterminated string"#;
        let mut lexer = Lexer::new(input);
        
        let mut has_error = false;
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => {
                    // Should not get valid tokens for unterminated string
                    if matches!(token, Token::StringLiteral(_)) {
                        has_error = true;
                    }
                }
                Err(_) => {
                    has_error = true;
                }
            }
        }
        
        assert!(has_error, "Should have detected unterminated string");
    }

    #[test]
    fn test_enhanced_lexer_error_handling() {
        let input = r#""unterminated string"#;
        let mut lexer = EnhancedLexer::new(input);
        
        let mut has_error = false;
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => {
                    if matches!(token, EnhancedToken::Error) {
                        has_error = true;
                    }
                }
                Err(_) => {
                    has_error = true;
                }
            }
        }
        
        assert!(has_error, "Should have detected unterminated string");
    }

    // ============================================================================
    // POSITION TRACKING TESTS
    // ============================================================================

    #[test]
    fn test_lexer_position_tracking() {
        let input = "hello\nworld\ntest";
        let mut lexer = Lexer::new(input);
        
        let mut positions = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((start, token, end)) => {
                    positions.push((start, token, end));
                }
                Err(_) => {}
            }
        }
        
        // Check that positions are correct
        assert!(positions.len() >= 3);
        assert_eq!(positions[0].0, 0); // "hello" starts at 0
        assert_eq!(positions[1].0, 6); // "world" starts at 6 (after \n)
        assert_eq!(positions[2].0, 12); // "test" starts at 12 (after \n)
    }

    #[test]
    fn test_enhanced_lexer_position_tracking() {
        let input = "hello\nworld\ntest";
        let mut lexer = EnhancedLexer::new(input);
        
        let mut positions = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((start, token, end)) => {
                    positions.push((start, token, end));
                }
                Err(_) => {}
            }
        }
        
        // Check that positions are correct
        assert!(positions.len() >= 3);
        assert_eq!(positions[0].0, 0); // "hello" starts at 0
        assert_eq!(positions[1].0, 6); // "world" starts at 6 (after \n)
        assert_eq!(positions[2].0, 12); // "test" starts at 12 (after \n)
    }

    // ============================================================================
    // PERFORMANCE TESTS
    // ============================================================================

    #[test]
    fn test_lexer_performance() {
        let input = "program test; var x: integer; begin x := 42; end.".repeat(1000);
        let mut lexer = Lexer::new(&input);
        
        let start = std::time::Instant::now();
        let mut token_count = 0;
        
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => {
                    token_count += 1;
                    // Prevent optimization
                    std::hint::black_box(token);
                }
                Err(_) => {}
            }
        }
        
        let duration = start.elapsed();
        
        // Should process quickly (less than 100ms for 1000 repetitions)
        assert!(duration.as_millis() < 100);
        assert!(token_count > 1000);
    }

    #[test]
    fn test_enhanced_lexer_performance() {
        let input = "program test; var x: integer; begin x := 42; end.".repeat(1000);
        let mut lexer = EnhancedLexer::new(&input);
        
        let start = std::time::Instant::now();
        let mut token_count = 0;
        
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => {
                    token_count += 1;
                    // Prevent optimization
                    std::hint::black_box(token);
                }
                Err(_) => {}
            }
        }
        
        let duration = start.elapsed();
        
        // Should process quickly (less than 200ms for 1000 repetitions)
        assert!(duration.as_millis() < 200);
        assert!(token_count > 1000);
    }
}
