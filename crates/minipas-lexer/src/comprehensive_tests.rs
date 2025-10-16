#[cfg(test)]
mod comprehensive_tests {
    use super::*;
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
            Token::DownTo,
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
            Token::Multiply,
            Token::Divide,
            Token::Equal,
            Token::NotEqual,
            Token::Less,
            Token::LessOrEqual,
            Token::Greater,
            Token::GreaterOrEqual,
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
        assert!(tokens.iter().any(|t| matches!(t, Token::Number(_))));
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
            Token::Integer,
            Token::Semicolon,
        ];
        
        assert_eq!(tokens, expected_tokens);
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
                Ok(_) => {},
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
                Err(error) => panic!("Lexer error: {}", error),
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
                    drop(token);
                }
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        let duration = start.elapsed();
        
        // Should process quickly (less than 100ms for 1000 repetitions)
        assert!(duration.as_millis() < 100);
        assert!(token_count > 1000);
    }
}
