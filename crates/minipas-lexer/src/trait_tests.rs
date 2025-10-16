use super::traits::*;
use super::{Token, LexerError, Lexer, MockLexer};

/// Test trait-based functionality
pub fn test_lexer_capability<L: LexerCapability>(mut lexer: L) -> Result<(), String> {
    // Test basic tokenization
    let token = lexer.next_token().ok_or("No token available")?;
    let token = token.map_err(|e| format!("Lexer error: {:?}", e))?;
    println!("First token: {:?}", token.1);
    
    // Test peek functionality
    if let Some(peeked) = lexer.peek_token() {
        let peeked = peeked.map_err(|e| format!("Lexer error: {:?}", e))?;
        println!("Peeked token: {:?}", peeked.1);
    }
    
    // Test position tracking
    let position = lexer.position();
    println!("Current position: {:?}", position);
    
    // Test source access
    let source = lexer.source();
    println!("Source length: {}", source.len());
    
    Ok(())
}

/// Test token validation
pub fn test_token_validation<V: TokenValidator>(validator: &V) -> Result<(), String> {
    let valid_tokens = vec![
        Token::Identifier("x".to_string()),
        Token::Number("42".to_string()),
        Token::Assign,
    ];
    
    let invalid_tokens = vec![
        Token::Whitespace,
        Token::LineComment,
        Token::BlockComment,
    ];
    
    // Test valid tokens
    for token in &valid_tokens {
        if !validator.is_valid_token(token, 0) {
            return Err(format!("Token {:?} should be valid", token));
        }
    }
    
    // Test invalid tokens
    for token in &invalid_tokens {
        if validator.is_valid_token(token, 0) {
            return Err(format!("Token {:?} should be invalid", token));
        }
    }
    
    // Test sequence validation
    if !validator.is_valid_sequence(&valid_tokens) {
        return Err("Valid token sequence should be valid".to_string());
    }
    
    if validator.is_valid_sequence(&invalid_tokens) {
        return Err("Invalid token sequence should be invalid".to_string());
    }
    
    Ok(())
}

/// Test error reporting
pub fn test_error_reporting<R: ErrorReporter>(reporter: &R) -> Result<(), String> {
    let error = LexerError::InvalidToken {
        position: 10,
    };
    
    let context = "test input";
    let error_msg = reporter.report_error(&error, context);
    
    if !error_msg.contains("Lexer error") {
        return Err("Error message should contain 'Lexer error'".to_string());
    }
    
    if !error_msg.contains(context) {
        return Err("Error message should contain context".to_string());
    }
    
    let warning = reporter.report_warning("Test warning", 5);
    if !warning.contains("Warning") {
        return Err("Warning message should contain 'Warning'".to_string());
    }
    
    Ok(())
}

/// Test token stream functionality
pub fn test_token_stream<S: TokenStream>(mut stream: S) -> Result<(), String> {
    // Test iteration
    let mut count = 0;
    for result in stream.token_iter() {
        match result {
            Ok((_, token, _)) => {
                println!("Stream token: {:?}", token);
                count += 1;
                if count > 10 { // Prevent infinite loops
                    break;
                }
            }
            Err(e) => {
                println!("Stream error: {:?}", e);
                break;
            }
        }
    }
    
    // Test reset
    stream.reset();
    
    // Test token count (if available)
    if let Some(count) = stream.token_count() {
        println!("Token count: {}", count);
    }
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;
    
    #[test]
    fn test_real_lexer_capability() {
        let input = "x := 42;";
        let lexer = Lexer::new(input);
        test_lexer_capability(lexer).unwrap();
    }
    
    #[test]
    fn test_mock_lexer_capability() {
        let tokens = vec![
            (0, Identifier("x".to_string()), 1),
            (2, Assign, 3),
            (4, Number("42".to_string()), 6),
            (6, Semicolon, 7),
        ];
        let lexer = MockLexer::new(tokens, "x := 42;".to_string());
        test_lexer_capability(lexer).unwrap();
    }
    
    #[test]
    fn test_real_lexer_validation() {
        let input = "x := 42;";
        let lexer = Lexer::new(input);
        test_token_validation(&lexer).unwrap();
    }
    
    #[test]
    fn test_mock_lexer_validation() {
        let tokens = vec![(0, Identifier("x".to_string()), 1)];
        let lexer = MockLexer::new(tokens, "x".to_string());
        test_token_validation(&lexer).unwrap();
    }
    
    #[test]
    fn test_real_lexer_error_reporting() {
        let input = "x := 42;";
        let lexer = Lexer::new(input);
        test_error_reporting(&lexer).unwrap();
    }
    
    #[test]
    fn test_mock_lexer_error_reporting() {
        let tokens = vec![(0, Identifier("x".to_string()), 1)];
        let lexer = MockLexer::new(tokens, "x".to_string());
        test_error_reporting(&lexer).unwrap();
    }
    
    #[test]
    fn test_real_lexer_stream() {
        let input = "x := 42;";
        let lexer = Lexer::new(input);
        test_token_stream(lexer).unwrap();
    }
    
    #[test]
    fn test_mock_lexer_stream() {
        let tokens = vec![
            (0, Identifier("x".to_string()), 1),
            (2, Assign, 3),
            (4, Number("42".to_string()), 6),
        ];
        let lexer = MockLexer::new(tokens, "x := 42".to_string());
        test_token_stream(lexer).unwrap();
    }
}
