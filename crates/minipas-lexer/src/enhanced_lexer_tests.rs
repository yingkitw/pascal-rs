#[cfg(test)]
mod tests {
    use super::*;
    use crate::enhanced_lexer::EnhancedLexer;
    use crate::enhanced_tokens::EnhancedToken;

    #[test]
    fn test_enhanced_lexer_basic() {
        let input = "program Hello; begin writeln('Hello, World!'); end.";
        let mut lexer = EnhancedLexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((start, token, end)) => {
                    let text = &input[start..end];
                    tokens.push((token, text.to_string()));
                }
                Err(error) => {
                    panic!("Lexer error: {}", error);
                }
            }
        }
        
        // Check that we got some tokens
        assert!(!tokens.is_empty());
        
        // Check for specific tokens
        let token_types: Vec<_> = tokens.iter().map(|(token, _)| token).collect();
        assert!(token_types.contains(&&EnhancedToken::Program));
        assert!(token_types.contains(&&EnhancedToken::Begin));
        assert!(token_types.contains(&&EnhancedToken::End));
    }
    
    #[test]
    fn test_enhanced_lexer_operators() {
        let input = "a + b * c - d / e";
        let mut lexer = EnhancedLexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        // Check for operator tokens
        assert!(tokens.contains(&EnhancedToken::Plus));
        assert!(tokens.contains(&EnhancedToken::Star));
        assert!(tokens.contains(&EnhancedToken::Minus));
        assert!(tokens.contains(&EnhancedToken::Slash));
    }
    
    #[test]
    fn test_enhanced_lexer_literals() {
        let input = r#"42 3.14 "hello" 'a'"#;
        let mut lexer = EnhancedLexer::new(input);
        
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next_token() {
            match result {
                Ok((_, token, _)) => tokens.push(token),
                Err(error) => panic!("Lexer error: {}", error),
            }
        }
        
        // Check for literal tokens
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::IntegerLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::RealLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::StringLiteral(_))));
        assert!(tokens.iter().any(|t| matches!(t, EnhancedToken::CharLiteral(_))));
    }
}
