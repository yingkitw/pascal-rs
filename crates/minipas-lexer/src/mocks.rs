use super::traits::*;
use super::{Token, LexerError};

/// Mock lexer for testing purposes
pub struct MockLexer {
    tokens: Vec<(usize, Token, usize)>,
    current_index: usize,
    source: String,
}

impl MockLexer {
    pub fn new(tokens: Vec<(usize, Token, usize)>, source: String) -> Self {
        Self {
            tokens,
            current_index: 0,
            source,
        }
    }
    
    pub fn with_single_token(token: Token) -> Self {
        Self::new(vec![(0, token, 1)], "test".to_string())
    }
}

impl LexerCapability for MockLexer {
    fn new(input: &str) -> Self {
        Self::new(vec![], input.to_string())
    }
    
    fn next_token(&mut self) -> Option<Result<(usize, Token, usize), LexerError>> {
        if self.current_index < self.tokens.len() {
            let token = self.tokens[self.current_index].clone();
            self.current_index += 1;
            Some(Ok(token))
        } else {
            None
        }
    }
    
    fn peek_token(&mut self) -> Option<Result<(usize, Token, usize), LexerError>> {
        if self.current_index < self.tokens.len() {
            Some(Ok(self.tokens[self.current_index].clone()))
        } else {
            None
        }
    }
    
    fn position(&self) -> std::ops::Range<usize> {
        if self.current_index < self.tokens.len() {
            let (start, _, end) = self.tokens[self.current_index];
            start..end
        } else {
            0..0
        }
    }
    
    fn current_span(&self) -> std::ops::Range<usize> {
        self.position()
    }
    
    fn remaining_input(&self) -> &str {
        if self.current_index < self.tokens.len() {
            let (start, _, _) = self.tokens[self.current_index];
            &self.source[start..]
        } else {
            ""
        }
    }
    
    fn source(&self) -> &str {
        &self.source
    }
}

impl TokenValidator for MockLexer {
    fn is_valid_token(&self, token: &Token, _position: usize) -> bool {
        !matches!(token, Token::Whitespace | Token::LineComment | Token::BlockComment)
    }
    
    fn is_valid_sequence(&self, tokens: &[Token]) -> bool {
        !tokens.is_empty() && tokens.iter().all(|t| self.is_valid_token(t, 0))
    }
}

impl ErrorReporter for MockLexer {
    fn report_error(&self, error: &LexerError, context: &str) -> String {
        format!("Mock lexer error in '{}': {}", context, error)
    }
    
    fn report_warning(&self, message: &str, position: usize) -> String {
        format!("Mock warning at position {}: {}", position, message)
    }
}

impl Iterator for MockLexer {
    type Item = Result<(usize, Token, usize), LexerError>;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl TokenStream for MockLexer {
    fn token_iter(&mut self) -> Box<dyn Iterator<Item = Result<(usize, Token, usize), LexerError>> + '_> {
        Box::new(self)
    }
    
    fn reset(&mut self) {
        self.current_index = 0;
    }
    
    fn token_count(&self) -> Option<usize> {
        Some(self.tokens.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;
    
    #[test]
    fn test_mock_lexer_basic() {
        let tokens = vec![
            (0, Identifier("x".to_string()), 1),
            (2, Assign, 3),
            (4, Number("42".to_string()), 6),
        ];
        let mut lexer = MockLexer::new(tokens, "x := 42".to_string());
        
        assert_eq!(lexer.next_token(), Some(Ok((0, Identifier("x".to_string()), 1))));
        assert_eq!(lexer.next_token(), Some(Ok((2, Assign, 3))));
        assert_eq!(lexer.next_token(), Some(Ok((4, Number("42".to_string()), 6))));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_mock_lexer_peek() {
        let tokens = vec![
            (0, Identifier("x".to_string()), 1),
            (2, Assign, 3),
        ];
        let mut lexer = MockLexer::new(tokens, "x :=".to_string());
        
        assert_eq!(lexer.peek_token(), Some(Ok((0, Identifier("x".to_string()), 1))));
        assert_eq!(lexer.next_token(), Some(Ok((0, Identifier("x".to_string()), 1))));
        assert_eq!(lexer.peek_token(), Some(Ok((2, Assign, 3))));
    }
    
    #[test]
    fn test_mock_lexer_reset() {
        let tokens = vec![
            (0, Identifier("x".to_string()), 1),
            (2, Assign, 3),
        ];
        let mut lexer = MockLexer::new(tokens, "x :=".to_string());
        
        assert_eq!(lexer.next_token(), Some(Ok((0, Identifier("x".to_string()), 1))));
        lexer.reset();
        assert_eq!(lexer.next_token(), Some(Ok((0, Identifier("x".to_string()), 1))));
    }
}
