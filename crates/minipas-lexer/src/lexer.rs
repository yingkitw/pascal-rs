use crate::tokens::Token;
use crate::traits::*;
use logos::Logos;
use thiserror::Error;

/// Represents different types of errors that can occur during lexing
#[derive(Error, Debug, Clone, PartialEq)]
pub enum LexerError {
    /// Invalid token at the given position
    #[error("Invalid token at position {position}")]
    InvalidToken { position: usize },
    
    /// Unexpected token found
    #[error("Unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        expected: String,
        found: String,
    },
    
    /// Unterminated string literal
    #[error("Unterminated string")]
    UnterminatedString,
    
    /// Invalid numeric literal
    #[error("Invalid numeric literal")]
    InvalidNumber,
    
    /// End of file reached unexpectedly
    #[error("Unexpected end of file")]
    UnexpectedEof,
    
    /// Number too large for the target type
    #[error("Number too large for the target type")]
    NumberTooLarge,
    
    /// Invalid escape sequence in string
    #[error("Invalid escape sequence: {sequence}")]
    InvalidEscapeSequence {
        sequence: String,
    },
    
    /// Other lexing error
    #[error("Lexing error: {0}")]
    Other(String),
}

impl From<std::num::ParseIntError> for LexerError {
    fn from(_: std::num::ParseIntError) -> Self {
        LexerError::InvalidNumber
    }
}

impl From<std::num::ParseFloatError> for LexerError {
    fn from(_: std::num::ParseFloatError) -> Self {
        LexerError::InvalidNumber
    }
}

/// The main lexer struct
pub struct Lexer<'a> {
    source: &'a str,
    inner: logos::Lexer<'a, Token>,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer for the given input string
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: Token::lexer(input),
            source: input,
        }
    }
    
    /// Returns the current position in the input string
    pub fn position(&self) -> std::ops::Range<usize> {
        self.inner.span()
    }
    
    /// Returns the current span in the input string
    pub fn current_span(&self) -> std::ops::Range<usize> {
        self.inner.span()
    }
    
    /// Returns the remaining input as a string slice
    pub fn remaining_input(&self) -> &'a str {
        &self.source[self.inner.span().end..]
    }
    
    /// Returns the source string
    pub fn source(&self) -> &'a str {
        self.source
    }
    
    /// Gets the next token from the input, skipping whitespace and comments
    pub fn next_token(&mut self) -> Option<Result<(usize, Token, usize), LexerError>> {
        loop {
            match self.inner.next() {
                Some(Ok(token)) => {
                    let span = self.inner.span();
                    match token {
                        // Skip whitespace and comments
                        Token::Whitespace | Token::LineComment | Token::BlockComment => continue,
                        // Handle other tokens
                        _ => return Some(Ok((span.start, token, span.end))),
                    }
                }
                Some(Err(_)) => {
                    let span = self.inner.span();
                    return Some(Err(LexerError::InvalidToken { position: span.start }));
                }
                None => return None,
            }
        }
    }
}

impl<'a> Lexer<'a> {
    /// Peeks at the next token without consuming it
    pub fn peek_token(&mut self) -> Option<Result<(usize, Token, usize), LexerError>> {
        let mut clone = self.inner.clone();
        
        loop {
            match clone.next()? {
                Ok(token) => {
                    if !matches!(token, Token::Whitespace | Token::LineComment | Token::BlockComment) {
                        let span = clone.span();
                        // Store the token and its position
                        let result = Ok((span.start, token, span.end));
                        return Some(result);
                    }
                }
                Err(_) => {
                    let span = clone.span();
                    return Some(Err(LexerError::InvalidToken { position: span.start }));
                }
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<(usize, Token, usize), LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

// Note: Lexer<'a> doesn't implement LexerCapability directly due to lifetime constraints
// Use MockLexer for trait-based testing instead

impl<'a> TokenValidator for Lexer<'a> {
    fn is_valid_token(&self, token: &Token, _position: usize) -> bool {
        !matches!(token, Token::Whitespace | Token::LineComment | Token::BlockComment)
    }
    
    fn is_valid_sequence(&self, tokens: &[Token]) -> bool {
        // Basic validation - could be enhanced with grammar rules
        !tokens.is_empty() && tokens.iter().all(|t| self.is_valid_token(t, 0))
    }
}

impl<'a> ErrorReporter for Lexer<'a> {
    fn report_error(&self, error: &LexerError, context: &str) -> String {
        format!("Lexer error in '{}': {}", context, error)
    }
    
    fn report_warning(&self, message: &str, position: usize) -> String {
        format!("Warning at position {}: {}", position, message)
    }
}

impl<'a> TokenStream for Lexer<'a> {
    fn token_iter(&mut self) -> Box<dyn Iterator<Item = Result<(usize, Token, usize), LexerError>> + '_> {
        Box::new(self)
    }
    
    fn reset(&mut self) {
        // Reset the inner lexer
        self.inner = Token::lexer(self.source);
    }
    
    fn token_count(&self) -> Option<usize> {
        None // Not easily determinable without consuming all tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;
    
    #[test]
    fn test_basic_tokens() {
        let input = "program Test; begin end.";
        let mut lexer = Lexer::new(input);
        
        assert_matches!(lexer.next_token(), Some(Ok((_, Token::Program, _))));
        assert_matches!(lexer.next_token(), Some(Ok((_, Token::Identifier(_), _))));
        assert_matches!(lexer.next_token(), Some(Ok((_, Token::Semicolon, _))));
        assert_matches!(lexer.next_token(), Some(Ok((_, Token::Begin, _))));
        assert_matches!(lexer.next_token(), Some(Ok((_, Token::End, _))));
        assert_matches!(lexer.next_token(), Some(Ok((_, Token::Dot, _))));
        assert_matches!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_peek() {
        let input = "x := 42";
        let mut lexer = Lexer::new(input);
        
        // First peek should return the first token
        let first_peek = lexer.peek_token();
        assert_matches!(first_peek, Some(Ok((_, Token::Identifier(_), _))));
        
        // Second peek should return the same token
        let second_peek = lexer.peek_token();
        assert_eq!(
            first_peek.as_ref().map(|r| r.as_ref().map(|(_, t, _)| t)),
            second_peek.as_ref().map(|r| r.as_ref().map(|(_, t, _)| t))
        );
        
        // Consume the token
        let consumed = lexer.next_token();
        assert_eq!(
            first_peek.as_ref().map(|r| r.as_ref().map(|(_, t, _)| t)),
            consumed.as_ref().map(|r| r.as_ref().map(|(_, t, _)| t))
        );
    }
}
