use crate::{Token, LexerError};

/// Trait for lexical analysis capabilities
pub trait LexerCapability {
    /// Creates a new lexer instance
    fn new(input: &str) -> Self where Self: Sized;
    
    /// Gets the next token from the input, skipping whitespace and comments
    fn next_token(&mut self) -> Option<Result<(usize, Token, usize), LexerError>>;
    
    /// Peeks at the next token without consuming it
    fn peek_token(&mut self) -> Option<Result<(usize, Token, usize), LexerError>>;
    
    /// Returns the current position in the input string
    fn position(&self) -> std::ops::Range<usize>;
    
    /// Returns the current span in the input string
    fn current_span(&self) -> std::ops::Range<usize>;
    
    /// Returns the remaining input as a string slice
    fn remaining_input(&self) -> &str;
    
    /// Returns the source string
    fn source(&self) -> &str;
}

/// Trait for token validation capabilities
pub trait TokenValidator {
    /// Validates if a token is valid at the current position
    fn is_valid_token(&self, token: &Token, position: usize) -> bool;
    
    /// Validates if a token sequence is valid
    fn is_valid_sequence(&self, tokens: &[Token]) -> bool;
}

/// Trait for error reporting capabilities
pub trait ErrorReporter {
    /// Reports a lexer error with context
    fn report_error(&self, error: &LexerError, context: &str) -> String;
    
    /// Reports a warning with context
    fn report_warning(&self, message: &str, position: usize) -> String;
}

/// Trait for token stream capabilities
pub trait TokenStream {
    /// Returns an iterator over tokens
    fn token_iter(&mut self) -> Box<dyn Iterator<Item = Result<(usize, Token, usize), LexerError>> + '_>;
    
    /// Resets the lexer to the beginning
    fn reset(&mut self);
    
    /// Returns the total number of tokens (if known)
    fn token_count(&self) -> Option<usize>;
}
