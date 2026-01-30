//! Lexer trait definitions

use crate::lexer::LexerError;

/// Core lexer capability trait
pub trait LexerCapability {
    /// Token type produced by this lexer
    type Token;

    /// Get the next token from the input
    fn next_token(&mut self) -> Option<Result<(usize, Self::Token, usize), LexerError>>;

    /// Peek at the next token without consuming it
    fn peek_token(&mut self) -> Option<Result<(usize, Self::Token, usize), LexerError>>;

    /// Get the current position in the input
    fn position(&self) -> std::ops::Range<usize>;

    /// Get source code reference
    fn source(&self) -> &str;
}

/// Token validation trait
pub trait TokenValidator {
    type Token;

    /// Validate a single token
    fn is_valid_token(&self, token: &Self::Token, position: usize) -> bool;

    /// Validate a sequence of tokens
    fn is_valid_sequence(&self, tokens: &[Self::Token]) -> bool;

    /// Validate token with context
    fn validate_token(&self, token: &Self::Token) -> bool;
}

/// Error reporting trait for lexers
pub trait ErrorReporter {
    /// Report a lexing error
    fn report_error(&self, error: &LexerError, context: &str) -> String;

    /// Report a warning
    fn report_warning(&self, message: &str, position: usize) -> String;
}

/// Token stream iteration trait
pub trait TokenStream {
    type Token;

    /// Get iterator over tokens
    fn token_iter(
        &mut self,
    ) -> Box<dyn Iterator<Item = Result<(usize, Self::Token, usize), LexerError>> + '_>;

    /// Reset the stream to beginning
    fn reset(&mut self);

    /// Get total token count (if available)
    fn token_count(&self) -> Option<usize>;
}

/// Source location tracking
pub trait SourceLocation {
    /// Get line and column for a position
    fn line_column(&self, pos: usize) -> (usize, usize);

    /// Get source line for a position
    fn source_line(&self, pos: usize) -> Option<&str>;
}
