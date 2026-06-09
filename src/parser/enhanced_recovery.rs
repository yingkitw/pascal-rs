//! Enhanced parser error recovery for Pascal compiler
//! 
//! Provides sophisticated error recovery mechanisms including:
//! - Context-aware synchronization points
//! - Error classification and recovery strategies
//! - Comprehensive error reporting with suggestions
//! - Parse state management for better recovery

use crate::lexer::Lexer;
use crate::tokens::Token;
use crate::enhanced_error::{SourceLocation, Diagnostic, ErrorSuggestion, DiagnosticSeverity};

/// Recovery strategy for parsing errors
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecoveryStrategy {
    /// Skip tokens until a synchronization point is found
    Synchronize,
    /// Insert a missing token and continue
    InsertMissing,
    /// Replace incorrect token with expected one
    ReplaceIncorrect,
    /// Abort parsing for this construct
    Abort,
}

/// Error classification for better recovery decisions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorType {
    /// Missing tokens (semicolons, end, etc.)
    MissingToken,
    /// Unexpected tokens in context
    UnexpectedToken,
    /// Syntax errors in expressions
    ExpressionError,
    /// Declaration errors
    DeclarationError,
    /// Statement errors
    StatementError,
    /// Type errors
    TypeError,
}

/// Enhanced parse result with error information
pub type EnhancedParseResult<T> = Result<T, ParseErrorInfo>;

/// Parse error information with enhanced details
#[derive(Debug, Clone)]
pub struct ParseErrorInfo {
    pub error_type: ErrorType,
    pub location: SourceLocation,
    pub message: String,
    pub expected_tokens: Vec<Token>,
    pub found_token: Option<Token>,
    pub suggestions: Vec<ErrorSuggestion>,
    pub recovery_strategy: RecoveryStrategy,
}

impl ParseErrorInfo {
    /// Create a new parse error
    pub fn new(error_type: ErrorType, location: SourceLocation, message: String) -> Self {
        Self {
            error_type,
            location,
            message,
            expected_tokens: Vec::new(),
            found_token: None,
            suggestions: Vec::new(),
            recovery_strategy: RecoveryStrategy::Synchronize,
        }
    }

    /// Add expected tokens
    pub fn with_expected(mut self, tokens: Vec<Token>) -> Self {
        self.expected_tokens = tokens;
        self
    }

    /// Add found token
    pub fn with_found(mut self, token: Option<Token>) -> Self {
        self.found_token = token;
        self
    }

    /// Add suggestions
    pub fn with_suggestions(mut self, suggestions: Vec<ErrorSuggestion>) -> Self {
        self.suggestions = suggestions;
        self
    }

    /// Set recovery strategy
    pub fn with_recovery_strategy(mut self, strategy: RecoveryStrategy) -> Self {
        self.recovery_strategy = strategy;
        self
    }

    /// Convert to diagnostic for reporting
    pub fn to_diagnostic(&self, source_manager: &crate::enhanced_error::SourceManager) -> Diagnostic {
        let severity = DiagnosticSeverity::Error;
        let mut diagnostic = Diagnostic::error(self.location, self.message);

        // Add suggestions
        diagnostic = diagnostic.with_suggestions(self.suggestions.clone());

        // Add error code
        let error_code = match self.error_type {
            ErrorType::MissingToken => "E001",
            ErrorType::UnexpectedToken => "E002",
            ErrorType::ExpressionError => "E003",
            ErrorType::DeclarationError => "E004",
            ErrorType::StatementError => "E005",
            ErrorType::TypeError => "E006",
        };
        diagnostic = diagnostic.with_code(error_code.to_string());

        diagnostic
    }
}

/// Enhanced parser with sophisticated error recovery
pub struct EnhancedParser<'a> {
    lexer: Lexer<'a>,
    source: &'a str,
    current_token: Option<Token>,
    current_location: SourceLocation,
    errors: Vec<ParseErrorInfo>,
    recovery_state: RecoveryState,
}

/// Parser state for managing recovery
#[derive(Debug, Clone)]
pub struct RecoveryState {
    pub recovery_depth: usize,
    pub last_error_position: SourceLocation,
    pub sync_points: Vec<SyncPoint>,
    pub in_recovery_mode: bool,
    pub recovery_count: usize,
}

/// Synchronization point for error recovery
#[derive(Debug, Clone)]
pub struct SyncPoint {
    pub tokens: Vec<Token>,
    pub description: String,
    pub priority: SyncPriority,
}

/// Priority level for synchronization points
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SyncPriority {
    High,    // Critical statements (end, ;, .)
    Medium,  // Block boundaries (begin, do, then)
    Low,     // Expression boundaries (,), [, etc.)
}

impl<'a> EnhancedParser<'a> {
    /// Create a new enhanced parser
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let (current_token, location) = Self::next_token_with_loc(&mut lexer, source);

        Self {
            lexer,
            source,
            current_token,
            current_location: location,
            errors: Vec::new(),
            recovery_state: RecoveryState::new(),
        }
    }

    /// Create recovery state
    pub fn new() -> RecoveryState {
        RecoveryState {
            recovery_depth: 0,
            last_error_position: SourceLocation::new(0, 1, 1, 0, 0),
            sync_points: Vec::new(),
            in_recovery_mode: false,
            recovery_count: 0,
        }
    }

    /// Internal token advance with location tracking
    fn next_token_with_loc(lexer: &mut Lexer, source: &str) -> (Option<Token>, SourceLocation) {
        match lexer.next() {
            Some(Ok((start, token, _end))) => {
                let loc = Self::offset_to_location(source, start);
                (Some(token), loc)
            }
            _ => (
                None,
                SourceLocation {
                    line: 1,
                    column: 1,
                    offset: 0,
                },
            ),
        }
    }

    /// Convert byte offset to line/column
    fn offset_to_location(source: &str, offset: usize) -> SourceLocation {
        let mut line = 1;
        let mut col = 1;
        for (i, ch) in source.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        SourceLocation {
            line,
            column: col,
            offset,
        }
    }

    /// Get current source location
    pub fn location(&self) -> SourceLocation {
        self.current_location
    }

    /// Advance to next token
    pub fn advance(&mut self) {
        let (token, loc) = Self::next_token_with_loc(&mut self.lexer, self.source);
        self.current_token = token;
        self.current_location = loc;
    }

    /// Peek at current token
    pub fn peek(&self) -> Option<&Token> {
        self.current_token.as_ref()
    }

    /// Check if current token matches expected
    pub fn check(&self, token: &Token) -> bool {
        self.peek() == Some(token)
    }

    /// Consume token if it matches, otherwise error with location
    pub fn consume(&mut self, expected: Token) -> EnhancedParseResult<Token> {
        if self.check(&expected) {
            let token = self.current_token.take().unwrap();
            self.advance();
            Ok(token)
        } else {
            let found = self.peek().cloned();
            self.report_error(
                ErrorType::UnexpectedToken,
                format!("Expected {:?}, found {:?}", expected, found),
                Some(expected),
                found,
            )
        }
    }

    /// Consume token with error recovery
    pub fn consume_with_recovery(
        &mut self,
        expected: Token,
        sync_points: &[SyncPoint],
    ) -> EnhancedParseResult<Token> {
        match self.consume(expected.clone()) {
            Ok(token) => Ok(token),
            Err(e) => {
                self.handle_error(e, sync_points);
                Ok(expected) // Return expected token to continue parsing
            }
        }
    }

    /// Report a parsing error with recovery
    fn report_error(
        &mut self,
        error_type: ErrorType,
        message: String,
        expected: Option<Token>,
        found: Option<Token>,
    ) -> EnhancedParseResult<Token> {
        let error = ParseErrorInfo::new(error_type, self.current_location, message)
            .with_found(found)
            .with_recovery_strategy(self.determine_recovery_strategy(error_type));

        // Add suggestions based on error type
        let suggestions = self.generate_suggestions(error_type, &expected, &found);
        if !suggestions.is_empty() {
            error.with_suggestions(suggestions);
        }

        self.errors.push(error);
        Err(self.errors.last().unwrap().clone())
    }

    /// Determine recovery strategy based on error type
    fn determine_recovery_strategy(&mut self, error_type: ErrorType) -> RecoveryStrategy {
        // Enter recovery mode if we're recovering too much
        if self.recovery_state.recovery_count > 5 {
            return RecoveryStrategy::Abort;
        }

        match error_type {
            ErrorType::MissingToken => RecoveryStrategy::InsertMissing,
            ErrorType::UnexpectedToken => RecoveryStrategy::Synchronize,
            ErrorType::ExpressionError => RecoveryStrategy::Synchronize,
            ErrorType::DeclarationError => RecoveryStrategy::Synchronize,
            ErrorType::StatementError => RecoveryStrategy::Synchronize,
            ErrorType::TypeError => RecoveryStrategy::Synchronize,
        }
    }

    /// Generate suggestions for error recovery
    fn generate_suggestions(
        &self,
        error_type: ErrorType,
        expected: &Option<Token>,
        found: &Option<Token>,
    ) -> Vec<ErrorSuggestion> {
        let mut suggestions = Vec::new();

        match error_type {
            ErrorType::MissingToken => {
                if let Some(expected_token) = expected {
                    let suggestion = match expected_token {
                        Token::Semicolon => {
                            ErrorSuggestion::with_replacement(
                                "Missing semicolon".to_string(),
                                ";".to_string(),
                            )
                        },
                        Token::Dot => {
                            ErrorSuggestion::with_replacement(
                                "Missing period".to_string(),
                                ".".to_string(),
                            )
                        },
                        Token::End => {
                            ErrorSuggestion::with_replacement(
                                "Missing 'end' keyword".to_string(),
                                "end".to_string(),
                            )
                        },
                        _ => ErrorSuggestion::new(format!("Consider adding {:?}", expected_token)),
                    };
                    suggestions.push(suggestion);
                }
            },
            ErrorType::UnexpectedToken => {
                if let Some(found_token) = found {
                    let suggestion = match found_token {
                        Token::Identifier(_) => {
                            ErrorSuggestion::new("Unexpected identifier".to_string())
                        },
                        Token::Number(_) => {
                            ErrorSuggestion::new("Unexpected number".to_string())
                        },
                        Token::StringLiteral(_) => {
                            ErrorSuggestion::new("Unexpected string".to_string())
                        },
                        _ => ErrorSuggestion::new(format!("Unexpected token {:?}", found_token)),
                    };
                    suggestions.push(suggestion);
                }
            },
            _ => {}
        }

        suggestions
    }

    /// Handle parsing error with recovery
    fn handle_error(&mut self, error: ParseErrorInfo, sync_points: &[SyncPoint]) {
        self.recovery_state.recovery_count += 1;
        
        // Enter recovery mode
        self.recovery_state.in_recovery_mode = true;
        self.recovery_state.recovery_depth += 1;
        
        // Apply recovery strategy
        match error.recovery_strategy {
            RecoveryStrategy::Synchronize => {
                self.synchronize(sync_points);
            },
            RecoveryStrategy::InsertMissing => {
                self.insert_missing_token(&error);
            },
            RecoveryStrategy::ReplaceIncorrect => {
                self.replace_incorrect_token(&error);
            },
            RecoveryStrategy::Abort => {
                self.abort_recovery();
            },
        }
        
        // Exit recovery mode
        self.recovery_state.in_recovery_mode = false;
        self.recovery_state.recovery_depth = self.recovery_state.recovery_depth.saturating_sub(1);
    }

    /// Synchronize parser after error
    pub fn synchronize(&mut self, sync_points: &[SyncPoint]) {
        let mut best_sync_point = None;
        let mut best_priority = SyncPriority::Low;
        
        // Find the best synchronization point
        for sync_point in sync_points {
            if sync_point.tokens.contains(self.peek().unwrap_or(&Token::Eof)) {
                if sync_point.priority > best_priority {
                    best_priority = sync_point.priority;
                    best_sync_point = Some(sync_point);
                }
            }
        }
        
        if let Some(sync_point) = best_sync_point {
            eprintln!("[parser] Synchronizing at: {}", sync_point.description);
        }
        
        // Advance until we find a synchronization point
        while let Some(token) = self.peek() {
            if self.is_sync_token(token, sync_points) {
                break;
            }
            self.advance();
        }
    }

    /// Check if token is a synchronization point
    fn is_sync_token(&self, token: &Token, sync_points: &[SyncPoint]) -> bool {
        for sync_point in sync_points {
            if sync_point.tokens.contains(token) {
                return true;
            }
        }
        false
    }

    /// Insert missing token
    fn insert_missing_token(&mut self, error: &ParseErrorInfo) {
        eprintln!("[parser] Inserting missing token at {}", error.location.format());
        // In a real implementation, this would insert the token into the token stream
        self.advance();
    }

    /// Replace incorrect token
    fn replace_incorrect_token(&mut self, error: &ParseErrorInfo) {
        eprintln!("[parser] Replacing incorrect token at {}", error.location.format());
        // In a real implementation, this would replace the token in the token stream
        self.advance();
    }

    /// Abort recovery for this construct
    fn abort_recovery(&mut self) {
        eprintln!("[parser] Aborting recovery due to excessive errors");
        self.recovery_state.recovery_count = 0;
    }

    /// Get all parsing errors
    pub fn errors(&self) -> &[ParseErrorInfo] {
        &self.errors
    }

    /// Check if parsing completed with errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get error count
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    /// Create default synchronization points
    pub fn create_default_sync_points() -> Vec<SyncPoint> {
        vec![
            SyncPoint {
                tokens: vec![Token::Semicolon],
                description: "Statement end".to_string(),
                priority: SyncPriority::High,
            },
            SyncPoint {
                tokens: vec![Token::End],
                description: "Block end".to_string(),
                priority: SyncPriority::High,
            },
            SyncPoint {
                tokens: vec![Token::Dot],
                description: "Program end".to_string(),
                priority: SyncPriority::High,
            },
            SyncPoint {
                tokens: vec![Token::Then, Token::Do, Token::Else],
                description: "Control flow".to_string(),
                priority: SyncPriority::Medium,
            },
            SyncPoint {
                tokens: vec![Token::Begin, Token::Program, Token::Var, Token::Type],
                description: "Declaration start".to_string(),
                priority: SyncPriority::Medium,
            },
            SyncPoint {
                tokens: vec![Token::LParen, Token::RParen, Token::LBracket, Token::RBracket],
                description: "Expression boundaries".to_string(),
                priority: SyncPriority::Low,
            },
        ]
    }

    /// Reset parser state
    pub fn reset(&mut self) {
        self.errors.clear();
        self.recovery_state = RecoveryState::new();
    }
}

/// Helper functions for common parsing operations
pub mod helpers {
    use super::*;

    /// Create error for missing semicolon
    pub fn missing_semicolon(location: SourceLocation) -> ParseErrorInfo {
        ParseErrorInfo::new(
            ErrorType::MissingToken,
            location,
            "Missing semicolon".to_string(),
        )
        .with_expected(vec![Token::Semicolon])
        .with_recovery_strategy(RecoveryStrategy::InsertMissing)
    }

    /// Create error for unexpected token
    pub fn unexpected_token(location: SourceLocation, found: Token) -> ParseErrorInfo {
        ParseErrorInfo::new(
            ErrorType::UnexpectedToken,
            location,
            format!("Unexpected token: {:?}", found),
        )
        .with_found(Some(found))
        .with_recovery_strategy(RecoveryStrategy::Synchronize)
    }

    /// Create error for mismatched parentheses
    pub fn mismatched_parentheses(location: SourceLocation, expected: Token, found: Token) -> ParseErrorInfo {
        ParseErrorInfo::new(
            ErrorType::ExpressionError,
            location,
            format!("Mismatched parentheses: expected {:?}, found {:?}", expected, found),
        )
        .with_expected(vec![expected])
        .with_found(Some(found))
        .with_recovery_strategy(RecoveryStrategy::ReplaceIncorrect)
    }

    /// Create error for incomplete declaration
    pub fn incomplete_declaration(location: SourceLocation, missing: &str) -> ParseErrorInfo {
        ParseErrorInfo::new(
            ErrorType::DeclarationError,
            location,
            format!("Incomplete declaration: missing {}", missing),
        )
        .with_recovery_strategy(RecoveryStrategy::Synchronize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_creation() {
        let source = "program Test; begin end.";
        let parser = EnhancedParser::new(source);
        assert!(!parser.has_errors());
    }

    #[test]
    fn test_error_creation() {
        let location = SourceLocation::new(0, 1, 1, 0, 0);
        let error = ParseErrorInfo::new(ErrorType::MissingToken, location, "Test error".to_string());
        
        assert_eq!(error.error_type, ErrorType::MissingToken);
        assert_eq!(error.location, location);
        assert_eq!(error.message, "Test error");
    }

    #[test]
    fn test_sync_points() {
        let sync_points = EnhancedParser::create_default_sync_points();
        
        assert!(sync_points.len() > 0);
        assert!(sync_points.iter().any(|sp| sp.description == "Statement end"));
    }

    #[test]
    fn test_recovery_strategy() {
        let location = SourceLocation::new(0, 1, 1, 0, 0);
        let error = helpers::missing_semicolon(location);
        
        assert_eq!(error.recovery_strategy, RecoveryStrategy::InsertMissing);
    }

    #[test]
    fn test_suggestions() {
        let location = SourceLocation::new(0, 1, 1, 0, 0);
        let error = helpers::missing_semicolon(location);
        
        let suggestions = error.suggestions;
        assert!(!suggestions.is_empty());
        assert!(suggestions.iter().any(|s| s.message.contains("semicolon")));
    }
}