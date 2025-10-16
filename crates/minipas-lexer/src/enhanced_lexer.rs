use crate::enhanced_tokens::EnhancedToken;
use crate::traits::*;
use logos::Logos;
use std::collections::HashMap;

/// Enhanced lexer based on Free Pascal Compiler token definitions
/// Provides comprehensive Pascal language support
pub struct EnhancedLexer<'a> {
    lexer: logos::Lexer<'a, EnhancedToken>,
    current_token: Option<(usize, EnhancedToken, usize)>,
    position: usize,
    line: usize,
    column: usize,
    errors: Vec<String>,
}

impl<'a> EnhancedLexer<'a> {
    /// Creates a new enhanced lexer
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: EnhancedToken::lexer(input),
            current_token: None,
            position: 0,
            line: 1,
            column: 1,
            errors: Vec::new(),
        }
    }
    
    /// Gets the next token from the input
    pub fn next_token(&mut self) -> Option<Result<(usize, EnhancedToken, usize), String>> {
        if let Some(token) = self.lexer.next() {
            let token_start = self.lexer.span().start;
            let token_end = self.lexer.span().end;
            
            // Update position tracking
            self.position = token_end;
            
            // Update line and column tracking
            for ch in self.lexer.source()[self.position..token_end].chars() {
                if ch == '\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
            }
            
            match token {
                EnhancedToken::Error => {
                    let error_msg = format!("Invalid token at line {}, column {}", self.line, self.column);
                    self.errors.push(error_msg.clone());
                    Some(Err(error_msg))
                }
                token => {
                    self.current_token = Some((token_start, token.clone(), token_end));
                    Some(Ok((token_start, token, token_end)))
                }
            }
        } else {
            None
        }
    }
    
    /// Peeks at the next token without consuming it
    pub fn peek_token(&self) -> Option<&(usize, EnhancedToken, usize)> {
        self.current_token.as_ref()
    }
    
    /// Gets the current source position
    pub fn position(&self) -> usize {
        self.position
    }
    
    /// Gets the current line number
    pub fn line(&self) -> usize {
        self.line
    }
    
    /// Gets the current column number
    pub fn column(&self) -> usize {
        self.column
    }
    
    /// Gets all errors encountered during lexing
    pub fn errors(&self) -> &[String] {
        &self.errors
    }
    
    /// Clears all errors
    pub fn clear_errors(&mut self) {
        self.errors.clear();
    }
    
    /// Resets the lexer to the beginning
    pub fn reset(&mut self) {
        self.lexer = EnhancedToken::lexer(self.lexer.source());
        self.current_token = None;
        self.position = 0;
        self.line = 1;
        self.column = 1;
        self.errors.clear();
    }
    
    /// Gets the source text for a token span
    pub fn token_text(&self, start: usize, end: usize) -> &'a str {
        &self.lexer.source()[start..end]
    }
    
    /// Checks if a token is a keyword
    pub fn is_keyword(&self, token: &EnhancedToken) -> bool {
        token.is_keyword()
    }
    
    /// Checks if a token is an operator
    pub fn is_operator(&self, token: &EnhancedToken) -> bool {
        token.is_operator()
    }
    
    /// Checks if a token is a literal
    pub fn is_literal(&self, token: &EnhancedToken) -> bool {
        token.is_literal()
    }
    
    /// Gets operator precedence
    pub fn operator_precedence(&self, token: &EnhancedToken) -> Option<u8> {
        token.operator_precedence()
    }
    
    /// Checks if an operator is left-associative
    pub fn is_left_associative(&self, token: &EnhancedToken) -> bool {
        token.is_left_associative()
    }
}

impl<'a> Iterator for EnhancedLexer<'a> {
    type Item = Result<(usize, EnhancedToken, usize), String>;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl<'a> TokenValidator for EnhancedLexer<'a> {
    fn validate_token(&self, token: &EnhancedToken) -> bool {
        match token {
            EnhancedToken::Error => false,
            _ => true,
        }
    }
    
    fn is_valid_identifier(&self, token: &EnhancedToken) -> bool {
        matches!(token, EnhancedToken::Identifier(_))
    }
    
    fn is_valid_literal(&self, token: &EnhancedToken) -> bool {
        token.is_literal()
    }
}

impl<'a> ErrorReporter for EnhancedLexer<'a> {
    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    fn get_errors(&self) -> Vec<String> {
        self.errors.clone()
    }
    
    fn clear_errors(&mut self) {
        self.errors.clear();
    }
}

impl<'a> TokenStream for EnhancedLexer<'a> {
    type Token = EnhancedToken;
    
    fn next_token(&mut self) -> Option<Result<(usize, Self::Token, usize), String>> {
        self.next_token()
    }
    
    fn peek_token(&self) -> Option<&(usize, Self::Token, usize)> {
        self.peek_token()
    }
    
    fn position(&self) -> usize {
        self.position()
    }
}

/// Enhanced lexer capability trait
pub trait EnhancedLexerCapability {
    /// Creates a new enhanced lexer
    fn new(input: &str) -> Self where Self: Sized;
    
    /// Gets the next token
    fn next_token(&mut self) -> Option<Result<(usize, EnhancedToken, usize), String>>;
    
    /// Peeks at the next token
    fn peek_token(&self) -> Option<&(usize, EnhancedToken, usize)>;
    
    /// Gets the current position
    fn position(&self) -> usize;
    
    /// Gets the current line
    fn line(&self) -> usize;
    
    /// Gets the current column
    fn column(&self) -> usize;
    
    /// Gets all errors
    fn errors(&self) -> &[String];
    
    /// Clears all errors
    fn clear_errors(&mut self);
    
    /// Resets the lexer
    fn reset(&mut self);
    
    /// Gets token text
    fn token_text(&self, start: usize, end: usize) -> &str;
    
    /// Checks if token is keyword
    fn is_keyword(&self, token: &EnhancedToken) -> bool;
    
    /// Checks if token is operator
    fn is_operator(&self, token: &EnhancedToken) -> bool;
    
    /// Checks if token is literal
    fn is_literal(&self, token: &EnhancedToken) -> bool;
    
    /// Gets operator precedence
    fn operator_precedence(&self, token: &EnhancedToken) -> Option<u8>;
    
    /// Checks if operator is left-associative
    fn is_left_associative(&self, token: &EnhancedToken) -> bool;
}

impl<'a> EnhancedLexerCapability for EnhancedLexer<'a> {
    fn new(input: &str) -> Self {
        Self::new(input)
    }
    
    fn next_token(&mut self) -> Option<Result<(usize, EnhancedToken, usize), String>> {
        if let Some(token) = self.lexer.next() {
            let token_start = self.lexer.span().start;
            let token_end = self.lexer.span().end;
            
            // Update position tracking
            self.position = token_end;
            
            // Update line and column tracking
            for ch in self.lexer.source()[self.position..token_end].chars() {
                if ch == '\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
            }
            
            match token {
                EnhancedToken::Error => {
                    let error_msg = format!("Invalid token at position {}", token_start);
                    self.errors.push(error_msg.clone());
                    return Some(Err(error_msg));
                }
                _ => {
                    let token_info = (token_start, token, token_end);
                    self.current_token = Some(token_info.clone());
                    return Some(Ok(token_info));
                }
            }
        }
        None
    }
    
    fn peek_token(&self) -> Option<&(usize, EnhancedToken, usize)> {
        self.current_token.as_ref()
    }
    
    fn position(&self) -> usize {
        self.position
    }
    
    fn line(&self) -> usize {
        self.line
    }
    
    fn column(&self) -> usize {
        self.column
    }
    
    fn errors(&self) -> &[String] {
        &self.errors
    }
    
    fn clear_errors(&mut self) {
        self.errors.clear()
    }
    
    fn reset(&mut self) {
        self.lexer = EnhancedToken::lexer(self.lexer.source());
        self.current_token = None;
        self.position = 0;
        self.line = 1;
        self.column = 1;
        self.errors.clear();
    }
    
    fn token_text(&self, start: usize, end: usize) -> &str {
        &self.lexer.source()[start..end]
    }
    
    fn is_keyword(&self, token: &EnhancedToken) -> bool {
        token.is_keyword()
    }
    
    fn is_operator(&self, token: &EnhancedToken) -> bool {
        token.is_operator()
    }
    
    fn is_literal(&self, token: &EnhancedToken) -> bool {
        token.is_literal()
    }
    
    fn operator_precedence(&self, token: &EnhancedToken) -> Option<u8> {
        token.operator_precedence()
    }
    
    fn is_left_associative(&self, token: &EnhancedToken) -> bool {
        token.is_left_associative()
    }
}
