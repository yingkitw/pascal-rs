//! Robust recursive descent Pascal parser
//! Supports full Pascal language with error recovery

mod decl;
mod expression;
mod statement;

use crate::lexer::Lexer;
use crate::tokens::Token;
use crate::ParseError;

/// Result type for parsing operations
pub type ParseResult<T> = Result<T, ParseError>;

/// Source location for error reporting
#[derive(Debug, Clone, Copy)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

/// Main Pascal parser with error recovery
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    source: &'a str,
    current_token: Option<Token>,
    current_location: SourceLocation,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    /// Create a new parser for the given source
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let (current_token, location) = Self::next_token_with_loc(&mut lexer, source);

        Parser {
            lexer,
            source,
            current_token,
            current_location: location,
            errors: Vec::new(),
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
    pub fn check(&self, token: Token) -> bool {
        self.peek() == Some(&token)
    }

    /// Consume token if it matches, otherwise error with location
    pub fn consume(&mut self, expected: Token) -> ParseResult<Token> {
        if self.check(expected.clone()) {
            let token = self.current_token.take().unwrap();
            self.advance();
            Ok(token)
        } else {
            let found = self
                .peek()
                .map(|t| format!("{:?}", t))
                .unwrap_or_else(|| "EOF".to_string());
            let loc = self.current_location;
            Err(ParseError::UnexpectedToken(format!(
                "at {}: expected {:?}, found {}",
                loc, expected, found
            )))
        }
    }

    /// Consume token or skip to synchronization point
    pub fn consume_or_skip(&mut self, expected: Token, sync_tokens: &[Token]) -> Token {
        match self.consume(expected.clone()) {
            Ok(token) => token,
            Err(e) => {
                self.errors.push(e);
                self.synchronize(sync_tokens);
                expected // Return expected token to continue parsing
            }
        }
    }

    /// Synchronize parser after error
    pub fn synchronize(&mut self, sync_tokens: &[Token]) {
        while let Some(token) = self.peek() {
            if sync_tokens.contains(token) {
                return;
            }
            if *token == Token::Dot {
                return; // End of statement
            }
            self.advance();
        }
    }

    /// Get all parsing errors
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    /// Check if parsing completed with errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_program() {
        let source = r#"
program Test;
var
  x: integer;
begin
  x := 42;
end.
"#;

        let mut parser = Parser::new(source);
        let result = parser.parse_program();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_with_expressions() {
        let source = r#"
program Calc;
var
  x, y, z: integer;
begin
  x := 10 + 20;
  y := x * 2;
  z := (x + y) div 3;
end.
"#;

        let mut parser = Parser::new(source);
        let result = parser.parse_program();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_if_while() {
        let source = r#"
program Control;
var
  x: integer;
begin
  if x > 0 then
    x := x - 1;

  while x > 0 do
    x := x - 1;
end.
"#;

        let mut parser = Parser::new(source);
        let result = parser.parse_program();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_for_loop() {
        let source = r#"
program LoopTest;
var
  i: integer;
begin
  for i := 1 to 10 do
    i := i + 1;
end.
"#;

        let mut parser = Parser::new(source);
        let result = parser.parse_program();
        assert!(result.is_ok());
    }
}
