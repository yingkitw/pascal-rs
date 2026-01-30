//! Basic lexer implementation

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Integer(i64),
    String(String),
    Semicolon,
    Colon,
    Comma,
    Equals,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Begin,
    End,
    Program,
    Writeln,
    Error,
}

pub struct Lexer {
    source: String,
    position: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
            source: source.to_string(),
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        // TODO: Implement actual tokenization
        if self.position >= self.source.len() {
            None
        } else {
            self.position += 1;
            Some(Token::Error) // Placeholder
        }
    }
}
