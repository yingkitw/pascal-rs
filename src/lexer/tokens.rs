use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    #[token("program")]
    Program,
    #[token("procedure")]
    Procedure,
    #[token("function")]
    Function,
    #[token("var")]
    Var,
    #[token("const")]
    Const,
    #[token("type")]
    Type,
    #[token("begin")]
    Begin,
    #[token("end")]
    End,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("do")]
    Do,
    #[token("repeat")]
    Repeat,
    #[token("until")]
    Until,
    #[token("for")]
    For,
    #[token("to")]
    To,
    #[token("downto")]
    DownTo,
    #[token("integer")]
    Integer,
    #[token("real")]
    Real,
    #[token("boolean")]
    Boolean,
    #[token("char")]
    Char,
    #[token("string")]
    String,
    #[token("array")]
    Array,
    #[token("of")]
    Of,
    #[token("record")]
    Record,
    #[token("case")]
    Case,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    #[token("div")]
    Div,
    #[token("mod")]
    Mod,
    #[token("with")]
    With,
    #[token("exit")]
    Exit,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("nil")]
    Nil,
    
    // Identifiers and literals
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| {
        let slice = lex.slice();
        slice.to_string()
    })]
    Identifier(String),
    
    #[regex(r"\d+\.?\d*([Ee][+-]?\d+)?", |lex| {
        let slice = lex.slice();
        slice.to_string()
    })]
    Number(String),
    
    #[regex(r#""[^"]*""#, |lex| {
        let slice = lex.slice();
        slice[1..slice.len()-1].to_string()
    })]
    StringLiteral(String),
    // Operators
    #[token(":=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("..")]
    Range,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("=")]
    Equal,
    #[token("<>")]
    NotEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessOrEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterOrEqual,
    
    // Delimiters
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("^")]
    Caret,
    #[token("@")]
    At,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("xor")]
    Xor,
    
    // Comments (to be skipped)
    #[regex(r"\{[^}]*\}", |_| ())]
    #[regex(r"//[^\n]*", |_| ())]
    Comment,
    
    // Whitespace (to be skipped)
    #[regex(r"[ \t\n\r]+", |_| ())]
    Whitespace,
    
    // Error token for invalid input
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_lex_keywords() {
        let mut lex = Token::lexer("program begin end if then else while do");
        // Skip whitespace tokens and collect only the actual tokens
        let tokens: Vec<_> = lex.filter(|t| !matches!(t, Ok(Token::Whitespace))).collect();
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0], Ok(Token::Program));
        assert_eq!(tokens[1], Ok(Token::Begin));
        assert_eq!(tokens[2], Ok(Token::End));
        assert_eq!(tokens[3], Ok(Token::If));
        assert_eq!(tokens[4], Ok(Token::Then));
        assert_eq!(tokens[5], Ok(Token::Else));
        assert_eq!(tokens[6], Ok(Token::While));
        assert_eq!(tokens[7], Ok(Token::Do));
    }
    
    #[test]
    fn test_lex_identifiers() {
        let mut lex = Token::lexer("x y1 _test variable_name");
        // Skip whitespace tokens and collect only the actual tokens
        let tokens: Vec<_> = lex.filter(|t| !matches!(t, Ok(Token::Whitespace))).collect();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], Ok(Token::Identifier("x".to_string())));
        assert_eq!(tokens[1], Ok(Token::Identifier("y1".to_string())));
        assert_eq!(tokens[2], Ok(Token::Identifier("_test".to_string())));
        assert_eq!(tokens[3], Ok(Token::Identifier("variable_name".to_string())));
    }
}
