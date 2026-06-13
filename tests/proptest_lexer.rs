//! Property-based tests for the lexer.

use pascal::lexer::Lexer;
use pascal::tokens::Token;
use proptest::prelude::*;

proptest! {
    #[test]
    fn lexer_never_panics_on_any_utf8(s in "\\PC{0,200}") {
        let mut lexer = Lexer::new(&s);
        while let Some(result) = lexer.next_token() {
            let _ = result;
        }
    }

    #[test]
    fn keywords_tokenize_without_panic(kw in prop::sample::select(vec![
        "program", "begin", "end", "var", "if", "then", "else", "while", "for"
    ])) {
        let mut lexer = Lexer::new(kw);
        let token = lexer.next_token().unwrap().unwrap();
        prop_assert!(!matches!(token.1, Token::Whitespace | Token::LineComment));
    }
}
