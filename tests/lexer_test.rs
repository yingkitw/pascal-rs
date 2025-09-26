use minipas::lexer::{Lexer, Token};

#[test]
fn test_lexer() {
    let input = "x";
    let mut lexer = Lexer::new(input);
    assert_matches::assert_matches!(lexer.next_token(), Some(Ok((_, Token::Identifier(_), _))));
}
