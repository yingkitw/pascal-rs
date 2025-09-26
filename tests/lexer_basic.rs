use minipas::lexer::{Lexer, Token};
use assert_matches::assert_matches;

#[test]
fn test_lexer_identifier() {
    let input = "x";
    let mut lexer = Lexer::new(input);
    assert_matches!(lexer.next_token(), Some(Ok((_, Token::Identifier(_), _))));
}
