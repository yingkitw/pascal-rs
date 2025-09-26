use super::*;

#[test]
fn minimal_test() {
    let input = "program";
    let mut lexer = Lexer::new(input);
    assert!(lexer.expect(Token::Program).is_ok());
}
