//! Comprehensive lexer tests for pascal-rs compiler
//! Tests tokenization of all Pascal language constructs

use pascal::Lexer;
use pascal::Token;

#[test]
fn test_keywords() {
    let source = r#"program begin end if then else while do for to downto case of repeat until function procedure var const type array record set pointer object class interface implementation unit uses string goto label and or not xor div mod shr shl in is"#;
    let mut lexer = Lexer::new(source);

    let expected_tokens = vec![
        Token::ProgramKeyword,
        Token::Begin,
        Token::End,
        Token::If,
        Token::Then,
        Token::Else,
        Token::While,
        Token::Do,
        Token::For,
        Token::To,
        Token::DownTo,
        Token::Case,
        Token::Of,
        Token::Repeat,
        Token::Until,
        Token::Function,
        Token::Procedure,
        Token::Var,
        Token::Const,
        Token::Type,
        Token::Array,
        Token::Record,
        Token::Set,
        Token::Pointer,
        Token::Object,
        Token::Class,
        Token::Interface,
        Token::Implementation,
        Token::Unit,
        Token::Uses,
        Token::StringKeyword,
        Token::Goto,
        Token::Label,
        Token::And,
        Token::Or,
        Token::Not,
        Token::Xor,
        Token::Div,
        Token::Mod,
        Token::Shr,
        Token::Shl,
        Token::In,
        Token::Is,
    ];

    for expected in expected_tokens {
        let result = lexer.next_token();
        assert!(result.is_some(), "Expected token {:?}, got None", expected);
        let (_, token, _) = result.unwrap().unwrap();
        assert_eq!(token, expected, "Token mismatch");
    }
}

#[test]
fn test_identifiers() {
    let source = r#"myVariable MyVariable _variable variable123"#;
    let mut lexer = Lexer::new(source);

    let identifiers = vec!["myVariable", "MyVariable", "_variable", "variable123"];

    for expected in identifiers {
        let result = lexer.next_token();
        assert!(result.is_some(), "Expected identifier {}, got None", expected);
        let (_, token, _) = result.unwrap().unwrap();
        match token {
            Token::Identifier(s) => assert_eq!(s, expected),
            _ => panic!("Expected Identifier, got {:?}", token),
        }
    }
}

#[test]
fn test_integer_literals() {
    let source = r#"0 1 42 123 65535 2147483647"#;
    let mut lexer = Lexer::new(source);

    let expected_values = vec![0, 1, 42, 123, 65535, 2147483647i64];

    for expected in expected_values {
        let result = lexer.next_token();
        assert!(result.is_some(), "Expected integer, got None");
        let (_, token, _) = result.unwrap().unwrap();
        match token {
            Token::IntegerLiteral(n) => assert_eq!(n, expected),
            _ => panic!("Expected IntegerLiteral, got {:?}", token),
        }
    }
}

#[test]
fn test_hex_literals() {
    let source = r#"#$00 $FF $AB $1234 $DEADBEEF"#;
    let mut lexer = Lexer::new(source);

    let expected_values = vec![0x00, 0xFF, 0xAB, 0x1234, 0xDEADBEEFi64];

    for expected in expected_values {
        let result = lexer.next_token();
        assert!(result.is_some(), "Expected hex integer, got None");
        let (_, token, _) = result.unwrap().unwrap();
        match token {
            Token::IntegerLiteral(n) => assert_eq!(n, expected),
            _ => panic!("Expected IntegerLiteral, got {:?}", token),
        }
    }
}

#[test]
fn test_real_literals() {
    let source = r#"0.0 1.5 3.14159 1.0e10 1.5e-10"#;
    let mut lexer = Lexer::new(source);

    let expected_values = vec![0.0, 1.5, 3.14159, 1.0e10, 1.5e-10];

    for expected in expected_values {
        let result = lexer.next_token();
        assert!(result.is_some(), "Expected real number, got None");
        let (_, token, _) = result.unwrap().unwrap();
        match token {
            Token::RealLiteral(n) => {
                let diff = (n - expected).abs();
                assert!(diff < 1e-9, "Expected {}, got {}", expected, n);
            }
            _ => panic!("Expected RealLiteral, got {:?}", token),
        }
    }
}

#[test]
fn test_string_literals() {
    let source = r#""Hello World" "Test" """"#;
    let mut lexer = Lexer::new(source);

    let expected_strings = vec!["Hello World", "Test", ""];

    for expected in expected_strings {
        let result = lexer.next_token();
        assert!(result.is_some(), "Expected string literal, got None");
        let (_, token, _) = result.unwrap().unwrap();
        match token {
            Token::StringLiteral(s) => assert_eq!(s, expected),
            _ => panic!("Expected StringLiteral, got {:?}", token),
        }
    }
}

#[test]
fn test_string_escape_sequences() {
    let source = r#""Line1\nLine2" "Tab\there" "Quote\"Test""#;
    let mut lexer = Lexer::new(source);

    // Test that escape sequences are handled
    let result = lexer.next_token();
    assert!(result.is_some());
    let (_, token, _) = result.unwrap().unwrap();
    match token {
        Token::StringLiteral(s) => {
            assert!(s.contains('\n'), "Expected newline escape sequence");
        }
        _ => panic!("Expected StringLiteral with escape sequences"),
    }
}

#[test]
fn test_character_literals() {
    let source = r#"#'A' #'z' #'0' #9 #10"#;
    let mut lexer = Lexer::new(source);

    let expected_values = vec!['A', 'z', '0', '\t', '\n'];

    for expected in expected_values {
        let result = lexer.next_token();
        assert!(result.is_some(), "Expected character literal, got None");
        let (_, token, _) = result.unwrap().unwrap();
        match token {
            Token::CharacterLiteral(c) => assert_eq!(c, expected),
            _ => panic!("Expected CharacterLiteral, got {:?}", token),
        }
    }
}

#[test]
fn test_operators() {
    let source = r"+ - * / = < > <= >= <> := .. ^ @ && ||"#;
    let mut lexer = Lexer::new(source);

    let expected_tokens = vec![
        Token::Plus,
        Token::Minus,
        Token::Asterisk,
        Token::Slash,
        Token::Equal,
        Token::LessThan,
        Token::GreaterThan,
        Token::LessThanOrEqual,
        Token::GreaterThanOrEqual,
        Token::NotEqual,
        Token::Assign,
        Token::DotDot,
        Token::Caret,
        Token::At,
        Token::And,
        Token::Or,
    ];

    for expected in expected_tokens {
        let result = lexer.next_token();
        assert!(result.is_some(), "Expected operator {:?}, got None", expected);
        let (_, token, _) = result.unwrap().unwrap();
        assert_eq!(token, expected, "Operator mismatch");
    }
}

#[test]
fn test_delimiters() {
    let source = r#"( ) [ ] . , ; :"#;
    let mut lexer = Lexer::new(source);

    let expected_tokens = vec![
        Token::LeftParen,
        Token::RightParen,
        Token::LeftBracket,
        Token::RightBracket,
        Token::Dot,
        Token::Comma,
        Token::Semicolon,
        Token::Colon,
    ];

    for expected in expected_tokens {
        let result = lexer.next_token();
        assert!(result.is_some(), "Expected delimiter {:?}, got None", expected);
        let (_, token, _) = result.unwrap().unwrap();
        assert_eq!(token, expected, "Delimiter mismatch");
    }
}

#[test]
fn test_comments_block() {
    let source = r#"{ This is a block comment } program"#;
    let mut lexer = Lexer::new(source);

    let result = lexer.next_token();
    assert!(result.is_some(), "Expected token after comment");
    let (_, token, _) = result.unwrap().unwrap();
    assert_eq!(token, Token::ProgramKeyword, "Should skip block comment");
}

#[test]
fn test_comments_line() {
    let source = r#"// This is a line comment
program"#;
    let mut lexer = Lexer::new(source);

    let result = lexer.next_token();
    assert!(result.is_some(), "Expected token after line comment");
    let (_, token, _) = result.unwrap().unwrap();
    assert_eq!(token, Token::ProgramKeyword, "Should skip line comment");
}

#[test]
fn test_whitespace_handling() {
    let source = r#"program     myProgram
    var"#;
    let mut lexer = Lexer::new(source);

    // Should skip all whitespace
    let result = lexer.next_token();
    assert!(result.is_some());
    let (_, token, _) = result.unwrap().unwrap();
    assert_eq!(token, Token::ProgramKeyword);

    let result = lexer.next_token();
    assert!(result.is_some());
    let (_, token, _) = result.unwrap().unwrap();
    match token {
        Token::Identifier(s) => assert_eq!(s, "myProgram"),
        _ => panic!("Expected identifier"),
    }

    let result = lexer.next_token();
    assert!(result.is_some());
    let (_, token, _) = result.unwrap().unwrap();
    assert_eq!(token, Token::Var);
}

#[test]
fn test_position_tracking() {
    let source = "program test;";
    let mut lexer = Lexer::new(source);

    let result = lexer.next_token();
    assert!(result.is_some());
    let (start, token, end) = result.unwrap().unwrap();

    assert_eq!(token, Token::ProgramKeyword);
    assert_eq!(start, 0);
    assert_eq!(end, 7); // "program" is 7 chars
}

#[test]
fn test_lexer_position() {
    let source = "var x: integer;";
    let lexer = Lexer::new(source);

    // Test position method
    let span = lexer.position();
    assert_eq!(span.start, 0);
    assert_eq!(span.end, 0);
}

#[test]
fn test_remaining_input() {
    let source = "program test";
    let lexer = Lexer::new(source);

    let remaining = lexer.remaining_input();
    assert_eq!(remaining, source);
}

#[test]
fn test_empty_source() {
    let source = "";
    let mut lexer = Lexer::new(source);

    let result = lexer.next_token();
    assert!(result.is_none(), "Empty source should produce no tokens");
}

#[test]
fn test_only_whitespace() {
    let source = "   \n\t   \r\n  ";
    let mut lexer = Lexer::new(source);

    let result = lexer.next_token();
    assert!(result.is_none(), "Only whitespace should produce no tokens");
}

#[test]
fn test_only_comments() {
    let source = r#"{ This is just a comment }
    // And another comment"#;
    let mut lexer = Lexer::new(source);

    let result = lexer.next_token();
    assert!(result.is_none(), "Only comments should produce no tokens");
}

#[test]
fn test_complex_program_structure() {
    let source = r#"
program HelloWorld;
var
  x, y: integer;
  name: string;
begin
  x := 10;
  y := x + 5;
  name := 'Test';
end.
"#;

    let mut lexer = Lexer::new(source);

    // program
    let result = lexer.next_token();
    assert!(result.is_some());
    let (_, token, _) = result.unwrap().unwrap();
    assert_eq!(token, Token::ProgramKeyword);

    // HelloWorld
    let result = lexer.next_token();
    assert!(result.is_some());
    let (_, token, _) = result.unwrap().unwrap();
    match token {
        Token::Identifier(s) => assert_eq!(s, "HelloWorld"),
        _ => panic!("Expected identifier"),
    }

    // ;
    let result = lexer.next_token();
    assert!(result.is_some());
    let (_, token, _) = result.unwrap().unwrap();
    assert_eq!(token, Token::Semicolon);

    // var
    let result = lexer.next_token();
    assert!(result.is_some());
    let (_, token, _) = result.unwrap().unwrap();
    assert_eq!(token, Token::Var);

    // Continue checking structure
    let mut token_count = 0;
    while let Some(result) = lexer.next_token() {
        result.unwrap();
        token_count += 1;
        if token_count > 100 {
            break; // Safety limit
        }
    }

    assert!(token_count > 10, "Should have many tokens in a program");
}

#[test]
fn test_array_access_syntax() {
    let source = "arr[0] arr[i] arr[5]";
    let mut lexer = Lexer::new(source);

    // arr
    let result = lexer.next_token();
    let (_, token, _) = result.unwrap().unwrap();
    match token {
        Token::Identifier(s) => assert_eq!(s, "arr"),
        _ => panic!("Expected identifier"),
    }

    // [
    let result = lexer.next_token();
    let (_, token, _) = result.unwrap().unwrap();
    assert_eq!(token, Token::LeftBracket);

    // 0
    let result = lexer.next_token();
    let (_, token, _) = result.unwrap().unwrap();
    match token {
        Token::IntegerLiteral(n) => assert_eq!(n, 0),
        _ => panic!("Expected integer literal"),
    }

    // ]
    let result = lexer.next_token();
    let (_, token, _) = result.unwrap().unwrap();
    assert_eq!(token, Token::RightBracket);
}

#[test]
fn test_function_call_syntax() {
    let source = "func() func(a, b, c) writeln('test')";
    let mut lexer = Lexer::new(source);

    // func
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::Identifier(s) => assert_eq!(s, "func"),
        _ => panic!("Expected identifier"),
    }

    // (
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::LeftParen);

    // )
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::RightParen);

    // func
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::Identifier(s) => assert_eq!(s, "func"),
        _ => panic!("Expected identifier"),
    }

    // (
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::LeftParen);

    // a
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::Identifier(s) => assert_eq!(s, "a"),
        _ => panic!("Expected identifier"),
    }

    // ,
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Comma);

    // b
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::Identifier(s) => assert_eq!(s, "b"),
        _ => panic!("Expected identifier"),
    }

    // ,
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Comma);

    // c
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::Identifier(s) => assert_eq!(s, "c"),
        _ => panic!("Expected identifier"),
    }

    // )
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::RightParen);
}

#[test]
fn test_pointer_syntax() {
    let source = "ptr^ @var pointer";
    let mut lexer = Lexer::new(source);

    // ptr
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::Identifier(s) => assert_eq!(s, "ptr"),
        _ => panic!("Expected identifier"),
    }

    // ^
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Caret);

    // @
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::At);

    // var
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Var);

    // pointer
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Pointer);
}

#[test]
fn test_assignment_syntax() {
    let source = "x := 10";
    let mut lexer = Lexer::new(source);

    // x
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::Identifier(s) => assert_eq!(s, "x"),
        _ => panic!("Expected identifier"),
    }

    // :=
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Assign);

    // 10
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::IntegerLiteral(n) => assert_eq!(n, 10),
        _ => panic!("Expected integer literal"),
    }
}

#[test]
fn test_range_syntax() {
    let source = "0..10 'A'..'Z'";
    let mut lexer = Lexer::new(source);

    // 0
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::IntegerLiteral(n) => assert_eq!(n, 0),
        _ => panic!("Expected integer literal"),
    }

    // ..
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::DotDot);

    // 10
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::IntegerLiteral(n) => assert_eq!(n, 10),
        _ => panic!("Expected integer literal"),
    }

    // 'A'
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::CharacterLiteral(c) => assert_eq!(c, 'A'),
        _ => panic!("Expected character literal"),
    }

    // ..
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::DotDot);

    // 'Z'
    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::CharacterLiteral(c) => assert_eq!(c, 'Z'),
        _ => panic!("Expected character literal"),
    }
}

#[test]
fn test_set_constructor() {
    let source = "[1, 2, 3] ['a', 'b', 'c'] []";
    let mut lexer = Lexer::new(source);

    // [1, 2, 3]
    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::LeftBracket);

    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::IntegerLiteral(n) => assert_eq!(n, 1),
        _ => panic!("Expected integer literal"),
    }

    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Comma);

    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::IntegerLiteral(n) => assert_eq!(n, 2),
        _ => panic!("Expected integer literal"),
    }

    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Comma);

    let result = lexer.next_token();
    match result.unwrap().1 {
        Token::IntegerLiteral(n) => assert_eq!(n, 3),
        _ => panic!("Expected integer literal"),
    }

    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::RightBracket);
}

#[test]
fn test_boolean_operators() {
    let source = "and or xor not";
    let mut lexer = Lexer::new(source);

    let expected = vec![Token::And, Token::Or, Token::Xor, Token::Not];

    for exp in expected {
        let result = lexer.next_token();
        assert_eq!(result.unwrap().1, exp);
    }
}

#[test]
fn test_bitwise_operators() {
    let source = "shl shr";
    let mut lexer = Lexer::new(source);

    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Shl);

    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Shr);
}

#[test]
fn test_arithmetic_operators() {
    let source = "div mod";
    let mut lexer = Lexer::new(source);

    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Div);

    let result = lexer.next_token();
    assert_eq!(result.unwrap().1, Token::Mod);
}

#[test]
fn test_comparison_operators() {
    let source = "= < > <= >= <>";
    let mut lexer = Lexer::new(source);

    let expected = vec![
        Token::Equal,
        Token::LessThan,
        Token::GreaterThan,
        Token::LessThanOrEqual,
        Token::GreaterThanOrEqual,
        Token::NotEqual,
    ];

    for exp in expected {
        let result = lexer.next_token();
        assert_eq!(result.unwrap().1, exp);
    }
}
