use minipas::lexer::{Token, Lexer};

#[cfg(test)]
mod lexer_tests {
    use super::*;

    fn collect_tokens(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        lexer.filter(|t| !matches!(t, Ok((_, Token::Whitespace, _)) | Ok((_, Token::Comment, _))))
             .map(|t| t.unwrap().1)
             .collect()
    }

    #[test]
    fn test_string_literals_with_escape_sequences() {
        let input = r#""Hello\nWorld\tTest""#;
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], Token::StringLiteral("Hello\nWorld\tTest".to_string()));
    }

    #[test]
    fn test_character_literals() {
        let input = "'A' '\\n' '\\t' '\\0' '\\\\' '\\'' '\\\"'";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 7);
        assert_eq!(tokens[0], Token::CharLiteral('A'));
        assert_eq!(tokens[1], Token::CharLiteral('\n'));
        assert_eq!(tokens[2], Token::CharLiteral('\t'));
        assert_eq!(tokens[3], Token::CharLiteral('\0'));
        assert_eq!(tokens[4], Token::CharLiteral('\\'));
        assert_eq!(tokens[5], Token::CharLiteral('\''));
        assert_eq!(tokens[6], Token::CharLiteral('"'));
    }

    #[test]
    fn test_numeric_character_codes() {
        let input = "#65 #10 #13 #9 #0";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], Token::CharLiteral('A')); // #65
        assert_eq!(tokens[1], Token::CharLiteral('\n')); // #10
        assert_eq!(tokens[2], Token::CharLiteral('\r')); // #13
        assert_eq!(tokens[3], Token::CharLiteral('\t')); // #9
        assert_eq!(tokens[4], Token::CharLiteral('\0')); // #0
    }

    #[test]
    fn test_preprocessor_directives() {
        let input = "{$mode objfpc} {$h+} {$R+} {$I+}";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], Token::PreprocessorDirective("mode objfpc".to_string()));
        assert_eq!(tokens[1], Token::PreprocessorDirective("h+".to_string()));
        assert_eq!(tokens[2], Token::PreprocessorDirective("R+".to_string()));
        assert_eq!(tokens[3], Token::PreprocessorDirective("I+".to_string()));
    }

    #[test]
    fn test_new_keywords() {
        let input = "class object interface try except finally raise break continue exit with label goto";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 12);
        assert_eq!(tokens[0], Token::Class);
        assert_eq!(tokens[1], Token::Object);
        assert_eq!(tokens[2], Token::Interface);
        assert_eq!(tokens[3], Token::Try);
        assert_eq!(tokens[4], Token::Except);
        assert_eq!(tokens[5], Token::Finally);
        assert_eq!(tokens[6], Token::Raise);
        assert_eq!(tokens[7], Token::Break);
        assert_eq!(tokens[8], Token::Continue);
        assert_eq!(tokens[9], Token::Exit);
        assert_eq!(tokens[10], Token::With);
        assert_eq!(tokens[11], Token::Label);
        assert_eq!(tokens[12], Token::Goto);
    }

    #[test]
    fn test_additional_keywords() {
        let input = "in is as forward external inline override virtual abstract private protected public published";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 12);
        assert_eq!(tokens[0], Token::In);
        assert_eq!(tokens[1], Token::Is);
        assert_eq!(tokens[2], Token::As);
        assert_eq!(tokens[3], Token::Forward);
        assert_eq!(tokens[4], Token::External);
        assert_eq!(tokens[5], Token::Inline);
        assert_eq!(tokens[6], Token::Override);
        assert_eq!(tokens[7], Token::Virtual);
        assert_eq!(tokens[8], Token::Abstract);
        assert_eq!(tokens[9], Token::Private);
        assert_eq!(tokens[10], Token::Protected);
        assert_eq!(tokens[11], Token::Public);
        assert_eq!(tokens[12], Token::Published);
    }

    #[test]
    fn test_bitwise_operators() {
        let input = "& | ~ << >>";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], Token::BitwiseAnd);
        assert_eq!(tokens[1], Token::BitwiseOr);
        assert_eq!(tokens[2], Token::BitwiseNot);
        assert_eq!(tokens[3], Token::Shl);
        assert_eq!(tokens[4], Token::Shr);
    }

    #[test]
    fn test_assignment_operators() {
        let input = "+= -= *= /= :=";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], Token::PlusAssign);
        assert_eq!(tokens[1], Token::MinusAssign);
        assert_eq!(tokens[2], Token::MultiplyAssign);
        assert_eq!(tokens[3], Token::DivideAssign);
        assert_eq!(tokens[4], Token::Assign);
    }

    #[test]
    fn test_range_operators() {
        let input = ".. ...";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::Range);
        assert_eq!(tokens[1], Token::Ellipsis);
    }

    #[test]
    fn test_complex_string_literals() {
        let input = r#""Line 1\nLine 2\tTabbed""#;
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], Token::StringLiteral("Line 1\nLine 2\tTabbed".to_string()));
    }

    #[test]
    fn test_mixed_content() {
        let input = r#"program Test; {$mode objfpc} var s: string; begin s := "Hello\nWorld"; end."#;
        let tokens = collect_tokens(input);
        
        // Expected tokens: program, Test, ;, {$mode objfpc}, var, s, :, string, ;, begin, s, :=, "Hello\nWorld", ;, end, .
        assert!(tokens.len() >= 15);
        assert_eq!(tokens[0], Token::Program);
        assert_eq!(tokens[1], Token::Identifier("Test".to_string()));
        assert_eq!(tokens[2], Token::Semicolon);
        assert_eq!(tokens[3], Token::PreprocessorDirective("mode objfpc".to_string()));
        assert_eq!(tokens[4], Token::Var);
        assert_eq!(tokens[5], Token::Identifier("s".to_string()));
        assert_eq!(tokens[6], Token::Colon);
        assert_eq!(tokens[7], Token::String);
        assert_eq!(tokens[8], Token::Semicolon);
        assert_eq!(tokens[9], Token::Begin);
        assert_eq!(tokens[10], Token::Identifier("s".to_string()));
        assert_eq!(tokens[11], Token::Assign);
        assert_eq!(tokens[12], Token::StringLiteral("Hello\nWorld".to_string()));
        assert_eq!(tokens[13], Token::Semicolon);
        assert_eq!(tokens[14], Token::End);
        assert_eq!(tokens[15], Token::Dot);
    }

    #[test]
    fn test_real_numbers() {
        let input = "3.14159 1.23e-4 2.5E+3 0.0";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], Token::Number("3.14159".to_string()));
        assert_eq!(tokens[1], Token::Number("1.23e-4".to_string()));
        assert_eq!(tokens[2], Token::Number("2.5E+3".to_string()));
        assert_eq!(tokens[3], Token::Number("0.0".to_string()));
    }

    #[test]
    fn test_identifiers_with_underscores() {
        let input = "my_variable _private test_123";
        let tokens = collect_tokens(input);
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0], Token::Identifier("my_variable".to_string()));
        assert_eq!(tokens[1], Token::Identifier("_private".to_string()));
        assert_eq!(tokens[2], Token::Identifier("test_123".to_string()));
    }

    #[test]
    fn test_comments_are_skipped() {
        let input = "program Test; { This is a comment } var x: integer; // Another comment";
        let tokens = collect_tokens(input);
        // Should only have: program, Test, ;, var, x, :, integer, ;
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0], Token::Program);
        assert_eq!(tokens[1], Token::Identifier("Test".to_string()));
        assert_eq!(tokens[2], Token::Semicolon);
        assert_eq!(tokens[3], Token::Var);
        assert_eq!(tokens[4], Token::Identifier("x".to_string()));
        assert_eq!(tokens[5], Token::Colon);
        assert_eq!(tokens[6], Token::Integer);
        assert_eq!(tokens[7], Token::Semicolon);
    }
}
