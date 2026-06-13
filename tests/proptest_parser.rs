//! Property-based tests for the parser.

use pascal::parser::Parser;
use proptest::prelude::*;

proptest! {
    #[test]
    fn parser_rejects_garbage_without_panic(garbage in "[a-zA-Z_]{1,20}") {
        let src = format!("program P;\nbegin\n  {garbage};\nend.");
        let mut parser = Parser::new(&src);
        let _ = parser.parse_program();
    }

    #[test]
    fn valid_minimal_programs_parse(name in "[A-Za-z][A-Za-z0-9_]{0,15}") {
        let src = format!("program {name};\nbegin\nend.");
        let mut parser = Parser::new(&src);
        let result = parser.parse_program();
        prop_assert!(result.is_ok(), "failed for program name {name}");
    }
}
