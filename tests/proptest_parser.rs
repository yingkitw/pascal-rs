//! Property-based tests for the parser.

use pascal::parser::Parser;
use proptest::prelude::*;

const PASCAL_KEYWORDS: &[&str] = &[
    "program", "var", "const", "type", "begin", "end", "if", "then", "else",
    "while", "do", "for", "to", "downto", "repeat", "until", "case", "of", "when",
    "with", "procedure", "function", "forward", "external", "inline", "assembler",
    "uses", "unit", "interface", "implementation", "initialization", "finalization",
    "class", "object", "inherited", "virtual", "override", "abstract", "sealed",
    "private", "protected", "public", "published", "property", "read", "write",
    "constructor", "destructor", "operator", "try", "except", "finally", "raise",
    "exit", "break", "continue", "goto", "label", "array", "record", "set", "file",
    "packed", "string", "integer", "real", "boolean", "char", "pointer", "nil",
    "true", "false", "and", "or", "not", "xor", "shl", "shr", "mod", "in", "is", "as",
    "div",
];

proptest! {
    #[test]
    fn parser_rejects_garbage_without_panic(garbage in "[a-zA-Z_]{1,20}") {
        let src = format!("program P;\nbegin\n  {garbage};\nend.");
        let mut parser = Parser::new(&src);
        let _ = parser.parse_program();
    }

    #[test]
    fn valid_minimal_programs_parse(name in "[A-Za-z][A-Za-z0-9_]{0,15}") {
        prop_assume!(!PASCAL_KEYWORDS.contains(&name.as_str()));
        let src = format!("program {name};\nbegin\nend.");
        let mut parser = Parser::new(&src);
        let result = parser.parse_program();
        prop_assert!(result.is_ok(), "failed for program name {name}");
    }
}
