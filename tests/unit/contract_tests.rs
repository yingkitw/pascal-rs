//! Contract tests for module boundaries (lexer → parser → interpreter).

use pascal::interpreter::Interpreter;
use pascal::lexer::Lexer;
use pascal::parser::Parser;

#[test]
fn contract_lexer_parser_interpreter_pipeline() {
    let source = "program Contract;\nvar\n  n: integer;\nbegin\n  n := 7;\nend.";
    let mut lexer = Lexer::new(source);
    let mut tokens = 0;
    while lexer.next_token().is_some() {
        tokens += 1;
    }
    assert!(tokens > 0);

    let mut parser = Parser::new(source);
    let program = parser.parse_program().expect("parse should succeed");

    let mut interp = Interpreter::new(false);
    interp.run_program(&program).expect("interpret should succeed");
}

#[test]
fn contract_parser_errors_are_deterministic() {
    let source = "program Bad;\nbegin\n  if then\nend.";
    let mut parser1 = Parser::new(source);
    let mut parser2 = Parser::new(source);
    let e1 = parser1.parse_program().unwrap_err().to_string();
    let e2 = parser2.parse_program().unwrap_err().to_string();
    assert_eq!(e1, e2);
}

#[test]
fn contract_docgen_accepts_parsed_program() {
    let source = "program Doc;\nbegin\nend.";
    let md = pascal::generate_docs_from_source(
        source,
        std::path::Path::new("doc.pas"),
        pascal::DocFormat::Markdown,
    )
    .expect("docgen should accept valid source");
    assert!(md.contains("Doc"));
}
