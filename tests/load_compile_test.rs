//! Load test: compile many small programs sequentially.

use pascal::parser::Parser;
use pascal::interpreter::Interpreter;

fn make_program(i: usize) -> String {
    format!(
        "program Load{i};\nvar\n  x: integer;\nbegin\n  x := {i};\nend."
    )
}

#[test]
fn test_load_compile_100_programs() {
    for i in 0..100 {
        let src = make_program(i);
        let mut parser = Parser::new(&src);
        let program = parser.parse_program().expect("parse");
        let mut interp = Interpreter::new(false);
        interp.run_program(&program).expect("run");
    }
}

#[test]
fn test_load_parse_500_programs() {
    for i in 0..500 {
        let src = make_program(i);
        let mut parser = Parser::new(&src);
        assert!(parser.parse_program().is_ok(), "parse failed at {i}");
    }
}
