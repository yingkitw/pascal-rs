//! Main validation tests for Pascal compiler

#[cfg(test)]
mod main_validation {
    use pascal::parser::Parser;
    use pascal::interpreter::Interpreter;

    #[test]
    pub fn test_backward_compatibility() {
        let old_style_code = r#"
        program OldStyle;
        var
          i: integer;
        begin
          for i := 1 to 10 do
            writeln(i);
        end.
        "#;

        let mut parser = Parser::new(old_style_code);
        let program = parser.parse_program();
        assert!(program.is_ok());

        let mut interpreter = Interpreter::new(false);
        let result = interpreter.run_program(&program.unwrap());
        assert!(result.is_ok());

        println!("  ✓ Backward compatibility validation passed");
    }
}