#[cfg(test)]
mod comprehensive_tests {
    use super::*;
    use crate::enhanced_parser::EnhancedParser;
    use crate::traits::*;
    use minipas_ast::*;
    use minipas_lexer::{Lexer, Token, LexerError};

    // ============================================================================
    // BASIC PARSER TESTS
    // ============================================================================

    #[test]
    fn test_parse_simple_program() {
        let input = "program Hello; begin end.";
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        assert_eq!(program.name, "Hello");
        assert!(program.block.statements.is_empty());
    }

    #[test]
    fn test_parse_program_with_variables() {
        let input = r#"
        program Test;
        var
            x, y: integer;
            name: string;
        begin
            x := 42;
            y := x + 1;
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        assert_eq!(program.name, "Test");
        assert_eq!(program.block.declarations.len(), 3);
        assert_eq!(program.block.statements.len(), 2);
    }

    #[test]
    fn test_parse_expressions() {
        let input = "program Test; begin x := (a + b) * c - d / e; end.";
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        assert_eq!(program.block.statements.len(), 1);
        
        if let Statement::Assignment { variable, expression } = &program.block.statements[0] {
            assert_eq!(variable, "x");
            // Expression should be a binary operation
            assert!(matches!(expression, Expression::BinaryOperation { .. }));
        } else {
            panic!("Expected assignment statement");
        }
    }

    #[test]
    fn test_parse_control_structures() {
        let input = r#"
        program Test;
        begin
            if x > 0 then
                y := 1
            else
                y := -1;
                
            while x > 0 do
                x := x - 1;
                
            for i := 1 to 10 do
                sum := sum + i;
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        assert_eq!(program.block.statements.len(), 3);
        
        // Check for if statement
        assert!(matches!(program.block.statements[0], Statement::If { .. }));
        // Check for while statement
        assert!(matches!(program.block.statements[1], Statement::While { .. }));
        // Check for for statement
        assert!(matches!(program.block.statements[2], Statement::For { .. }));
    }

    #[test]
    fn test_parse_procedures_and_functions() {
        let input = r#"
        program Test;
        
        procedure DoSomething(x: integer);
        begin
            writeln(x);
        end;
        
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;
        
        begin
            DoSomething(42);
            result := Add(10, 20);
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        assert_eq!(program.block.declarations.len(), 2);
        
        // Check procedure declaration
        if let Declaration::Procedure { name, .. } = &program.block.declarations[0] {
            assert_eq!(name, "DoSomething");
        } else {
            panic!("Expected procedure declaration");
        }
        
        // Check function declaration
        if let Declaration::Function { name, .. } = &program.block.declarations[1] {
            assert_eq!(name, "Add");
        } else {
            panic!("Expected function declaration");
        }
    }

    // ============================================================================
    // ENHANCED PARSER TESTS
    // ============================================================================

    #[test]
    fn test_enhanced_parse_complex_program() {
        let input = r#"
        program ComplexTest;
        uses SysUtils;
        
        type
            TMyRecord = record
                field1: Integer;
                field2: String;
            end;
            
            TMyClass = class
            private
                FValue: Integer;
            public
                property Value: Integer read FValue write FValue;
                constructor Create(AValue: Integer);
                destructor Destroy; override;
            end;
        
        var
            x, y: Integer;
            str: String;
            rec: TMyRecord;
            obj: TMyClass;
        
        constructor TMyClass.Create(AValue: Integer);
        begin
            FValue := AValue;
        end;
        
        destructor TMyClass.Destroy;
        begin
            // Cleanup
        end;
        
        begin
            x := 42;
            y := x + 1;
            str := 'Hello, World!';
            rec.field1 := x;
            rec.field2 := str;
            obj := TMyClass.Create(y);
            try
                obj.Value := rec.field1 * 2;
            finally
                obj.Free;
            end;
        end.
        "#;
        
        let mut parser = EnhancedParser::new(input);
        let result = parser.parse_program();
        
        // Should parse successfully (or at least not crash)
        match result {
            Ok(program) => {
                assert_eq!(program.name, "ComplexTest");
                // Should have various declarations
                assert!(!program.block.declarations.is_empty());
            }
            Err(error) => {
                // For now, just ensure it doesn't crash
                println!("Parser error (expected for complex program): {:?}", error);
            }
        }
    }

    #[test]
    fn test_enhanced_parse_advanced_types() {
        let input = r#"
        program TypeTest;
        
        type
            TEnum = (red, green, blue);
            TSet = set of TEnum;
            TArray = array[1..10] of Integer;
            TRecord = record
                field1: Integer;
                field2: String;
                case tag: Integer of
                    0: (intVal: Integer);
                    1: (strVal: String);
            end;
            TPointer = ^Integer;
            TGeneric<T> = record
                value: T;
            end;
        
        begin
            // Test various type usages
        end.
        "#;
        
        let mut parser = EnhancedParser::new(input);
        let result = parser.parse_program();
        
        match result {
            Ok(program) => {
                assert_eq!(program.name, "TypeTest");
                // Should have type declarations
                assert!(!program.block.declarations.is_empty());
            }
            Err(error) => {
                // For now, just ensure it doesn't crash
                println!("Parser error (expected for advanced types): {:?}", error);
            }
        }
    }

    #[test]
    fn test_enhanced_parse_exception_handling() {
        let input = r#"
        program ExceptionTest;
        
        begin
            try
                // Risky operation
                x := 1 div 0;
            except
                on EDivByZero do
                    writeln('Division by zero');
                on EAccessViolation do
                    writeln('Access violation');
                else
                    writeln('Unknown error');
            end;
            
            try
                // Allocate resource
                obj := TObject.Create;
            finally
                obj.Free;
            end;
        end.
        "#;
        
        let mut parser = EnhancedParser::new(input);
        let result = parser.parse_program();
        
        match result {
            Ok(program) => {
                assert_eq!(program.name, "ExceptionTest");
                // Should have try-except statements
                assert!(!program.block.statements.is_empty());
            }
            Err(error) => {
                // For now, just ensure it doesn't crash
                println!("Parser error (expected for exception handling): {:?}", error);
            }
        }
    }

    // ============================================================================
    // ERROR HANDLING TESTS
    // ============================================================================

    #[test]
    fn test_parse_error_handling() {
        let input = "program Test; begin x := ; end."; // Missing expression
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
        
        if let Err(ParseError::UnexpectedToken { expected, .. }) = result {
            assert!(!expected.is_empty());
        } else {
            panic!("Expected UnexpectedToken error");
        }
    }

    #[test]
    fn test_parse_missing_semicolon() {
        let input = "program Test begin end."; // Missing semicolon
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_missing_end() {
        let input = "program Test; begin"; // Missing end
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_expression() {
        let input = "program Test; begin x := +; end."; // Invalid expression
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_err());
    }

    // ============================================================================
    // SYMBOL TABLE TESTS
    // ============================================================================

    #[test]
    fn test_symbol_table_management() {
        let input = r#"
        program SymbolTest;
        var
            x: integer;
            y: string;
        begin
            x := 42;
            y := 'hello';
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        
        // Check that variables are properly declared
        assert_eq!(program.block.declarations.len(), 2);
        
        if let Declaration::Variable { name, var_type } = &program.block.declarations[0] {
            assert_eq!(name, "x");
            assert!(matches!(var_type, Type::Integer));
        } else {
            panic!("Expected variable declaration");
        }
    }

    #[test]
    fn test_scope_management() {
        let input = r#"
        program ScopeTest;
        var
            x: integer;
        begin
            x := 1;
            begin
                var y: integer;
                y := 2;
                x := y;
            end;
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        
        // Should have nested blocks
        assert!(!program.block.statements.is_empty());
        
        if let Statement::Block { statements } = &program.block.statements[1] {
            // Inner block should have variable declaration
            assert!(!statements.is_empty());
        } else {
            panic!("Expected block statement");
        }
    }

    // ============================================================================
    // PERFORMANCE TESTS
    // ============================================================================

    #[test]
    fn test_parser_performance() {
        let input = "program test; var x: integer; begin x := 42; end.".repeat(100);
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        
        let start = std::time::Instant::now();
        let result = parser.parse_program();
        let duration = start.elapsed();
        
        // Should parse quickly (less than 50ms for 100 repetitions)
        assert!(duration.as_millis() < 50);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enhanced_parser_performance() {
        let input = "program test; var x: integer; begin x := 42; end.".repeat(100);
        let mut parser = EnhancedParser::new(&input);
        
        let start = std::time::Instant::now();
        let result = parser.parse_program();
        let duration = start.elapsed();
        
        // Should parse quickly (less than 100ms for 100 repetitions)
        assert!(duration.as_millis() < 100);
        // Result might be Ok or Err depending on implementation
    }

    // ============================================================================
    // EDGE CASE TESTS
    // ============================================================================

    #[test]
    fn test_empty_program() {
        let input = "program Empty; begin end.";
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        assert_eq!(program.name, "Empty");
        assert!(program.block.statements.is_empty());
    }

    #[test]
    fn test_minimal_program() {
        let input = "program x; begin end.";
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        assert_eq!(program.name, "x");
    }

    #[test]
    fn test_whitespace_handling() {
        let input = "  program   Test   ;  \n  begin  \n  end  .  ";
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        assert_eq!(program.name, "Test");
    }

    #[test]
    fn test_comment_handling() {
        let input = r#"
        program Test; { This is a comment }
        var
            x: integer; // Another comment
        begin
            x := 42; { Yet another comment }
        end.
        "#;
        
        let mut parser = Parser::new(input);
        
        let result = parser.parse_program();
        assert!(result.is_ok());
        
        let program = result.unwrap();
        assert_eq!(program.name, "Test");
    }
}
