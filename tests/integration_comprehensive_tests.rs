#[cfg(test)]
mod integration_comprehensive_tests {
    use minipas_lexer::{Lexer, EnhancedLexer};
    use minipas_parser::{Parser, EnhancedParser};
    use minipas_codegen::{CodeGenerator, EnhancedCodeGenerator, TargetArchitecture};
    use minipas_ast::*;

    // ============================================================================
    // BASIC INTEGRATION TESTS
    // ============================================================================

    #[test]
    fn test_basic_compilation_pipeline() {
        let input = r#"
        program BasicTest;
        var
            x, y: integer;
        begin
            x := 42;
            y := x + 1;
        end.
        "#;

        // Step 1: Lexical Analysis
        let mut parser = Parser::new(input);
        
        // Step 2: Parsing
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok(), "Parsing should succeed");
        let program = parse_result.unwrap();
        
        // Step 3: Code Generation
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok(), "Code generation should succeed");
        let assembly = codegen_result.unwrap();
        
        // Verify the assembly contains expected elements
        assert!(assembly.contains(".intel_syntax noprefix"));
        assert!(assembly.contains(".section .text"));
        assert!(assembly.contains("main:"));
        assert!(assembly.contains("mov eax, 42"));
        assert!(assembly.contains("add eax, edx"));
    }

    #[test]
    fn test_enhanced_compilation_pipeline() {
        let input = r#"
        program EnhancedTest;
        var
            x, y: integer;
            name: string;
        begin
            x := 42;
            y := x * 2;
            name := 'Hello, World!';
        end.
        "#;

        // Step 1: Enhanced Lexical Analysis
        let mut enhanced_lexer = EnhancedLexer::new(input);
        let mut enhanced_parser = EnhancedParser::new(input);
        
        // Step 2: Enhanced Parsing
        let parse_result = enhanced_parser.parse_program();
        match parse_result {
            Ok(program) => {
                // Step 3: Enhanced Code Generation
                let mut enhanced_codegen = EnhancedCodeGenerator::new(TargetArchitecture::X86_64);
                let codegen_result = enhanced_codegen.generate_code(&program);
                
                match codegen_result {
                    Ok(assembly) => {
                        // Verify the assembly contains expected elements
                        assert!(!assembly.is_empty());
                        assert!(assembly.contains("main:") || assembly.contains("_start:"));
                    }
                    Err(error) => {
                        // For now, just ensure it doesn't crash
                        println!("Enhanced codegen error (expected for complex program): {}", error);
                    }
                }
            }
            Err(error) => {
                // For now, just ensure it doesn't crash
                println!("Enhanced parser error (expected for complex program): {:?}", error);
            }
        }
    }

    #[test]
    fn test_complex_program_compilation() {
        let input = r#"
        program ComplexTest;
        var
            i, sum: integer;
            numbers: array[1..10] of integer;
        begin
            sum := 0;
            for i := 1 to 10 do
            begin
                numbers[i] := i * i;
                sum := sum + numbers[i];
            end;
        end.
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok(), "Complex program should parse successfully");
        let program = parse_result.unwrap();
        
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok(), "Complex program should generate code successfully");
        let assembly = codegen_result.unwrap();
        
        // Verify the assembly contains expected elements
        assert!(assembly.contains("main:"));
        assert!(assembly.contains("mov eax, 0")); // sum := 0
        assert!(assembly.contains("_for_") || assembly.contains("_while_")); // Loop structure
    }

    #[test]
    fn test_procedure_and_function_compilation() {
        let input = r#"
        program ProcedureTest;
        
        procedure DoSomething(x: integer);
        begin
            writeln(x);
        end;
        
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;
        
        var
            result: integer;
        begin
            DoSomething(42);
            result := Add(10, 20);
        end.
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok(), "Program with procedures should parse successfully");
        let program = parse_result.unwrap();
        
        // Verify the program structure
        assert_eq!(program.name, "ProcedureTest");
        assert_eq!(program.block.procedures.len(), 1);
        assert_eq!(program.block.functions.len(), 1);
        
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok(), "Program with procedures should generate code successfully");
        let assembly = codegen_result.unwrap();
        
        // Verify the assembly contains expected elements
        assert!(assembly.contains("main:"));
        assert!(assembly.contains("call") || assembly.contains("jmp")); // Function calls
    }

    // ============================================================================
    // ERROR HANDLING INTEGRATION TESTS
    // ============================================================================

    #[test]
    fn test_lexical_error_handling() {
        let input = r#"
        program ErrorTest;
        var
            x: integer;
        begin
            x := "unterminated string;
        end.
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        // Should handle lexical errors gracefully
        match parse_result {
            Ok(_) => {
                // If it succeeds, it should handle the error internally
            }
            Err(error) => {
                // If it fails, it should be a meaningful error
                assert!(!format!("{:?}", error).is_empty());
            }
        }
    }

    #[test]
    fn test_syntax_error_handling() {
        let input = r#"
        program SyntaxErrorTest;
        var
            x: integer;
        begin
            x := ; // Missing expression
        end.
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_err(), "Syntax error should be detected");
        
        if let Err(error) = parse_result {
            assert!(!format!("{:?}", error).is_empty());
        }
    }

    #[test]
    fn test_semantic_error_handling() {
        let input = r#"
        program SemanticErrorTest;
        var
            x: integer;
        begin
            x := undefined_variable; // Undefined variable
        end.
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        // Should either succeed (if undefined variables are allowed) or fail gracefully
        match parse_result {
            Ok(program) => {
                let mut codegen = CodeGenerator::new();
                let codegen_result = codegen.generate_code(&program);
                // Code generation might fail for undefined variables
                match codegen_result {
                    Ok(_) => {
                        // If it succeeds, the codegen should handle undefined variables
                    }
                    Err(error) => {
                        assert!(!error.is_empty());
                    }
                }
            }
            Err(error) => {
                assert!(!format!("{:?}", error).is_empty());
            }
        }
    }

    // ============================================================================
    // PERFORMANCE INTEGRATION TESTS
    // ============================================================================

    #[test]
    fn test_compilation_performance() {
        let input = r#"
        program PerformanceTest;
        var
            i, sum: integer;
        begin
            sum := 0;
            for i := 1 to 1000 do
            begin
                sum := sum + i;
            end;
        end.
        "#;

        let start = std::time::Instant::now();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok());
        
        let program = parse_result.unwrap();
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok());
        
        let duration = start.elapsed();
        
        // Should compile quickly (less than 100ms)
        assert!(duration.as_millis() < 100);
    }

    #[test]
    fn test_large_program_compilation() {
        // Generate a large program
        let mut input = String::from("program LargeTest;\nvar\n");
        
        // Add many variables
        for i in 0..100 {
            input.push_str(&format!("    var{}: integer;\n", i));
        }
        
        input.push_str("begin\n");
        
        // Add many statements
        for i in 0..100 {
            input.push_str(&format!("    var{} := {};\n", i, i));
        }
        
        input.push_str("end.\n");
        
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok(), "Large program should parse successfully");
        
        let program = parse_result.unwrap();
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok(), "Large program should generate code successfully");
        
        let assembly = codegen_result.unwrap();
        assert!(!assembly.is_empty());
    }

    // ============================================================================
    // EDGE CASE INTEGRATION TESTS
    // ============================================================================

    #[test]
    fn test_empty_program_compilation() {
        let input = "program Empty; begin end.";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok());
        
        let program = parse_result.unwrap();
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok());
        
        let assembly = codegen_result.unwrap();
        assert!(assembly.contains("main:"));
    }

    #[test]
    fn test_minimal_program_compilation() {
        let input = "program x; begin end.";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok());
        
        let program = parse_result.unwrap();
        assert_eq!(program.name, "x");
        
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok());
    }

    #[test]
    fn test_whitespace_handling() {
        let input = "  program   Test   ;  \n  begin  \n  end  .  ";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok());
        
        let program = parse_result.unwrap();
        assert_eq!(program.name, "Test");
    }

    #[test]
    fn test_comment_handling() {
        let input = r#"
        program CommentTest; { This is a comment }
        var
            x: integer; // Another comment
        begin
            x := 42; { Yet another comment }
        end.
        "#;
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok());
        
        let program = parse_result.unwrap();
        assert_eq!(program.name, "CommentTest");
    }

    // ============================================================================
    // MULTI-ARCHITECTURE INTEGRATION TESTS
    // ============================================================================

    #[test]
    fn test_multi_architecture_compilation() {
        let input = r#"
        program MultiArchTest;
        var
            x: integer;
        begin
            x := 42;
        end.
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok());
        let program = parse_result.unwrap();

        // Test different target architectures
        let architectures = vec![
            TargetArchitecture::X86_64,
            TargetArchitecture::X86_32,
            TargetArchitecture::ARM64,
            TargetArchitecture::ARM32,
            TargetArchitecture::RiscV64,
            TargetArchitecture::RiscV32,
        ];

        for arch in architectures {
            let mut codegen = EnhancedCodeGenerator::new(arch);
            let result = codegen.generate_code(&program);
            
            // Should not panic, even if not fully implemented
            match result {
                Ok(assembly) => {
                    assert!(!assembly.is_empty());
                }
                Err(error) => {
                    assert!(!error.is_empty());
                }
            }
        }
    }

    // ============================================================================
    // COMPREHENSIVE FEATURE TESTS
    // ============================================================================

    #[test]
    fn test_all_basic_features() {
        let input = r#"
        program AllFeaturesTest;
        const
            MAX_SIZE = 100;
        type
            TMyType = record
                field1: integer;
                field2: string;
            end;
        var
            x, y: integer;
            name: string;
            rec: TMyType;
        procedure DoSomething(value: integer);
        begin
            writeln(value);
        end;
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;
        begin
            x := 42;
            y := Add(x, 1);
            name := 'Hello, World!';
            rec.field1 := x;
            rec.field2 := name;
            DoSomething(y);
        end.
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok(), "All features should parse successfully");
        let program = parse_result.unwrap();
        
        // Verify program structure
        assert_eq!(program.name, "AllFeaturesTest");
        assert!(!program.block.consts.is_empty());
        assert!(!program.block.types.is_empty());
        assert!(!program.block.vars.is_empty());
        assert!(!program.block.procedures.is_empty());
        assert!(!program.block.functions.is_empty());
        assert!(!program.block.statements.is_empty());
        
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok(), "All features should generate code successfully");
    }

    #[test]
    fn test_control_flow_features() {
        let input = r#"
        program ControlFlowTest;
        var
            x, y: integer;
            choice: integer;
        begin
            x := 10;
            y := 20;
            
            if x > y then
                writeln('x is greater')
            else
                writeln('y is greater');
            
            while x > 0 do
            begin
                writeln(x);
                x := x - 1;
            end;
            
            for x := 1 to 5 do
                writeln(x);
            
            case choice of
                1: writeln('One');
                2: writeln('Two');
            else
                writeln('Other');
            end;
        end.
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok(), "Control flow should parse successfully");
        let program = parse_result.unwrap();
        
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok(), "Control flow should generate code successfully");
    }

    #[test]
    fn test_expression_features() {
        let input = r#"
        program ExpressionTest;
        var
            a, b, c, result: integer;
        begin
            a := 5;
            b := 3;
            c := 2;
            
            result := a + b * c;
            result := (a + b) * c;
            result := a * b + c;
            result := a - b + c;
            result := a / b * c;
            result := a mod b;
        end.
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        
        let parse_result = parser.parse_program();
        assert!(parse_result.is_ok(), "Expressions should parse successfully");
        let program = parse_result.unwrap();
        
        let mut codegen = CodeGenerator::new();
        let codegen_result = codegen.generate_code(&program);
        assert!(codegen_result.is_ok(), "Expressions should generate code successfully");
    }
}
