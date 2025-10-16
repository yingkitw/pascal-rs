use minipas::lexer::{Token, Lexer};
use minipas::parser::Parser;
use minipas::codegen::CodeGenerator;

#[cfg(test)]
mod integration_tests {
    use super::*;

    fn compile_program(source: &str) -> Result<String, Box<dyn std::error::Error>> {
        let mut parser = Parser::new(source);
        let program = parser.parse_program().map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        
        let mut codegen = CodeGenerator::new();
        let assembly = codegen.generate(&program)?;
        
        Ok(assembly)
    }

    #[test]
    fn test_basic_program_with_new_features() {
        let source = r#"
program BasicNewFeatures;
{$mode objfpc}

var
  message: string;
  ch: char;
  number: real;

begin
  message := "Hello\nWorld\tTest";
  ch := 'A';
  ch := #65;
  number := 3.14159;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Basic program with new features should compile");
        
        let assembly = result.unwrap();
        assert!(assembly.contains(".intel_syntax"));
        assert!(assembly.contains("main:"));
    }

    #[test]
    fn test_string_literals_with_escape_sequences() {
        let source = r#"
program StringTest;

var
  s1, s2, s3: string;

begin
  s1 := "Line 1\nLine 2";
  s2 := "Tab\tSeparated";
  s3 := "Quote\"Test";
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "String literals with escape sequences should compile");
    }

    #[test]
    fn test_character_literals() {
        let source = r#"
program CharTest;

var
  ch1, ch2, ch3, ch4: char;

begin
  ch1 := 'A';
  ch2 := '\n';
  ch3 := '\t';
  ch4 := #65;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Character literals should compile");
    }

    #[test]
    fn test_real_numbers() {
        let source = r#"
program RealTest;

var
  r1, r2, r3, r4: real;

begin
  r1 := 3.14159;
  r2 := 1.23e-4;
  r3 := 2.5E+3;
  r4 := 0.0;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Real numbers should compile");
    }

    #[test]
    fn test_preprocessor_directives() {
        let source = r#"
program PreprocessorTest;
{$mode objfpc}
{$h+}

var
  s: string;

begin
  s := "Test";
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Preprocessor directives should be handled");
    }

    #[test]
    fn test_complex_expressions() {
        let source = r#"
program ComplexExpressions;

var
  a, b, c: integer;
  flag: boolean;

begin
  a := 10;
  b := 5;
  c := a + b * 2;
  flag := (a > b) and (c < 20);
  
  if flag then
    c := c + 1
  else
    c := c - 1;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Complex expressions should compile");
    }

    #[test]
    fn test_nested_blocks() {
        let source = r#"
program NestedBlocks;

var
  x, y: integer;

begin
  x := 10;
  y := 20;
  
  if x > 5 then
  begin
    if y > 15 then
    begin
      x := x + y;
    end
    else
    begin
      x := x - y;
    end;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Nested blocks should compile");
    }

    #[test]
    fn test_while_loops() {
        let source = r#"
program WhileLoops;

var
  i, sum: integer;

begin
  i := 1;
  sum := 0;
  
  while i <= 10 do
  begin
    sum := sum + i;
    i := i + 1;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "While loops should compile");
    }

    #[test]
    fn test_for_loops() {
        let source = r#"
program ForLoops;

var
  i, sum: integer;

begin
  sum := 0;
  
  for i := 1 to 10 do
  begin
    sum := sum + i;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "For loops should compile");
    }

    #[test]
    fn test_case_statements() {
        let source = r#"
program CaseStatements;

var
  choice: integer;
  result: string;

begin
  choice := 2;
  
  case choice of
    1: result := "One";
    2: result := "Two";
    3: result := "Three";
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Case statements should compile");
    }

    #[test]
    fn test_boolean_logic() {
        let source = r#"
program BooleanLogic;

var
  a, b, c: integer;
  flag1, flag2, flag3: boolean;

begin
  a := 10;
  b := 5;
  c := 3;
  
  flag1 := a > b;
  flag2 := b < c;
  flag3 := flag1 and flag2;
  
  if flag3 then
    c := a + b
  else
    c := a - b;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Boolean logic should compile");
    }

    #[test]
    fn test_arithmetic_operations() {
        let source = r#"
program ArithmeticOps;

var
  a, b, c, d: integer;

begin
  a := 20;
  b := 4;
  c := 3;
  d := 2;
  
  a := a + b;
  b := b - c;
  c := c * d;
  d := d div 2;
  a := a mod 3;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Arithmetic operations should compile");
    }

    #[test]
    fn test_bitwise_operations() {
        let source = r#"
program BitwiseOps;

var
  a, b, c: integer;

begin
  a := 5;  // 101 in binary
  b := 3;  // 011 in binary
  
  c := a and b;  // 001 = 1
  c := a or b;   // 111 = 7
  c := a xor b;  // 110 = 6
  c := a shl 2;  // 10100 = 20
  c := a shr 1;  // 10 = 2
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Bitwise operations should compile");
    }

    #[test]
    fn test_comparison_operations() {
        let source = r#"
program ComparisonOps;

var
  a, b: integer;
  flag1, flag2, flag3, flag4: boolean;

begin
  a := 10;
  b := 5;
  
  flag1 := a = b;
  flag2 := a <> b;
  flag3 := a > b;
  flag4 := a <= b;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Comparison operations should compile");
    }

    #[test]
    fn test_complex_nested_structures() {
        let source = r#"
program ComplexNested;

var
  i, j, sum: integer;
  flag: boolean;

begin
  sum := 0;
  flag := true;
  
  for i := 1 to 5 do
  begin
    if flag then
    begin
      for j := 1 to 3 do
      begin
        if (i + j) mod 2 = 0 then
        begin
          sum := sum + i * j;
        end;
      end;
    end
    else
    begin
      sum := sum - i;
    end;
    
    if sum > 20 then
      flag := false;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Complex nested structures should compile");
    }

    #[test]
    fn test_error_handling() {
        let source = r#"
program ErrorTest;

var
  x: integer;

begin
  x := 10;
  // This should cause a compilation error
  x := "string";
end.
"#;

        let result = compile_program(source);
        // This should fail due to type mismatch
        assert!(result.is_err(), "Type mismatch should cause compilation error");
    }

    #[test]
    fn test_undefined_variable() {
        let source = r#"
program UndefinedVar;

begin
  x := 10;  // x is not declared
end.
"#;

        let result = compile_program(source);
        // This should fail due to undefined variable
        assert!(result.is_err(), "Undefined variable should cause compilation error");
    }

    #[test]
    fn test_missing_semicolon() {
        let source = r#"
program MissingSemicolon;

var
  x: integer

begin
  x := 10
end.
"#;

        let result = compile_program(source);
        // This should fail due to missing semicolon
        assert!(result.is_err(), "Missing semicolon should cause compilation error");
    }

    #[test]
    fn test_empty_program() {
        let source = r#"
program EmptyProgram;

begin
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Empty program should compile");
    }

    #[test]
    fn test_single_statement() {
        let source = r#"
program SingleStatement;

var
  x: integer;

begin
  x := 42;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Single statement program should compile");
    }

    #[test]
    fn test_multiple_variables() {
        let source = r#"
program MultipleVars;

var
  a, b, c: integer;
  x, y: real;
  flag1, flag2: boolean;

begin
  a := 1;
  b := 2;
  c := 3;
  x := 1.5;
  y := 2.5;
  flag1 := true;
  flag2 := false;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Multiple variables should compile");
    }

    #[test]
    fn test_constant_declarations() {
        let source = r#"
program Constants;

const
  MAX_SIZE = 100;
  PI = 3.14159;
  MESSAGE = 'Hello World';

var
  size: integer;
  radius: real;
  msg: string;

begin
  size := MAX_SIZE;
  radius := PI * 2.0;
  msg := MESSAGE;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Constant declarations should compile");
    }
}
