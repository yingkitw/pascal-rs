use minipas::parser::Parser;
use minipas::ast::*;

#[cfg(test)]
mod parser_tests {
    use super::*;

    fn parse_program(source: &str) -> Result<Program, Box<dyn std::error::Error>> {
        let mut parser = Parser::new(source);
        parser.parse_program().map_err(|e| e.into())
    }

    #[test]
    fn test_parse_string_literals() {
        let source = r#"
program StringTest;

var
  s: string;

begin
  s := "Hello\nWorld\tTest";
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse string literals with escape sequences");
        
        let program = result.unwrap();
        assert_eq!(program.name, "StringTest");
        assert_eq!(program.block.vars.len(), 1);
        assert_eq!(program.block.statements.len(), 1);
    }

    #[test]
    fn test_parse_character_literals() {
        let source = r#"
program CharTest;

var
  ch1, ch2, ch3: char;

begin
  ch1 := 'A';
  ch2 := '\n';
  ch3 := #65;
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse character literals");
        
        let program = result.unwrap();
        assert_eq!(program.name, "CharTest");
        assert_eq!(program.block.vars.len(), 3);
    }

    #[test]
    fn test_parse_real_numbers() {
        let source = r#"
program RealTest;

var
  r1, r2, r3: real;

begin
  r1 := 3.14159;
  r2 := 1.23e-4;
  r3 := 2.5E+3;
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse real numbers");
        
        let program = result.unwrap();
        assert_eq!(program.name, "RealTest");
        assert_eq!(program.block.vars.len(), 3);
    }

    #[test]
    fn test_parse_preprocessor_directives() {
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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should handle preprocessor directives");
        
        let program = result.unwrap();
        assert_eq!(program.name, "PreprocessorTest");
    }

    #[test]
    fn test_parse_complex_expressions() {
        let source = r#"
program ComplexExpr;

var
  a, b, c: integer;
  flag: boolean;

begin
  a := 10;
  b := 5;
  c := a + b * 2;
  flag := (a > b) and (c < 20);
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse complex expressions");
        
        let program = result.unwrap();
        assert_eq!(program.name, "ComplexExpr");
        assert_eq!(program.block.vars.len(), 4);
        assert_eq!(program.block.statements.len(), 4);
    }

    #[test]
    fn test_parse_nested_blocks() {
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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse nested blocks");
        
        let program = result.unwrap();
        assert_eq!(program.name, "NestedBlocks");
        assert_eq!(program.block.vars.len(), 2);
        assert_eq!(program.block.statements.len(), 3);
    }

    #[test]
    fn test_parse_while_loops() {
        let source = r#"
program WhileTest;

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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse while loops");
        
        let program = result.unwrap();
        assert_eq!(program.name, "WhileTest");
        assert_eq!(program.block.vars.len(), 2);
        assert_eq!(program.block.statements.len(), 3);
    }

    #[test]
    fn test_parse_for_loops() {
        let source = r#"
program ForTest;

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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse for loops");
        
        let program = result.unwrap();
        assert_eq!(program.name, "ForTest");
        assert_eq!(program.block.vars.len(), 2);
        assert_eq!(program.block.statements.len(), 2);
    }

    #[test]
    fn test_parse_case_statements() {
        let source = r#"
program CaseTest;

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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse case statements");
        
        let program = result.unwrap();
        assert_eq!(program.name, "CaseTest");
        assert_eq!(program.block.vars.len(), 2);
        assert_eq!(program.block.statements.len(), 2);
    }

    #[test]
    fn test_parse_boolean_logic() {
        let source = r#"
program BooleanTest;

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
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse boolean logic");
        
        let program = result.unwrap();
        assert_eq!(program.name, "BooleanTest");
        assert_eq!(program.block.vars.len(), 6);
        assert_eq!(program.block.statements.len(), 6);
    }

    #[test]
    fn test_parse_arithmetic_operations() {
        let source = r#"
program ArithmeticTest;

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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse arithmetic operations");
        
        let program = result.unwrap();
        assert_eq!(program.name, "ArithmeticTest");
        assert_eq!(program.block.vars.len(), 4);
        assert_eq!(program.block.statements.len(), 9);
    }

    #[test]
    fn test_parse_bitwise_operations() {
        let source = r#"
program BitwiseTest;

var
  a, b, c: integer;

begin
  a := 5;
  b := 3;
  
  c := a and b;
  c := a or b;
  c := a xor b;
  c := a shl 2;
  c := a shr 1;
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse bitwise operations");
        
        let program = result.unwrap();
        assert_eq!(program.name, "BitwiseTest");
        assert_eq!(program.block.vars.len(), 3);
        assert_eq!(program.block.statements.len(), 7);
    }

    #[test]
    fn test_parse_comparison_operations() {
        let source = r#"
program ComparisonTest;

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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse comparison operations");
        
        let program = result.unwrap();
        assert_eq!(program.name, "ComparisonTest");
        assert_eq!(program.block.vars.len(), 6);
        assert_eq!(program.block.statements.len(), 6);
    }

    #[test]
    fn test_parse_constant_declarations() {
        let source = r#"
program ConstantTest;

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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse constant declarations");
        
        let program = result.unwrap();
        assert_eq!(program.name, "ConstantTest");
        assert_eq!(program.block.consts.len(), 3);
        assert_eq!(program.block.vars.len(), 3);
        assert_eq!(program.block.statements.len(), 3);
    }

    #[test]
    fn test_parse_type_declarations() {
        let source = r#"
program TypeTest;

type
  Age = 0..120;
  Color = (Red, Green, Blue);
  Person = record
    name: string;
    age: Age;
    favorite_color: Color;
  end;

var
  p: Person;
  a: Age;
  c: Color;

begin
  a := 25;
  c := Red;
  p.name := 'John';
  p.age := a;
  p.favorite_color := c;
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse type declarations");
        
        let program = result.unwrap();
        assert_eq!(program.name, "TypeTest");
        assert_eq!(program.block.types.len(), 3);
        assert_eq!(program.block.vars.len(), 3);
    }

    #[test]
    fn test_parse_procedure_declarations() {
        let source = r#"
program ProcedureTest;

procedure PrintMessage(msg: string);
begin
  // In a real implementation, this would print the message
end;

var
  message: string;

begin
  message := 'Hello World';
  PrintMessage(message);
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse procedure declarations");
        
        let program = result.unwrap();
        assert_eq!(program.name, "ProcedureTest");
        assert_eq!(program.block.procedures.len(), 1);
        assert_eq!(program.block.vars.len(), 1);
        assert_eq!(program.block.statements.len(), 2);
    }

    #[test]
    fn test_parse_function_declarations() {
        let source = r#"
program FunctionTest;

function Add(a, b: integer): integer;
begin
  Result := a + b;
end;

var
  x, y, sum: integer;

begin
  x := 10;
  y := 20;
  sum := Add(x, y);
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse function declarations");
        
        let program = result.unwrap();
        assert_eq!(program.name, "FunctionTest");
        assert_eq!(program.block.functions.len(), 1);
        assert_eq!(program.block.vars.len(), 3);
        assert_eq!(program.block.statements.len(), 3);
    }

    #[test]
    fn test_parse_complex_nested_structures() {
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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse complex nested structures");
        
        let program = result.unwrap();
        assert_eq!(program.name, "ComplexNested");
        assert_eq!(program.block.vars.len(), 4);
        assert_eq!(program.block.statements.len(), 4);
    }

    #[test]
    fn test_parse_error_handling() {
        let source = r#"
program ErrorTest;

var
  x: integer;

begin
  x := 10;
  x := "string";  // This should cause a type error
end.
"#;

        let result = parse_program(source);
        // The parser might not catch this error, but the type checker should
        // For now, we'll just test that it parses (syntax is correct)
        assert!(result.is_ok(), "Should parse syntax even with type errors");
    }

    #[test]
    fn test_parse_undefined_variable() {
        let source = r#"
program UndefinedVar;

begin
  x := 10;  // x is not declared
end.
"#;

        let result = parse_program(source);
        // The parser might not catch this error, but the symbol table should
        // For now, we'll just test that it parses (syntax is correct)
        assert!(result.is_ok(), "Should parse syntax even with undefined variables");
    }

    #[test]
    fn test_parse_missing_semicolon() {
        let source = r#"
program MissingSemicolon;

var
  x: integer

begin
  x := 10
end.
"#;

        let result = parse_program(source);
        // This should fail due to missing semicolon
        assert!(result.is_err(), "Missing semicolon should cause parse error");
    }

    #[test]
    fn test_parse_empty_program() {
        let source = r#"
program EmptyProgram;

begin
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse empty program");
        
        let program = result.unwrap();
        assert_eq!(program.name, "EmptyProgram");
        assert_eq!(program.block.statements.len(), 0);
    }

    #[test]
    fn test_parse_single_statement() {
        let source = r#"
program SingleStatement;

var
  x: integer;

begin
  x := 42;
end.
"#;

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse single statement");
        
        let program = result.unwrap();
        assert_eq!(program.name, "SingleStatement");
        assert_eq!(program.block.vars.len(), 1);
        assert_eq!(program.block.statements.len(), 1);
    }

    #[test]
    fn test_parse_multiple_variables() {
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

        let result = parse_program(source);
        assert!(result.is_ok(), "Should parse multiple variables");
        
        let program = result.unwrap();
        assert_eq!(program.name, "MultipleVars");
        assert_eq!(program.block.vars.len(), 7);
        assert_eq!(program.block.statements.len(), 7);
    }
}
