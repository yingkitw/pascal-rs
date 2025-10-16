use minipas::parser::Parser;
use minipas::codegen::CodeGenerator;
use minipas::ast::*;

#[cfg(test)]
mod codegen_tests {
    use super::*;

    fn compile_and_get_assembly(source: &str) -> Result<String, Box<dyn std::error::Error>> {
        let mut parser = Parser::new(source);
        let program = parser.parse_program().map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        
        let mut codegen = CodeGenerator::new();
        let assembly = codegen.generate(&program)?;
        
        Ok(assembly)
    }

    #[test]
    fn test_basic_arithmetic_codegen() {
        let source = r#"
program ArithmeticTest;

var
  a, b, c: integer;

begin
  a := 10;
  b := 5;
  c := a + b;
  c := c * 2;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for basic arithmetic");
        
        let assembly = result.unwrap();
        assert!(assembly.contains(".intel_syntax"));
        assert!(assembly.contains("main:"));
        assert!(assembly.contains("mov eax, 10"));
        assert!(assembly.contains("mov eax, 5"));
        assert!(assembly.contains("add eax, edx"));
        assert!(assembly.contains("imul eax, edx"));
    }

    #[test]
    fn test_conditional_codegen() {
        let source = r#"
program ConditionalTest;

var
  x, y: integer;

begin
  x := 15;
  y := 10;
  
  if x > y then
  begin
    x := x - y;
  end
  else
  begin
    y := y - x;
  end;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for conditionals");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("cmp eax, edx"));
        assert!(assembly.contains("jg")); // Jump if greater
        assert!(assembly.contains("sub eax, edx"));
    }

    #[test]
    fn test_loop_codegen() {
        let source = r#"
program LoopTest;

var
  i, sum: integer;

begin
  i := 1;
  sum := 0;
  
  while i <= 5 do
  begin
    sum := sum + i;
    i := i + 1;
  end;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for loops");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("cmp eax, 5"));
        assert!(assembly.contains("jle")); // Jump if less or equal
        assert!(assembly.contains("add eax, edx"));
        assert!(assembly.contains("inc eax"));
    }

    #[test]
    fn test_boolean_logic_codegen() {
        let source = r#"
program BooleanTest;

var
  a, b, c: integer;
  flag1, flag2: boolean;

begin
  a := 10;
  b := 5;
  c := 3;
  
  flag1 := a > b;
  flag2 := b < c;
  
  if flag1 and flag2 then
  begin
    c := a + b;
  end;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for boolean logic");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("cmp eax, edx"));
        assert!(assembly.contains("setg")); // Set if greater
        assert!(assembly.contains("and eax, edx"));
    }

    #[test]
    fn test_bitwise_operations_codegen() {
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

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for bitwise operations");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("and eax, edx"));
        assert!(assembly.contains("or eax, edx"));
        assert!(assembly.contains("xor eax, edx"));
        assert!(assembly.contains("shl eax, 2"));
        assert!(assembly.contains("shr eax, 1"));
    }

    #[test]
    fn test_comparison_operations_codegen() {
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

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for comparison operations");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("cmp eax, edx"));
        assert!(assembly.contains("sete")); // Set if equal
        assert!(assembly.contains("setne")); // Set if not equal
        assert!(assembly.contains("setg")); // Set if greater
        assert!(assembly.contains("setle")); // Set if less or equal
    }

    #[test]
    fn test_arithmetic_operations_codegen() {
        let source = r#"
program ArithmeticOpsTest;

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

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for arithmetic operations");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("add eax, edx"));
        assert!(assembly.contains("sub eax, edx"));
        assert!(assembly.contains("imul eax, edx"));
        assert!(assembly.contains("idiv edx"));
    }

    #[test]
    fn test_nested_blocks_codegen() {
        let source = r#"
program NestedBlocksTest;

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

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for nested blocks");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("cmp eax, 5"));
        assert!(assembly.contains("jg"));
        assert!(assembly.contains("cmp eax, 15"));
        assert!(assembly.contains("jg"));
        assert!(assembly.contains("add eax, edx"));
        assert!(assembly.contains("sub eax, edx"));
    }

    #[test]
    fn test_case_statement_codegen() {
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

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for case statements");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("cmp eax, 1"));
        assert!(assembly.contains("je")); // Jump if equal
        assert!(assembly.contains("cmp eax, 2"));
        assert!(assembly.contains("je"));
        assert!(assembly.contains("cmp eax, 3"));
        assert!(assembly.contains("je"));
    }

    #[test]
    fn test_for_loop_codegen() {
        let source = r#"
program ForLoopTest;

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

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for for loops");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("mov eax, 1")); // Initialize i
        assert!(assembly.contains("cmp eax, 10")); // Compare i with 10
        assert!(assembly.contains("jg")); // Jump if greater (exit loop)
        assert!(assembly.contains("inc eax")); // Increment i
    }

    #[test]
    fn test_complex_expressions_codegen() {
        let source = r#"
program ComplexExprTest;

var
  a, b, c, d: integer;

begin
  a := 10;
  b := 5;
  c := 3;
  d := 2;
  
  a := (a + b) * (c - d);
  b := a div (b + c);
  c := (a * b) + (c * d);
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for complex expressions");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("add eax, edx"));
        assert!(assembly.contains("sub eax, edx"));
        assert!(assembly.contains("imul eax, edx"));
        assert!(assembly.contains("idiv edx"));
    }

    #[test]
    fn test_variable_scope_codegen() {
        let source = r#"
program ScopeTest;

var
  x, y: integer;

begin
  x := 10;
  y := 20;
  
  if x > 5 then
  begin
    x := x + y;
  end;
  
  y := x * 2;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly with proper variable scope");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("mov [rbp - 8], eax")); // Store x
        assert!(assembly.contains("mov [rbp - 16], eax")); // Store y
        assert!(assembly.contains("mov eax, [rbp - 8]")); // Load x
        assert!(assembly.contains("mov eax, [rbp - 16]")); // Load y
    }

    #[test]
    fn test_constant_folding_codegen() {
        let source = r#"
program ConstantFoldingTest;

const
  MAX_SIZE = 100;
  PI = 3;

var
  size: integer;

begin
  size := MAX_SIZE + 50;
  size := size * 2;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly with constant folding");
        
        let assembly = result.unwrap();
        // The compiler should fold MAX_SIZE + 50 to 150
        assert!(assembly.contains("mov eax, 150"));
        assert!(assembly.contains("imul eax, 2"));
    }

    #[test]
    fn test_register_allocation_codegen() {
        let source = r#"
program RegisterTest;

var
  a, b, c, d, e, f: integer;

begin
  a := 1;
  b := 2;
  c := 3;
  d := 4;
  e := 5;
  f := 6;
  
  a := a + b + c + d + e + f;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly with register allocation");
        
        let assembly = result.unwrap();
        // Should use multiple registers efficiently
        assert!(assembly.contains("mov eax, 1"));
        assert!(assembly.contains("mov edx, 2"));
        assert!(assembly.contains("add eax, edx"));
    }

    #[test]
    fn test_stack_frame_codegen() {
        let source = r#"
program StackFrameTest;

var
  x, y, z, w, v: integer;

begin
  x := 1;
  y := 2;
  z := 3;
  w := 4;
  v := 5;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly with proper stack frame");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("push rbp"));
        assert!(assembly.contains("mov rbp, rsp"));
        assert!(assembly.contains("sub rsp, 40")); // Space for 5 integers
        assert!(assembly.contains("mov [rbp - 8], eax")); // Store variables
    }

    #[test]
    fn test_boolean_operations_codegen() {
        let source = r#"
program BooleanOpsTest;

var
  a, b: integer;
  flag1, flag2, flag3: boolean;

begin
  a := 10;
  b := 5;
  
  flag1 := a > b;
  flag2 := b < 3;
  flag3 := flag1 and flag2;
  
  if flag3 then
  begin
    a := a + b;
  end;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for boolean operations");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("cmp eax, edx"));
        assert!(assembly.contains("setg")); // Set if greater
        assert!(assembly.contains("and eax, edx"));
        assert!(assembly.contains("test eax, eax"));
        assert!(assembly.contains("jz")); // Jump if zero
    }

    #[test]
    fn test_division_operations_codegen() {
        let source = r#"
program DivisionTest;

var
  a, b, c: integer;

begin
  a := 20;
  b := 4;
  
  c := a div b;
  c := a mod b;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for division operations");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("cdq")); // Sign extend
        assert!(assembly.contains("idiv edx")); // Integer division
        assert!(assembly.contains("mov eax, edx")); // Move remainder for mod
    }

    #[test]
    fn test_negative_numbers_codegen() {
        let source = r#"
program NegativeTest;

var
  a, b: integer;

begin
  a := -10;
  b := 5;
  
  a := a + b;
  a := a - b;
  a := a * b;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for negative numbers");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("mov eax, -10"));
        assert!(assembly.contains("add eax, edx"));
        assert!(assembly.contains("sub eax, edx"));
        assert!(assembly.contains("imul eax, edx"));
    }

    #[test]
    fn test_large_numbers_codegen() {
        let source = r#"
program LargeNumbersTest;

var
  a, b: integer;

begin
  a := 1000000;
  b := 2000000;
  
  a := a + b;
end.
"#;

        let result = compile_and_get_assembly(source);
        assert!(result.is_ok(), "Should generate assembly for large numbers");
        
        let assembly = result.unwrap();
        assert!(assembly.contains("mov eax, 1000000"));
        assert!(assembly.contains("mov eax, 2000000"));
        assert!(assembly.contains("add eax, edx"));
    }
}
