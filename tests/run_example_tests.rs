//! Integration tests that parse and interpret example Pascal programs
//! These validate the full pipeline: source -> lexer -> parser -> interpreter

use pascal::interpreter::Interpreter;
use pascal::parser::Parser;

#[allow(dead_code)]
fn run_example(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    let program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).map_err(|e| format!("Runtime error: {:?}", e))?;
    Ok(())
}

fn parse_only(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;
    Ok(())
}

// ========== 01: Basics ==========

#[test]
fn test_example_01_basics_parse() {
    let source = include_str!("../examples/01_basics.pas");
    parse_only(source).unwrap();
}

#[test]
fn test_example_01_basics_run() {
    let source = r#"
program Basics;
var
  x, y, z: integer;
  flag: boolean;
begin
  x := 10;
  y := 25;
  z := x + y * 2;

  flag := (x > 5) and (y < 30);

  if flag then
    z := z + 100
  else
    z := z - 100;

  while x > 0 do
    x := x - 1;

  for y := 1 to 10 do
    z := z + y;

  x := 5;
  repeat
    x := x - 1;
  until x = 0;

  x := 3;
  case x of
    1: z := 100;
    2: z := 200;
    3: z := 300;
  else
    z := 0;
  end;
end.
"#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().unwrap();
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).unwrap();

    // z should be 300 from case statement
    assert_eq!(
        interp.get_variable_value("z"),
        Some(pascal::interpreter::Value::Integer(300))
    );
    // x should be 3 (set before case)
    assert_eq!(
        interp.get_variable_value("x"),
        Some(pascal::interpreter::Value::Integer(3))
    );
}

// ========== 02: Functions ==========

#[test]
fn test_example_02_functions_parse() {
    let source = include_str!("../examples/02_functions.pas");
    parse_only(source).unwrap();
}

#[test]
fn test_example_02_factorial_and_fibonacci() {
    let source = r#"
program FuncTest;
var
  f, fib: integer;

function Factorial(n: integer): integer;
var
  i, acc: integer;
begin
  acc := 1;
  for i := 2 to n do
    acc := acc * i;
  Factorial := acc;
end;

function Fibonacci(n: integer): integer;
var
  a, b, temp, i: integer;
begin
  if n <= 1 then
  begin
    Fibonacci := n;
    exit;
  end;
  a := 0;
  b := 1;
  for i := 2 to n do
  begin
    temp := a + b;
    a := b;
    b := temp;
  end;
  Fibonacci := b;
end;

begin
  f := Factorial(10);
  fib := Fibonacci(10);
end.
"#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().unwrap();
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).unwrap();

    assert_eq!(
        interp.get_variable_value("f"),
        Some(pascal::interpreter::Value::Integer(3628800))
    );
    assert_eq!(
        interp.get_variable_value("fib"),
        Some(pascal::interpreter::Value::Integer(55))
    );
}

#[test]
fn test_example_02_gcd() {
    let source = r#"
program GCDTest;
var
  g: integer;

function GCD(a, b: integer): integer;
begin
  while b <> 0 do
  begin
    result := b;
    b := a mod b;
    a := result;
  end;
  GCD := a;
end;

begin
  g := GCD(48, 18);
end.
"#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().unwrap();
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).unwrap();

    assert_eq!(
        interp.get_variable_value("g"),
        Some(pascal::interpreter::Value::Integer(6))
    );
}

// ========== 03: Strings ==========

#[test]
fn test_example_03_strings_parse() {
    let source = include_str!("../examples/03_strings.pas");
    parse_only(source).unwrap();
}

#[test]
fn test_example_03_string_ops() {
    let source = r#"
program StringTest;
var
  s, t: string;
  p, len: integer;
  ch: char;
begin
  s := 'Hello, World!';
  len := length(s);
  t := copy(s, 1, 5);
  p := pos('World', s);
  ch := s[1];
end.
"#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().unwrap();
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).unwrap();

    assert_eq!(
        interp.get_variable_value("len"),
        Some(pascal::interpreter::Value::Integer(13))
    );
    assert_eq!(
        interp.get_variable_value("t"),
        Some(pascal::interpreter::Value::String("Hello".to_string()))
    );
    assert_eq!(
        interp.get_variable_value("p"),
        Some(pascal::interpreter::Value::Integer(8))
    );
    assert_eq!(
        interp.get_variable_value("ch"),
        Some(pascal::interpreter::Value::Char('H'))
    );
}

// ========== 04: Arrays ==========

#[test]
fn test_example_04_arrays_parse() {
    let source = include_str!("../examples/04_arrays.pas");
    parse_only(source).unwrap();
}

// ========== 05: Classes ==========

#[test]
fn test_example_05_classes_parse() {
    let source = include_str!("../examples/05_classes.pas");
    parse_only(source).unwrap();
}

// ========== 06: Exceptions ==========

#[test]
fn test_example_06_exceptions_parse() {
    let source = include_str!("../examples/06_exceptions.pas");
    parse_only(source).unwrap();
}

// ========== 07: Nested Functions ==========

#[test]
fn test_example_07_nested_parse() {
    let source = include_str!("../examples/07_nested_functions.pas");
    parse_only(source).unwrap();
}

// ========== 08: Math Algorithms ==========

#[test]
fn test_example_08_math_parse() {
    let source = include_str!("../examples/08_math_algorithms.pas");
    parse_only(source).unwrap();
}

#[test]
fn test_example_08_math_algorithms_run() {
    let source = r#"
program MathTest;
var
  g, c, ds, pw: integer;

function GCD(a, b: integer): integer;
begin
  while b <> 0 do
  begin
    result := b;
    b := a mod b;
    a := result;
  end;
  GCD := a;
end;

function Collatz(n: integer): integer;
var
  steps: integer;
begin
  steps := 0;
  while n <> 1 do
  begin
    if n mod 2 = 0 then
      n := n div 2
    else
      n := 3 * n + 1;
    inc(steps);
  end;
  Collatz := steps;
end;

function DigitSum(n: integer): integer;
var
  sum: integer;
begin
  sum := 0;
  if n < 0 then n := -n;
  while n > 0 do
  begin
    sum := sum + (n mod 10);
    n := n div 10;
  end;
  DigitSum := sum;
end;

function IntPow(base, exp: integer): integer;
var
  result: integer;
begin
  result := 1;
  while exp > 0 do
  begin
    if exp mod 2 = 1 then
      result := result * base;
    base := base * base;
    exp := exp div 2;
  end;
  IntPow := result;
end;

begin
  g := GCD(48, 18);
  c := Collatz(27);
  ds := DigitSum(12345);
  pw := IntPow(2, 16);
end.
"#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().unwrap();
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).unwrap();

    assert_eq!(
        interp.get_variable_value("g"),
        Some(pascal::interpreter::Value::Integer(6))
    );
    assert_eq!(
        interp.get_variable_value("c"),
        Some(pascal::interpreter::Value::Integer(111))
    );
    assert_eq!(
        interp.get_variable_value("ds"),
        Some(pascal::interpreter::Value::Integer(15))
    );
    assert_eq!(
        interp.get_variable_value("pw"),
        Some(pascal::interpreter::Value::Integer(65536))
    );
}

// ========== 09: Class Hierarchy ==========

#[test]
fn test_example_09_class_hierarchy_parse() {
    let source = include_str!("../examples/09_class_hierarchy.pas");
    parse_only(source).unwrap();
}

// ========== 10: Comprehensive ==========

#[test]
fn test_example_10_comprehensive_parse() {
    let source = include_str!("../examples/10_comprehensive.pas");
    parse_only(source).unwrap();
}

// ========== Debug: exit inside if block ==========

#[test]
fn test_exit_in_if_block() {
    let source = r#"
program ExitTest;
var r: boolean;
function Check(n: integer): boolean;
begin
  if n < 5 then
  begin
    Check := false;
    exit;
  end;
  Check := true;
end;
begin
  r := Check(3);
end.
"#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().unwrap();
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).unwrap();
    assert_eq!(
        interp.get_variable_value("r"),
        Some(pascal::interpreter::Value::Boolean(false))
    );
}

// ========== Cross-cutting: IsPrime via full pipeline ==========

#[test]
fn test_isprime_full_pipeline() {
    let source = r#"
program PrimeTest;
var
  count, i: integer;

function IsPrime(n: integer): boolean;
var
  i: integer;
begin
  if n < 2 then
  begin
    IsPrime := false;
    exit;
  end;
  i := 2;
  while i * i <= n do
  begin
    if n mod i = 0 then
    begin
      IsPrime := false;
      exit;
    end;
    inc(i);
  end;
  IsPrime := true;
end;

begin
  count := 0;
  for i := 2 to 100 do
    if IsPrime(i) then
      inc(count);
end.
"#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().unwrap();
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).unwrap();

    // There are 25 primes below 100
    assert_eq!(
        interp.get_variable_value("count"),
        Some(pascal::interpreter::Value::Integer(25))
    );
}

// ========== Cross-cutting: Recursive Fibonacci ==========

#[test]
fn test_recursive_fibonacci() {
    let source = r#"
program RecFib;
var
  f: integer;

function Fib(n: integer): integer;
begin
  if n <= 1 then
    Fib := n
  else
    Fib := Fib(n - 1) + Fib(n - 2);
end;

begin
  f := Fib(15);
end.
"#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().unwrap();
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).unwrap();

    assert_eq!(
        interp.get_variable_value("f"),
        Some(pascal::interpreter::Value::Integer(610))
    );
}

// ========== Cross-cutting: Nested control flow ==========

#[test]
fn test_nested_control_flow() {
    let source = r#"
program NestedControl;
var
  i, j, count: integer;
begin
  count := 0;
  for i := 1 to 10 do
  begin
    for j := 1 to 10 do
    begin
      if (i + j) mod 3 = 0 then
        inc(count);
    end;
  end;
end.
"#;
    let mut parser = Parser::new(source);
    let program = parser.parse_program().unwrap();
    let mut interp = Interpreter::new(false);
    interp.run_program(&program).unwrap();

    // Count pairs (i,j) in 1..10 x 1..10 where (i+j) mod 3 = 0
    // i+j ranges from 2..20, divisible by 3: 3,6,9,12,15,18
    // For each sum s, number of pairs = min(s-1, 10) - max(1, s-10) + 1
    // s=3: 2, s=6: 5, s=9: 8, s=12: 9, s=15: 6, s=18: 3 => total = 33
    assert_eq!(
        interp.get_variable_value("count"),
        Some(pascal::interpreter::Value::Integer(33))
    );
}
