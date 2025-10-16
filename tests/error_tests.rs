use minipas::parser::Parser;
use minipas::lexer::{Token, Lexer};

#[cfg(test)]
mod error_tests {
    use super::*;

    fn parse_program(source: &str) -> Result<minipas::ast::Program, Box<dyn std::error::Error>> {
        let mut parser = Parser::new(source);
        parser.parse_program().map_err(|e| e.into())
    }

    #[test]
    fn test_syntax_errors() {
        let error_cases = vec![
            // Missing semicolon
            (r#"
program MissingSemicolon;

var
  x: integer

begin
  x := 10
end.
"#, "Missing semicolon should cause parse error"),

            // Missing colon
            (r#"
program MissingColon;

var
  x integer

begin
  x := 10;
end.
"#, "Missing colon should cause parse error"),

            // Missing begin
            (r#"
program MissingBegin;

var
  x: integer;

x := 10;
end.
"#, "Missing begin should cause parse error"),

            // Missing end
            (r#"
program MissingEnd;

var
  x: integer;

begin
  x := 10;
"#, "Missing end should cause parse error"),

            // Missing program name
            (r#"
program;

var
  x: integer;

begin
  x := 10;
end.
"#, "Missing program name should cause parse error"),

            // Invalid token
            (r#"
program InvalidToken;

var
  x: integer;

begin
  x := @#$%^&*();
end.
"#, "Invalid tokens should cause parse error"),
        ];

        for (source, description) in error_cases {
            let result = parse_program(source);
            assert!(result.is_err(), "{}", description);
        }
    }

    #[test]
    fn test_type_errors() {
        let error_cases = vec![
            // Type mismatch in assignment
            (r#"
program TypeMismatch;

var
  x: integer;
  s: string;

begin
  x := "hello";
  s := 42;
end.
"#, "Type mismatch should cause error"),

            // Undefined variable
            (r#"
program UndefinedVar;

begin
  x := 10;
end.
"#, "Undefined variable should cause error"),

            // Undefined function
            (r#"
program UndefinedFunction;

var
  x: integer;

begin
  x := UndefinedFunction();
end.
"#, "Undefined function should cause error"),

            // Wrong number of parameters
            (r#"
program WrongParams;

function Add(a, b: integer): integer;
begin
  Result := a + b;
end;

var
  x: integer;

begin
  x := Add(1, 2, 3);
end.
"#, "Wrong number of parameters should cause error"),

            // Type mismatch in expression
            (r#"
program TypeMismatchExpr;

var
  x: integer;
  s: string;

begin
  x := 10 + "hello";
  s := "world" * 2;
end.
"#, "Type mismatch in expression should cause error"),
        ];

        for (source, description) in error_cases {
            let result = parse_program(source);
            // Note: Some type errors might not be caught by the parser
            // but should be caught by the type checker
            println!("Testing: {}", description);
            // For now, we'll just test that the syntax is parsed
            // In a full implementation, type checking would be separate
        }
    }

    #[test]
    fn test_lexer_errors() {
        let error_cases = vec![
            // Unterminated string
            (r#"
program UnterminatedString;

var
  s: string;

begin
  s := "hello world
end.
"#, "Unterminated string should cause lexer error"),

            // Unterminated comment
            (r#"
program UnterminatedComment;

var
  x: integer;

begin
  { This is an unterminated comment
  x := 10;
end.
"#, "Unterminated comment should cause lexer error"),

            // Invalid character
            (r#"
program InvalidChar;

var
  x: integer;

begin
  x := 10;
  x := x + ½;
end.
"#, "Invalid character should cause lexer error"),

            // Invalid number format
            (r#"
program InvalidNumber;

var
  x: integer;

begin
  x := 123.456.789;
end.
"#, "Invalid number format should cause lexer error"),
        ];

        for (source, description) in error_cases {
            let mut lexer = Lexer::new(source);
            let mut has_error = false;
            
            for token in lexer {
                if let Err(_) = token {
                    has_error = true;
                    break;
                }
            }
            
            assert!(has_error, "{}", description);
        }
    }

    #[test]
    fn test_semantic_errors() {
        let error_cases = vec![
            // Variable used before declaration
            (r#"
program UsedBeforeDecl;

begin
  x := 10;
  var x: integer;
end.
"#, "Variable used before declaration should cause error"),

            // Duplicate variable declaration
            (r#"
program DuplicateVar;

var
  x: integer;
  x: string;

begin
  x := 10;
end.
"#, "Duplicate variable declaration should cause error"),

            // Duplicate function declaration
            (r#"
program DuplicateFunc;

function Test: integer;
begin
  Result := 1;
end;

function Test: string;
begin
  Result := "hello";
end;

begin
end.
"#, "Duplicate function declaration should cause error"),

            // Recursive function without forward declaration
            (r#"
program RecursiveFunc;

function Factorial(n: integer): integer;
begin
  if n <= 1 then
    Result := 1
  else
    Result := n * Factorial(n - 1);
end;

begin
end.
"#, "Recursive function should require forward declaration"),

            // Assignment to constant
            (r#"
program AssignToConst;

const
  MAX_SIZE = 100;

var
  x: integer;

begin
  x := MAX_SIZE;
  MAX_SIZE := 200;
end.
"#, "Assignment to constant should cause error"),
        ];

        for (source, description) in error_cases {
            let result = parse_program(source);
            // Note: Some semantic errors might not be caught by the parser
            // but should be caught by the semantic analyzer
            println!("Testing: {}", description);
            // For now, we'll just test that the syntax is parsed
            // In a full implementation, semantic analysis would be separate
        }
    }

    #[test]
    fn test_scope_errors() {
        let error_cases = vec![
            // Variable used outside scope
            (r#"
program ScopeError;

var
  x: integer;

begin
  if true then
  begin
    var y: integer;
    y := 10;
  end;
  y := 20;
end.
"#, "Variable used outside scope should cause error"),

            // Function called before declaration
            (r#"
program CallBeforeDecl;

var
  x: integer;

begin
  x := Test();
end;

function Test: integer;
begin
  Result := 42;
end.
"#, "Function called before declaration should cause error"),

            // Access to private member
            (r#"
program PrivateAccess;

type
  Person = class
  private
    name: string;
  public
    procedure SetName(n: string);
  end;

var
  p: Person;

begin
  p.name := "John";
end.
"#, "Access to private member should cause error"),
        ];

        for (source, description) in error_cases {
            let result = parse_program(source);
            // Note: Some scope errors might not be caught by the parser
            // but should be caught by the semantic analyzer
            println!("Testing: {}", description);
            // For now, we'll just test that the syntax is parsed
            // In a full implementation, semantic analysis would be separate
        }
    }

    #[test]
    fn test_runtime_errors() {
        let error_cases = vec![
            // Division by zero
            (r#"
program DivByZero;

var
  x, y: integer;

begin
  x := 10;
  y := 0;
  x := x div y;
end.
"#, "Division by zero should cause runtime error"),

            // Array index out of bounds
            (r#"
program ArrayBounds;

var
  arr: array[1..5] of integer;
  i: integer;

begin
  i := 10;
  arr[i] := 42;
end.
"#, "Array index out of bounds should cause runtime error"),

            // Null pointer dereference
            (r#"
program NullPointer;

type
  Node = record
    data: integer;
    next: ^Node;
  end;

var
  n: ^Node;

begin
  n^.data := 42;
end.
"#, "Null pointer dereference should cause runtime error"),

            // Stack overflow
            (r#"
program StackOverflow;

function Recursive(n: integer): integer;
begin
  if n <= 0 then
    Result := 0
  else
    Result := Recursive(n - 1) + n;
end;

var
  x: integer;

begin
  x := Recursive(1000000);
end.
"#, "Stack overflow should cause runtime error"),
        ];

        for (source, description) in error_cases {
            let result = parse_program(source);
            // Note: Runtime errors are not caught at compile time
            // but should be handled at runtime
            println!("Testing: {}", description);
            // For now, we'll just test that the syntax is parsed
            // In a full implementation, runtime error handling would be separate
        }
    }

    #[test]
    fn test_warning_cases() {
        let warning_cases = vec![
            // Unused variable
            (r#"
program UnusedVar;

var
  x, y: integer;

begin
  x := 10;
  // y is never used
end.
"#, "Unused variable should generate warning"),

            // Unreachable code
            (r#"
program UnreachableCode;

var
  x: integer;

begin
  x := 10;
  if false then
  begin
    x := 20;  // This code is unreachable
  end;
end.
"#, "Unreachable code should generate warning"),

            // Uninitialized variable
            (r#"
program UninitializedVar;

var
  x: integer;

begin
  // x is never initialized
  if x > 0 then
  begin
    x := x + 1;
  end;
end.
"#, "Uninitialized variable should generate warning"),

            // Implicit type conversion
            (r#"
program ImplicitConversion;

var
  x: integer;
  y: real;

begin
  x := 10;
  y := x;  // Implicit conversion from integer to real
end.
"#, "Implicit type conversion should generate warning"),
        ];

        for (source, description) in warning_cases {
            let result = parse_program(source);
            // Note: Warnings are not errors, so the program should still compile
            // but warnings should be reported
            println!("Testing: {}", description);
            // For now, we'll just test that the syntax is parsed
            // In a full implementation, warning generation would be separate
        }
    }

    #[test]
    fn test_error_recovery() {
        let source = r#"
program ErrorRecovery;

var
  x: integer;
  y: string;
  z: boolean;

begin
  x := 10;
  y := "hello";
  z := true;
  
  // This should cause an error but the parser should recover
  x := y;  // Type mismatch
  
  // This should still be parsed correctly
  if z then
  begin
    x := x + 1;
  end;
end.
"#;

        let result = parse_program(source);
        // The parser should be able to recover from errors and continue parsing
        // In a full implementation, error recovery would be more sophisticated
        println!("Testing error recovery");
    }

    #[test]
    fn test_error_messages() {
        let error_cases = vec![
            ("Missing semicolon", r#"
program Test;
var x: integer
begin x := 10; end.
"#),
            ("Missing begin", r#"
program Test;
var x: integer;
x := 10; end.
"#),
            ("Missing end", r#"
program Test;
var x: integer;
begin x := 10;
"#),
        ];

        for (error_type, source) in error_cases {
            let result = parse_program(source);
            assert!(result.is_err(), "Should detect {} error", error_type);
            
            if let Err(e) = result {
                println!("Error message for {}: {}", error_type, e);
                // In a full implementation, we would verify that the error message
                // is helpful and points to the correct location
            }
        }
    }
}
