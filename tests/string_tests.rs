use minipas::lexer::{Token, Lexer};
use minipas::parser::Parser;
use minipas::codegen::CodeGenerator;

#[cfg(test)]
mod string_tests {
    use super::*;

    fn compile_program(source: &str) -> Result<String, Box<dyn std::error::Error>> {
        let mut parser = Parser::new(source);
        let program = parser.parse_program().map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        
        let mut codegen = CodeGenerator::new();
        let assembly = codegen.generate(&program)?;
        
        Ok(assembly)
    }

    #[test]
    fn test_basic_string_literals() {
        let source = r#"
program StringBasic;

var
  s: string;

begin
  s := "Hello World";
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile basic string literals");
    }

    #[test]
    fn test_string_escape_sequences() {
        let test_cases = vec![
            (r#""Hello\nWorld""#, "Hello\nWorld"),
            (r#""Tab\tSeparated""#, "Tab\tSeparated"),
            (r#""Quote\"Test""#, "Quote\"Test"),
            (r#""Backslash\\Test""#, "Backslash\\Test"),
            (r#""Null\0Terminated""#, "Null\0Terminated"),
            (r#""Carriage\rReturn""#, "Carriage\rReturn"),
        ];

        for (source, expected) in test_cases {
            let full_source = format!(r#"
program StringEscape;

var
  s: string;

begin
  s := {};
end.
"#, source);

            let result = compile_program(&full_source);
            assert!(result.is_ok(), "Should compile string with escape sequence: {}", source);
        }
    }

    #[test]
    fn test_character_literals() {
        let test_cases = vec![
            ("'A'", 'A'),
            ("'\\n'", '\n'),
            ("'\\t'", '\t'),
            ("'\\0'", '\0'),
            ("'\\\\'", '\\'),
            ("'\\''", '\''),
            ("'\\\"'", '"'),
        ];

        for (source, expected) in test_cases {
            let full_source = format!(r#"
program CharTest;

var
  ch: char;

begin
  ch := {};
end.
"#, source);

            let result = compile_program(&full_source);
            assert!(result.is_ok(), "Should compile character literal: {}", source);
        }
    }

    #[test]
    fn test_numeric_character_codes() {
        let test_cases = vec![
            ("#65", 'A'),
            ("#10", '\n'),
            ("#13", '\r'),
            ("#9", '\t'),
            ("#0", '\0'),
            ("#32", ' '),
            ("#97", 'a'),
        ];

        for (source, expected) in test_cases {
            let full_source = format!(r#"
program CharCodeTest;

var
  ch: char;

begin
  ch := {};
end.
"#, source);

            let result = compile_program(&full_source);
            assert!(result.is_ok(), "Should compile numeric character code: {}", source);
        }
    }

    #[test]
    fn test_string_concatenation() {
        let source = r#"
program StringConcat;

var
  s1, s2, s3: string;

begin
  s1 := "Hello";
  s2 := "World";
  s3 := s1 + " " + s2;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string concatenation");
    }

    #[test]
    fn test_string_comparison() {
        let source = r#"
program StringCompare;

var
  s1, s2: string;
  equal: boolean;

begin
  s1 := "Hello";
  s2 := "World";
  equal := s1 = s2;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string comparison");
    }

    #[test]
    fn test_string_length() {
        let source = r#"
program StringLength;

var
  s: string;
  len: integer;

begin
  s := "Hello World";
  len := Length(s);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string length operation");
    }

    #[test]
    fn test_string_substring() {
        let source = r#"
program StringSubstring;

var
  s, sub: string;

begin
  s := "Hello World";
  sub := Copy(s, 1, 5);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string substring operation");
    }

    #[test]
    fn test_string_uppercase_lowercase() {
        let source = r#"
program StringCase;

var
  s, upper, lower: string;

begin
  s := "Hello World";
  upper := UpCase(s);
  lower := LowerCase(s);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string case operations");
    }

    #[test]
    fn test_string_trim() {
        let source = r#"
program StringTrim;

var
  s, trimmed: string;

begin
  s := "  Hello World  ";
  trimmed := Trim(s);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string trim operation");
    }

    #[test]
    fn test_string_pos() {
        let source = r#"
program StringPos;

var
  s, sub: string;
  pos: integer;

begin
  s := "Hello World";
  sub := "World";
  pos := Pos(sub, s);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string position operation");
    }

    #[test]
    fn test_string_replace() {
        let source = r#"
program StringReplace;

var
  s, old, new, result: string;

begin
  s := "Hello World";
  old := "World";
  new := "Universe";
  result := StringReplace(s, old, new);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string replace operation");
    }

    #[test]
    fn test_string_format() {
        let source = r#"
program StringFormat;

var
  s: string;
  num: integer;

begin
  num := 42;
  s := Format("The number is %d", num);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string format operation");
    }

    #[test]
    fn test_string_to_number_conversion() {
        let source = r#"
program StringToNumber;

var
  s: string;
  num: integer;
  real_num: real;

begin
  s := "123";
  num := StrToInt(s);
  real_num := StrToFloat(s);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string to number conversion");
    }

    #[test]
    fn test_number_to_string_conversion() {
        let source = r#"
program NumberToString;

var
  s1, s2: string;
  num: integer;
  real_num: real;

begin
  num := 123;
  real_num := 3.14159;
  s1 := IntToStr(num);
  s2 := FloatToStr(real_num);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile number to string conversion");
    }

    #[test]
    fn test_string_arrays() {
        let source = r#"
program StringArray;

var
  names: array[1..3] of string;
  i: integer;

begin
  names[1] := "Alice";
  names[2] := "Bob";
  names[3] := "Charlie";
  
  for i := 1 to 3 do
  begin
    // Process names[i]
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string arrays");
    }

    #[test]
    fn test_string_records() {
        let source = r#"
program StringRecord;

type
  Person = record
    name: string;
    email: string;
  end;

var
  p: Person;

begin
  p.name := "John Doe";
  p.email := "john@example.com";
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string records");
    }

    #[test]
    fn test_string_functions() {
        let source = r#"
program StringFunctions;

function GetGreeting(name: string): string;
begin
  Result := "Hello, " + name + "!";
end;

var
  greeting: string;

begin
  greeting := GetGreeting("World");
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string functions");
    }

    #[test]
    fn test_string_procedures() {
        let source = r#"
program StringProcedures;

procedure PrintMessage(msg: string);
begin
  // In a real implementation, this would print the message
end;

var
  message: string;

begin
  message := "Hello World";
  PrintMessage(message);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string procedures");
    }

    #[test]
    fn test_string_constants() {
        let source = r#"
program StringConstants;

const
  GREETING = "Hello";
  PUNCTUATION = "!";
  SPACE = " ";

var
  message: string;

begin
  message := GREETING + SPACE + "World" + PUNCTUATION;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string constants");
    }

    #[test]
    fn test_string_escape_sequence_combinations() {
        let source = r#"
program StringEscapeCombinations;

var
  s1, s2, s3: string;

begin
  s1 := "Line 1\nLine 2\nLine 3";
  s2 := "Tab\tSeparated\tValues";
  s3 := "Quote\"Test\"String";
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string escape sequence combinations");
    }

    #[test]
    fn test_empty_strings() {
        let source = r#"
program EmptyStrings;

var
  s1, s2: string;

begin
  s1 := "";
  s2 := "Non-empty";
  
  if s1 = "" then
  begin
    s1 := "Now it's not empty";
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile empty strings");
    }

    #[test]
    fn test_string_length_limits() {
        let source = r#"
program StringLengthLimits;

type
  ShortString = string[10];
  LongString = string[255];

var
  short: ShortString;
  long: LongString;

begin
  short := "Short";
  long := "This is a much longer string that should fit in a LongString type";
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile string length limits");
    }
}
