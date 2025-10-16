program AdvancedFeatures;

{$mode objfpc}
{$h+}

var
  message: string;
  ch: char;
  number: real;
  flag: boolean;

begin
  // String literals with escape sequences
  message := "Hello\nWorld\tTest";
  ch := 'A';
  ch := #65;  // Same as 'A'
  ch := '\n'; // Newline character
  
  // Real numbers
  number := 3.14159;
  number := 1.23e-4;
  
  // Boolean expressions
  flag := true and false;
  flag := (number > 0.0) or (ch = 'A');
  
  // Preprocessor directives are handled by lexer
  // but not yet processed by parser
end.
