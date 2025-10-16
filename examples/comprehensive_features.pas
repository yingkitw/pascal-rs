program ComprehensiveFeatures;

{$mode objfpc}
{$h+}

type
  // Enumeration type
  Color = (Red, Green, Blue, Yellow);
  
  // Range type
  Age = 0..120;
  
  // Record type
  Person = record
    name: string;
    age: Age;
    favorite_color: Color;
    is_student: boolean;
  end;
  
  // Packed record
  PackedData = packed record
    flag1: boolean;
    flag2: boolean;
    value: 0..15;
  end;
  
  // Array type
  Numbers = array[1..10] of integer;
  
  // Set type
  ColorSet = set of Color;
  
  // Pointer type
  PersonPtr = ^Person;
  
  // String type with length
  ShortString = string[255];

var
  p: Person;
  data: PackedData;
  nums: Numbers;
  colors: ColorSet;
  ptr: PersonPtr;
  msg: ShortString;
  i: integer;
  ch: char;

// Function with parameters
function CalculateSum(a, b: integer): integer;
begin
  Result := a + b;
end;

// Procedure with var parameter
procedure ModifyPerson(var person: Person; new_age: integer);
begin
  person.age := new_age;
end;

// Function with complex return type
function GetPersonInfo(const person: Person): string;
var
  info: string;
begin
  info := 'Name: ' + person.name + ', Age: ';
  // Note: In a real implementation, we'd need string conversion functions
  Result := info;
end;

begin
  // String literals with escape sequences
  msg := 'Hello\nWorld\tTest';
  ch := 'A';
  ch := #65;  // Same as 'A'
  ch := '\n'; // Newline character
  
  // Real numbers
  i := 42;
  
  // Enum usage
  p.favorite_color := Red;
  p.is_student := true;
  p.name := 'John';
  p.age := 25;
  
  // Record access
  if p.age > 18 then
  begin
    p.is_student := false;
  end;
  
  // Array usage
  for i := 1 to 10 do
  begin
    nums[i] := i * 2;
  end;
  
  // Set operations
  colors := [Red, Blue];
  if Red in colors then
  begin
    // Red is in the set
  end;
  
  // Pointer operations
  New(ptr);
  ptr^.name := 'Jane';
  ptr^.age := 30;
  
  // Function calls
  i := CalculateSum(10, 20);
  ModifyPerson(p, 26);
  
  // Case statement
  case p.favorite_color of
    Red: msg := 'Red is chosen';
    Green: msg := 'Green is chosen';
    Blue: msg := 'Blue is chosen';
    Yellow: msg := 'Yellow is chosen';
  end;
  
  // Try-except block (syntax only, not yet implemented)
  try
    i := 100 div 0; // This would cause division by zero
  except
    on EDivByZero do
    begin
      msg := 'Division by zero error';
    end;
  end;
  
  // With statement (syntax only, not yet implemented)
  with p do
  begin
    name := 'Modified';
    age := age + 1;
  end;
  
  // Clean up
  Dispose(ptr);
end.
