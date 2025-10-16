use minipas::parser::Parser;
use minipas::codegen::CodeGenerator;

#[cfg(test)]
mod type_tests {
    use super::*;

    fn compile_program(source: &str) -> Result<String, Box<dyn std::error::Error>> {
        let mut parser = Parser::new(source);
        let program = parser.parse_program().map_err(|e| Box::new(e) as Box<dyn std::error::Error>)?;
        
        let mut codegen = CodeGenerator::new();
        let assembly = codegen.generate(&program)?;
        
        Ok(assembly)
    }

    #[test]
    fn test_enum_types() {
        let source = r#"
program EnumTest;

type
  Color = (Red, Green, Blue, Yellow);
  Size = (Small, Medium, Large);

var
  c: Color;
  s: Size;

begin
  c := Red;
  s := Medium;
  
  if c = Red then
  begin
    s := Large;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile enum types");
    }

    #[test]
    fn test_enum_with_custom_values() {
        let source = r#"
program EnumCustomTest;

type
  Status = (Idle = 0, Running = 1, Paused = 2, Stopped = 3);

var
  status: Status;

begin
  status := Idle;
  
  case status of
    Idle: status := Running;
    Running: status := Paused;
    Paused: status := Stopped;
    Stopped: status := Idle;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile enum types with custom values");
    }

    #[test]
    fn test_range_types() {
        let source = r#"
program RangeTest;

type
  Age = 0..120;
  Grade = 'A'..'F';
  Score = 0..100;

var
  age: Age;
  grade: Grade;
  score: Score;

begin
  age := 25;
  grade := 'B';
  score := 85;
  
  if age > 18 then
  begin
    score := score + 10;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile range types");
    }

    #[test]
    fn test_record_types() {
        let source = r#"
program RecordTest;

type
  Person = record
    name: string;
    age: integer;
    is_student: boolean;
  end;

var
  p: Person;

begin
  p.name := "John Doe";
  p.age := 25;
  p.is_student := true;
  
  if p.age > 18 then
  begin
    p.is_student := false;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile record types");
    }

    #[test]
    fn test_packed_records() {
        let source = r#"
program PackedRecordTest;

type
  PackedData = packed record
    flag1: boolean;
    flag2: boolean;
    value: 0..15;
  end;

var
  data: PackedData;

begin
  data.flag1 := true;
  data.flag2 := false;
  data.value := 10;
  
  if data.flag1 then
  begin
    data.value := data.value + 1;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile packed record types");
    }

    #[test]
    fn test_array_types() {
        let source = r#"
program ArrayTest;

type
  Numbers = array[1..10] of integer;
  Names = array[0..4] of string;

var
  nums: Numbers;
  names: Names;
  i: integer;

begin
  for i := 1 to 10 do
  begin
    nums[i] := i * 2;
  end;
  
  names[0] := "Alice";
  names[1] := "Bob";
  names[2] := "Charlie";
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile array types");
    }

    #[test]
    fn test_multi_dimensional_arrays() {
        let source = r#"
program MultiDimArrayTest;

type
  Matrix = array[1..3, 1..3] of integer;

var
  m: Matrix;
  i, j: integer;

begin
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    begin
      m[i, j] := i * j;
    end;
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile multi-dimensional array types");
    }

    #[test]
    fn test_set_types() {
        let source = r#"
program SetTest;

type
  Color = (Red, Green, Blue, Yellow);
  ColorSet = set of Color;

var
  colors: ColorSet;
  c: Color;

begin
  colors := [Red, Blue];
  
  if Red in colors then
  begin
    colors := colors + [Green];
  end;
  
  colors := colors - [Blue];
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile set types");
    }

    #[test]
    fn test_pointer_types() {
        let source = r#"
program PointerTest;

type
  Person = record
    name: string;
    age: integer;
  end;
  PersonPtr = ^Person;

var
  p: PersonPtr;
  person: Person;

begin
  New(p);
  p^.name := "John";
  p^.age := 30;
  
  person := p^;
  
  Dispose(p);
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile pointer types");
    }

    #[test]
    fn test_file_types() {
        let source = r#"
program FileTest;

type
  TextFile = file of char;
  DataFile = file of integer;

var
  text: TextFile;
  data: DataFile;

begin
  // File operations would be implemented in runtime
  // For now, just test that the types compile
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile file types");
    }

    #[test]
    fn test_variant_types() {
        let source = r#"
program VariantTest;

type
  Value = variant record
    case integer of
      0: (int_val: integer);
      1: (real_val: real);
      2: (str_val: string);
  end;

var
  v: Value;

begin
  v.int_val := 42;
  
  case v of
    0: v.int_val := v.int_val + 1;
    1: v.real_val := 3.14;
    2: v.str_val := "Hello";
  end;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile variant types");
    }

    #[test]
    fn test_class_types() {
        let source = r#"
program ClassTest;

type
  Animal = class
    name: string;
    age: integer;
    
    procedure Speak;
    function GetInfo: string;
  end;

var
  animal: Animal;

begin
  animal := Animal.Create;
  animal.name := "Buddy";
  animal.age := 5;
  
  animal.Speak;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile class types");
    }

    #[test]
    fn test_interface_types() {
        let source = r#"
program InterfaceTest;

type
  IDrawable = interface
    procedure Draw;
    function GetArea: real;
  end;

  Circle = class(IDrawable)
    radius: real;
    
    procedure Draw;
    function GetArea: real;
  end;

var
  circle: Circle;

begin
  circle := Circle.Create;
  circle.radius := 5.0;
  
  circle.Draw;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile interface types");
    }

    #[test]
    fn test_nested_types() {
        let source = r#"
program NestedTypeTest;

type
  Address = record
    street: string;
    city: string;
    zip: string;
  end;
  
  Person = record
    name: string;
    age: integer;
    address: Address;
  end;
  
  Company = record
    name: string;
    employees: array[1..100] of Person;
  end;

var
  company: Company;

begin
  company.name := "ACME Corp";
  company.employees[1].name := "John Doe";
  company.employees[1].age := 30;
  company.employees[1].address.street := "123 Main St";
  company.employees[1].address.city := "Anytown";
  company.employees[1].address.zip := "12345";
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile nested types");
    }

    #[test]
    fn test_type_aliases() {
        let source = r#"
program TypeAliasTest;

type
  Integer = integer;
  String = string;
  Boolean = boolean;
  
  ID = integer;
  Name = string;
  IsActive = boolean;

var
  id: ID;
  name: Name;
  active: IsActive;

begin
  id := 123;
  name := "Test";
  active := true;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile type aliases");
    }

    #[test]
    fn test_constrained_types() {
        let source = r#"
program ConstrainedTypeTest;

type
  PositiveInt = 1..MaxInt;
  NonNegativeInt = 0..MaxInt;
  UppercaseChar = 'A'..'Z';
  LowercaseChar = 'a'..'z';

var
  pos: PositiveInt;
  non_neg: NonNegativeInt;
  upper: UppercaseChar;
  lower: LowercaseChar;

begin
  pos := 1;
  non_neg := 0;
  upper := 'A';
  lower := 'z';
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile constrained types");
    }

    #[test]
    fn test_generic_types() {
        let source = r#"
program GenericTypeTest;

type
  generic TList<T> = class
    items: array of T;
    count: integer;
    
    procedure Add(item: T);
    function Get(index: integer): T;
  end;

var
  int_list: specialize TList<integer>;
  str_list: specialize TList<string>;

begin
  int_list := specialize TList<integer>.Create;
  str_list := specialize TList<string>.Create;
  
  int_list.Add(42);
  str_list.Add("Hello");
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile generic types");
    }

    #[test]
    fn test_anonymous_types() {
        let source = r#"
program AnonymousTypeTest;

var
  person: record
    name: string;
    age: integer;
  end;
  
  point: record
    x, y: real;
  end;

begin
  person.name := "John";
  person.age := 25;
  
  point.x := 1.0;
  point.y := 2.0;
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile anonymous types");
    }

    #[test]
    fn test_type_compatibility() {
        let source = r#"
program TypeCompatibilityTest;

type
  Int1 = integer;
  Int2 = integer;
  Real1 = real;
  Real2 = real;

var
  i1: Int1;
  i2: Int2;
  r1: Real1;
  r2: Real2;

begin
  i1 := 10;
  i2 := i1;  // Should be compatible
  
  r1 := 3.14;
  r2 := r1;  // Should be compatible
  
  // i1 := r1;  // Should cause type error
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile type compatibility");
    }

    #[test]
    fn test_type_casting() {
        let source = r#"
program TypeCastingTest;

var
  i: integer;
  r: real;
  c: char;
  s: string;

begin
  i := 42;
  r := real(i);  // Integer to real
  
  c := 'A';
  i := integer(c);  // Char to integer
  
  s := string(i);  // Integer to string
end.
"#;

        let result = compile_program(source);
        assert!(result.is_ok(), "Should compile type casting");
    }
}
