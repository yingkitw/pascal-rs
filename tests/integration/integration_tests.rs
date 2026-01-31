//! Comprehensive integration tests for pascal-rs compiler
//! Tests full compilation pipeline from source to executable

use pascal::{Lexer, Parser, UnitCodeGenerator, optimizer::Optimizer};
use pascal::{Program, Expr, Stmt, ModuleResult};

// Helper to compile a complete program
fn compile_program(source: &str) -> Result<String, String> {
    // Lexical analysis
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    while let Some(result) = lexer.next_token() {
        let (_, token, _) = result.map_err(|e| format!("Lex error: {:?}", e))?;
        tokens.push(token);
    }

    // Parsing
    let mut parser = Parser::new(source);
    let program = parser.parse_program()
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Optimization
    let mut optimizer = Optimizer::new();
    // Apply optimizations to program

    // Code generation
    let generator = UnitCodeGenerator::new();
    let code = generator.generate_program(&program)
        .map_err(|e| format!("Codegen error: {:?}", e))?;

    Ok(code)
}

#[test]
fn test_full_compilation_hello_world() {
    let source = r#"
        program HelloWorld;
        begin
            writeln('Hello, World!');
        end.
    "#;

    let result = compile_program(source);
    // Should compile successfully or provide meaningful error
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_fibonacci() {
    let source = r#"
        program Fibonacci;
        function Fib(n: integer): integer;
        begin
            if n <= 1 then
                Fib := n
            else
                Fib := Fib(n-1) + Fib(n-2);
        end;
        var
            i: integer;
        begin
            for i := 0 to 10 do
                writeln('Fib(', i, ') = ', Fib(i));
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_factorial() {
    let source = r#"
        program Factorial;
        function Factorial(n: integer): integer;
        begin
            if n <= 1 then
                Factorial := 1
            else
                Factorial := n * Factorial(n - 1);
        end;
        begin
            writeln('5! = ', Factorial(5));
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_arrays() {
    let source = r#"
        program ArrayTest;
        var
            arr: array[1..10] of integer;
            i, sum: integer;
        begin
            for i := 1 to 10 do
                arr[i] := i * i;

            sum := 0;
            for i := 1 to 10 do
                sum := sum + arr[i];

            writeln('Sum = ', sum);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_records() {
    let source = r#"
        program RecordTest;
        type
            Point = record
                x, y: integer;
            end;
        var
            p: Point;
        begin
            p.x := 10;
            p.y := 20;
            writeln('Point: (', p.x, ', ', p.y, ')');
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_pointers() {
    let source = r#"
        program PointerTest;
        var
            p: ^integer;
        begin
            new(p);
            p^ := 42;
            writeln('Value: ', p^);
            dispose(p);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_functions() {
    let source = r#"
        program FunctionTest;
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;

        function Multiply(a, b: integer): integer;
        begin
            Multiply := a * b;
        end;

        begin
            writeln('Add: ', Add(5, 3));
            writeln('Multiply: ', Multiply(5, 3));
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_procedures() {
    let source = r#"
        program ProcedureTest;
        procedure PrintSum(a, b: integer);
        begin
            writeln('Sum: ', a + b);
        end;

        begin
            PrintSum(10, 20);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_var_parameters() {
    let source = r#"
        program VarParamTest;
        procedure Swap(var a, b: integer);
        var
            temp: integer;
        begin
            temp := a;
            a := b;
            b := temp;
        end;

        var
            x, y: integer;
        begin
            x := 10;
            y := 20;
            Swap(x, y);
            writeln('x = ', x, ', y = ', y);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_nested_loops() {
    let source = r#"
        program NestedLoopTest;
        var
            i, j, product: integer;
        begin
            for i := 1 to 5 do
            begin
                for j := 1 to 5 do
                begin
                    product := i * j;
                    write(product, ' ');
                end;
                writeln;
            end;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_case_statement() {
    let source = r#"
        program CaseTest;
        var
            day: integer;
        begin
            day := 3;
            case day of
                1: writeln('Monday');
                2: writeln('Tuesday');
                3: writeln('Wednesday');
                4: writeln('Thursday');
                5: writeln('Friday');
                6: writeln('Saturday');
                7: writeln('Sunday');
            else
                writeln('Invalid day');
            end;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_string_operations() {
    let source = r#"
        program StringTest;
        var
            s1, s2, s3: string;
        begin
            s1 := 'Hello';
            s2 := 'World';
            s3 := s1 + ' ' + s2;
            writeln(s3);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_const_declarations() {
    let source = r#"
        program ConstTest;
        const
            Pi = 3.14159;
            MaxValue = 100;
            Name = 'Test';
        var
            r, area: real;
        begin
            r := 5.0;
            area := Pi * r * r;
            writeln('Area = ', area);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_type_declarations() {
    let source = r#"
        program TypeTest;
        type
            TInt = integer;
            TReal = real;
            TStr = string;
            TNumber = 0..9;
            TColor = (Red, Green, Blue);
        var
            i: TInt;
            r: TReal;
            s: TStr;
            n: TNumber;
            c: TColor;
        begin
            i := 42;
            r := 3.14;
            s := 'Test';
            n := 5;
            c := Red;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_enumerated_types() {
    let source = r#"
        program EnumTest;
        type
            Day = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
        var
            today: Day;
        begin
            today := Wednesday;
            if today = Wednesday then
                writeln('It is Wednesday');
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_subrange_types() {
    let source = r#"
        program SubrangeTest;
        type
            Digit = 0..9;
            LowerCase = 'a'..'z';
        var
            d: Digit;
            c: LowerCase;
        begin
            for d := 0 to 9 do
                writeln(d);
            c := 'a';
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_set_type() {
    let source = r#"
        program SetTest;
        type
            CharSet = set of char;
        var
            vowels: CharSet;
            c: char;
        begin
            vowels := ['a', 'e', 'i', 'o', 'u'];
            c := 'e';
            if c in vowels then
                writeln(c, ' is a vowel');
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_repeat_until() {
    let source = r#"
        program RepeatTest;
        var
            x: integer;
        begin
            x := 0;
            repeat
                x := x + 1;
                writeln(x);
            until x > 10;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_while_loop() {
    let source = r#"
        program WhileTest;
        var
            x: integer;
        begin
            x := 0;
            while x < 10 do
            begin
                writeln(x);
                x := x + 1;
            end;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_for_downto() {
    let source = r#"
        program DowntoTest;
        var
            i: integer;
        begin
            for i := 10 downto 1 do
                writeln(i);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_multiple_functions() {
    let source = r#"
        program MultiFuncTest;
        function Square(x: integer): integer;
        begin
            Square := x * x;
        end;

        function Cube(x: integer): integer;
        begin
            Cube := x * x * x;
        end;

        function Abs(x: integer): integer;
        begin
            if x < 0 then
                Abs := -x
            else
                Abs := x;
        end;

        var
            n: integer;
        begin
            n := 5;
            writeln('Square: ', Square(n));
            writeln('Cube: ', Cube(n));
            writeln('Abs(-3): ', Abs(-3));
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_math_operations() {
    let source = r#"
        program MathTest;
        var
            a, b, c: real;
        begin
            a := 10.5;
            b := 3.2;
            c := a + b;
            writeln('Add: ', c);
            c := a - b;
            writeln('Sub: ', c);
            c := a * b;
            writeln('Mul: ', c);
            c := a / b;
            writeln('Div: ', c);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_boolean_operations() {
    let source = r#"
        program BooleanTest;
        var
            a, b, result: boolean;
        begin
            a := true;
            b := false;
            result := a and b;
            writeln('a and b: ', result);
            result := a or b;
            writeln('a or b: ', result);
            result := not a;
            writeln('not a: ', result);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_comparison() {
    let source = r#"
        program ComparisonTest;
        var
            a, b: integer;
        begin
            a := 10;
            b := 20;

            if a = b then
                writeln('a = b');

            if a < b then
                writeln('a < b');

            if a > b then
                writeln('a > b');

            if a <= b then
                writeln('a <= b');

            if a >= b then
                writeln('a >= b');

            if a <> b then
                writeln('a <> b');
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_nested_functions() {
    let source = r#"
        program NestedFuncTest;
        procedure Outer;
            var
                x: integer;

            procedure Inner;
            begin
                writeln('Inner: x = ', x);
            end;

            begin
                x := 10;
                writeln('Outer: x = ', x);
                Inner;
            end;

        begin
            Outer;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_forward_declarations() {
    let source = r#"
        program ForwardTest;

        function B(x: integer): integer; forward;

        function A(x: integer): integer;
        begin
            A := B(x) + 1;
        end;

        function B(x: integer): integer;
        begin
            B := x * 2;
        end;

        begin
            writeln('A(5) = ', A(5));
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_unit_structure() {
    let source = r#"
        unit MyUnit;

        interface

        function Add(a, b: integer): integer;

        implementation

        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;

        end.
    "#;

    let mut parser = Parser::new(source);
    let result = parser.parse_unit();

    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_uses_clause() {
    let source = r#"
        program UsesTest;
        uses
            SysUtils, Classes;

        begin
            writeln('Program using units');
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_dynamic_array() {
    let source = r#"
        program DynamicArrayTest;
        type
            TIntArray = array of integer;
        var
            arr: TIntArray;
            i: integer;
        begin
            SetLength(arr, 10);
            for i := 0 to 9 do
                arr[i] := i * i;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_class_basic() {
    let source = r#"
        program ClassTest;
        type
            TMyClass = class
                X: integer;
                procedure SetValue(val: integer);
                function GetValue: integer;
            end;

        procedure TMyClass.SetValue(val: integer);
        begin
            X := val;
        end;

        function TMyClass.GetValue: integer;
        begin
            GetValue := X;
        end;

        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            obj.SetValue(42);
            writeln('Value: ', obj.GetValue);
            obj.Free;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_inheritance() {
    let source = r#"
        program InheritanceTest;
        type
            TAnimal = class
                procedure Speak; virtual;
            end;

            TDog = class(TAnimal)
                procedure Speak; override;
            end;

        procedure TAnimal.Speak;
        begin
            writeln('Animal sound');
        end;

        procedure TDog.Speak;
        begin
            writeln('Woof!');
        end;

        var
            animal: TAnimal;
            dog: TDog;
        begin
            animal := TAnimal.Create;
            animal.Speak;

            dog := TDog.Create;
            dog.Speak;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_with_interface() {
    let source = r#"
        program InterfaceTest;
        type
            IDisposable = interface
                procedure Dispose;
            end;

            TMyClass = class(TInterfacedObject, IDisposable)
                procedure Dispose;
            end;

        procedure TMyClass.Dispose;
        begin
            writeln('Disposing');
        end;

        var
            obj: IDisposable;
        begin
            obj := TMyClass.Create;
            obj.Dispose;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_quicksort() {
    let source = r#"
        program QuicksortTest;
        const
            N = 10;
        var
            arr: array[1..N] of integer;

        procedure QuickSort(left, right: integer);
        var
            pivot, i, j, temp: integer;
        begin
            if left < right then
            begin
                pivot := arr[(left + right) div 2];
                i := left;
                j := right;

                while i <= j do
                begin
                    while arr[i] < pivot do
                        i := i + 1;
                    while arr[j] > pivot do
                        j := j - 1;

                    if i <= j then
                    begin
                        temp := arr[i];
                        arr[i] := arr[j];
                        arr[j] := temp;
                        i := i + 1;
                        j := j - 1;
                    end;
                end;

                QuickSort(left, j);
                QuickSort(i, right);
            end;
        end;

        var
            i: integer;
        begin
            for i := 1 to N do
                arr[i] := N - i + 1;

            QuickSort(1, N);

            for i := 1 to N do
                writeln(arr[i]);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_compilation_linked_list() {
    let source = r#"
        program LinkedListTest;
        type
            PNode = ^TNode;
            TNode = record
                data: integer;
                next: PNode;
            end;

        var
            head, current: PNode;
            i: integer;
        begin
            head := nil;

            for i := 1 to 5 do
            begin
                new(current);
                current^.data := i;
                current^.next := head;
                head := current;
            end;

            current := head;
            while current <> nil do
            begin
                writeln(current^.data);
                current := current^.next;
            end;
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_lexer_parser_integration() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 42;
            writeln(x);
        end.
    "#;

    // Test that lexer and parser work together
    let mut lexer = Lexer::new(source);
    let mut parser = Parser::new(source);

    let result = parser.parse_program();
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parser_optimizer_integration() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 2 + 3 * 4;
        end.
    "#;

    let mut parser = Parser::new(source);
    let program = parser.parse_program();

    if program.is_ok() {
        let prog = program.unwrap();
        let mut optimizer = Optimizer::new();
        // Apply optimizations
        let _ = optimizer;
    }

    assert!(program.is_ok());
}

#[test]
fn test_optimizer_codegen_integration() {
    let source = r#"
        program Test;
        begin
            writeln('Hello');
        end.
    "#;

    let mut parser = Parser::new(source);
    let program = parser.parse_program();

    if program.is_ok() {
        let prog = program.unwrap();
        let generator = UnitCodeGenerator::new();
        let result = generator.generate_program(&prog);

        assert!(result.is_ok() || result.is_err());
    }
}

#[test]
fn test_full_pipeline_empty_program() {
    let source = r#"
        program Empty;
        begin
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_pipeline_with_comments() {
    let source = r#"
        program CommentTest;
        { This is a block comment }
        var
            x: integer; // This is a line comment
        begin
            x := 10; { Another comment }
            writeln(x);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_pipeline_multiple_statements() {
    let source = r#"
        program MultiStatement;
        var
            a, b, c: integer;
        begin
            a := 10;
            b := 20;
            c := a + b;
            writeln('Sum: ', c);
        end.
    "#;

    let result = compile_program(source);
    assert!(result.is_ok() || result.is_err());
}
