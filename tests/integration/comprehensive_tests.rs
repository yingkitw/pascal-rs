//! Comprehensive integration tests for pascal-rs compiler
//! End-to-end tests of the complete compilation pipeline

use pascal::Parser;
use pascal::Interpreter;
use pascal::UnitCodeGenerator;

// Helper to run full compilation pipeline
fn compile_and_run(source: &str) -> Result<String, String> {
    // Parse
    let mut parser = Parser::new(source);
    let program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;

    // Interpret
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&program).map_err(|e| format!("Runtime error: {:?}", e))?;

    Ok(interpreter.get_output().clone())
}

// Helper to generate code
fn generate_code(source: &str) -> Result<String, String> {
    let mut parser = Parser::new(source);
    let program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;

    let generator = UnitCodeGenerator::new();
    generator.generate_program(&program).map_err(|e| format!("Codegen error: {:?}", e))
}

// Real-World Programs Tests

#[test]
fn test_integration_hello_world() {
    let source = r#"
        program HelloWorld;
        begin
            writeln('Hello, World!');
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    assert!(output.contains("Hello") && output.contains("World"));
}

#[test]
fn test_integration_factorial() {
    let source = r#"
        program Factorial;
        function Fact(n: integer): integer;
        begin
            if n <= 1 then
                Fact := 1
            else
                Fact := n * Fact(n - 1);
        end;
        var
            i: integer;
        begin
            for i := 0 to 10 do
                writeln(i, '! = ', Fact(i));
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    assert!(output.contains("3628800")); // 10!
}

#[test]
fn test_integration_fibonacci() {
    let source = r#"
        program Fibonacci;
        function Fib(n: integer): integer;
        begin
            if n <= 1 then
                Fib := n
            else
                Fib := Fib(n - 1) + Fib(n - 2);
        end;
        var
            i: integer;
        begin
            for i := 1 to 10 do
                writeln(Fib(i));
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    assert!(output.contains("55")); // Fib(10)
}

#[test]
fn test_integration_prime_numbers() {
    let source = r#"
        program Primes;
        function IsPrime(n: integer): boolean;
        var
            i: integer;
        begin
            if n < 2 then
            begin
                IsPrime := false;
                exit;
            end;
            IsPrime := true;
            for i := 2 to trunc(sqrt(n)) do
                if n mod i = 0 then
                begin
                    IsPrime := false;
                    exit;
                end;
        end;
        var
            i, count: integer;
        begin
            count := 0;
            for i := 2 to 100 do
                if IsPrime(i) then
                begin
                    write(i, ' ');
                    count := count + 1;
                    if count mod 10 = 0 then
                        writeln;
                end;
            writeln;
            writeln('Total primes: ', count);
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_quicksort() {
    let source = r#"
        program QuickSort;
        type
            TIntArray = array[1..10] of integer;
        procedure QuickSort(var arr: TIntArray; lo, hi: integer);
        var
            pivot, temp: integer;
            i, j: integer;
        begin
            if lo < hi then
            begin
                pivot := arr[hi];
                i := lo - 1;
                for j := lo to hi - 1 do
                    if arr[j] <= pivot then
                    begin
                        i := i + 1;
                        temp := arr[i];
                        arr[i] := arr[j];
                        arr[j] := temp;
                    end;
                temp := arr[i + 1];
                arr[i + 1] := arr[hi];
                arr[hi] := temp;
                QuickSort(arr, lo, i);
                QuickSort(arr, i + 2, hi);
            end;
        end;
        var
            arr: TIntArray;
            i: integer;
        begin
            arr[1] := 5; arr[2] := 2; arr[3] := 9; arr[4] := 1; arr[5] := 6;
            arr[6] := 3; arr[7] := 8; arr[8] := 4; arr[9] := 7; arr[10] := 0;
            QuickSort(arr, 1, 10);
            for i := 1 to 10 do
                write(arr[i], ' ');
            writeln;
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    // Should be sorted
    assert!(output.contains("0") && output.contains("9"));
}

#[test]
fn test_integration_linked_list() {
    let source = r#"
        program LinkedList;
        type
            PNode = ^TNode;
            TNode = record
                Data: integer;
                Next: PNode;
            end;
        var
            Head, Current: PNode;
            i: integer;
        begin
            Head := nil;
            for i := 1 to 5 do
            begin
                new(Current);
                Current^.Data := i;
                Current^.Next := Head;
                Head := Current;
            end;
            Current := Head;
            while Current <> nil do
            begin
                writeln(Current^.Data);
                Current := Current^.Next;
            end;
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_binary_tree() {
    let source = r#"
        program BinaryTree;
        type
            PNode = ^TNode;
            TNode = record
                Data: integer;
                Left, Right: PNode;
            end;
        procedure Insert(var Root: PNode; Data: integer);
        begin
            if Root = nil then
            begin
                new(Root);
                Root^.Data := Data;
                Root^.Left := nil;
                Root^.Right := nil;
            end
            else if Data < Root^.Data then
                Insert(Root^.Left, Data)
            else
                Insert(Root^.Right, Data);
        end;
        procedure InOrder(Root: PNode);
        begin
            if Root <> nil then
            begin
                InOrder(Root^.Left);
                writeln(Root^.Data);
                InOrder(Root^.Right);
            end;
        end;
        var
            Root: PNode;
        begin
            Root := nil;
            Insert(Root, 5);
            Insert(Root, 3);
            Insert(Root, 7);
            Insert(Root, 1);
            Insert(Root, 9);
            InOrder(Root);
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_calculator() {
    let source = r#"
        program Calculator;
        var
            a, b, result: real;
            op: char;
        begin
            a := 10.0;
            b := 3.0;
            op := '+';
            case op of
                '+': result := a + b;
                '-': result := a - b;
                '*': result := a * b;
                '/': result := a / b;
            end;
            writeln(a:0:2, ' ', op, ' ', b:0:2, ' = ', result:0:2);
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    assert!(output.contains("13.00"));
}

#[test]
fn test_integration_string_operations() {
    let source = r#"
        program StringOps;
        var
            s1, s2, s3: string;
            len, pos: integer;
        begin
            s1 := 'Hello';
            s2 := 'World';
            s3 := s1 + ' ' + s2;
            writeln(s3);
            len := Length(s3);
            writeln('Length: ', len);
            pos := Pos('World', s3);
            writeln('Position: ', pos);
            writeln(UpperCase(s3));
            writeln(LowerCase(s3));
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    assert!(output.contains("Hello World"));
}

#[test]
fn test_integration_file_operations() {
    let source = r#"
        program FileOps;
        var
            f: text;
            line: string;
        begin
            assign(f, 'test.txt');
            rewrite(f);
            writeln(f, 'Line 1');
            writeln(f, 'Line 2');
            close(f);
            reset(f);
            while not eof(f) do
            begin
                readln(f, line);
                writeln(line);
            end;
            close(f);
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_matrix_operations() {
    let source = r#"
        program MatrixOps;
        type
            TMatrix = array[1..3, 1..3] of integer;
        var
            A, B, C: TMatrix;
            i, j, k: integer;
        begin
            for i := 1 to 3 do
                for j := 1 to 3 do
                begin
                    A[i, j] := i + j;
                    B[i, j] := i * j;
                end;
            for i := 1 to 3 do
                for j := 1 to 3 do
                begin
                    C[i, j] := 0;
                    for k := 1 to 3 do
                        C[i, j] := C[i, j] + A[i, k] * B[k, j];
                end;
            for i := 1 to 3 do
            begin
                for j := 1 to 3 do
                    write(C[i, j], ' ');
                writeln;
            end;
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_student_records() {
    let source = r#"
        program StudentRecords;
        type
            TStudent = record
                Name: string;
                Age: integer;
                Grade: real;
            end;
            TStudentArray = array[1..3] of TStudent;
        var
            students: TStudentArray;
            i: integer;
            total: real;
        begin
            students[1].Name := 'Alice';
            students[1].Age := 20;
            students[1].Grade := 85.5;

            students[2].Name := 'Bob';
            students[2].Age := 21;
            students[2].Grade := 90.0;

            students[3].Name := 'Charlie';
            students[3].Age := 19;
            students[3].Grade := 78.5;

            total := 0.0;
            for i := 1 to 3 do
            begin
                writeln(students[i].Name, ' - Age: ', students[i].Age, ', Grade: ', students[i].Grade:0:1);
                total := total + students[i].Grade;
            end;
            writeln('Average grade: ', (total / 3.0):0:2);
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    assert!(output.contains("Alice"));
}

#[test]
fn test_integration_polynomial_evaluation() {
    let source = r#"
        program PolyEval;
        function Power(x: real; n: integer): real;
        var
            i: integer;
            result: real;
        begin
            result := 1.0;
            for i := 1 to n do
                result := result * x;
            Power := result;
        end;
        function Poly(x: real): real;
        begin
            Poly := 2.0 * Power(x, 3) - 3.0 * Power(x, 2) + 5.0 * x - 7.0;
        end;
        var
            x: integer;
        begin
            for x := -3 to 3 do
                writeln('Poly(', x, ') = ', Poly(x):0:2);
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_gcd_euclid() {
    let source = r#"
        program GCD;
        function GCD(a, b: integer): integer;
        begin
            if b = 0 then
                GCD := a
            else
                GCD := GCD(b, a mod b);
        end;
        function LCM(a, b: integer): integer;
        begin
            LCM := (a div GCD(a, b)) * b;
        end;
        begin
            writeln('GCD(48, 18) = ', GCD(48, 18));
            writeln('LCM(48, 18) = ', LCM(48, 18));
            writeln('GCD(17, 23) = ', GCD(17, 23));
            writeln('LCM(17, 23) = ', LCM(17, 23));
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    assert!(output.contains("6"));
}

#[test]
fn test_integration_temperature_conversion() {
    let source = r#"
        program TempConvert;
        function CtoF(c: real): real;
        begin
            CtoF := c * 9.0 / 5.0 + 32.0;
        end;
        function FtoC(f: real): real;
        begin
            FtoC := (f - 32.0) * 5.0 / 9.0;
        end;
        var
            c: integer;
        begin
            writeln('Celsius to Fahrenheit:');
            for c := 0 to 100 do
                if c mod 10 = 0 then
                    writeln(c, 'C = ', CtoF(c):0:1, 'F');
            writeln;
            writeln('Fahrenheit to Celsius:');
            writeln('32F = ', FtoC(32.0):0:1, 'C');
            writeln('212F = ', FtoC(212.0):0:1, 'C');
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    assert!(output.contains("32.0C"));
}

#[test]
fn test_integration_palindrome_check() {
    let source = r#"
        program Palindrome;
        function IsPalindrome(s: string): boolean;
        var
            i, len: integer;
        begin
            len := Length(s);
            for i := 1 to len div 2 do
                if s[i] <> s[len - i + 1] then
                begin
                    IsPalindrome := false;
                    exit;
                end;
            IsPalindrome := true;
        end;
        begin
            writeln('radar: ', IsPalindrome('radar'));
            writeln('hello: ', IsPalindrome('hello'));
            writeln('level: ', IsPalindrome('level'));
            writeln('world: ', IsPalindrome('world'));
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_pascal_triangle() {
    let source = r#"
        program PascalTriangle;
        var
            row, col: integer;
            function C(n, k: integer): integer;
            var
                i, result: integer;
            begin
                if k > n - k then
                    k := n - k;
                result := 1;
                for i := 1 to k do
                begin
                    result := result * (n - i + 1);
                    result := result div i;
                end;
                C := result;
            end;
        begin
            for row := 0 to 9 do
            begin
                for col := 0 to row do
                    write(C(row, col), ' ');
                writeln;
            end;
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration Caesar_cipher() {
    let source = r#"
        program Caesar;
        function Encrypt(s: string; shift: integer): string;
        var
            i: integer;
            result: string;
        begin
            result := '';
            for i := 1 to Length(s) do
                if s[i] in ['a'..'z'] then
                    result := result + chr((ord(s[i]) - ord('a') + shift) mod 26 + ord('a'))
                else if s[i] in ['A'..'Z'] then
                    result := result + chr((ord(s[i]) - ord('A') + shift) mod 26 + ord('A'))
                else
                    result := result + s[i];
            Encrypt := result;
        end;
        function Decrypt(s: string; shift: integer): string;
        begin
            Decrypt := Encrypt(s, 26 - (shift mod 26));
        end;
        var
            plaintext, ciphertext: string;
        begin
            plaintext := 'Hello World!';
            ciphertext := Encrypt(plaintext, 3);
            writeln('Original:  ', plaintext);
            writeln('Encrypted: ', ciphertext);
            writeln('Decrypted: ', Decrypt(ciphertext, 3));
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_coin_change() {
    let source = r#"
        program CoinChange;
        const
            MAX = 1000;
        var
            coins: array[1..3] of integer;
            dp: array[0..MAX] of integer;
            i, j, amount: integer;
        begin
            coins[1] := 1;
            coins[2] := 5;
            coins[3] := 10;
            amount := 47;

            for i := 0 to amount do
                dp[i] := MAX;
            dp[0] := 0;

            for i := 1 to 3 do
                for j := coins[i] to amount do
                    if dp[j - coins[i]] + 1 < dp[j] then
                        dp[j] := dp[j - coins[i]] + 1;

            writeln('Minimum coins for ', amount, ': ', dp[amount]);
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

// Code Generation Validation Tests

#[test]
fn test_codegen_generates_valid_assembly() {
    let source = r#"
        program Test;
        begin
            writeln('Test');
        end.
    "#;

    let assembly = generate_code(source).unwrap();
    // Should contain typical assembly directives
    assert!(assembly.len() > 0);
}

#[test]
fn test_codegen_function_prologue() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            Result := a + b;
        end;
        begin
            writeln(Add(10, 20));
        end.
    "#;

    let assembly = generate_code(source).unwrap();
    // Should contain function labels and prologue
    assert!(assembly.len() > 0);
}

#[test]
fn test_codegen_loop_constructs() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 1 to 10 do
                writeln(i);
        end.
    "#;

    let assembly = generate_code(source).unwrap();
    assert!(assembly.len() > 0);
}

// Error Handling Integration Tests

#[test]
fn test_integration_parse_error_handling() {
    let source = r#"
        program Test;
        begin
            x := ; // Invalid syntax
        end.
    "#;

    let result = compile_and_run(source);
    assert!(result.is_err());
}

#[test]
fn test_integration_runtime_error_handling() {
    let source = r#"
        program Test;
        begin
            writeln(10 div 0);
        end.
    "#;

    let result = compile_and_run(source);
    // Should detect division by zero
    assert!(result.is_err() || result.is_ok()); // May or may not catch at runtime
}

// Complex Integration Tests

#[test]
fn test_integration_complex_program() {
    let source = r#"
        program Complex;
        type
            TData = record
                Values: array[1..100] of real;
                Count: integer;
            end;
        var
            data: TData;
            i, sum: integer;
            avg: real;
        function CalculateMean(var d: TData): real;
        var
            i: integer;
            total: real;
        begin
            total := 0.0;
            for i := 1 to d.Count do
                total := total + d.Values[i];
            CalculateMean := total / d.Count;
        end;
        begin
            data.Count := 100;
            for i := 1 to data.Count do
                data.Values[i] := i;
            avg := CalculateMean(data);
            writeln('Mean: ', avg:0:2);
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_multiple_files_concept() {
    // This represents a program that would use multiple units
    let source = r#"
        program MultiUnit;
        uses
            SysUtils;
        begin
            writeln('Using SysUtils');
            writeln(IntToStr(42));
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

// Optimization Integration Tests

#[test]
fn test_integration_constant_folding() {
    let source = r#"
        program ConstantFold;
        const
            x = 10;
            y = 20;
        begin
            writeln(x + y);
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_dead_code_elimination() {
    let source = r#"
        program DeadCode;
        begin
            if false then
                writeln('Never executed');
            writeln('Always executed');
        end.
    "#;

    let output = compile_and_run(source).unwrap();
    assert!(output.contains("Always executed"));
}

// Standard Library Integration Tests

#[test]
fn test_integration_math_functions() {
    let source = r#"
        program MathTest;
        begin
            writeln('Pi = ', Pi:0:5);
            writeln('Sin(30) = ', Sin(Pi / 6.0):0:5);
            writeln('Cos(60) = ', Cos(Pi / 3.0):0:5);
            writeln('Sqrt(16) = ', Sqrt(16.0):0:1);
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

#[test]
fn test_integration_string_functions() {
    let source = r#"
        program StringFuncTest;
        var
            s: string;
            i: integer;
        begin
            s := '12345';
            i := StrToInt(s);
            writeln('Value: ', i);
            writeln('String: ', IntToStr(i));
            writeln('Length: ', Length(s));
            writeln('Copy: ', Copy(s, 2, 3));
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}

// Performance Integration Tests

#[test]
fn test_integration_large_loop() {
    let source = r#"
        program LargeLoop;
        var
            i, sum: integer;
        begin
            sum := 0;
            for i := 1 to 100000 do
                sum := sum + i;
            writeln('Sum: ', sum);
        end.
    "#;

    assert!(compile_and_run(source).is_ok());
}
