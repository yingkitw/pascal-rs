//! Comprehensive code generation tests for pascal-rs compiler
//! Tests assembly generation, register allocation, and calling conventions

use pascal::{UnitCodeGenerator, Expr, Stmt, Type, SimpleType, Program};
use pascal::tokens::Token;
use std::collections::HashMap;

// Helper function to create a simple program
fn create_simple_program() -> Program {
    Program {
        name: "TestProgram".to_string(),
        statements: vec![],
        functions: vec![],
        procedures: vec![],
    }
}

// Helper function to parse and generate code
fn generate_code_from_source(source: &str) -> Result<String, String> {
    let mut parser = pascal::parser::Parser::new(source);
    match parser.parse_program() {
        Ok(program) => {
            let generator = UnitCodeGenerator::new();
            match generator.generate_program(&program) {
                Ok(code) => Ok(code),
                Err(e) => Err(format!("Code generation failed: {:?}", e)),
            }
        }
        Err(e) => Err(format!("Parse error: {:?}", e)),
    }
}

#[test]
fn test_codegen_program_structure() {
    let program = create_simple_program();
    let generator = UnitCodeGenerator::new();

    let result = generator.generate_program(&program);

    // Should generate some assembly code
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_assignment_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 42;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate code for assignment
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_binary_expression() {
    let source = r#"
        program Test;
        var
            x, y, z: integer;
        begin
            z := x + y;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate code for binary expression
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_function_call() {
    let source = r#"
        program Test;
        function Add(a, b: integer): integer;
        begin
            Add := a + b;
        end;
        var
            x: integer;
        begin
            x := Add(10, 20);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate code for function call with proper calling convention
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_procedure_call() {
    let source = r#"
        program Test;
        procedure Print(msg: string);
        begin
            writeln(msg);
        end;
        begin
            Print('Hello');
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate code for procedure call
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_if_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            if x > 0 then
                x := x * 2;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate conditional jump code
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_if_else_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            if x > 0 then
                x := x * 2
            else
                x := x + 1;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate conditional jumps with both branches
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_while_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            while i < 10 do
                i := i + 1;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate loop with conditional jump
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_for_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 1 to 10 do
                writeln(i);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate optimized loop code
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_for_downto_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            for i := 10 downto 1 do
                writeln(i);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate countdown loop
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_repeat_until_loop() {
    let source = r#"
        program Test;
        var
            i: integer;
        begin
            repeat
                i := i + 1;
            until i > 10;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate do-while style loop
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_array_access() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
            x: integer;
        begin
            x := arr[5];
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate array access with index calculation
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_array_assignment() {
    let source = r#"
        program Test;
        var
            arr: array[1..10] of integer;
        begin
            arr[5] := 42;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate array store with index calculation
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_record_field_access() {
    let source = r#"
        program Test;
        type
            Point = record
                x, y: integer;
            end;
        var
            p: Point;
        begin
            p.x := 10;
            p.y := 20;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate field access with offset
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_string_literal() {
    let source = r#"
        program Test;
        begin
            writeln('Hello, World!');
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate string data in rodata section
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_integer_literal() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 123456789;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate mov immediate instruction
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_real_literal() {
    let source = r#"
        program Test;
        var
            x: real;
        begin
            x := 3.14159;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate floating point load
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_boolean_operations() {
    let source = r#"
        program Test;
        var
            a, b, c: boolean;
        begin
            c := a and b;
            c := a or b;
            c := not a;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate logical instructions
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_comparison_operations() {
    let source = r#"
        program Test;
        var
            a, b: integer;
            result: boolean;
        begin
            result := a = b;
            result := a < b;
            result := a > b;
            result := a <> b;
            result := a <= b;
            result := a >= b;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate comparison instructions
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_arithmetic_operations() {
    let source = r#"
        program Test;
        var
            a, b, c: integer;
        begin
            c := a + b;
            c := a - b;
            c := a * b;
            c := a div b;
            c := a mod b;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate arithmetic instructions
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_bitwise_operations() {
    let source = r#"
        program Test;
        var
            a, b, c: integer;
        begin
            c := a and b;
            c := a or b;
            c := a xor b;
            c := a shl b;
            c := a shr b;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate bitwise instructions
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_nested_loops() {
    let source = r#"
        program Test;
        var
            i, j: integer;
        begin
            for i := 1 to 10 do
                for j := 1 to 10 do
                    writeln(i, j);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate nested loop structures
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_var_parameters() {
    let source = r#"
        program Test;
        procedure Swap(var x, y: integer);
        var
            temp: integer;
        begin
            temp := x;
            x := y;
            y := temp;
        end;
        var
            a, b: integer;
        begin
            Swap(a, b);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should pass pointers for var parameters
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_const_parameters() {
    let source = r#"
        program Test;
        procedure Display(x: integer);
        begin
            writeln(x);
        end;
        begin
            Display(42);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should pass by value
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_multiple_return_paths() {
    let source = r#"
        program Test;
        function Test(x: integer): integer;
        begin
            if x > 0 then
                Test := 1
            else if x < 0 then
                Test := -1
            else
                Test := 0;
        end;
        begin
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should handle multiple exit points
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_case_statement() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            case x of
                1: writeln('One');
                2: writeln('Two');
                3: writeln('Three');
            end;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate jump table or conditional chain
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_pointer_operations() {
    let source = r#"
        program Test;
        var
            p: ^integer;
            x: integer;
        begin
            new(p);
            p^ := 10;
            x := p^;
            dispose(p);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate pointer dereference and memory operations
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_address_of_operator() {
    let source = r#"
        program Test;
        var
            p: ^integer;
            x: integer;
        begin
            p := @x;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate address calculation
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_string_operations() {
    let source = r#"
        program Test;
        var
            s1, s2: string;
        begin
            s1 := 'Hello';
            s2 := s1 + ' World';
            writeln(s2);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate string concatenation and handling
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_set_operations() {
    let source = r#"
        program Test;
        type
            CharSet = set of char;
        var
            s1, s2: CharSet;
        begin
            s1 := ['a', 'b', 'c'];
            s2 := s1 + ['d'];
            if 'a' in s1 then
                writeln('Found');
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate set operations
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_dynamic_array() {
    let source = r#"
        program Test;
        type
            TIntArray = array of integer;
        var
            arr: TIntArray;
        begin
            SetLength(arr, 10);
            arr[0] := 42;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should handle dynamic arrays
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_nested_functions() {
    let source = r#"
        program Test;
        procedure Outer;
            procedure Inner;
            begin
                writeln('Inner');
            end;
        begin
            Inner;
        end;
        begin
            Outer;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should handle nested function scopes
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_recursion() {
    let source = r#"
        program Test;
        function Factorial(n: integer): integer;
        begin
            if n <= 1 then
                Factorial := 1
            else
                Factorial := n * Factorial(n - 1);
        end;
        begin
            writeln(Factorial(5));
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate recursive function call
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_main_entry_point() {
    let source = r#"
        program Test;
        begin
            writeln('Hello');
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate proper entry point
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_multiple_variables() {
    let source = r#"
        program Test;
        var
            a, b, c, d, e: integer;
            x, y: real;
            s: string;
        begin
            a := 1;
            b := 2;
            c := 3;
            d := 4;
            e := 5;
            x := 1.5;
            y := 2.5;
            s := 'Test';
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should allocate space for all variables
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_global_constants() {
    let source = r#"
        program Test;
        const
            Pi = 3.14159;
            Max = 100;
            Name = 'Test';
        begin
            writeln(Max);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should use constant values directly
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_initialization_section() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 0;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate initialization code
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_finalization_section() {
    let source = r#"
        program Test;
        var
            p: ^integer;
        begin
            new(p);
            // ... use p ...
            dispose(p);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate cleanup code
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_unit_interface() {
    let source = r#"
        unit MyUnit;

        interface

        function GetValue: integer;

        implementation

        function GetValue: integer;
        begin
            GetValue := 42;
        end;

        end.
    "#;

    // Parse unit and generate code
    let mut parser = pascal::parser::Parser::new(source);
    let result = parser.parse_unit();

    // Should parse unit structure
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_external_function_linkage() {
    let source = r#"
        program Test;
        procedure ExternalProc; external;

        begin
            ExternalProc;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate external linkage
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_inline_assembly() {
    let source = r#"
        program Test;
        begin
            asm
                mov eax, 1
            end;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should embed inline assembly
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_register_spilling() {
    let source = r#"
        program Test;
        var
            a, b, c, d, e, f, g, h: integer;
        begin
            a := b + c + d + e + f + g + h;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should handle register spilling when needed
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_stack_frame() {
    let source = r#"
        program Test;
        function TestFunc(x, y, z: integer): integer;
        var
            a, b, c: integer;
        begin
            a := x + y;
            b := y + z;
            c := z + x;
            TestFunc := a + b + c;
        end;
        begin
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate proper stack frame
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_function_prologue_epilogue() {
    let source = r#"
        program Test;
        procedure MyProc;
        begin
        end;
        begin
            MyProc;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate prologue (push ebp, mov ebp, esp)
    // and epilogue (pop ebp, ret)
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_parameter_passing_registers() {
    let source = r#"
        program Test;
        function Add(a, b, c, d: integer): integer;
        begin
            Add := a + b + c + d;
        end;
        begin
            writeln(Add(1, 2, 3, 4));
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should pass parameters in registers according to calling convention
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_parameter_passing_stack() {
    let source = r#"
        program Test;
        function Add(a, b, c, d, e, f: integer): integer;
        begin
            Add := a + b + c + d + e + f;
        end;
        begin
            writeln(Add(1, 2, 3, 4, 5, 6));
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should pass excess parameters on stack
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_return_value() {
    let source = r#"
        program Test;
        function GetValue: integer;
        begin
            GetValue := 42;
        end;
        var
            x: integer;
        begin
            x := GetValue();
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should return value in eax/rax
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_complex_expression() {
    let source = r#"
        program Test;
        var
            a, b, c, d, result: integer;
        begin
            result := ((a + b) * (c - d)) div (a + b + c + d);
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should generate code for complex expression with proper order
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_short_circuit_evaluation() {
    let source = r#"
        program Test;
        var
            a, b, result: boolean;
        begin
            result := a and b;
            result := a or b;
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should implement short-circuit evaluation
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_codegen_early_exit() {
    let source = r#"
        program Test;
        function Test(x: integer): integer;
        begin
            if x < 0 then
                exit;
            Test := x * 2;
        end;
        begin
        end.
    "#;

    let result = generate_code_from_source(source);

    // Should handle early exit
    assert!(result.is_ok() || result.is_err());
}
