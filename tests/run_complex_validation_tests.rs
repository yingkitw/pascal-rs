//! Complex validation tests runner
//! Tests complex language features and edge cases

use pascal::parser::Parser;
use pascal::interpreter::Interpreter;

// Helper to run full compilation pipeline
fn compile_and_run(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    let program = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;
    let mut interpreter = Interpreter::new(false);
    interpreter.run_program(&program).map_err(|e| format!("Runtime error: {:?}", e))?;
    Ok(())
}

/// Test complex arithmetic operations with operator precedence
#[test]
fn test_complex_arithmetic() {
    let source = r#"
program test;
var
    a, b, c, d, e, f: integer;
begin
    a := 10;
    b := 5;
    c := 3;
    d := 2;

    // Complex expression: 10 + 5 * 3 - 2 / 2
    e := a + b * c - d div d;

    // Nested parentheses: (10 + 5) * (3 - 2)
    f := (a + b) * (c - d);

    // Test modulo: 10 mod 3 = 1
    a := a mod c;

    // Test power-like behavior with multiplication
    b := b * b;  // 5 * 5 = 25

    // Test unary minus
    c := -c;  // -3
end.
"#;

    assert!(compile_and_run(source).is_ok());
}

/// Test nested control structures
#[test]
fn test_nested_control_structures() {
    let source = r#"
program test;
var
    i, j, k, sum, product: integer;
    found: boolean;
begin
    sum := 0;
    product := 1;
    found := false;

    // Nested loops with conditionals
    for i := 1 to 5 do
    begin
        for j := 1 to 3 do
        begin
            sum := sum + i * j;

            // Break condition simulation
            if (i = 3) and (j = 2) then
                found := true;

            product := product * (i + j);
        end;
    end;

    // Test while loop
    k := 0;
    while k < 10 do
    begin
        k := k + 1;
        if k > 5 then
            sum := sum + k;
    end;

    // Test repeat until
    i := 0;
    repeat
        i := i + 1;
    until i >= 5;
end.
"#;

    assert!(compile_and_run(source).is_ok());
}

/// Test string operations and character handling
#[test]
fn test_string_operations() {
    let source = r#"
program test;
var
    ch: char;
    success: boolean;
begin
    // Test character assignments
    ch := 'A';
    success := true;

    // Test character comparisons
    if ch = 'A' then
        ch := 'B';

    if ch <> 'A' then
        success := true;

    // Test character operations
    if ch > 'A' then
        ch := 'C';

    // Test boolean logic with characters
    if (ch = 'C') and success then
        success := true;
end.
"#;

    assert!(compile_and_run(source).is_ok());
}

/// Test complex boolean expressions
#[test]
fn test_complex_boolean_expressions() {
    let source = r#"
program test;
var
    a, b, c, d: boolean;
    result: boolean;
begin
    a := true;
    b := false;
    c := true;
    d := false;

    // Test AND operations
    result := a and c;
    result := result and (not b);

    // Test OR operations
    result := b or d;
    result := result or a;

    // Test complex expression
    result := (a and b) or (c and d);
    result := (a or b) and (c or d);

    // Test NOT with parentheses
    result := not ((a and b) or (c and d));

    // Test combination
    result := (a and c) or (not b and not d);
end.
"#;

    assert!(compile_and_run(source).is_ok());
}

/// Test real number operations
#[test]
fn test_real_number_operations() {
    let source = r#"
program test;
var
    r1, r2, r3: real;
    i: integer;
begin
    r1 := 3.14;
    r2 := 2.0;
    r3 := r1 * r2;

    r1 := r3 / r2;
    r2 := r1 + r3;

    // Mix real and integer
    i := 5;
    r1 := r2 + i;

    r3 := r1 * 2.0;
end.
"#;

    assert!(compile_and_run(source).is_ok());
}

/// Test assignment chains and complex expressions
#[test]
fn test_assignment_chains() {
    let source = r#"
program test;
var
    a, b, c, d, e: integer;
begin
    // Individual assignments (Pascal doesn't support chaining)
    a := 5;
    b := 5;
    c := 5;
    d := 5;
    e := 5;

    // Verify all are 5
    a := a + b + c + d + e;

    // Complex expression with multiple operations
    a := (b + c) * (d - e);

    // Reset and test different operations
    a := 10;
    b := 3;
    c := a div b;
    d := a mod b;
    e := a - b * c;
end.
"#;

    assert!(compile_and_run(source).is_ok());
}

/// Test boundary conditions and edge cases
#[test]
fn test_boundary_conditions() {
    let source = r#"
program test;
var
    min, max, zero: integer;
    result: boolean;
begin
    // Test zero
    zero := 0;
    result := (zero = 0);

    // Test positive integers
    max := 1000;
    result := result and (max > 0);

    // Test negative integers
    min := -1000;
    result := result and (min < 0);

    // Test boundary comparisons
    result := (max > min) and (min < zero) and (zero < max);

    // Test arithmetic at boundaries
    max := max + min;
    min := min * -1;
end.
"#;

    assert!(compile_and_run(source).is_ok());
}

/// Test comprehensive program with multiple features
#[test]
fn test_comprehensive_program() {
    let source = r#"
program test;
var
    sum, i, j: integer;
    product: integer;
    flag: boolean;
    count: integer;
begin
    // Initialize
    sum := 0;
    product := 1;
    flag := false;
    count := 0;

    // Calculate sum of squares from 1 to 5
    for i := 1 to 5 do
    begin
        sum := sum + i * i;

        // Count even numbers
        if (i mod 2) = 0 then
            count := count + 1;
    end;

    // Calculate product of first 3 numbers
    for i := 1 to 3 do
    begin
        product := product * i;
    end;

    // Test nested loops
    for i := 1 to 3 do
    begin
        for j := 1 to 3 do
        begin
            if i * j > 5 then
                flag := true;
        end;
    end;

    // Test while loop with condition
    i := 10;
    while i > 0 do
    begin
        i := i - 1;
        if i = 5 then
            sum := sum + 100;
    end;

    // Test boolean logic
    flag := (sum > 0) and (product > 0) and (count = 2);
end.
"#;

    assert!(compile_and_run(source).is_ok());
}

/// Test operator precedence and associativity
#[test]
fn test_operator_precedence() {
    let source = r#"
program test;
var
    a, b, c, d, result: integer;
begin
    a := 10;
    b := 5;
    c := 2;
    d := 3;

    // Test precedence: * and div before + and -
    result := a + b * c;

    // Test precedence with parentheses
    result := (a + b) * c;

    // Test associativity (left to right)
    result := a - b - c;

    // Test mixed operators
    result := a + b * c - d div c;

    // Test complex expression
    result := (a + b) * (c - d) div 2;

    // Test mod and div
    result := a mod b + c div d;

    // Reset for final test
    a := 20;
    b := 3;
    result := a div b * b + a mod b;
end.
"#;

    assert!(compile_and_run(source).is_ok());
}
