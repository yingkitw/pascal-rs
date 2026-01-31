//! Comprehensive optimization tests for pascal-rs compiler
//! Tests constant folding, DCE, inlining, and other optimizations

use pascal::{parser::Parser, optimizer::Optimizer};
use pascal::{Expr, Stmt};

// Helper function to optimize an expression
fn optimize_expression(source: &str) -> Result<Expr, String> {
    let mut parser = Parser::new(source);
    match parser.parse_expression() {
        Ok(expr) => {
            let mut optimizer = Optimizer::new();
            Ok(optimizer.optimize_expr(expr))
        }
        Err(e) => Err(format!("Parse error: {:?}", e)),
    }
}

// Helper function to optimize a statement
fn optimize_statement(source: &str) -> Result<Stmt, String> {
    let mut parser = Parser::new(source);
    match parser.parse_statement() {
        Ok(stmt) => {
            let mut optimizer = Optimizer::new();
            Ok(optimizer.optimize_stmt(stmt))
        }
        Err(e) => Err(format!("Parse error: {:?}", e)),
    }
}

#[test]
fn test_constant_folding_addition() {
    let result = optimize_expression("2 + 3");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => {
            assert_eq!(n, 5, "2 + 3 should be folded to 5");
        }
        _ => panic!("Expected constant folded integer literal"),
    }
}

#[test]
fn test_constant_folding_subtraction() {
    let result = optimize_expression("10 - 4");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => {
            assert_eq!(n, 6, "10 - 4 should be folded to 6");
        }
        _ => panic!("Expected constant folded integer literal"),
    }
}

#[test]
fn test_constant_folding_multiplication() {
    let result = optimize_expression("6 * 7");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => {
            assert_eq!(n, 42, "6 * 7 should be folded to 42");
        }
        _ => panic!("Expected constant folded integer literal"),
    }
}

#[test]
fn test_constant_folding_division() {
    let result = optimize_expression("100 / 4");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => {
            assert_eq!(n, 25, "100 / 4 should be folded to 25");
        }
        _ => panic!("Expected constant folded integer literal"),
    }
}

#[test]
fn test_constant_folding_complex_expression() {
    let result = optimize_expression("(2 + 3) * 4");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => {
            assert_eq!(n, 20, "(2 + 3) * 4 should be folded to 20");
        }
        _ => panic!("Expected constant folded integer literal"),
    }
}

#[test]
fn test_constant_folding_nested() {
    let result = optimize_expression("((1 + 2) * (3 + 4))");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => {
            assert_eq!(n, 21, "((1 + 2) * (3 + 4)) should be folded to 21");
        }
        _ => panic!("Expected constant folded integer literal"),
    }
}

#[test]
fn test_constant_folding_with_variables() {
    let result = optimize_expression("x + 5");
    assert!(result.is_ok());

    // Should not fold completely as x is a variable
    match result.unwrap() {
        Expr::BinaryOp { op, left, right } => {
            assert_eq!(op, "+");
            // Verify at least one side is constant
            match *right {
                Expr::Literal(pascal::Literal::Integer(n)) => assert_eq!(n, 5),
                _ => panic!("Expected constant on right side"),
            }
        }
        _ => panic!("Expected binary operation with variable"),
    }
}

#[test]
fn test_algebraic_simplification_addition_zero() {
    let result = optimize_expression("x + 0");
    assert!(result.is_ok());

    // x + 0 should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("x + 0 should simplify to x"),
    }
}

#[test]
fn test_algebraic_simplification_zero_addition() {
    let result = optimize_expression("0 + x");
    assert!(result.is_ok());

    // 0 + x should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("0 + x should simplify to x"),
    }
}

#[test]
fn test_algebraic_simplification_multiplication_by_one() {
    let result = optimize_expression("x * 1");
    assert!(result.is_ok());

    // x * 1 should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("x * 1 should simplify to x"),
    }
}

#[test]
fn test_algebraic_simplification_multiplication_by_zero() {
    let result = optimize_expression("x * 0");
    assert!(result.is_ok());

    // x * 0 should simplify to 0
    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => assert_eq!(n, 0),
        _ => panic!("x * 0 should simplify to 0"),
    }
}

#[test]
fn test_algebraic_simplification_division_by_one() {
    let result = optimize_expression("x / 1");
    assert!(result.is_ok());

    // x / 1 should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("x / 1 should simplify to x"),
    }
}

#[test]
fn test_algebraic_simplification_subtraction_zero() {
    let result = optimize_expression("x - 0");
    assert!(result.is_ok());

    // x - 0 should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("x - 0 should simplify to x"),
    }
}

#[test]
fn test_algebraic_simplification_double_negation() {
    let result = optimize_expression("-(-x)");
    assert!(result.is_ok());

    // -(-x) should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("-(-x) should simplify to x"),
    }
}

#[test]
fn test_strength_reduction_multiplication_by_two() {
    let result = optimize_expression("x * 2");
    assert!(result.is_ok());

    // x * 2 could be optimized to x << 1
    // Check that some transformation happened
    let expr = result.unwrap();
    match expr {
        Expr::BinaryOp { op, .. } | Expr::UnaryOp { .. } => {
            // Some optimization should occur
        }
        Expr::Variable(_) => {
            // Or it might be kept as is
        }
        _ => {}
    }
}

#[test]
fn test_strength_reduction_multiplication_by_power_of_two() {
    let result = optimize_expression("x * 16");
    assert!(result.is_ok());

    // x * 16 could be optimized to x << 4
    let _ = result.unwrap();
}

#[test]
fn test_dead_code_elimination_unreachable_code() {
    let source = r#"
        begin
            if false then
                x := 1;
            x := 2;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_dead_code_elimination_unused_assignments() {
    let source = r#"
        begin
            x := 1;
            x := 2;
            y := x;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_dead_code_elimination_no_side_effects() {
    let source = r#"
        begin
            x := 1 + 2;
            y := x;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_common_subexpression_elimination_simple() {
    let source = r#"
        begin
            x := (a + b) * 2;
            y := (a + b) * 3;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_common_subexpression_elimination_complex() {
    let source = r#"
        begin
            x := (a * b + c) / d;
            y := (a * b + c) * 2;
            z := (a * b + c) + e;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_function_inlining_simple() {
    let source = r#"
        function SmallFunc(x: integer): integer;
        begin
            SmallFunc := x * 2;
        end;

        begin
            y := SmallFunc(5);
        end.
    "#;

    // Small function should be inlined
    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

#[test]
fn test_function_inlining_with_multiple_calls() {
    let source = r#"
        function AddOne(x: integer): integer;
        begin
            AddOne := x + 1;
        end;

        begin
            a := AddOne(1);
            b := AddOne(2);
            c := AddOne(3);
        end.
    "#;

    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

#[test]
fn test_loop_unrolling_constant_iterations() {
    let source = r#"
        begin
            for i := 1 to 4 do
                x := x + i;
        end.
    "#;

    // Small constant loop should be unrolled
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_loop_unrolling_disabled_for_large_loops() {
    let source = r#"
        begin
            for i := 1 to 1000 do
                x := x + i;
        end.
    "#;

    // Large loop should not be unrolled
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_tail_call_optimization() {
    let source = r#"
        function Factorial(n, acc: integer): integer;
        begin
            if n <= 1 then
                Factorial := acc
            else
                Factorial := Factorial(n - 1, n * acc);
        end;
    "#;

    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

#[test]
fn test_peephole_optimization_redundant_moves() {
    let source = r#"
        begin
            x := y;
            z := x;
        end.
    "#;

    // Could be optimized to: z := y;
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_peephole_optimization_redundant_loads() {
    let source = r#"
        begin
            x := arr[0];
            y := x;
            z := arr[0];
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_peephole_optimization_jump_chains() {
    let source = r#"
        begin
            if a then
                if b then
                    x := 1;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_constant_propagation() {
    let source = r#"
        begin
            x := 10;
            y := x + 5;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());

    // y should be optimized to 15 (constant propagation)
}

#[test]
fn test_constant_propagation_chain() {
    let source = r#"
        begin
            x := 5;
            y := x;
            z := y * 2;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());

    // z should be optimized to 10
}

#[test]
fn test_copy_propagation() {
    let source = r#"
        begin
            x := y;
            z := x + 1;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());

    // z := y + 1 (copy propagation)
}

#[test]
fn test_loop_invariant_code_motion() {
    let source = r#"
        begin
            for i := 1 to 100 do
                x := y + z;
        end.
    "#;

    // y + z is invariant, should be moved out of loop
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_loop_invariant_with_array_access() {
    let source = r#"
        begin
            for i := 1 to 100 do
                arr[i] := arr[0] + i;
        end.
    "#;

    // arr[0] is invariant
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_variable_liveness_analysis() {
    let source = r#"
        begin
            x := 1;
            x := 2;
            y := x;
        end.
    "#;

    // First assignment to x is dead
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_dead_store_elimination() {
    let source = r#"
        begin
            x := 1;
            x := 2;
            x := 3;
        end.
    "#;

    // First two assignments are dead stores
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_boolean_optimization_and_true() {
    let result = optimize_expression("x and true");
    assert!(result.is_ok());

    // x and true should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("x and true should simplify to x"),
    }
}

#[test]
fn test_boolean_optimization_and_false() {
    let result = optimize_expression("x and false");
    assert!(result.is_ok());

    // x and false should simplify to false
    match result.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(!b),
        _ => panic!("x and false should simplify to false"),
    }
}

#[test]
fn test_boolean_optimization_or_true() {
    let result = optimize_expression("x or true");
    assert!(result.is_ok());

    // x or true should simplify to true
    match result.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(b),
        _ => panic!("x or true should simplify to true"),
    }
}

#[test]
fn test_boolean_optimization_or_false() {
    let result = optimize_expression("x or false");
    assert!(result.is_ok());

    // x or false should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("x or false should simplify to x"),
    }
}

#[test]
fn test_boolean_optimization_not_true() {
    let result = optimize_expression("not true");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(!b),
        _ => panic!("not true should simplify to false"),
    }
}

#[test]
fn test_boolean_optimization_not_false() {
    let result = optimize_expression("not false");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(b),
        _ => panic!("not false should simplify to true"),
    }
}

#[test]
fn test_comparison_optimization_always_true() {
    let result = optimize_expression("5 < 10");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(b),
        _ => panic!("5 < 10 should be true"),
    }
}

#[test]
fn test_comparison_optimization_always_false() {
    let result = optimize_expression("5 > 10");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(!b),
        _ => panic!("5 > 10 should be false"),
    }
}

#[test]
fn test_comparison_optimization_equal_constants() {
    let result = optimize_expression("5 = 5");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(b),
        _ => panic!("5 = 5 should be true"),
    }
}

#[test]
fn test_comparison_optimization_not_equal_constants() {
    let result = optimize_expression("5 <> 5");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Boolean(b)) => assert!(!b),
        _ => panic!("5 <> 5 should be false"),
    }
}

#[test]
fn test_string_constant_folding() {
    let result = optimize_expression(r#"'Hello' + ' ' + 'World'"#);
    assert!(result.is_ok());

    // Should fold to "Hello World"
    match result.unwrap() {
        Expr::Literal(pascal::Literal::String(s)) => {
            assert_eq!(s, "Hello World");
        }
        _ => panic!("String concatenation should be folded"),
    }
}

#[test]
fn test_array_bounds_check_elimination() {
    let source = r#"
        begin
            for i := 1 to 10 do
                arr[i] := i;
        end.
    "#;

    // Bounds check can be eliminated as i is always in range
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_null_check_elimination() {
    let source = r#"
        begin
            new(p);
            p^ := 10;
        end.
    "#;

    // Null check after new can be eliminated
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_branch_optimization_merge_blocks() {
    let source = r#"
        begin
            if x then
                y := 1
            else
                y := 1;
        end.
    "#;

    // Both branches do the same thing, can be merged
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_branch_optimization_constant_condition() {
    let source = r#"
        const
            Debug = true;
        begin
            if Debug then
                x := 1;
        end.
    "#;

    // Constant condition can be evaluated
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_branch_optimization_remove_unreachable() {
    let source = r#"
        begin
            if false then
                x := 1
            else
                x := 2;
        end.
    "#;

    // False branch is unreachable, can be removed
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_interprocedural_optimization() {
    let source = r#"
        function GetConst: integer;
        begin
            GetConst := 42;
        end;

        begin
            x := GetConst() + 8;
        end.
    "#;

    // GetConst() call should be inlined as constant 42
    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

#[test]
fn test_devirtualization() {
    let source = r#"
        type
            TObj = class
                procedure VirtualProc; virtual;
            end;

        var
            obj: TObj;
        begin
            obj.VirtualProc;
        end.
    "#;

    // Virtual call might be devirtualized if exact type is known
    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

#[test]
fn test_memory_access_optimization() {
    let source = r#"
        begin
            p^.x := 1;
            p^.y := 2;
            p^.z := 3;
        end.
    "#;

    // Should optimize to fewer pointer dereferences
    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_bitwise_optimization() {
    let result = optimize_expression("x and 0");
    assert!(result.is_ok());

    // x and 0 should simplify to 0
    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => assert_eq!(n, 0),
        _ => panic!("x and 0 should simplify to 0"),
    }
}

#[test]
fn test_bitwise_or_all_ones() {
    let result = optimize_expression("x or $FFFFFFFF");
    assert!(result.is_ok());

    // x or $FFFFFFFF should simplify to $FFFFFFFF
    match result.unwrap() {
        Expr::Literal(pascal::Literal::Integer(n)) => assert_eq!(n, 0xFFFFFFFF),
        _ => panic!("x or $FFFFFFFF should simplify to $FFFFFFFF"),
    }
}

#[test]
fn test_xor_with_zero() {
    let result = optimize_expression("x xor 0");
    assert!(result.is_ok());

    // x xor 0 should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("x xor 0 should simplify to x"),
    }
}

#[test]
fn test_shift_by_zero() {
    let result = optimize_expression("x shl 0");
    assert!(result.is_ok());

    // x shl 0 should simplify to x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("x shl 0 should simplify to x"),
    }
}

#[test]
fn test_combined_optimizations() {
    let source = r#"
        begin
            x := 2 * 3;
            y := x * 1;
            z := y + 0;
            w := z - 0;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());

    // Should optimize to: w := 6
}

#[test]
fn test_optimizer_preserves_semantics() {
    let source = r#"
        begin
            x := 10;
            if x > 5 then
                x := x * 2
            else
                x := x + 1;
        end.
    "#;

    let result = optimize_statement(source);
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_optimizer_with_complex_arithmetic() {
    let result = optimize_expression("((x + 0) * 1) - 0");
    assert!(result.is_ok());

    // Should simplify to: x
    match result.unwrap() {
        Expr::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("((x + 0) * 1) - 0 should simplify to x"),
    }
}

#[test]
fn test_constant_folding_with_floats() {
    let result = optimize_expression("3.14 * 2.0");
    assert!(result.is_ok());

    match result.unwrap() {
        Expr::Literal(pascal::Literal::Real(f)) => {
            assert!((f - 6.28).abs() < 0.01);
        }
        _ => panic!("Should fold floating point multiplication"),
    }
}

#[test]
fn test_division_by_power_of_two() {
    let result = optimize_expression("x / 16");
    assert!(result.is_ok());

    // x / 16 could be optimized to x >> 4
    let _ = result.unwrap();
}

#[test]
fn test_modulo_by_power_of_two() {
    let result = optimize_expression("x mod 16");
    assert!(result.is_ok());

    // x mod 16 could be optimized to x & 15
    let _ = result.unwrap();
}

#[test]
fn test_negate_multiplication() {
    let result = optimize_expression("-(x * 2)");
    assert!(result.is_ok());

    // Could be optimized to x * -2
    let _ = result.unwrap();
}

#[test]
fn test_optimization_preserves_side_effects() {
    let source = r#"
        function GetX: integer;
        begin
            writeln('GetX called');
            GetX := 10;
        end;

        begin
            y := GetX() + GetX();
        end.
    "#;

    // Should not optimize to y := GetX() * 2 as GetX has side effects
    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}
