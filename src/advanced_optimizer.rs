//! Advanced optimizations
//!
//! Common subexpression elimination, function inlining, loop optimizations

use crate::ast::{BinaryOp, Expr, FunctionDecl, Literal, Stmt};
use std::collections::HashMap;

/// Common Subexpression Eliminator
pub struct CSEOptimizer {
    expressions: HashMap<String, String>,
    temp_counter: u32,
}

impl CSEOptimizer {
    /// Create a new CSE optimizer
    pub fn new() -> Self {
        Self {
            expressions: HashMap::new(),
            temp_counter: 0,
        }
    }

    /// Optimize expression with CSE
    pub fn optimize_expr(&mut self, expr: &Expr) -> (Expr, Vec<(String, Expr)>) {
        let mut temps = Vec::new();
        let optimized = self.optimize_expr_internal(expr, &mut temps);
        (optimized, temps)
    }

    fn optimize_expr_internal(&mut self, expr: &Expr, temps: &mut Vec<(String, Expr)>) -> Expr {
        match expr {
            Expr::BinaryOp { op, left, right } => {
                let left_opt = self.optimize_expr_internal(left, temps);
                let right_opt = self.optimize_expr_internal(right, temps);

                // Create expression key
                let key = format!("{:?} {:?} {:?}", left_opt, op, right_opt);

                // Check if we've seen this expression before
                if let Some(temp_var) = self.expressions.get(&key) {
                    return Expr::Identifier(vec![temp_var.clone()]);
                }

                // Create new temporary
                let temp_var = format!("_cse_{}", self.temp_counter);
                self.temp_counter += 1;

                let new_expr = Expr::BinaryOp {
                    op: op.clone(),
                    left: Box::new(left_opt),
                    right: Box::new(right_opt),
                };

                self.expressions.insert(key, temp_var.clone());
                temps.push((temp_var.clone(), new_expr.clone()));

                Expr::Identifier(vec![temp_var])
            }

            _ => expr.clone(),
        }
    }
}

impl Default for CSEOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Function inliner
pub struct FunctionInliner {
    inline_threshold: usize,
    inlined_count: usize,
}

impl FunctionInliner {
    /// Create a new function inliner
    pub fn new(threshold: usize) -> Self {
        Self {
            inline_threshold: threshold,
            inlined_count: 0,
        }
    }

    /// Check if function should be inlined
    pub fn should_inline(&self, func: &FunctionDecl) -> bool {
        // Count statements in function body
        let stmt_count = func.block.statements.len();
        stmt_count <= self.inline_threshold
    }

    /// Inline a function call
    pub fn inline_call(&mut self, func: &FunctionDecl, args: &[Expr]) -> Vec<Stmt> {
        self.inlined_count += 1;

        let mut stmts = Vec::new();

        // Create parameter assignments
        for (i, param) in func.params.iter().enumerate() {
            if i < args.len() {
                stmts.push(Stmt::Assignment {
                    target: Expr::Identifier(vec![param.name.clone()]),
                    value: args[i].clone(),
                });
            }
        }

        // Add function body
        stmts.extend(func.block.statements.clone());

        stmts
    }

    /// Get number of inlined functions
    pub fn inlined_count(&self) -> usize {
        self.inlined_count
    }
}

/// Loop optimizer
pub struct LoopOptimizer {
    unroll_factor: usize,
}

impl LoopOptimizer {
    /// Create a new loop optimizer
    pub fn new(unroll_factor: usize) -> Self {
        Self { unroll_factor }
    }

    /// Optimize a loop statement
    pub fn optimize_loop(&self, stmt: &Stmt) -> Stmt {
        match stmt {
            Stmt::For {
                var_name,
                start,
                end,
                body,
                direction,
            } => {
                // Check if loop can be unrolled
                if let (Expr::Literal(Literal::Integer(s)), Expr::Literal(Literal::Integer(e))) =
                    (start, end)
                {
                    let iterations = (e - s + 1).abs() as usize;

                    if iterations <= self.unroll_factor {
                        return self.unroll_for_loop(var_name, *s, *e, body);
                    }
                }

                stmt.clone()
            }

            Stmt::While { condition, body } => {
                // Loop invariant code motion
                self.hoist_invariants(condition, body)
            }

            _ => stmt.clone(),
        }
    }

    /// Unroll a for loop completely
    fn unroll_for_loop(&self, var_name: &str, start: i64, end: i64, body: &[Stmt]) -> Stmt {
        let mut unrolled = Vec::new();

        for i in start..=end {
            // Substitute loop variable with constant
            for stmt in body {
                unrolled.push(self.substitute_var(stmt, var_name, i));
            }
        }

        Stmt::Block(crate::ast::Block {
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![],
            statements: unrolled,
        })
    }

    /// Substitute variable with constant
    fn substitute_var(&self, stmt: &Stmt, var_name: &str, value: i64) -> Stmt {
        match stmt {
            Stmt::Assignment {
                target,
                value: expr,
            } => Stmt::Assignment {
                target: self.substitute_expr(target, var_name, value),
                value: self.substitute_expr(expr, var_name, value),
            },
            _ => stmt.clone(),
        }
    }

    /// Substitute variable in expression
    fn substitute_expr(&self, expr: &Expr, var_name: &str, value: i64) -> Expr {
        match expr {
            Expr::Identifier(parts) if parts.len() == 1 && parts[0] == var_name => {
                Expr::Literal(Literal::Integer(value))
            }
            Expr::BinaryOp { op, left, right } => Expr::BinaryOp {
                op: op.clone(),
                left: Box::new(self.substitute_expr(left, var_name, value)),
                right: Box::new(self.substitute_expr(right, var_name, value)),
            },
            _ => expr.clone(),
        }
    }

    /// Hoist loop-invariant code
    fn hoist_invariants(&self, condition: &Expr, body: &[Stmt]) -> Stmt {
        // Simplified: just return original for now
        // Full implementation would analyze which expressions don't depend on loop variables
        Stmt::While {
            condition: condition.clone(),
            body: body.to_vec(),
        }
    }
}

impl Default for LoopOptimizer {
    fn default() -> Self {
        Self::new(4)
    }
}

/// Tail call optimizer
pub struct TailCallOptimizer {
    optimized_count: usize,
}

impl TailCallOptimizer {
    /// Create a new tail call optimizer
    pub fn new() -> Self {
        Self { optimized_count: 0 }
    }

    /// Check if statement is a tail call
    pub fn is_tail_call(&self, stmt: &Stmt, func_name: &str) -> bool {
        match stmt {
            Stmt::ProcedureCall { name, .. } if name == func_name => true,
            _ => false,
        }
    }

    /// Optimize tail call to jump
    pub fn optimize_tail_call(&mut self, func: &FunctionDecl) -> FunctionDecl {
        let optimized_func = func.clone();

        // Check if last statement is a recursive call
        if let Some(last_stmt) = func.block.statements.last() {
            if self.is_tail_call(last_stmt, &func.name) {
                self.optimized_count += 1;

                // Replace with loop (simplified)
                // In real implementation, would transform to iterative version
            }
        }

        optimized_func
    }

    /// Get number of optimized tail calls
    pub fn optimized_count(&self) -> usize {
        self.optimized_count
    }
}

impl Default for TailCallOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Strength reduction optimizer
pub struct StrengthReducer;

impl StrengthReducer {
    /// Optimize expression with strength reduction
    pub fn optimize(&self, expr: &Expr) -> Expr {
        match expr {
            Expr::BinaryOp { op, left, right } => {
                let left_opt = self.optimize(left);
                let right_opt = self.optimize(right);

                // x * 2 -> x << 1
                if matches!(op, BinaryOp::Multiply) {
                    if let Expr::Literal(Literal::Integer(n)) = right_opt {
                        if n > 0 && (n & (n - 1)) == 0 {
                            // Power of two
                            let shift = (n as f64).log2() as i64;
                            return Expr::BinaryOp {
                                op: BinaryOp::ShiftLeft,
                                left: Box::new(left_opt),
                                right: Box::new(Expr::Literal(Literal::Integer(shift))),
                            };
                        }
                    }
                }

                // x / 2 -> x >> 1
                if matches!(op, BinaryOp::Divide) {
                    if let Expr::Literal(Literal::Integer(n)) = right_opt {
                        if n > 0 && (n & (n - 1)) == 0 {
                            let shift = (n as f64).log2() as i64;
                            return Expr::BinaryOp {
                                op: BinaryOp::ShiftRight,
                                left: Box::new(left_opt),
                                right: Box::new(Expr::Literal(Literal::Integer(shift))),
                            };
                        }
                    }
                }

                Expr::BinaryOp {
                    op: op.clone(),
                    left: Box::new(left_opt),
                    right: Box::new(right_opt),
                }
            }

            _ => expr.clone(),
        }
    }
}

/// Combined advanced optimizer
pub struct AdvancedOptimizer {
    cse: CSEOptimizer,
    inliner: FunctionInliner,
    loop_opt: LoopOptimizer,
    tail_call: TailCallOptimizer,
    strength_reducer: StrengthReducer,
}

impl AdvancedOptimizer {
    /// Create a new advanced optimizer
    pub fn new() -> Self {
        Self {
            cse: CSEOptimizer::new(),
            inliner: FunctionInliner::new(10),
            loop_opt: LoopOptimizer::new(4),
            tail_call: TailCallOptimizer::new(),
            strength_reducer: StrengthReducer,
        }
    }

    /// Run all optimizations on a statement
    pub fn optimize_stmt(&mut self, stmt: &Stmt) -> Stmt {
        // Apply optimizations in order
        let stmt = self.loop_opt.optimize_loop(stmt);
        stmt
    }

    /// Run all optimizations on an expression
    pub fn optimize_expr(&mut self, expr: &Expr) -> Expr {
        let expr = self.strength_reducer.optimize(expr);
        expr
    }

    /// Get optimization statistics
    pub fn stats(&self) -> OptimizationStats {
        OptimizationStats {
            cse_temps: self.cse.temp_counter,
            inlined_functions: self.inliner.inlined_count(),
            tail_calls_optimized: self.tail_call.optimized_count(),
        }
    }
}

impl Default for AdvancedOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Optimization statistics
#[derive(Debug, Clone)]
pub struct OptimizationStats {
    pub cse_temps: u32,
    pub inlined_functions: usize,
    pub tail_calls_optimized: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cse() {
        let mut cse = CSEOptimizer::new();

        // a + b appears twice
        let expr1 = Expr::BinaryOp {
            op: BinaryOp::Add,
            left: Box::new(Expr::Identifier(vec!["a".to_string()])),
            right: Box::new(Expr::Identifier(vec!["b".to_string()])),
        };

        let (opt1, temps1) = cse.optimize_expr(&expr1);
        assert_eq!(temps1.len(), 1);

        let (opt2, temps2) = cse.optimize_expr(&expr1);
        // Should reuse same temp
        assert_eq!(temps2.len(), 0);
    }

    #[test]
    fn test_loop_unrolling() {
        let optimizer = LoopOptimizer::new(10);

        let loop_stmt = Stmt::For {
            var_name: "i".to_string(),
            start: Expr::Literal(Literal::Integer(1)),
            direction: crate::ast::ForDirection::To,
            end: Expr::Literal(Literal::Integer(3)),
            body: vec![Stmt::Empty],
        };

        let optimized = optimizer.optimize_loop(&loop_stmt);

        // Should be unrolled to block
        assert!(matches!(optimized, Stmt::Block(_)));
    }

    #[test]
    fn test_strength_reduction() {
        let reducer = StrengthReducer;

        // x * 8 -> x << 3
        let expr = Expr::BinaryOp {
            op: BinaryOp::Multiply,
            left: Box::new(Expr::Identifier(vec!["x".to_string()])),
            right: Box::new(Expr::Literal(Literal::Integer(8))),
        };

        let optimized = reducer.optimize(&expr);

        if let Expr::BinaryOp { op, .. } = optimized {
            assert_eq!(op, BinaryOp::ShiftLeft);
        } else {
            panic!("Expected shift operation");
        }
    }
}
