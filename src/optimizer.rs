//! Code optimization
//!
//! Provides constant folding, dead code elimination, and peephole optimization

use crate::ast::{Block, Expr, Literal, Stmt};
use std::collections::HashSet;

/// Optimizer for expressions and statements
pub struct Optimizer {
    enable_constant_folding: bool,
    enable_dead_code_elimination: bool,
}

impl Optimizer {
    /// Create a new optimizer
    pub fn new() -> Self {
        Self {
            enable_constant_folding: true,
            enable_dead_code_elimination: true,
        }
    }

    /// Optimize an expression
    pub fn optimize_expr(&self, expr: &Expr) -> Expr {
        if !self.enable_constant_folding {
            return expr.clone();
        }

        match expr {
            Expr::BinaryOp {
                operator,
                left,
                right,
            } => {
                let left_opt = self.optimize_expr(left);
                let right_opt = self.optimize_expr(right);

                // Try constant folding
                if let Some(result) = self.fold_binary_op(operator, &left_opt, &right_opt) {
                    return result;
                }

                Expr::BinaryOp {
                    operator: operator.clone(),
                    left: Box::new(left_opt),
                    right: Box::new(right_opt),
                }
            }

            Expr::UnaryOp { operator, operand } => {
                let operand_opt = self.optimize_expr(operand);

                // Try constant folding
                if let Some(result) = self.fold_unary_op(operator, &operand_opt) {
                    return result;
                }

                Expr::UnaryOp {
                    operator: operator.clone(),
                    operand: Box::new(operand_opt),
                }
            }

            _ => expr.clone(),
        }
    }

    /// Fold binary operation with constants
    fn fold_binary_op(&self, op: &str, left: &Expr, right: &Expr) -> Option<Expr> {
        match (left, right) {
            (Expr::Literal(Literal::Integer(l)), Expr::Literal(Literal::Integer(r))) => {
                let result = match op {
                    "+" => l + r,
                    "-" => l - r,
                    "*" => l * r,
                    "/" if *r != 0 => l / r,
                    "div" if *r != 0 => l / r,
                    "mod" if *r != 0 => l % r,
                    _ => return None,
                };
                Some(Expr::Literal(Literal::Integer(result)))
            }

            (Expr::Literal(Literal::Boolean(l)), Expr::Literal(Literal::Boolean(r))) => {
                let result = match op {
                    "and" => *l && *r,
                    "or" => *l || *r,
                    "xor" => *l ^ *r,
                    "=" => l == r,
                    "<>" => l != r,
                    _ => return None,
                };
                Some(Expr::Literal(Literal::Boolean(result)))
            }

            // Algebraic simplifications
            (Expr::Literal(Literal::Integer(0)), _) if matches!(op, "+") => Some(right.clone()),
            (_, Expr::Literal(Literal::Integer(0))) if matches!(op, "+" | "-") => {
                Some(left.clone())
            }
            (Expr::Literal(Literal::Integer(1)), _) if matches!(op, "*") => Some(right.clone()),
            (_, Expr::Literal(Literal::Integer(1))) if matches!(op, "*" | "/") => {
                Some(left.clone())
            }
            (_, Expr::Literal(Literal::Integer(0))) if matches!(op, "*") => {
                Some(Expr::Literal(Literal::Integer(0)))
            }
            (Expr::Literal(Literal::Integer(0)), _) if matches!(op, "*") => {
                Some(Expr::Literal(Literal::Integer(0)))
            }

            _ => None,
        }
    }

    /// Fold unary operation with constants
    fn fold_unary_op(&self, op: &str, expr: &Expr) -> Option<Expr> {
        match expr {
            Expr::Literal(Literal::Integer(val)) => match op {
                "-" | "not" => Some(Expr::Literal(Literal::Integer(-val))),
                "+" => Some(expr.clone()),
                _ => None,
            },

            Expr::Literal(Literal::Boolean(val)) => match op {
                "not" => Some(Expr::Literal(Literal::Boolean(!val))),
                _ => None,
            },

            _ => None,
        }
    }

    /// Optimize a statement
    pub fn optimize_stmt(&self, stmt: &Stmt) -> Option<Stmt> {
        if !self.enable_dead_code_elimination {
            return Some(stmt.clone());
        }

        match stmt {
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let opt_condition = self.optimize_expr(condition);

                // Dead code elimination for constant conditions
                if let Expr::Literal(Literal::Boolean(val)) = opt_condition {
                    if val {
                        // Condition is always true, keep only then branch
                        if then_branch.len() == 1 {
                            return Some(then_branch[0].clone());
                        } else {
                            return Some(Stmt::Block(Block::with_statements(then_branch.clone())));
                        }
                    } else if let Some(else_stmts) = else_branch {
                        // Condition is always false, keep only else branch
                        if else_stmts.len() == 1 {
                            return Some(else_stmts[0].clone());
                        } else {
                            return Some(Stmt::Block(Block::with_statements(else_stmts.clone())));
                        }
                    } else {
                        // Condition is false and no else, remove entire if
                        return None;
                    }
                }

                Some(Stmt::If {
                    condition: opt_condition,
                    then_branch: then_branch.clone(),
                    else_branch: else_branch.clone(),
                })
            }

            Stmt::While { condition, body } => {
                let opt_condition = self.optimize_expr(condition);

                // Dead code elimination for constant false condition
                if let Expr::Literal(Literal::Boolean(false)) = opt_condition {
                    return None; // No statement for always-false while
                }

                Some(Stmt::While {
                    condition: opt_condition,
                    body: body.clone(),
                })
            }

            Stmt::Assignment { target, value } => {
                let opt_value = self.optimize_expr(value);
                Some(Stmt::Assignment {
                    target: target.clone(),
                    value: opt_value,
                })
            }

            _ => Some(stmt.clone()),
        }
    }

    /// Collect procedure/function names called from block (recursively, fixed-point)
    fn collect_calls(block: &Block, calls: &mut HashSet<String>) {
        for stmt in &block.statements {
            Optimizer::collect_calls_from_stmt(stmt, calls);
        }
        loop {
            let prev_len = calls.len();
            for proc in &block.procedures {
                if calls.contains(&proc.name.to_lowercase()) {
                    Optimizer::collect_calls(&proc.block, calls);
                }
            }
            for func in &block.functions {
                if calls.contains(&func.name.to_lowercase()) {
                    Optimizer::collect_calls(&func.block, calls);
                }
            }
            if calls.len() == prev_len {
                break;
            }
        }
    }

    fn collect_calls_from_stmt(stmt: &Stmt, calls: &mut HashSet<String>) {
        match stmt {
            Stmt::ProcedureCall { name, arguments } => {
                calls.insert(name.to_lowercase());
                for arg in arguments {
                    Optimizer::collect_calls_from_expr(arg, calls);
                }
            }
            Stmt::Assignment { value, .. } => Optimizer::collect_calls_from_expr(value, calls),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                Optimizer::collect_calls_from_expr(condition, calls);
                for s in then_branch {
                    Optimizer::collect_calls_from_stmt(s, calls);
                }
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        Optimizer::collect_calls_from_stmt(s, calls);
                    }
                }
            }
            Stmt::While { condition, body } => {
                Optimizer::collect_calls_from_expr(condition, calls);
                for s in body {
                    Optimizer::collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::For {
                start,
                end,
                body,
                ..
            } => {
                Optimizer::collect_calls_from_expr(start, calls);
                Optimizer::collect_calls_from_expr(end, calls);
                for s in body {
                    Optimizer::collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::Repeat { body, until_condition } => {
                Optimizer::collect_calls_from_expr(until_condition, calls);
                for s in body {
                    Optimizer::collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::Case {
                expression,
                branches,
                else_branch,
            } => {
                Optimizer::collect_calls_from_expr(expression, calls);
                for branch in branches {
                    if let Some(ref g) = branch.guard {
                        Optimizer::collect_calls_from_expr(g, calls);
                    }
                    for s in &branch.body {
                        Optimizer::collect_calls_from_stmt(s, calls);
                    }
                }
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        Optimizer::collect_calls_from_stmt(s, calls);
                    }
                }
            }
            Stmt::Try {
                try_block,
                except_clauses,
                finally_block,
            } => {
                for s in try_block {
                    Optimizer::collect_calls_from_stmt(s, calls);
                }
                for ec in except_clauses {
                    for s in &ec.body {
                        Optimizer::collect_calls_from_stmt(s, calls);
                    }
                }
                if let Some(fb) = finally_block {
                    for s in fb {
                        Optimizer::collect_calls_from_stmt(s, calls);
                    }
                }
            }
            Stmt::With { variable, statements } => {
                Optimizer::collect_calls_from_expr(variable, calls);
                for s in statements {
                    Optimizer::collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::Block(b) => Optimizer::collect_calls(b, calls),
            _ => {}
        }
    }

    fn collect_calls_from_expr(expr: &Expr, calls: &mut HashSet<String>) {
        match expr {
            Expr::FunctionCall { name, arguments } => {
                calls.insert(name.to_lowercase());
                for arg in arguments {
                    Optimizer::collect_calls_from_expr(arg, calls);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                Optimizer::collect_calls_from_expr(left, calls);
                Optimizer::collect_calls_from_expr(right, calls);
            }
            Expr::UnaryOp { operand, .. } => Optimizer::collect_calls_from_expr(operand, calls),
            _ => {}
        }
    }

    /// Eliminate procedures and functions never called from the block (single-unit DCE)
    pub fn eliminate_dead_procedures_and_functions(block: &mut Block) {
        let mut used = HashSet::new();
        Optimizer::collect_calls(block, &mut used);

        block.procedures.retain(|p| used.contains(&p.name.to_lowercase()));
        block.functions.retain(|f| used.contains(&f.name.to_lowercase()));
    }

    /// Peephole optimization on assembly instructions
    pub fn peephole_optimize(&self, instructions: &[String]) -> Vec<String> {
        let mut optimized = Vec::new();
        let mut i = 0;

        while i < instructions.len() {
            let inst = &instructions[i];

            // Remove redundant moves: mov rax, rax
            if inst.contains("mov") && inst.contains("rax") {
                let parts: Vec<&str> = inst.split(',').collect();
                if parts.len() == 2 && parts[0].trim().ends_with("rax") && parts[1].trim() == "rax"
                {
                    i += 1;
                    continue;
                }
            }

            // Optimize push/pop pairs: push rax; pop rax -> nothing
            if i + 1 < instructions.len() {
                if inst.trim().starts_with("push") && instructions[i + 1].trim().starts_with("pop")
                {
                    let push_reg = inst.trim().strip_prefix("push").unwrap().trim();
                    let pop_reg = instructions[i + 1]
                        .trim()
                        .strip_prefix("pop")
                        .unwrap()
                        .trim();
                    if push_reg == pop_reg {
                        i += 2;
                        continue;
                    }
                }
            }

            // Optimize add rax, 0 -> nothing
            if inst.contains("add") && inst.contains(", 0") {
                i += 1;
                continue;
            }

            // Optimize sub rax, 0 -> nothing
            if inst.contains("sub") && inst.contains(", 0") {
                i += 1;
                continue;
            }

            optimized.push(inst.clone());
            i += 1;
        }

        optimized
    }
}

impl Default for Optimizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_folding() {
        let optimizer = Optimizer::new();

        // 2 + 3 = 5
        let expr = Expr::BinaryOp {
            operator: "+".to_string(),
            left: Box::new(Expr::Literal(Literal::Integer(2))),
            right: Box::new(Expr::Literal(Literal::Integer(3))),
        };

        let result = optimizer.optimize_expr(&expr);
        assert_eq!(result, Expr::Literal(Literal::Integer(5)));
    }

    #[test]
    fn test_algebraic_simplification() {
        let optimizer = Optimizer::new();

        // x + 0 = x
        let expr = Expr::BinaryOp {
            operator: "+".to_string(),
            left: Box::new(Expr::Variable("x".to_string())),
            right: Box::new(Expr::Literal(Literal::Integer(0))),
        };

        let result = optimizer.optimize_expr(&expr);
        assert_eq!(result, Expr::Variable("x".to_string()));
    }

    #[test]
    fn test_dead_code_elimination() {
        let optimizer = Optimizer::new();

        // if false then ... -> nothing
        let stmt = Stmt::If {
            condition: Expr::Literal(Literal::Boolean(false)),
            then_branch: vec![],
            else_branch: None,
        };

        let result = optimizer.optimize_stmt(&stmt);
        assert!(result.is_none());
    }

    #[test]
    fn test_eliminate_dead_procedures() {
        use crate::ast::{Block, ProcedureDecl};

        let mut block = Block {
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![
                ProcedureDecl {
                    name: "Used".to_string(),
                    parameters: vec![],
                    block: Block::empty(),
                    visibility: crate::ast::FieldVisibility::Public,
                    is_external: false,
                    external_name: None,
                    is_inline: false,
                    is_forward: false,
                    is_class_method: false,
                    is_virtual: false,
                    is_override: false,
                    is_overload: false,
                },
                ProcedureDecl {
                    name: "Dead".to_string(),
                    parameters: vec![],
                    block: Block::empty(),
                    visibility: crate::ast::FieldVisibility::Public,
                    is_external: false,
                    external_name: None,
                    is_inline: false,
                    is_forward: false,
                    is_class_method: false,
                    is_virtual: false,
                    is_override: false,
                    is_overload: false,
                },
            ],
            functions: vec![],
            classes: vec![],
            statements: vec![Stmt::ProcedureCall {
                name: "Used".to_string(),
                arguments: vec![],
            }],
        };

        Optimizer::eliminate_dead_procedures_and_functions(&mut block);
        assert_eq!(block.procedures.len(), 1);
        assert_eq!(block.procedures[0].name, "Used");
    }
}
