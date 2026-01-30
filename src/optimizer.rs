//! Code optimization
//!
//! Provides constant folding, dead code elimination, and peephole optimization

use crate::ast::{BinaryOp, Expr, Literal, Stmt, UnaryOp};

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
            Expr::BinaryOp { op, left, right } => {
                let left_opt = self.optimize_expr(left);
                let right_opt = self.optimize_expr(right);

                // Try constant folding
                if let Some(result) = self.fold_binary_op(op, &left_opt, &right_opt) {
                    return result;
                }

                Expr::BinaryOp {
                    op: op.clone(),
                    left: Box::new(left_opt),
                    right: Box::new(right_opt),
                }
            }

            Expr::UnaryOp { op, expr: inner } => {
                let inner_opt = self.optimize_expr(inner);

                // Try constant folding
                if let Some(result) = self.fold_unary_op(op, &inner_opt) {
                    return result;
                }

                Expr::UnaryOp {
                    op: op.clone(),
                    expr: Box::new(inner_opt),
                }
            }

            _ => expr.clone(),
        }
    }

    /// Fold binary operation with constants
    fn fold_binary_op(&self, op: &BinaryOp, left: &Expr, right: &Expr) -> Option<Expr> {
        match (left, right) {
            (Expr::Literal(Literal::Integer(l)), Expr::Literal(Literal::Integer(r))) => {
                let result = match op {
                    BinaryOp::Add => l + r,
                    BinaryOp::Subtract => l - r,
                    BinaryOp::Multiply => l * r,
                    BinaryOp::Divide if *r != 0 => l / r,
                    BinaryOp::IntDivide if *r != 0 => l / r,
                    BinaryOp::Modulo if *r != 0 => l % r,
                    _ => return None,
                };
                Some(Expr::Literal(Literal::Integer(result)))
            }

            (Expr::Literal(Literal::Boolean(l)), Expr::Literal(Literal::Boolean(r))) => {
                let result = match op {
                    BinaryOp::And => *l && *r,
                    BinaryOp::Or => *l || *r,
                    BinaryOp::Xor => *l ^ *r,
                    BinaryOp::Equal => l == r,
                    BinaryOp::NotEqual => l != r,
                    _ => return None,
                };
                Some(Expr::Literal(Literal::Boolean(result)))
            }

            // Algebraic simplifications
            (Expr::Literal(Literal::Integer(0)), _) if matches!(op, BinaryOp::Add) => {
                Some(right.clone())
            }
            (_, Expr::Literal(Literal::Integer(0)))
                if matches!(op, BinaryOp::Add | BinaryOp::Subtract) =>
            {
                Some(left.clone())
            }
            (Expr::Literal(Literal::Integer(1)), _) if matches!(op, BinaryOp::Multiply) => {
                Some(right.clone())
            }
            (_, Expr::Literal(Literal::Integer(1)))
                if matches!(op, BinaryOp::Multiply | BinaryOp::Divide) =>
            {
                Some(left.clone())
            }
            (_, Expr::Literal(Literal::Integer(0))) if matches!(op, BinaryOp::Multiply) => {
                Some(Expr::Literal(Literal::Integer(0)))
            }
            (Expr::Literal(Literal::Integer(0)), _) if matches!(op, BinaryOp::Multiply) => {
                Some(Expr::Literal(Literal::Integer(0)))
            }

            _ => None,
        }
    }

    /// Fold unary operation with constants
    fn fold_unary_op(&self, op: &UnaryOp, expr: &Expr) -> Option<Expr> {
        match expr {
            Expr::Literal(Literal::Integer(val)) => match op {
                UnaryOp::Minus | UnaryOp::Negate => Some(Expr::Literal(Literal::Integer(-val))),
                UnaryOp::Plus => Some(expr.clone()),
                _ => None,
            },

            Expr::Literal(Literal::Boolean(val)) => match op {
                UnaryOp::Not => Some(Expr::Literal(Literal::Boolean(!val))),
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
                            return Some(Stmt::Block(crate::ast::Block {
                                consts: vec![],
                                types: vec![],
                                vars: vec![],
                                procedures: vec![],
                                functions: vec![],
                                statements: then_branch.clone(),
                            }));
                        }
                    } else if let Some(else_stmts) = else_branch {
                        // Condition is always false, keep only else branch
                        if else_stmts.len() == 1 {
                            return Some(else_stmts[0].clone());
                        } else {
                            return Some(Stmt::Block(crate::ast::Block {
                                consts: vec![],
                                types: vec![],
                                vars: vec![],
                                procedures: vec![],
                                functions: vec![],
                                statements: else_stmts.clone(),
                            }));
                        }
                    } else {
                        // Condition is false and no else, remove entire if
                        return Some(Stmt::Empty);
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
                    return Some(Stmt::Empty);
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
            op: BinaryOp::Add,
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
            op: BinaryOp::Add,
            left: Box::new(Expr::Identifier(vec!["x".to_string()])),
            right: Box::new(Expr::Literal(Literal::Integer(0))),
        };

        let result = optimizer.optimize_expr(&expr);
        assert_eq!(result, Expr::Identifier(vec!["x".to_string()]));
    }

    #[test]
    fn test_dead_code_elimination() {
        let optimizer = Optimizer::new();

        // if false then ... -> empty
        let stmt = Stmt::If {
            condition: Expr::Literal(Literal::Boolean(false)),
            then_branch: vec![Stmt::Empty],
            else_branch: None,
        };

        let result = optimizer.optimize_stmt(&stmt);
        assert!(matches!(result, Some(Stmt::Empty)));
    }
}
