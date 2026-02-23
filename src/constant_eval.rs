//! Compile-time constant evaluation
//!
//! Evaluates constant expressions at compile time (e.g. const x = 1 + 2).

use crate::ast::{Expr, Literal};
use anyhow::{anyhow, Result};

/// Evaluate a constant expression at compile time.
/// Returns None if the expression is not constant (contains variables, function calls, etc.).
pub fn eval_constant(expr: &Expr) -> Result<Option<Literal>> {
    match expr {
        Expr::Literal(lit) => Ok(Some(lit.clone())),

        Expr::BinaryOp {
            operator,
            left,
            right,
        } => {
            let left_val = match eval_constant(left)? {
                Some(l) => l,
                None => return Ok(None),
            };
            let right_val = match eval_constant(right)? {
                Some(r) => r,
                None => return Ok(None),
            };
            eval_binary_op(operator, &left_val, &right_val).map(Some)
        }

        Expr::UnaryOp { operator, operand } => {
            let val = match eval_constant(operand)? {
                Some(v) => v,
                None => return Ok(None),
            };
            eval_unary_op(operator, &val).map(Some)
        }

        _ => Ok(None), // Variable, function call, etc. - not constant
    }
}

fn eval_binary_op(op: &str, left: &Literal, right: &Literal) -> Result<Literal> {
    match (left, right) {
        (Literal::Integer(l), Literal::Integer(r)) => {
            let result = match op {
                "+" => l + r,
                "-" => l - r,
                "*" => l * r,
                "/" | "div" if *r != 0 => l / r,
                "mod" if *r != 0 => l % r,
                "and" => (*l != 0 && *r != 0) as i64,
                "or" => (*l != 0 || *r != 0) as i64,
                "=" => (l == r) as i64,
                "<>" => (l != r) as i64,
                "<" => (l < r) as i64,
                "<=" => (l <= r) as i64,
                ">" => (l > r) as i64,
                ">=" => (l >= r) as i64,
                _ => return Err(anyhow!("Unknown operator: {}", op)),
            };
            Ok(Literal::Integer(result))
        }

        (Literal::Real(l), Literal::Real(r)) => match op {
            "+" => Ok(Literal::Real(l + r)),
            "-" => Ok(Literal::Real(l - r)),
            "*" => Ok(Literal::Real(l * r)),
            "/" if *r != 0.0 => Ok(Literal::Real(l / r)),
            "=" => Ok(Literal::Boolean((l - r).abs() < f64::EPSILON)),
            "<>" => Ok(Literal::Boolean((l - r).abs() >= f64::EPSILON)),
            "<" => Ok(Literal::Boolean(l < r)),
            "<=" => Ok(Literal::Boolean(l <= r)),
            ">" => Ok(Literal::Boolean(l > r)),
            ">=" => Ok(Literal::Boolean(l >= r)),
            _ => Err(anyhow!("Unknown operator for real: {}", op)),
        },

        (Literal::Boolean(l), Literal::Boolean(r)) => {
            let result = match op {
                "and" => *l && *r,
                "or" => *l || *r,
                "xor" => *l ^ *r,
                "=" => l == r,
                "<>" => l != r,
                _ => return Err(anyhow!("Unknown operator for boolean: {}", op)),
            };
            Ok(Literal::Boolean(result))
        }

        (Literal::Integer(l), Literal::Real(r)) => {
            let lf = *l as f64;
            let rf = *r;
            match op {
                "+" => Ok(Literal::Real(lf + rf)),
                "-" => Ok(Literal::Real(lf - rf)),
                "*" => Ok(Literal::Real(lf * rf)),
                "/" if rf != 0.0 => Ok(Literal::Real(lf / rf)),
                "=" => Ok(Literal::Boolean((lf - rf).abs() < f64::EPSILON)),
                "<>" => Ok(Literal::Boolean((lf - rf).abs() >= f64::EPSILON)),
                "<" => Ok(Literal::Boolean(lf < rf)),
                "<=" => Ok(Literal::Boolean(lf <= rf)),
                ">" => Ok(Literal::Boolean(lf > rf)),
                ">=" => Ok(Literal::Boolean(lf >= rf)),
                _ => Err(anyhow!("Mixed type operator: {}", op)),
            }
        }

        (Literal::Real(l), Literal::Integer(r)) => {
            let lf = *l;
            let rf = *r as f64;
            match op {
                "+" => Ok(Literal::Real(lf + rf)),
                "-" => Ok(Literal::Real(lf - rf)),
                "*" => Ok(Literal::Real(lf * rf)),
                "/" if rf != 0.0 => Ok(Literal::Real(lf / rf)),
                "=" => Ok(Literal::Boolean((lf - rf).abs() < f64::EPSILON)),
                "<>" => Ok(Literal::Boolean((lf - rf).abs() >= f64::EPSILON)),
                "<" => Ok(Literal::Boolean(lf < rf)),
                "<=" => Ok(Literal::Boolean(lf <= rf)),
                ">" => Ok(Literal::Boolean(lf > rf)),
                ">=" => Ok(Literal::Boolean(lf >= rf)),
                _ => Err(anyhow!("Mixed type operator: {}", op)),
            }
        }

        _ => Err(anyhow!("Cannot evaluate {} for {:?} and {:?}", op, left, right)),
    }
}

fn eval_unary_op(op: &str, operand: &Literal) -> Result<Literal> {
    match op {
        "-" => match operand {
            Literal::Integer(n) => Ok(Literal::Integer(-n)),
            Literal::Real(r) => Ok(Literal::Real(-r)),
            _ => Err(anyhow!("Negation requires numeric type")),
        },
        "not" => match operand {
            Literal::Boolean(b) => Ok(Literal::Boolean(!b)),
            Literal::Integer(n) => Ok(Literal::Integer(if *n == 0 { 1 } else { 0 })),
            _ => Err(anyhow!("Not requires boolean or integer")),
        },
        "+" => Ok(operand.clone()), // Unary plus is no-op
        _ => Err(anyhow!("Unknown unary operator: {}", op)),
    }
}

/// Check if an expression is a compile-time constant
pub fn is_constant(expr: &Expr) -> bool {
    eval_constant(expr).map(|o| o.is_some()).unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_literal() {
        let expr = Expr::Literal(Literal::Integer(42));
        assert_eq!(eval_constant(&expr).unwrap(), Some(Literal::Integer(42)));
    }

    #[test]
    fn test_eval_binary_op() {
        let expr = Expr::BinaryOp {
            operator: "+".to_string(),
            left: Box::new(Expr::Literal(Literal::Integer(1))),
            right: Box::new(Expr::Literal(Literal::Integer(2))),
        };
        assert_eq!(eval_constant(&expr).unwrap(), Some(Literal::Integer(3)));
    }

    #[test]
    fn test_is_constant() {
        assert!(is_constant(&Expr::Literal(Literal::Boolean(true))));
        assert!(!is_constant(&Expr::Variable("x".to_string())));
    }
}
