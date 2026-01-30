//! AST helper utilities
//!
//! Common operations on AST nodes to reduce code duplication.

use crate::ast::{Expr, Literal, Stmt, Type};

/// Create a binary operation expression
pub fn binop(left: Expr, op: &str, right: Expr) -> Expr {
    Expr::BinaryOp {
        left: Box::new(left),
        operator: op.to_string(),
        right: Box::new(right),
    }
}

/// Create a unary operation expression
pub fn unop(op: &str, operand: Expr) -> Expr {
    Expr::UnaryOp {
        operator: op.to_string(),
        operand: Box::new(operand),
    }
}

/// Create a variable reference expression
pub fn var(name: &str) -> Expr {
    Expr::Variable(name.to_string())
}

/// Create a literal expression
pub fn lit(value: Literal) -> Expr {
    Expr::Literal(value)
}

/// Create an integer literal
pub fn int_lit(n: i64) -> Expr {
    lit(Literal::Integer(n))
}

/// Create a boolean literal
pub fn bool_lit(b: bool) -> Expr {
    lit(Literal::Boolean(b))
}

/// Create a string literal
pub fn string_lit(s: &str) -> Expr {
    lit(Literal::String(s.to_string()))
}

/// Create a function call expression
pub fn call(name: &str, args: Vec<Expr>) -> Expr {
    Expr::FunctionCall {
        name: name.to_string(),
        arguments: args,
    }
}

/// Create an assignment statement
pub fn assign(target: &str, value: Expr) -> Stmt {
    Stmt::Assignment {
        target: target.to_string(),
        value,
    }
}

/// Create an if statement
pub fn if_stmt(condition: Expr, then_branch: Vec<Stmt>, else_branch: Option<Vec<Stmt>>) -> Stmt {
    Stmt::If {
        condition,
        then_branch,
        else_branch,
    }
}

/// Create a while loop statement
pub fn while_stmt(condition: Expr, body: Vec<Stmt>) -> Stmt {
    Stmt::While {
        condition,
        body,
    }
}

/// Create a procedure call statement
pub fn proc_call(name: &str, args: Vec<Expr>) -> Stmt {
    Stmt::ProcedureCall {
        name: name.to_string(),
        arguments: args,
    }
}

/// Check if an expression is a literal
pub fn is_literal(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(_))
}

/// Check if an expression is a variable reference
pub fn is_variable(expr: &Expr) -> bool {
    matches!(expr, Expr::Variable(_))
}

/// Get the operator precedence for a binary operator
pub fn operator_precedence(op: &str) -> u8 {
    match op {
        "or" => 1,
        "xor" => 2,
        "and" => 3,
        "=" | "<>" | "<" | ">" | "<=" | ">=" | "in" => 4,
        "+" | "-" => 5,
        "*" | "/" | "div" | "mod" | "shl" | "shr" => 6,
        _ => 0,
    }
}

/// Check if an operator is left-associative
pub fn is_left_associative(op: &str) -> bool {
    !matches!(op, "^")
}

/// Create a simple type
pub fn simple_type(type_name: &str) -> Type {
    Type::Simple(match type_name {
        "integer" => crate::ast::SimpleType::Integer,
        "real" => crate::ast::SimpleType::Real,
        "boolean" => crate::ast::SimpleType::Boolean,
        "char" => crate::ast::SimpleType::Char,
        "string" => crate::ast::SimpleType::String,
        _ => crate::ast::SimpleType::Integer,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binop_creation() {
        let left = int_lit(1);
        let right = int_lit(2);
        let expr = binop(left, "+", right);
        assert!(matches!(expr, Expr::BinaryOp { .. }));
    }

    #[test]
    fn test_operator_precedence() {
        assert_eq!(operator_precedence("+"), 5);
        assert_eq!(operator_precedence("*"), 6);
        assert_eq!(operator_precedence("or"), 1);
        assert_eq!(operator_precedence("="), 4);
    }

    #[test]
    fn test_literal_helpers() {
        assert!(is_literal(&int_lit(42)));
        assert!(is_variable(&var("x")));
    }
}
