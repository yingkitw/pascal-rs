//! Type checking for code generation
//!
//! Validates types, performs type inference, and handles type conversions

use crate::ast::{BinaryOp, Expr, Literal, Type, UnaryOp};
use crate::symbol_table::SymbolTable;
use anyhow::{anyhow, Result};

/// Type checker
pub struct TypeChecker<'a> {
    symbol_table: &'a SymbolTable,
}

impl<'a> TypeChecker<'a> {
    /// Create a new type checker
    pub fn new(symbol_table: &'a SymbolTable) -> Self {
        Self { symbol_table }
    }

    /// Check expression type
    pub fn check_expr(&self, expr: &Expr) -> Result<Type> {
        match expr {
            Expr::Literal(lit) => Ok(self.literal_type(lit)),

            Expr::Identifier(parts) => {
                if let Some(name) = parts.first() {
                    if let Some(symbol) = self.symbol_table.lookup(name) {
                        Ok(symbol.typ.clone())
                    } else {
                        Err(anyhow!("Undefined variable: {}", name))
                    }
                } else {
                    Err(anyhow!("Empty identifier"))
                }
            }

            Expr::BinaryOp { op, left, right } => {
                let left_type = self.check_expr(left)?;
                let right_type = self.check_expr(right)?;
                self.check_binary_op(op, &left_type, &right_type)
            }

            Expr::UnaryOp { op, expr } => {
                let expr_type = self.check_expr(expr)?;
                self.check_unary_op(op, &expr_type)
            }

            Expr::FunctionCall { name, args } => {
                // Look up function signature to determine return type
                if let Some(name) = name.first() {
                    if let Some(sig) = self.symbol_table.lookup_function(name) {
                        // Validate argument count
                        if args.len() != sig.parameters.len() {
                            return Err(anyhow!(
                                "Function '{}' expects {} arguments, got {}",
                                name,
                                sig.parameters.len(),
                                args.len()
                            ));
                        }

                        // Validate argument types
                        for (i, arg_expr) in args.iter().enumerate() {
                            let arg_type = self.check_expr(arg_expr)?;
                            let expected_type = &sig.parameters[i].1;

                            if !self.types_compatible(&arg_type, expected_type) {
                                return Err(anyhow!(
                                    "Type mismatch in argument {} of function '{}': expected {:?}, got {:?}",
                                    i + 1,
                                    name,
                                    expected_type,
                                    arg_type
                                ));
                            }
                        }

                        Ok(sig.return_type.clone())
                    } else {
                        Err(anyhow!("Undefined function: {}", name))
                    }
                } else {
                    Err(anyhow!("Empty function name"))
                }
            }

            _ => Ok(Type::Integer), // Default for unsupported expressions
        }
    }

    /// Get literal type
    fn literal_type(&self, lit: &Literal) -> Type {
        match lit {
            Literal::Integer(_) => Type::Integer,
            Literal::Real(_) => Type::Real,
            Literal::Boolean(_) => Type::Boolean,
            Literal::Char(_) => Type::Char,
            Literal::String(_) => Type::String(None),
            Literal::Nil => Type::Pointer(Box::new(Type::Integer)), // Use Integer as placeholder
            _ => Type::Integer,                                     // Default
        }
    }

    /// Check binary operation types
    fn check_binary_op(&self, op: &BinaryOp, left: &Type, right: &Type) -> Result<Type> {
        match op {
            // Arithmetic operations
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                if matches!(left, Type::Integer) && matches!(right, Type::Integer) {
                    Ok(Type::Integer)
                } else if matches!(left, Type::Real) || matches!(right, Type::Real) {
                    Ok(Type::Real)
                } else {
                    Err(anyhow!("Type mismatch in arithmetic operation"))
                }
            }

            // Comparison operations
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessOrEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterOrEqual => {
                if self.types_compatible(left, right) {
                    Ok(Type::Boolean)
                } else {
                    Err(anyhow!("Type mismatch in comparison"))
                }
            }

            // Logical operations
            BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => {
                if matches!(left, Type::Boolean) && matches!(right, Type::Boolean) {
                    Ok(Type::Boolean)
                } else {
                    Err(anyhow!("Logical operations require boolean operands"))
                }
            }

            _ => Ok(Type::Integer), // Default
        }
    }

    /// Check unary operation type
    fn check_unary_op(&self, op: &UnaryOp, operand: &Type) -> Result<Type> {
        match op {
            UnaryOp::Minus | UnaryOp::Negate => {
                if matches!(operand, Type::Integer | Type::Real) {
                    Ok(operand.clone())
                } else {
                    Err(anyhow!("Negation requires numeric type"))
                }
            }

            UnaryOp::Not => {
                if matches!(operand, Type::Boolean) {
                    Ok(Type::Boolean)
                } else {
                    Err(anyhow!("Not operation requires boolean"))
                }
            }

            _ => Ok(operand.clone()),
        }
    }

    /// Check if types are compatible
    fn types_compatible(&self, t1: &Type, t2: &Type) -> bool {
        match (t1, t2) {
            (Type::Integer, Type::Integer) => true,
            (Type::Real, Type::Real) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Char, Type::Char) => true,
            (Type::String(_), Type::String(_)) => true,
            // Allow integer to real conversion
            (Type::Integer, Type::Real) | (Type::Real, Type::Integer) => true,
            _ => false,
        }
    }

    /// Check if type needs conversion
    pub fn needs_conversion(&self, from: &Type, to: &Type) -> bool {
        match (from, to) {
            (Type::Integer, Type::Integer) => false,
            (Type::Real, Type::Real) => false,
            (Type::Boolean, Type::Boolean) => false,
            (Type::Char, Type::Char) => false,
            (Type::String(_), Type::String(_)) => false,
            _ => self.types_compatible(from, to),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_types() {
        let table = SymbolTable::new();
        let checker = TypeChecker::new(&table);

        assert_eq!(checker.literal_type(&Literal::Integer(42)), Type::Integer);
        assert_eq!(checker.literal_type(&Literal::Boolean(true)), Type::Boolean);
        assert_eq!(checker.literal_type(&Literal::Real(3.14)), Type::Real);
    }

    #[test]
    fn test_binary_op_types() {
        let table = SymbolTable::new();
        let checker = TypeChecker::new(&table);

        // Integer + Integer = Integer
        let result = checker.check_binary_op(&BinaryOp::Add, &Type::Integer, &Type::Integer);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Integer);

        // Integer < Integer = Boolean
        let result = checker.check_binary_op(&BinaryOp::Less, &Type::Integer, &Type::Integer);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Boolean);
    }
}
