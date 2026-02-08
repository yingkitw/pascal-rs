//! Type checking for code generation
//!
//! Validates types, performs type inference, and handles type conversions

use crate::ast::{Expr, Literal, Type};
use crate::symbol_table::SymbolTable as LocalSymbolTable;
use anyhow::{anyhow, Result};

/// Type checker
pub struct TypeChecker<'a> {
    symbol_table: &'a LocalSymbolTable,
}

impl<'a> TypeChecker<'a> {
    /// Create a new type checker
    pub fn new(symbol_table: &'a LocalSymbolTable) -> Self {
        Self { symbol_table }
    }

    /// Check expression type
    pub fn check_expr(&self, expr: &Expr) -> Result<Type> {
        match expr {
            Expr::Literal(lit) => Ok(self.literal_type(lit)),

            Expr::Variable(name) => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    Ok(symbol.typ.clone())
                } else {
                    Err(anyhow!("Undefined variable: {}", name))
                }
            }

            Expr::BinaryOp {
                operator,
                left,
                right,
            } => {
                let left_type = self.check_expr(left)?;
                let right_type = self.check_expr(right)?;
                self.check_binary_op(operator, &left_type, &right_type)
            }

            Expr::UnaryOp { operator, operand } => {
                let expr_type = self.check_expr(operand)?;
                self.check_unary_op(operator, &expr_type)
            }

            Expr::FunctionCall { name, arguments } => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    if let Some(sig) = &symbol.function_signature {
                        if arguments.len() != sig.parameters.len() {
                            return Err(anyhow!(
                                "Function '{}' expects {} arguments, got {}",
                                name,
                                sig.parameters.len(),
                                arguments.len()
                            ));
                        }

                        for (i, arg_expr) in arguments.iter().enumerate() {
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
                        Err(anyhow!("'{}' is not a function", name))
                    }
                } else {
                    Err(anyhow!("Undefined function: {}", name))
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
            Literal::String(_) => Type::String,
            Literal::WideString(_) => Type::WideString,
            Literal::Nil => Type::Pointer(Box::new(Type::Integer)),
            Literal::Set(_) => Type::Integer, // Simplified
        }
    }

    /// Check binary operation types
    fn check_binary_op(&self, op: &str, left: &Type, right: &Type) -> Result<Type> {
        match op {
            // Arithmetic operations
            "+" | "-" | "*" | "/" => {
                if matches!(left, Type::Integer) && matches!(right, Type::Integer) {
                    Ok(Type::Integer)
                } else if matches!(left, Type::Real) || matches!(right, Type::Real) {
                    Ok(Type::Real)
                } else {
                    Err(anyhow!("Type mismatch in arithmetic operation"))
                }
            }

            // Comparison operations
            "=" | "<>" | "<" | "<=" | ">" | ">=" => {
                if self.types_compatible(left, right) {
                    Ok(Type::Boolean)
                } else {
                    Err(anyhow!("Type mismatch in comparison"))
                }
            }

            // Logical operations
            "and" | "or" | "xor" => {
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
    fn check_unary_op(&self, op: &str, operand: &Type) -> Result<Type> {
        match op {
            "-" => {
                if matches!(operand, Type::Integer | Type::Real) {
                    Ok(operand.clone())
                } else {
                    Err(anyhow!("Negation requires numeric type"))
                }
            }

            "not" => {
                if matches!(operand, Type::Boolean) {
                    Ok(Type::Boolean)
                } else if matches!(operand, Type::Integer | Type::Real) {
                    Ok(operand.clone())
                } else {
                    Err(anyhow!("Not operation requires boolean or numeric type"))
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
            (Type::String, Type::String) => true,
            (Type::WideString, Type::WideString) => true,
            // Allow integer to real conversion
            (Type::Integer, Type::Real) | (Type::Real, Type::Integer) => true,
            // Allow string conversions
            (Type::String, Type::WideString) | (Type::WideString, Type::String) => true,
            // Pointer compatibility
            (Type::Pointer(_), Type::Pointer(_)) => true,
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
            (Type::String, Type::String) => false,
            (Type::WideString, Type::WideString) => false,
            _ => self.types_compatible(from, to),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_types() {
        let table = LocalSymbolTable::new();
        let checker = TypeChecker::new(&table);

        assert_eq!(
            checker.literal_type(&Literal::Integer(42)),
            Type::Integer
        );
        assert_eq!(
            checker.literal_type(&Literal::Boolean(true)),
            Type::Boolean
        );
        assert_eq!(
            checker.literal_type(&Literal::Real(3.14)),
            Type::Real
        );
    }

    #[test]
    fn test_binary_op_types() {
        let table = LocalSymbolTable::new();
        let checker = TypeChecker::new(&table);

        // Integer + Integer = Integer
        let result = checker.check_binary_op(
            "+",
            &Type::Integer,
            &Type::Integer,
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Integer);

        // Integer < Integer = Boolean
        let result = checker.check_binary_op(
            "<",
            &Type::Integer,
            &Type::Integer,
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Boolean);
    }
}
