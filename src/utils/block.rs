//! Block construction utilities
//!
//! This module provides helper functions to reduce code duplication
//! when constructing AST blocks.

use crate::ast::Block;

/// Create an empty block
pub fn empty_block() -> Block {
    Block::empty()
}

/// Create a block with a single statement
pub fn block_with_statement(stmt: crate::ast::Stmt) -> Block {
    Block::with_statements(vec![stmt])
}

/// Create a block with multiple statements
pub fn block_with_statements(stmts: Vec<crate::ast::Stmt>) -> Block {
    Block::with_statements(stmts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Stmt};

    #[test]
    fn test_empty_block() {
        let block = empty_block();
        assert!(block.statements.is_empty());
        assert!(block.consts.is_empty());
        assert!(block.types.is_empty());
        assert!(block.vars.is_empty());
    }

    #[test]
    fn test_block_with_statement() {
        let stmt = Stmt::Assignment {
            target: "x".to_string(),
            value: Expr::Literal(crate::ast::Literal::Integer(42)),
        };
        let block = block_with_statement(stmt.clone());
        assert_eq!(block.statements.len(), 1);
        assert!(matches!(block.statements[0], Stmt::Assignment { .. }));
    }
}
