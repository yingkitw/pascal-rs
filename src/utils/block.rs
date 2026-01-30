//! Block construction utilities
//!
//! This module provides helper functions to reduce code duplication
//! when constructing AST blocks.

use crate::ast::Block;
use std::collections::HashMap;

/// Create an empty block
pub fn empty_block() -> Block {
    Block {
        const_decls: HashMap::new(),
        type_decls: HashMap::new(),
        var_decls: HashMap::new(),
        statements: vec![],
    }
}

/// Create a block with a single statement
pub fn block_with_statement(stmt: crate::ast::Stmt) -> Block {
    Block {
        const_decls: HashMap::new(),
        type_decls: HashMap::new(),
        var_decls: HashMap::new(),
        statements: vec![stmt],
    }
}

/// Create a block with multiple statements
pub fn block_with_statements(stmts: Vec<crate::ast::Stmt>) -> Block {
    Block {
        const_decls: HashMap::new(),
        type_decls: HashMap::new(),
        var_decls: HashMap::new(),
        statements: stmts,
    }
}

/// Create a block with declarations and statements
pub fn block_with_decls(
    const_decls: HashMap<String, crate::ast::ConstDecl>,
    type_decls: HashMap<String, crate::ast::TypeDecl>,
    var_decls: HashMap<String, crate::ast::VariableDecl>,
    statements: Vec<crate::ast::Stmt>,
) -> Block {
    Block {
        const_decls,
        type_decls,
        var_decls,
        statements,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Stmt};

    #[test]
    fn test_empty_block() {
        let block = empty_block();
        assert!(block.statements.is_empty());
        assert!(block.const_decls.is_empty());
        assert!(block.type_decls.is_empty());
        assert!(block.var_decls.is_empty());
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
