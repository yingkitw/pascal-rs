//! Utility modules for common patterns (DRY principle)

pub mod ast_helpers;
pub mod block;
pub mod cow_ident;
pub mod string_utils;

pub use ast_helpers::*;
pub use block::*;
pub use cow_ident::*;
pub use string_utils::*;
