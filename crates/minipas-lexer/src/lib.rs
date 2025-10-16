pub mod tokens;
// pub mod enhanced_tokens; // TODO: Fix compilation errors
pub mod lexer;
// pub mod enhanced_lexer; // TODO: Fix compilation errors
pub mod traits;
pub mod mocks;

// #[cfg(test)]
// pub mod trait_tests; // TODO: Fix trait implementation

// #[cfg(test)]
// mod enhanced_lexer_tests; // TODO: Fix compilation errors

#[cfg(test)]
mod comprehensive_tests;

pub use self::tokens::Token;
// pub use self::enhanced_tokens::EnhancedToken; // TODO: Fix compilation errors
pub use self::lexer::{Lexer, LexerError};
// pub use self::enhanced_lexer::{EnhancedLexer, EnhancedLexerCapability}; // TODO: Fix compilation errors
pub use self::traits::*;
pub use self::mocks::*;
