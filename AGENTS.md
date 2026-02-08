# AGENTS.md - Guide for Coding Agents

This document provides essential information for agentic coding agents working on the pascal-rs Pascal compiler project.

## Project Overview

pascal-rs is a production-ready optimizing Pascal compiler written in Rust. It features a modular architecture with comprehensive Pascal language support, advanced optimizations, and a modern CLI interface.

## Build Commands

```bash
# Build the project (debug)
cargo build

# Build optimized release version
cargo build --release

# Run all tests
cargo test

# Run specific test categories
cargo test --lib                    # Unit tests in src/
cargo test --test integration_test  # Integration tests in tests/
cargo test lexer                    # Lexer-specific tests
cargo test parser                   # Parser-specific tests
cargo test codegen                  # Code generation tests
cargo test threading                # Threading/concurrency tests

# Run a single test
cargo test test_name                # Replace test_name with actual test function

# Check code without building
cargo check

# Format code
cargo fmt

# Run linter
cargo clippy

# Generate documentation
cargo doc --open

# Run with specific features
cargo build --features lsp          # Build with LSP support
cargo build --features mcp          # Build with MCP server support
cargo build --features gui          # Build with macOS GUI support
```

## Project Structure

```
pascal-rs/
├── src/                    # Main source code
│   ├── lexer.rs           # Lexical analysis
│   ├── parser/            # Syntax parsing (modular)
│   │   ├── mod.rs
│   │   ├── expression.rs
│   │   ├── statement.rs
│   │   └── decl.rs
│   ├── ast.rs             # Abstract Syntax Tree definitions
│   ├── enhanced_ast.rs    # Extended AST with advanced features
│   ├── enhanced_codegen.rs # Code generation
│   ├── tokens.rs          # Token definitions
│   ├── enhanced_tokens.rs # Extended token definitions
│   ├── error.rs           # Error types
│   ├── loader.rs          # Module loading system
│   ├── resolver.rs        # Symbol resolution
│   ├── ppu.rs             # PPU (precompiled unit) handling
│   ├── parallel.rs        # Parallel compilation support
│   ├── simd.rs            # SIMD/vectorization support
│   ├── register_allocator.rs # Register allocation
│   ├── traits/            # Trait definitions for testability
│   │   ├── mod.rs
│   │   ├── lexer.rs
│   │   ├── parser.rs
│   │   ├── codegen.rs
│   │   └── optimizer.rs
│   └── utils/             # Utility modules
├── tests/                 # Test suites
│   ├── unit/             # Unit tests
│   └── integration/      # Integration tests
├── examples/             # Example Pascal programs
└── Cargo.toml           # Project configuration
```

## Code Style Guidelines

### General Conventions
- Follow Rust 2024 edition standards
- Use `rustfmt` for formatting (default settings)
- Use `clippy` for linting (default settings)
- Prefer `thiserror` for error types
- Use `anyhow::Result` for application-level error handling
- Comprehensive documentation with `///` for public APIs
- Module-level documentation with `//!`

### Naming Conventions
- **Types/Structs/Enums**: `PascalCase` (e.g., `Lexer`, `Parser`, `CodeGenerator`)
- **Functions/Methods**: `snake_case` (e.g., `parse_program`, `generate_code`)
- **Constants**: `SCREAMING_SNAKE_CASE` (e.g., `DEFAULT_OPTIMIZATION_LEVEL`)
- **Modules**: `snake_case` (e.g., `lexer`, `parser`, `codegen`)
- **Features**: `snake_case` (e.g., `lsp_support`, `mcp_server`)

### Imports Organization
```rust
// Standard library imports
use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf;

// External crate imports
use anyhow::Result;
use clap::{Parser, Subcommand};
use colored::Colorize;
use logos::Logos;
use serde::{Deserialize, Serialize};
use thiserror::Error;

// Internal imports
use crate::ast::{Expression, Statement, Type};
use crate::tokens::Token;
use crate::traits::LexerCapability;
```

### Error Handling
- Use `thiserror` for custom error types
- Implement proper error chaining with `source()`
- Include context information in error messages
- Use `anyhow::Result` for main application functions

Example:
```rust
#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Failed to parse file {path}: {source}")]
    ParseError { 
        path: PathBuf,
        #[source]
        source: ParseError,
    },
    
    #[error("Code generation failed: {0}")]
    CodegenError(String),
}
```

### Testing Guidelines
- Unit tests in `tests/unit/` directory
- Integration tests in `tests/integration/` directory
- Test modules follow same structure as source
- Use descriptive test names: `test_feature_under_test_condition`
- Use `tempfile` for temporary files in tests
- Use `assert_cmd` for CLI testing
- Use `insta` for snapshot testing when appropriate

### Traits and Interfaces
- Define traits for major components (`LexerCapability`, `ParserCapability`, etc.)
- Use trait objects for dependency injection
- Implement mock versions for testing
- Keep traits focused and cohesive

### Module System
- Use `mod.rs` for module organization
- Re-export commonly used types at crate level
- Use visibility modifiers (`pub(crate)`, `pub(super)`) appropriately
- Keep modules focused with single responsibility

### Async/Threading
- Use `Arc<RwLock<T>>` for shared mutable state
- Use `rayon` for parallel processing
- Implement thread-safe APIs where needed
- Include threading tests with concurrent access

### Performance Considerations
- Use `Vec<T>` for dynamic arrays
- Use `Box<T>` for large recursive data structures
- Prefer references over cloning where possible
- Use `Cow<str>` for string data that might be borrowed

## Common Patterns

### Parser Implementation
```rust
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer, current_token: None }
    }
    
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        self.expect_token(Token::ProgramKeyword)?;
        let name = self.parse_identifier()?;
        let block = self.parse_block()?;
        Ok(Program { name, block })
    }
}
```

### Error Conversion
```rust
impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> Self {
        ParseError::LexerError(err)
    }
}
```

### CLI Command Structure
```rust
#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Compile {
        input: PathBuf,
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}
```

## Feature Flags

The project uses optional features:
- `mcp`: MCP server support for AI integration
- `lsp`: Language Server Protocol support
- `gui`: macOS GUI support with Cocoa
- `debug`: Debug information generation
- `profile`: Profiling support

## Development Workflow

1. Create feature branch from main
2. Implement changes following style guidelines
3. Add comprehensive tests
4. Run `cargo fmt`, `cargo clippy`, and `cargo test`
5. Submit pull request

## Testing Single Components

To test a specific component:
```bash
# Test lexer only
cargo test lexer --lib

# Test parser with verbose output
cargo test parser --lib -- --nocapture

# Test specific function
cargo test test_function_name -- --exact

# Run tests with specific features
cargo test --features lsp
```

## Common Gotchas

- AST types are in both `ast.rs` and `enhanced_ast.rs` - use appropriate one
- Some modules are temporarily commented out in `lib.rs` due to compatibility
- Error conversion between modules needs explicit `From` implementations
- Token definitions split between basic and enhanced versions