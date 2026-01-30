# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Pascal is a production-ready optimizing Pascal compiler written in Rust, migrated from the Free Pascal Compiler (FPC). It features a modular architecture with trait-based design, comprehensive test coverage (87 tests), and advanced optimization capabilities.

**Status**: Milestone 3 Complete - Production Ready with full code generation, register allocation, SIMD support, and advanced type system.

## Build & Test Commands

### Building the Compiler

```bash
# Build all crates in the workspace
cargo build

# Build optimized release version
cargo build --release

# Build specific crate
cargo build -p pascal-lexer
cargo build -p pascal-cli
```

### Running Tests

```bash
# Run all tests (workspace)
cargo test

# Run library tests only (no integration tests)
cargo test --lib

# Run specific test module
cargo test lexer
cargo test parser
cargo test codegen
cargo test ast

# Run integration tests
cargo test --test integration_test

# Run tests for specific crate
cargo test -p pascal-lexer
cargo test -p pascal-codegen

# Run tests with output
cargo test -- --nocapture

# Run tests in parallel (faster)
cargo test -- --test-threads=4
```

### Building the Runtime Library

```bash
cd runtime
make          # Build both static and shared libraries
make clean    # Remove build artifacts
make test     # Build and run runtime tests
make install  # Install to ../lib/
```

The runtime library produces:
- `libpascal_runtime.a` - Static library
- `libpascal_runtime.dylib` (macOS) or `.so` (Linux) - Shared library

### Using the Compiler

```bash
# Build the CLI first
cargo build --release -p pascal-cli

# Compile a Pascal file
./target/release/pascal compile examples/hello.pas

# Compile with optimization and verbose output
./target/release/pascal compile examples/fibonacci.pas -O2 -v

# Compile with assembly output
./target/release/pascal compile examples/calculator.pas -S

# Show information about a PPU file
./target/release/pascal info mathutils.ppu

# Clean build artifacts
./target/release/pascal clean
```

## Architecture

### Compiler Pipeline

The compiler follows a traditional multi-pass architecture:

1. **Lexical Analysis** (`pascal-lexer`) - Tokenizes Pascal source code
2. **Syntax Analysis** (`pascal-parser`) - Builds AST from tokens
3. **Semantic Analysis** - Type checking and validation (integrated in parser/codegen)
4. **Code Generation** (`pascal-codegen`) - Generates x86-64 assembly
5. **Optimization** - Multiple optimization passes (constant folding, CSE, inlining, etc.)

### Crate Organization

The project uses a Cargo workspace with the following crates:

**Core Compiler Crates:**
- `pascal-lexer` - Lexical analysis with token definitions (basic + enhanced FPC tokens)
- `pascal-parser` - Syntax analysis with parser traits
- `pascal-ast` - Abstract Syntax Tree definitions (basic + enhanced)
- `pascal-codegen` - Code generation with optimization passes
- `pascal-module` - Module system with PPU file handling
- `pascal-driver` - Compilation driver orchestrating the pipeline

**User-Facing Crates:**
- `pascal-cli` - Command-line interface (binary crate)

**Supporting Crates:**
- `pascal-lcl` - Lazarus Component Library (macOS Cocoa GUI)
- `pascal-lsp` - Language Server Protocol for IDE integration
- `pascal-debug` - Debugging support
- `pascal-pkg` - Package management
- `pascal-plugin` - Plugin system
- `pascal-profile` - Profiling tools
- `pascal-rad` - Rapid Application Development GUI builder

### Trait-Based Architecture

The compiler uses trait-based design for testability and modularity. Key traits:

**Lexer Traits** (`pascal-lexer/src/traits.rs`):
- `LexerCapability` - Core lexing operations (next_token, peek_token)
- `TokenValidator` - Token validation
- `ErrorReporter` - Error reporting
- `TokenStream` - Token iteration

**Parser Traits** (`pascal-parser/src/traits.rs`):
- `ParserCapability` - Parsing operations (parse_program, parse_statement)
- Error handling and recovery

**Codegen Traits** (`pascal-codegen/src/traits.rs`):
- `CodeGeneratorCapability` - Code generation operations

This design allows easy mocking for tests and swapping implementations.

## Key Architecture Patterns

### Enhanced vs Basic Components

The codebase has "basic" and "enhanced" versions of major components, migrated from FPC:

- **Basic**: Simple, educational implementations
- **Enhanced**: Full-featured implementations with FPC compatibility

Enhanced components are in files named `enhanced_*.rs`:
- `enhanced_tokens.rs` - 100+ Pascal tokens
- `enhanced_lexer.rs` - Complete Pascal tokenization
- `enhanced_ast.rs` - Full Pascal language support
- `enhanced_codegen.rs` - Multi-architecture code generation

Some enhanced components may have commented-out test modules due to ongoing migration work (see `pascal-lexer/src/lib.rs` TODO comments).

### Module System & PPU Files

The compiler uses a sophisticated module system:
- **PPU Files**: Precompiled Unit files (binary format using bincode)
- **Automatic Dependency Resolution**: Compiles dependencies first
- **Incremental Compilation**: Reuses PPU files when source hasn't changed
- **Topological Sort**: Determines compilation order

Key types:
- `Module` - Represents a Pascal unit with interface/implementation
- `ModuleManager` - Manages module dependencies
- `ModuleLoader` - Loads/saves PPU files with caching
- `PpuFile` - PPU file format with checksums

### Code Generation Architecture

The code generator uses advanced optimization techniques:

**Optimization Passes** (`pascal-codegen/src/optimizer.rs`, `advanced_optimizer.rs`):
- Constant folding - Compile-time expression evaluation
- Dead code elimination - Remove unreachable code
- Common subexpression elimination (CSE) - Eliminate redundant calculations
- Function inlining - Inline small functions
- Loop unrolling - Unroll constant-iteration loops
- Strength reduction - Replace expensive ops (x*8 → x<<3)
- Tail call optimization - Convert recursion to iteration
- Peephole optimization - Assembly-level optimizations

**Register Allocation** (`pascal-codegen/src/register_allocator.rs`):
- Graph coloring algorithm
- Live range analysis
- Interference graph construction
- Spilling for register pressure

**Type System** (`pascal-codegen/src/type_checker.rs`, `advanced_types.rs`):
- Generic types with constraints
- Type inference (Hindley-Milner style)
- Operator overloading
- Type classes

**SIMD Support**:
- SSE/AVX/AVX-512 registers
- Vector operations
- Loop vectorization

## Testing Strategy

The project has comprehensive test coverage:

### Test Structure

- **Unit Tests**: Located in each crate's `src/` directory and `tests/` directory
- **Integration Tests**: `tests/integration_tests.rs`, `tests/integration_comprehensive_tests.rs`
- **Comprehensive Tests**: Individual test files for each feature area

### Test Modules

- `lexer_tests.rs` - Tokenization tests
- `parser_tests.rs` - Parsing tests
- `ast_tests.rs` - AST structure tests
- `codegen_tests.rs` - Code generation tests
- `integration_tests.rs` - End-to-end compilation tests
- `string_tests.rs` - String literal handling
- `type_tests.rs` - Type system tests
- `error_tests.rs` - Error handling tests
- `test_runner.rs` - Test infrastructure

### Running Specific Tests

```bash
# Run only lexer tests
cargo test lexer_tests

# Run only integration tests
cargo test integration_tests

# Run a specific test
cargo test test_basic_arithmetic

# Run tests matching a pattern
cargo test fibonacci
```

### Test Utilities

The `tests/test_runner.rs` provides:
- `TestRunner` - Orchestrates test execution
- `TestSuite` - Groups related tests
- `TestResult` - Captures test outcomes
- Performance testing utilities
- Benchmarking tools

## Standard Library

The standard library is located in `stdlib/`:

### Implemented Units

- **System.pas** - Core I/O, strings, math, memory, file operations (66 functions)
- **SysUtils.pas** - Utilities, exceptions, file/directory operations (53 functions)
- **Classes.pas** - OOP support with TObject, TList, TStringList, streams (7 classes)
- **Math.pas** - Comprehensive math functions (60+ functions)
- **Strings.pas** - String manipulation utilities

### Core Directory (`stdlib/core/`)

Additional core library files for runtime support.

### Runtime Library (`runtime/`)

C implementation of core runtime functions:
- `pascal_runtime.c` - Runtime functions
- `pascal_runtime.h` - Runtime header
- `test_runtime.c` - Runtime tests

Build with `make` in the `runtime/` directory.

## Common Development Tasks

### Adding a New Language Feature

1. **Lexer**: Add tokens to `pascal-lexer/src/tokens.rs` or `enhanced_tokens.rs`
2. **AST**: Add AST nodes to `pascal-ast/src/lib.rs` or `enhanced_ast.rs`
3. **Parser**: Add parsing logic to `pascal-parser/src/parser.rs`
4. **Codegen**: Add code generation to `pascal-codegen/src/codegen.rs`
5. **Tests**: Add tests to appropriate test file in `tests/`

### Adding Tests

1. Unit tests go in each crate's `src/` directory or in `tests/`
2. Integration tests go in `tests/integration_tests.rs`
3. Use `TestRunner` infrastructure for comprehensive test suites
4. Run `cargo test` to verify all tests pass

### Working with PPU Files

PPU files are binary serialized modules using `bincode`:
- `ModuleLoader::load_ppu()` - Load PPU file
- `ModuleLoader::save_ppu()` - Save PPU file
- PPU files include checksums for validation
- Automatic caching based on timestamps

### Code Generation Work

The code generator uses a visitor pattern over the AST:
- Expressions generate values in registers
- Statements generate control flow
- Symbol tables manage variable scopes
- Stack frames handle function calls

## Examples Directory

The `examples/` directory contains sample Pascal programs:
- `hello.pas` - Basic conditional and loop example
- `simple_math.pas` - Arithmetic operations
- `conditional.pas` - Complex if-else statements
- `boolean_logic.pas` - Boolean operations
- `fibonacci.pas` - Fibonacci sequence calculation
- `calculator.pas` - Calculator with multiple operations
- `loops.pas` - Complex loop structures
- `advanced_features.pas` - Advanced Pascal features
- `comprehensive_features.pas` - Comprehensive feature demonstration

Compile examples to test the compiler:
```bash
./target/release/pascal compile examples/hello.pas -v
./target/release/pascal compile examples/fibonacci.pas -S -O2
```

## Important Notes

### Edition & Dependencies

- Uses **Rust 2024 edition** (requires stable Rust 1.82+)
- Workspace resolver = "3" (latest edition)
- Key dependencies: logos (lexing), clap (CLI), thiserror (errors), anyhow (error propagation)

### Code Quality

- All 87 tests passing
- Trait-based architecture for testability
- FPC-migrated components for production quality
- Comprehensive error handling throughout

### Current Status

**Milestone 3 Complete**:
- ✅ Full code generation pipeline
- ✅ Register allocation with graph coloring
- ✅ Advanced optimizations (10+ passes)
- ✅ Type system enhancements (generics, inference)
- ✅ SIMD support and vectorization
- ✅ Module system with PPU files
- ✅ Standard library (60% complete - System, SysUtils, Classes, Math)
- ✅ CLI with colored output

**Next Phase (Milestone 4)**: Documentation, CI/CD, release management

### Known Limitations

- Some enhanced components have commented-out test modules (marked with TODO)
- String handling needs enhancements
- Cross-platform support needs more testing
- Some advanced language features still in progress

### Debugging

Use verbose mode to see compilation steps:
```bash
./target/release/pascal compile examples/hello.pas -v
```

This shows:
- Configuration details
- Dependency resolution
- PPU file loading/saving
- Code generation progress
- Optimization passes

## File Organization Patterns

- **Traits**: Each crate has `traits.rs` defining capability interfaces
- **Mocks**: Each crate has `mocks.rs` for test implementations
- **Enhanced Components**: Named `enhanced_*.rs` for FPC-migrated code
- **Tests**: Comprehensive test files in `tests/` directory
- **Documentation**: Inline rustdoc comments, plus docs/ directory

## Workspace Dependencies

Managed in `Cargo.toml` [workspace.dependencies]:
- `clap = "4.5"` - CLI argument parsing
- `logos = "0.15"` - Lexical analysis
- `thiserror = "2.0"` - Error handling
- `anyhow = "1.0"` - Error propagation
- Testing: `assert_cmd`, `predicates`, `tempfile`, `assert_matches`

All crates use workspace dependencies for consistency.
