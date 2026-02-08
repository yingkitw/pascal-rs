# Architecture - pascal-rs Pascal Compiler

## Overview

pascal-rs is a Pascal compiler and interpreter in a single Rust crate. It provides lexing, parsing, optimization, x86-64 code generation, and a tree-walking interpreter.

## Project Structure

```
pascal-rs/
├── src/
│   ├── main.rs              # CLI entry point (compile, run, info, clean)
│   ├── lib.rs               # Library root, re-exports
│   ├── ast.rs               # Unified AST definitions
│   ├── enhanced_ast.rs      # Extended AST types
│   ├── tokens.rs            # Token definitions (logos)
│   ├── enhanced_tokens.rs   # Extended token definitions
│   ├── lexer.rs             # Lexer (logos-based)
│   ├── parser/
│   │   ├── mod.rs           # Parser core (recursive descent)
│   │   ├── expression.rs    # Expression parsing with precedence
│   │   ├── statement.rs     # Statement parsing (if, while, for, repeat, etc.)
│   │   └── decl.rs          # Declaration parsing (program, const, type, var, block)
│   ├── interpreter.rs       # Tree-walking interpreter
│   ├── optimizer.rs         # Constant folding, dead code elimination, peephole
│   ├── advanced_optimizer.rs # CSE, inlining, loop unrolling, strength reduction, tail call
│   ├── unit_codegen.rs      # x86-64 assembly code generation
│   ├── enhanced_codegen.rs  # Extended code generation
│   ├── type_checker.rs      # Type checking and inference
│   ├── symbol_table.rs      # Symbol table with scope management
│   ├── resolver.rs          # Module symbol resolution
│   ├── loader.rs            # Module loading and PPU caching
│   ├── ppu.rs               # Precompiled unit file format
│   ├── parallel.rs          # Parallel compilation (rayon)
│   ├── register_allocator.rs # Graph-coloring register allocation
│   ├── simd.rs              # SIMD/vectorization support
│   ├── error.rs             # Error types
│   ├── traits/              # Trait definitions for testability
│   └── utils/               # Helpers (ast_helpers, block, string_utils)
├── examples/                # Pascal source files (.pas)
├── tests/                   # Integration tests
├── docs/                    # Additional documentation
├── Cargo.toml               # Project configuration
├── README.md
├── TODO.md
├── SPEC.md
└── ARCHITECTURE.md
```

## Compilation Pipeline

```
Source (.pas) → Lexer (tokens.rs) → Parser (parser/) → AST (ast.rs)
                                                          │
                                    ┌─────────────────────┼──────────────────┐
                                    ▼                     ▼                  ▼
                              Interpreter          Optimizer           Type Checker
                           (interpreter.rs)    (optimizer.rs)      (type_checker.rs)
                                    │                     │                  │
                                    ▼                     ▼                  ▼
                              Direct Output        Code Generator     Symbol Table
                                              (unit_codegen.rs)   (symbol_table.rs)
                                                      │
                                                      ▼
                                                Assembly (.asm)
```

## Key Types

### AST (`ast.rs`)
- `Program { name, uses, block }` - top-level program
- `Unit { name, uses, interface, implementation }` - Pascal unit
- `Block { consts, types, vars, procedures, functions, statements }` - declaration block
- `Statement` - enum: Assignment, If, While, For, Repeat, Case, ProcedureCall, Block, etc.
- `Expression` - enum: Literal, Variable, BinaryOp, UnaryOp, FunctionCall, etc.
- `Type` - enum: Simple, Integer, Real, Boolean, Array, Record, Pointer, etc.

### Interpreter (`interpreter.rs`)
- `Interpreter { scopes, functions, verbose }` - interpreter state
- `Value` - enum: Integer, Real, Boolean, Char, String, Nil
- `Scope { variables }` - variable scope with HashMap
- Built-in procedures/functions for standard Pascal operations

### Code Generator (`unit_codegen.rs`)
- `UnitCodeGenerator` - generates x86-64 Intel-syntax assembly
- Handles functions, procedures, statements, expressions, literals

## Design Principles

- **DRY**: Shared AST types, utility helpers, `Block::empty()` / `Block::with_statements()`
- **KISS**: Single crate, simple module boundaries
- **Testability**: 82+ unit tests, trait-based interfaces
- **Performance**: logos for lexing, rayon for parallel compilation

## Dependencies

- `logos` - lexical analysis
- `clap` - CLI argument parsing
- `anyhow` / `thiserror` - error handling
- `serde` / `bincode` - PPU serialization
- `rayon` / `num_cpus` - parallel compilation
- `colored` - terminal output

## Related Documentation

- [SPEC.md](./SPEC.md) - Language and CLI specification
- [TODO.md](./TODO.md) - Development roadmap
- [README.md](./README.md) - Project introduction
