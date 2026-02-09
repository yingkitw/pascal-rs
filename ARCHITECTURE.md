# Architecture - pascal-rs Pascal Compiler

## Overview

pascal-rs is a Pascal compiler and interpreter in a single Rust crate. It provides lexing, parsing, optimization, x86-64 code generation, and a tree-walking interpreter. The project is incrementally adding Object Pascal (Delphi/Lazarus) features.

## Project Structure

```
pascal-rs/
├── src/
│   ├── main.rs                # CLI entry point (compile, run, info, clean)
│   ├── lib.rs                 # Library root, re-exports
│   │
│   ├── ast.rs                 # Unified AST (Program, Unit, Block, Statement, Expression, Type, ClassDecl)
│   ├── tokens.rs              # Token definitions (logos) — 100+ tokens incl. OOP keywords
│   ├── lexer.rs               # Lexer (logos-based, single-quoted strings, all Pascal literals)
│   ├── error.rs               # ParseError, CompileError types
│   │
│   ├── parser/
│   │   ├── mod.rs             # Parser core: advance, peek, check, consume, synchronize
│   │   ├── expression.rs      # Precedence-climbing expression parser (binary, unary, primary)
│   │   ├── statement.rs       # Statement parser (if, while, for, repeat, case, try, raise, begin/end)
│   │   └── decl.rs            # Declaration parser (program, unit, const, type, var, function, procedure, class)
│   │
│   ├── interpreter.rs         # Tree-walking interpreter (Value, Scope, built-ins)
│   ├── interpreter_traits.rs  # Interpreter trait abstractions
│   ├── interpreter_value.rs   # Value type helpers
│   ├── interpreter_scope.rs   # Scope management
│   ├── interpreter_function.rs # Function/procedure execution
│   │
│   ├── optimizer.rs           # Constant folding, dead code elimination, peephole
│   ├── advanced_optimizer.rs  # CSE, inlining, loop unrolling, strength reduction, tail call
│   ├── advanced_types.rs      # Generics, type inference, operator overloading
│   │
│   ├── unit_codegen.rs        # x86-64 assembly code generation
│   ├── enhanced_codegen.rs    # Extended code generation helpers
│   ├── enhanced_ast.rs        # Extended AST types (legacy)
│   ├── enhanced_tokens.rs     # Extended token definitions (legacy)
│   │
│   ├── type_checker.rs        # Type checking and inference
│   ├── symbol_table.rs        # Symbol table with scope management + function signatures
│   ├── resolver.rs            # Module symbol resolution
│   ├── loader.rs              # Module loading and PPU caching (thread-safe)
│   ├── ppu.rs                 # Precompiled unit file format (bincode)
│   ├── parallel.rs            # Parallel compilation (rayon)
│   ├── register_allocator.rs  # Graph-coloring register allocation
│   ├── simd.rs                # SIMD/vectorization support
│   ├── mcp_server.rs          # MCP server support
│   │
│   ├── traits/                # Trait definitions for testability
│   │   ├── mod.rs
│   │   ├── lexer.rs
│   │   ├── parser.rs
│   │   ├── codegen.rs
│   │   └── optimizer.rs
│   └── utils/
│       ├── mod.rs
│       ├── ast_helpers.rs     # binop(), unop(), var(), call(), literal helpers
│       ├── block.rs           # Block::empty(), Block::with_statements()
│       └── string_utils.rs    # escape, unescape, case conversion
│
├── examples/                  # Pascal source files (.pas)
├── tests/                     # Integration + unit test suites
│   ├── unit/                  # Unit tests (oop, interpreter, parser, codegen, etc.)
│   └── integration/           # Integration tests
├── docs/                      # Additional documentation
├── Cargo.toml
├── SPEC.md                    # Language and CLI specification
├── TODO.md                    # Development roadmap
├── ARCHITECTURE.md            # This file
└── README.md
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
                                                Assembly (.s)
```

## Key Types

### AST (`ast.rs`)
- `Program { name, uses, block }` — top-level program
- `Unit { name, uses, interface, implementation }` — Pascal unit
- `Block { consts, types, vars, procedures, functions, statements }` — declaration block
- `Statement` — enum: Assignment, If, While, For, Repeat, Case, ProcedureCall, Block, Try, Raise, etc.
- `Expression` — enum: Literal, Variable, BinaryOp, UnaryOp, FunctionCall, Inherited, Is, As, etc.
- `Type` — enum: Simple, Array, Record, Pointer, Alias, Generic, GenericInstance, etc.
- `ClassDecl { name, parent, interfaces, fields, methods, properties }` — Object Pascal class
- `MethodDecl` — method with virtual/override/abstract/constructor/destructor flags
- `PropertyDecl` — property with read/write specifiers
- `ExceptClause { exception_type, variable, body }` — exception handler

### Parser (`parser/`)
- `Parser<'a>` — recursive descent parser with single-token lookahead
- `parse_program()` → `Program`
- `parse_unit()` → `Unit`
- `parse_block()` → `Block` (const, type, var, function, procedure sections)
- `parse_statement()` → dispatches to if/while/for/repeat/case/try/raise/begin/identifier
- `parse_expression()` → precedence-climbing with binary/unary/primary
- `parse_class_decl()` → class with visibility sections, methods, properties

### Interpreter (`interpreter.rs`)
- `Interpreter { scopes, functions, verbose }` — interpreter state
- `Value` — enum: Integer, Real, Boolean, Char, String, Nil
- `Scope { variables }` — variable scope with HashMap
- Built-in procedures: write, writeln, readln, inc, dec, halt
- Built-in functions: abs, sqr, sqrt, sin, cos, ln, exp, round, trunc, ord, chr, length, etc.

### Code Generator (`unit_codegen.rs`)
- `UnitCodeGenerator` — generates x86-64 Intel-syntax assembly
- Expression, statement, function/procedure generation
- Float support (XMM registers), string support (data section)

## Design Principles

- **DRY**: Shared AST types, utility helpers, `Block::empty()` / `Block::with_statements()`
- **KISS**: Single crate, simple module boundaries
- **Testability**: 88 unit tests, trait-based interfaces
- **Performance**: logos for lexing, rayon for parallel compilation

## Dependencies

- `logos` — lexical analysis
- `clap` — CLI argument parsing
- `anyhow` / `thiserror` — error handling
- `serde` / `bincode` — PPU serialization
- `rayon` / `num_cpus` — parallel compilation
- `colored` — terminal output

## Planned: Web GUI Runtime

```
Pascal Source → Interpreter → SSE events → React/Carbon Web UI
                    ↑                              │
                    └──── user events (POST) ──────┘
```

- Backend: Rust HTTP server (axum) with SSE streaming
- Frontend: React + Carbon Design System + TailwindCSS
- Bridge: interpreter emits GUI commands; user events POST back

## Related Documentation

- [SPEC.md](./SPEC.md) — Language and CLI specification
- [TODO.md](./TODO.md) — Development roadmap
- [README.md](./README.md) — Project introduction
