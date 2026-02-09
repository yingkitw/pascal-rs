# SPEC - pascal-rs Pascal Compiler

## Overview

pascal-rs is a Pascal compiler and interpreter written in Rust. It provides:
- **Interpreter** (`pascal run`) — tree-walking execution of Pascal programs (primary mode)
- **Compiler** (`pascal compile`) — x86-64 assembly generation (secondary mode)

The project is incrementally adding **Object Pascal** (Delphi/Lazarus) features: classes, inheritance, exceptions, and a web-rendered GUI runtime.

## CLI Commands

| Command | Description | Status |
|---------|-------------|--------|
| `pascal compile <file>` | Compile Pascal source to assembly | ✅ Working |
| `pascal run <file>` | Interpret/run a Pascal program | ✅ Working |
| `pascal info <ppu>` | Show PPU file information | ✅ Working |
| `pascal clean [dir]` | Remove compiled PPU files | ✅ Working |

### Compile Options

| Flag | Description |
|------|-------------|
| `-o, --output <dir>` | Output directory (default: `.`) |
| `-O <level>` | Optimization level 0-3 |
| `-S, --asm` | Generate assembly output |
| `-g, --debug` | Generate debug info |
| `-v, --verbose` | Verbose output |
| `-j, --parallel` | Enable parallel compilation |
| `-I, --include <path>` | Additional search paths |

---

## Language Features

### Data Types (Parser + Interpreter)

| Type | Parser | Interpreter | Codegen |
|------|--------|-------------|---------|
| `integer` (i64) | ✅ | ✅ | ✅ |
| `real` (f64) | ✅ | ✅ | ✅ |
| `boolean` | ✅ | ✅ | ✅ |
| `char` | ✅ | ✅ | ⚠️ partial |
| `string` | ✅ | ✅ | ✅ |
| `array[lo..hi] of T` | ✅ | ❌ | ⚠️ partial |
| `record ... end` | ✅ | ❌ | ⚠️ partial |
| `pointer ^T` | ✅ | ❌ | ❌ |
| `set of T` | ✅ AST only | ❌ | ❌ |
| `file of T` | ✅ AST only | ❌ | ❌ |

### Statements (Parser + Interpreter)

| Statement | Parser | Interpreter |
|-----------|--------|-------------|
| `:=` assignment | ✅ | ✅ |
| `if/then/else` | ✅ | ✅ |
| `while/do` | ✅ | ✅ |
| `for/to/downto/do` | ✅ | ✅ |
| `repeat/until` | ✅ | ✅ |
| `case/of/else/end` | ✅ | ✅ |
| `begin/end` block | ✅ | ✅ |
| procedure call | ✅ | ✅ |
| `record.field` access | ✅ | ❌ |
| `try/except/finally` | ✅ | ❌ planned |
| `raise` | ✅ | ❌ planned |
| `goto/label` | ✅ AST only | ❌ |
| `with` | ✅ AST only | ❌ |

### Expressions

| Expression | Parser | Interpreter |
|------------|--------|-------------|
| Arithmetic: `+`, `-`, `*`, `/`, `div`, `mod` | ✅ | ✅ |
| Comparison: `=`, `<>`, `<`, `>`, `<=`, `>=` | ✅ | ✅ |
| Logical: `and`, `or`, `not`, `xor` | ✅ | ✅ |
| Bitwise: `shl`, `shr` | ✅ | ✅ |
| String concatenation: `+` | ✅ | ✅ |
| Unary: `-`, `+`, `not`, `@` | ✅ | ✅ (except `@`) |
| Function calls | ✅ | ✅ |
| Parenthesized `(expr)` | ✅ | ✅ |
| `inherited` | ✅ AST only | ❌ planned |
| `is` / `as` type checks | ✅ AST only | ❌ planned |
| `sizeof` | ✅ AST only | ❌ |

### Declarations

| Declaration | Parser | Interpreter |
|-------------|--------|-------------|
| `program` header | ✅ | ✅ |
| `uses` clause | ✅ | ❌ |
| `const` section | ✅ | ✅ |
| `type` section | ✅ | ❌ |
| `var` section (comma names) | ✅ | ✅ |
| `function` with params/return | ✅ | ✅ |
| `procedure` with params | ✅ | ✅ |
| `forward` declarations | ✅ | ❌ |
| `unit` (interface/implementation) | ✅ | ❌ |

### Object Pascal (In Progress)

| Feature | AST | Parser | Interpreter |
|---------|-----|--------|-------------|
| `class` declaration | ✅ | ✅ | ❌ planned |
| `constructor`/`destructor` | ✅ | ✅ | ❌ planned |
| `virtual`/`override`/`abstract` | ✅ | ✅ | ❌ planned |
| `private`/`protected`/`public`/`published` | ✅ | ✅ | ❌ planned |
| `property` with read/write | ✅ | ✅ | ❌ planned |
| `interface` declarations | ✅ | ❌ | ❌ planned |
| inheritance `class(TParent)` | ✅ | ✅ | ❌ planned |
| `try/except/finally` | ✅ | ✅ | ❌ planned |
| `raise` | ✅ | ✅ | ❌ planned |
| object instantiation | ❌ | ❌ | ❌ planned |
| method dispatch (vtable) | ❌ | ❌ | ❌ planned |

---

## Interpreter Built-ins

### Procedures
- `write(args...)` — output without newline
- `writeln(args...)` — output with newline
- `readln(var)` — read line from stdin
- `inc(var [, amount])` — increment
- `dec(var [, amount])` — decrement
- `halt([code])` — exit program

### Functions
- **Math**: `abs`, `sqr`, `sqrt`, `sin`, `cos`, `ln`, `exp`
- **Conversion**: `round`, `trunc`, `ord`, `chr`, `inttostr`, `strtoint`
- **String**: `length`, `concat`, `copy`, `pos`, `upcase`, `lowercase`
- **Ordinal**: `odd`, `succ`, `pred`
- **Other**: `random`

---

## Compilation Pipeline

```
Source (.pas) → Lexer (logos) → Parser (recursive descent) → AST
                                                              │
                                    ┌─────────────────────────┼──────────────────┐
                                    ▼                         ▼                  ▼
                              Interpreter              Optimizer           Type Checker
                           (tree-walking)          (const fold, DCE)    (basic validation)
                                    │                     │
                                    ▼                     ▼
                              Direct Output        Code Generator → Assembly (.s)
```

## Module System

- Units with `interface` and `implementation` sections
- PPU (Precompiled Unit) files for caching (bincode serialization)
- Module dependency resolution via `ModuleLoader`
- Parallel compilation support via rayon

---

## Web GUI Runtime (Planned)

The goal is to render Delphi/Lazarus-style GUI components in a web browser:

```
Pascal Source → Interpreter → SSE events → React/Carbon Web UI
                    ↑                              │
                    └──── user events ─────────────┘
```

### Planned Components
- `TForm` — window/page container
- `TButton` — clickable button
- `TLabel` — text label
- `TEdit` — text input
- `TPanel` — container panel

### Architecture
- **Backend**: Rust HTTP server with SSE streaming
- **Frontend**: React + Carbon Design System + TailwindCSS
- **Bridge**: Pascal interpreter emits GUI commands via SSE; user events POST back

---

## Test Suite

| Category | Count | Status |
|----------|-------|--------|
| Unit tests (lib) | 88 | ✅ All passing |
| Integration tests | ~73 | ⚠️ Not all verified |

### Verified Unit Test Modules
- `interpreter` (11 tests) — arithmetic, control flow, functions
- `parser` (14 tests) — programs, units, expressions, statements
- `optimizer` (3 tests) — constant folding, DCE, algebraic simplification
- `advanced_optimizer` (3 tests) — CSE, loop unrolling, strength reduction
- `advanced_types` (3 tests) — generics, type inference, operator overloading
- `loader` (8 tests) — module loading, caching, concurrency
- `parallel` (16 tests) — parallel compilation, progress tracking
- `register_allocator` (3 tests) — live ranges, graph coloring
- `resolver` (3 tests) — symbol resolution
- `simd` (3 tests) — SIMD codegen, calling conventions
- `symbol_table` (3 tests) — scopes, constants
- `type_checker` (2 tests) — literal types, binary ops
- `unit_codegen` (2 tests) — empty unit generation
- `utils` (8 tests) — AST helpers, block, string utils
