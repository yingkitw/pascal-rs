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
| `array` (dynamic) | ✅ | ✅ | ⚠️ partial |
| `record ... end` | ✅ | ✅ | ⚠️ partial |
| `object` (class instance) | ✅ | ✅ | ❌ |
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
| `record.field` access | ✅ | ✅ |
| `obj.field` / `obj.Method()` | ✅ | ✅ |
| `try/except/finally` | ✅ | ✅ |
| `raise` | ✅ | ✅ |
| `with` | ✅ | ✅ |
| `exit` / `exit(value)` | ✅ | ✅ |
| `break` | ✅ | ✅ |
| `goto/label` | ✅ AST only | ❌ |

### Expressions

| Expression | Parser | Interpreter |
|------------|--------|-------------|
| Arithmetic: `+`, `-`, `*`, `/`, `div`, `mod` | ✅ | ✅ |
| Comparison: `=`, `<>`, `<`, `>`, `<=`, `>=` | ✅ | ✅ |
| Logical: `and`, `or`, `not`, `xor` | ✅ | ✅ |
| Bitwise: `shl`, `shr` | ✅ | ✅ |
| String concatenation: `+` | ✅ | ✅ |
| String indexing: `s[i]` | ✅ | ✅ |
| Array indexing: `arr[i]` | ✅ | ✅ |
| Unary: `-`, `+`, `not`, `@` | ✅ | ✅ (except `@`) |
| Function calls | ✅ | ✅ |
| Parenthesized `(expr)` | ✅ | ✅ |
| `inherited` | ✅ | ✅ |
| `is` / `as` type checks | ✅ | ✅ |
| `sizeof` | ✅ AST only | ❌ |

### Declarations

| Declaration | Parser | Interpreter |
|-------------|--------|-------------|
| `program` header | ✅ | ✅ |
| `uses` clause | ✅ | ✅ |
| `const` section | ✅ | ✅ |
| `type` section | ✅ | ⚠️ partial |
| `var` section (comma names) | ✅ | ✅ |
| `function` with params/return | ✅ | ✅ |
| `procedure` with params | ✅ | ✅ |
| nested functions/procedures | ✅ | ✅ |
| `forward` declarations | ✅ | ❌ |
| `unit` (interface/implementation) | ✅ | ✅ |

### Object Pascal

| Feature | AST | Parser | Interpreter |
|---------|-----|--------|-------------|
| `class` declaration | ✅ | ✅ | ✅ |
| `constructor`/`destructor` | ✅ | ✅ | ✅ |
| `virtual`/`override`/`abstract` | ✅ | ✅ | ✅ |
| `private`/`protected`/`public`/`published` | ✅ | ✅ | ⚠️ parsed, not enforced |
| `property` with read/write | ✅ | ✅ | ✅ infrastructure |
| `interface` declarations | ✅ | ❌ | ❌ |
| inheritance `class(TParent)` | ✅ | ✅ | ✅ |
| `try/except/finally` | ✅ | ✅ | ✅ |
| `raise` | ✅ | ✅ | ✅ |
| object instantiation (`ClassName.Create`) | ✅ | ✅ | ✅ |
| method dispatch (vtable) | ✅ | ✅ | ✅ |
| `is` / `as` type checks | ✅ | ✅ | ✅ |
| `inherited` calls | ✅ | ✅ | ✅ |
| inline class method bodies in `type` | ❌ | ❌ | N/A |

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
| Unit tests (lib) | 115 | ✅ All passing |
| Example pipeline tests | 19 | ✅ All passing |
| Compiler codegen tests | 10 | ✅ All passing |
| Complex validation tests | 9 | ✅ All passing |
| Integration tests | 10 | ✅ All passing |
| Interpreter tests | 11 | ✅ All passing |
| Simple compiler (parser) | 18 | ✅ All passing |
| Simple interpreter | 13 | ✅ All passing |
| Type checker | 10 | ✅ All passing |
| Basic | 1 | ✅ All passing |
| **Total** | **216** | **✅ All passing** |
