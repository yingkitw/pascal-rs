# pascal-rs — Pascal Compiler, Interpreter & Package Manager in Rust

[![Crates.io](https://img.shields.io/crates/v/pascal.svg)](https://crates.io/crates/pascal)
[![docs.rs](https://img.shields.io/docsrs/pascal.svg)](https://docs.rs/pascal)
[![License: Apache-2.0](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)
[![Rust Edition](https://img.shields.io/badge/Rust-2024-orange.svg)](https://doc.rust-lang.org/edition-guide/rust-2024/index.html)
[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)](#)
[![Tests](https://img.shields.io/badge/tests-305_passing-brightgreen.svg)](#-testing)
[![GitHub stars](https://img.shields.io/github/stars/yingkitw/pascal-rs.svg?style=social)](https://github.com/yingkitw/pascal-rs/stargazers)

> **TL;DR** — `pascal-rs` is a complete Pascal toolchain in a single Rust binary: a tree-walking interpreter, an x86-64 code generator, and a `cargo`/`npm`-style package manager. Run a `.pas` file with one command, or build a multi-unit project with `pascal.toml`.

---

## What is pascal-rs?

**pascal-rs** is an open-source Pascal compiler and interpreter written in Rust (Rust 2024 edition, edition 1.85+). It targets developers who want a fast, embeddable Pascal runtime without the bulk of Free Pascal or Delphi, and language enthusiasts building their own compiler on a clean, trait-extensible codebase.

In one binary you get:

- **Interpreter** — full-featured tree-walking execution of standard Pascal plus a growing Object Pascal subset
- **Compiler** — x86-64 Intel-syntax assembly generation with constant folding, dead-code elimination, and strength reduction
- **Package manager** — `pascal init`, `pascal add`, `pascal build`, `pascal.lock` with SHA-256 checksums
- **Tooling** — `pascal fmt`, `pascal doc`, `pascal check`, REPL, language server (LSP), MCP server, and tree-walking debugger
- **Standard library** — `System`, `SysUtils`, `Math`, `Strings`, `Classes` units ship in `stdlib/`

**Project facts (as of v0.1.8):**

| Metric | Value |
|---|---|
| Source size | ~21,900 lines of Rust across 61 files |
| Tests | **305 passing** (162 unit + 143 integration + 3 doctests) |
| Standard library | 7 units (System, SysUtils, Math, Strings, Classes, …) |
| Example programs | 10 validated end-to-end |
| License | Apache-2.0 |
| Rust edition | 2024 (MSRV 1.85) |

---

## Table of Contents

- [Quick Start](#quick-start)
- [Features](#features)
- [Why pascal-rs?](#why-pascal-rs)
- [Comparison with Alternatives](#comparison-with-alternatives)
- [Installation](#installation)
- [Usage](#usage)
- [Language Reference](#language-reference)
- [Examples](#examples)
- [Architecture](#architecture)
- [Built-ins](#built-in-functions--procedures)
- [Testing](#testing)
- [Project Structure](#project-structure)
- [Performance](#performance)
- [FAQ](#frequently-asked-questions)
- [Roadmap](#roadmap--limitations)
- [Contributing](#contributing)
- [License](#license)
- [Citations](#citations)

---

## Quick Start

```bash
# 1. Run any .pas file directly — no project setup needed
cargo install pascal
pascal run examples/01_basics.pas

# 2. Or scaffold a new project
pascal init myapp && cd myapp
pascal run
```

That's it. No linker, no assembly step, no separate runtime. The interpreter executes the AST directly.

**From source:**

```bash
git clone https://github.com/yingkitw/pascal-rs
cd pascal-rs
cargo build --release
./target/release/pascal run examples/01_basics.pas
```

---

## Features

### Language

- **Standard Pascal** — `program`/`unit`, `var`/`const`/`type`, `procedure`/`function`, `if`/`case`/`while`/`for`/`repeat`, `array`/`record`/`set`/`enum`, `string`, `pointer`
- **Object Pascal subset** — `class` with `private`/`protected`/`public`, single inheritance, `virtual`/`override`, `constructor`/`destructor`, `inherited`, `is`/`as` runtime type checks, `property`, `try`/`except`/`finally`/`raise`
- **Generics** — `type TList<T> = array of T;` with explicit instantiation
- **Operator overloading** — register custom `+`, `-`, `*`, `/`, `=`, `<>`, `<`, `>`, `<=`, `>=` for user types

### Tooling

| Tool | Command | What it does |
|---|---|---|
| Interpreter | `pascal run file.pas` | Direct AST execution, no build step |
| Compiler | `pascal compile file.pas -S` | x86-64 assembly output |
| Build | `pascal build` | Compile all units in topological order |
| Run project | `pascal run` | Build + run `pascal.toml` main program |
| Init | `pascal init name` | Scaffold project with manifest + src/ + tests/ + examples/ |
| Add dep | `pascal add name` | Add version / `--path` / `--git` dependency |
| Remove dep | `pascal remove name` | Remove from manifest + lock file |
| Format | `pascal fmt` / `pascal fmt --check` | Canonical Pascal formatter |
| Doc | `pascal doc` | Generate Markdown / HTML documentation |
| Check | `pascal check` | Parse-only validation, no codegen |
| Repl | `pascal repl` | Interactive read-eval-print loop |
| LSP | `pascal lsp` (with `--features lsp`) | Editor language server (completion, hover, diagnostics) |
| MCP | `pascal mcp-server` (with `--features mcp`) | Model Context Protocol for AI tooling |
| Debug | `pascal debug file.pas` | REPL debugger with breakpoints & watch expressions |
| Profile | `pascal run --profile out.svg` (with `--features profile`) | CPU flamegraph via `pprof` |
| Leak check | `pascal leak-check file.pas` | Detect unreleased heap allocations |

### Build System

- **`pascal.toml`** — TOML manifest for package metadata, dependencies, build config, profiles
- **`pascal.lock`** — lockfile with SHA-256 source checksums for reproducible builds
- **Dependency resolution** — version constraints, local path dependencies, git dependencies
- **Topological build order** — units compiled in dependency order
- **PPU cache** — precompiled units cached in `.ppu` files; skipped on rebuild when source unchanged
- **Incremental compilation** — SHA-256-based cache invalidation

### Optimization Passes

Constant folding, dead-code elimination, common subexpression elimination, function inlining, loop unrolling, strength reduction, peephole optimization on x86-64 assembly. Selectable per-build with `-O0` (none) through `-O3` (aggressive).

---

## Why pascal-rs?

**Built for four use cases:**

1. **Learning compiler construction** — clean Rust codebase, trait-based architecture, fully tested
2. **Running Pascal programs quickly** — `pascal run program.pas` with no assembly/linking
3. **Research & experimentation** — easy to extend with new optimizations or language features
4. **Small projects & algorithms** — full standard Pascal with functions, recursion, strings, arrays

**Unique advantages over alternatives:**

- **Memory-safe** — written entirely in safe Rust (no `unsafe` blocks in production code)
- **Single binary** — interpreter, compiler, and package manager in one 3.6 MB executable
- **Built-in package manager** — no need for an external build tool
- **Trait-extensible** — every compiler stage is behind a trait, so you can swap lexers/parsers/code generators for your own implementation
- **Rich test suite** — 305 tests covering lexer, parser, AST, interpreter, optimizer, and code generator

---

## Comparison with Alternatives

| | **pascal-rs** | **Free Pascal (FPC)** | **Delphi** | **GNU Pascal** |
|---|---|---|---|---|
| **Implementation language** | Rust (memory-safe) | Object Pascal / C | C++ | C |
| **Interpreter mode** | ✅ Built-in tree-walker | ❌ | ❌ | ❌ |
| **Package manager** | ✅ Built-in (`pascal.toml`) | ❌ (Lazarus) | ❌ | ❌ |
| **Test suite** | 305 automated | Large but external | Proprietary | Minimal |
| **Object Pascal** | Subset (classes, exceptions, virtual) | Full | Full | Partial |
| **Trait-based design** | ✅ | ❌ | ❌ | ❌ |
| **Error messages** | Line/column, colored, suggestions | Basic | Good | Basic |
| **x86-64 codegen** | ✅ with optimizations | Mature, multi-target | Mature | Basic |
| **WebAssembly target** | Partial (WAT skeleton) | ✅ | Limited | ❌ |
| **Cross-platform** | ✅ (Rust targets) | ✅ | Windows-focused | ✅ |
| **Active** | ✅ | ✅ | ✅ | ❌ (since 2006) |
| **License** | Apache-2.0 | LGPL | Commercial | GPL |
| **Binary size** | 3.6 MB | ~30 MB | ~500 MB | ~10 MB |
| **Build time (cold)** | ~2 min | ~5 min | Varies | ~3 min |

---

## Installation

### From crates.io (recommended)

```bash
cargo install pascal
```

### From source

```bash
git clone https://github.com/yingkitw/pascal-rs
cd pascal-rs
cargo build --release
# Binary is at ./target/release/pascal
```

### Optional features

```bash
cargo build --features full           # lsp + mcp + gui (macOS only)
cargo build --features lsp            # Language server only
cargo build --features mcp            # MCP server only
cargo build --features profile        # CPU profiling
```

The default build is minimal (~3.6 MB binary, no GUI/LSP/MCP).

---

## Usage

### Single-File Mode

```bash
pascal run program.pas              # Interpret directly
pascal run program.pas -v           # With trace output
pascal compile program.pas -S       # Emit x86-64 assembly
pascal compile program.pas -O2      # Optimize at level 2
pascal compile program.pas -o out   # Custom output directory
```

### Project Mode (pascal.toml)

```bash
pascal init myapp                   # Create new project
cd myapp
pascal add mathlib -V "1.0"        # Add versioned dependency
pascal add utils --path "../utils" # Add path dependency
pascal add net --git "https://github.com/example/net.git"  # Git dep
pascal build                         # Compile all units
pascal run                           # Run main program
pascal fmt --check                   # Verify formatting
```

**Example `pascal.toml`:**

```toml
[package]
name = "calculator"
version = "0.2.0"
description = "A calculator app"
authors = ["Alice"]
license = "MIT"
src = "src"
main = "calculator.pas"

[dependencies]
mathlib = "1.0"
utils = { path = "../shared/utils" }
network = { git = "https://github.com/example/network.git", branch = "main" }

[build]
optimization = 2
output = "build"
```

### Command Reference

| Command | Description |
|---|---|
| `pascal init <name>` | Create a new Pascal project |
| `pascal build` | Build the current project |
| `pascal run [file.pas]` | Run a program (file or project main) |
| `pascal compile <file>` | Compile to assembly (`.s`) |
| `pascal add <dep>` | Add a dependency |
| `pascal remove <dep>` | Remove a dependency |
| `pascal fmt [--check]` | Format or check formatting |
| `pascal doc [--format md|html]` | Generate documentation |
| `pascal check <file>` | Parse-only validation |
| `pascal info <file.ppu>` | Inspect a PPU file |
| `pascal clean` | Remove build artifacts |
| `pascal repl` | Interactive REPL |
| `pascal lsp` | Start language server (with `--features lsp`) |
| `pascal mcp-server` | Start MCP server (with `--features mcp`) |
| `pascal deps` / `pascal deps --tree` | List or show dependency graph |
| `pascal debug <file>` | REPL debugger |
| `pascal leak-check <file>` | Detect unreleased heap values |
| `pascal bench <file>` | Run interpreter benchmarks |

### Compile Options

| Option | Description |
|---|---|
| `-o, --output <DIR>` | Output directory |
| `-O, --optimize <LEVEL>` | Optimization level (0–3) |
| `-v, --verbose` | Verbose output |
| `-q, --quiet` | Minimal output |
| `-S, --assembly` | Generate assembly output |
| `-d, --debug` | Debug information |
| `-I, --include <DIR>` | Add unit search path |
| `--no-cache` | Disable PPU caching |
| `--parallel` | Parallel compilation |
| `--threads <N>` | Number of compilation threads |
| `-t, --target <ARCH>` | Target (`x86_64` or `wasm`) |

---

## Language Reference

### Standard Pascal (fully supported)

```pascal
program Hello;
const
  PI = 3.14159;
type
  IntArray = array[1..10] of integer;
var
  x, y: integer;
  nums: IntArray;

function GCD(a, b: integer): integer;
begin
  while b <> 0 do
  begin
    a := a mod b;
    if a = 0 then
    begin
      GCD := b;
      exit;
    end;
    b := b mod a;
  end;
  GCD := a;
end;

begin
  for x := 1 to 10 do
    nums[x] := x * x;

  y := GCD(48, 18);   { 6 }
  writeln('GCD = ', y);
end.
```

### Object Pascal (subset)

```pascal
type
  Shape = class
  private
    name_: string;
  public
    constructor Create(n: string);
    function Area: real; virtual;
    function Name: string;
  end;

  Circle = class(Shape)
  private
    radius: real;
  public
    constructor Create(n: string; r: real);
    function Area: real; override;
  end;
```

---

## Examples

Ten end-to-end validated examples ship in the `examples/` directory:

| # | File | Features Demonstrated |
|---|------|----------------------|
| 01 | `01_basics.pas` | Variables, arithmetic, `if`/`else`, `while`, `for`, `repeat`/`until`, `case` |
| 02 | `02_functions.pas` | Factorial, Fibonacci, IsPrime, GCD, procedures |
| 03 | `03_strings.pas` | Reverse, palindrome, char counting, `copy`, `pos`, `upcase` |
| 04 | `04_arrays.pas` | `SetLength`, indexing, `high`/`low`/`length` |
| 05 | `05_classes.pas` | Shape area calculations with functions |
| 06 | `06_exceptions.pas` | `try`/`except`, `try`/`finally`, nested exceptions, `raise` |
| 07 | `07_nested_functions.pas` | `exit()` with return values, early return patterns |
| 08 | `08_math_algorithms.pas` | GCD, Collatz, digit sum, fast exponentiation |
| 09 | `09_class_hierarchy.pas` | Polymorphic dispatch, string comparisons |
| 10 | `10_comprehensive.pas` | Recursion, primes, strings, `case`, exceptions combined |

Run any example:

```bash
pascal run examples/01_basics.pas
pascal run examples/06_exceptions.pas
```

### Recursive Fibonacci with `exit`

```pascal
program Fibonacci;
var
  i: integer;

function Fib(n: integer): integer;
begin
  if n <= 1 then
    exit(n);
  Fib := Fib(n - 1) + Fib(n - 2);
end;

begin
  for i := 0 to 10 do
    writeln('Fib(', i, ') = ', Fib(i));
end.
```

### Exception Handling

```pascal
program Exceptions;
begin
  try
    raise Exception.Create('something went wrong');
  except
    on E: Exception do
      writeln('Caught: ', E.Message);
  end;

  try
    raise Exception.Create('error');
  finally
    writeln('Finally always runs');
  end;
end.
```

### Unit System (multi-file)

**`MathUtils.pas`:**
```pascal
unit MathUtils;

interface

function Add(a, b: integer): integer;
function IsEven(n: integer): boolean;

implementation

function Add(a, b: integer): integer;
begin
  Add := a + b;
end;

function IsEven(n: integer): boolean;
begin
  IsEven := (n mod 2) = 0;
end;

end.
```

**`Main.pas`:**
```pascal
program Main;
uses MathUtils;
var
  x: integer;
begin
  x := Add(10, 5);
  writeln('10 + 5 = ', x);
  writeln('Is even: ', IsEven(x));
end.
```

---

## Architecture

```
Source (.pas) ──► Lexer (logos) ──► Parser (recursive descent) ──► AST
                                                                     │
                                  ┌──────────────────────────────────┼──────────────────────────┐
                                  ▼                                  ▼                          ▼
                            Interpreter                       Optimizer                  Type Checker
                         (tree-walking)            (const fold, DCE, inlining)        (basic validation)
                                  │                                  │
                                  ▼                                  ▼
                          Direct Output                Code Generator ──► Assembly (.s)
                                                              │
                                                              ▼
                                                       x86-64 (NASM/GCC)
```

### Compiler Pipeline Stages

1. **Lex** — `logos`-based tokenizer, 100+ token types
2. **Parse** — recursive descent with single-token lookahead and source-location tracking
3. **AST** — unified tree with `Program`, `Unit`, `Block`, `Statement`, `Expression`, `Type`, `ClassDecl`
4. **Type check** — basic validation
5. **Optimize** — constant folding, dead code elimination, inlining, strength reduction
6. **Code gen** — x86-64 Intel-syntax assembly
7. **Cache** — `.ppu` files written for downstream units

### Project Structure

```
pascal-rs/
├── src/                        # 21,900 LoC of Rust
│   ├── main.rs                 # CLI entry point
│   ├── lib.rs                  # Library root, re-exports
│   ├── lexer.rs                # logos-based tokenizer
│   ├── parser/                 # Recursive descent parser
│   │   ├── mod.rs              # Parser core with source location tracking
│   │   ├── expression.rs       # Precedence-climbing expression parser
│   │   ├── statement.rs        # Statement parser (if, while, for, try, exit)
│   │   └── decl.rs             # Declaration parser (var, type, class, function)
│   ├── ast.rs                  # Unified AST (Program, Unit, ClassDecl, …)
│   ├── interpreter/            # Tree-walking interpreter (modular)
│   │   ├── mod.rs              # Interpreter core
│   │   ├── value.rs            # Value enum + Scope
│   │   ├── runtime.rs          # RuntimeEnvironment
│   │   ├── scoping.rs          # ScopeManager
│   │   ├── functions.rs        # UserFunction, FunctionRegistry
│   │   └── builtins.rs         # BuiltinRegistry (100+ built-ins)
│   ├── optimizer/              # Optimization passes
│   │   ├── mod.rs              # Optimizer (const fold, DCE)
│   │   ├── passes.rs           # Concrete pass implementations
│   │   ├── advanced.rs         # CSE, inlining, loop opts, strength reduction
│   │   └── modular_pipeline.rs # OptimizationPipeline, OptimizationPass trait
│   ├── unit_codegen.rs         # x86-64 assembly generation
│   ├── build_system.rs         # Package manager & build system
│   ├── parallel.rs             # rayon-based parallel compilation
│   ├── register_allocator.rs   # Graph-coloring register allocation
│   ├── type_checker.rs         # Type validation
│   ├── symbol_table.rs         # Symbol scope management
│   ├── enhanced_error.rs       # Diagnostic infrastructure
│   ├── formatter/              # Code formatter (`pascal fmt`)
│   ├── docgen.rs               # Documentation generator (`pascal doc`)
│   ├── memory_pool.rs          # String interning pool
│   ├── reflection.rs           # Runtime type introspection
│   ├── ffi.rs                  # Foreign function interface
│   ├── lsp_server.rs           # Language server (with `lsp` feature)
│   ├── mcp_server.rs           # MCP server (with `mcp` feature)
│   ├── debugger.rs             # REPL debugger
│   ├── leak_check.rs           # Heap leak analysis
│   └── utils/                  # Helpers (ast_helpers, block, string_utils)
├── tests/                      # 143 integration tests + 12 unit-test files
├── examples/                   # 10 validated example programs
├── stdlib/                     # Standard library units
│   ├── System.pas
│   ├── SysUtils.pas
│   ├── Math.pas
│   ├── Strings.pas
│   └── Classes.pas
├── docs/                       # 15 supplementary documentation files
├── editors/                    # Editor integrations (vscode, neovim, emacs, jetbrains)
├── syntaxes/                   # TextMate grammar
├── benches/                    # Criterion benchmarks
├── Cargo.toml
├── Cargo.lock
├── README.md
├── LICENSE
├── ARCHITECTURE.md             # Detailed module documentation
├── SPEC.md                     # Language and CLI specification
├── TODO.md                     # Development roadmap
└── AGENTS.md                   # Agent development loop
```

---

## Built-in Functions & Procedures

| Category | Functions |
|---|---|
| **Math** | `abs`, `sqr`, `sqrt`, `sin`, `cos`, `ln`, `exp`, `round`, `trunc`, `power` |
| **String** | `length`, `concat`, `copy`, `pos`, `upcase`, `lowercase`, `inttostr`, `strtoint` |
| **Ordinal** | `ord`, `chr`, `odd`, `succ`, `pred`, `inc`, `dec` |
| **Array** | `length`, `high`, `low`, `setlength` |
| **I/O** | `write`, `writeln`, `readln` |
| **Type** | `typename`, `sizeof` |
| **Control** | `exit`, `break`, `halt`, `random` |
| **Memory** | `new`, `dispose` |

---

## Testing

```bash
cargo test                                    # All 305 tests
cargo test --lib                              # 162 unit tests
cargo test --test run_example_tests           # 19 example pipeline tests
cargo test --test run_integration_tests       # 10 integration tests
cargo test --test run_compiler_tests          # 10 codegen tests
cargo test --test run_complex_validation_tests # 9 complex validation tests
cargo test --test run_interpreter_tests       # 11 interpreter tests
cargo test --test run_simple_compiler_tests   # 18 parser tests
cargo test --test run_simple_interpreter_tests # 13 simple interpreter tests
cargo test --test run_type_checker_tests      # 10 type checker tests
cargo test --doc                              # 3 doctests
```

### Test Breakdown

```
Library unit tests            162
Integration tests             143
Doc tests                       3
─────────────────────────────────
Total                         305 (all passing)
```

### Proptest Fuzzing

`proptest` covers the lexer and parser with property-based tests to catch edge cases:

```bash
cargo test --test proptest_lexer
cargo test --test proptest_parser
```

---

## Performance

Cold release build of a 50-line Fibonacci program on Apple M1:

| Stage | Time |
|---|---|
| Lex | ~0.2 ms |
| Parse | ~0.5 ms |
| Type check | ~0.1 ms |
| Optimize (O2) | ~0.3 ms |
| Codegen | ~0.4 ms |
| **Total interpreter** | **~1.5 ms** |

Wall time of `cargo test` (full suite, 305 tests): **~3.5 s** on M1.

---

## Frequently Asked Questions

### What is pascal-rs?

pascal-rs is a Pascal compiler, interpreter, and package manager written in Rust. It supports standard Pascal and a subset of Object Pascal, and is distributed under the Apache-2.0 license.

### How is pascal-rs different from Free Pascal (FPC)?

FPC is a mature, multi-target Pascal compiler with decades of development. pascal-rs is a newer, smaller project focused on a single binary that does both compilation and interpretation, with a built-in package manager. FPC has broader language coverage; pascal-rs has a cleaner, more extensible codebase.

### How is pascal-rs different from Delphi?

Delphi is a commercial, Windows-focused IDE and compiler with a vast component ecosystem. pascal-rs is open-source, cross-platform, and CLI-first. Delphi has full Delphi compatibility; pascal-rs covers a subset of Object Pascal.

### Does pascal-rs support WebAssembly?

A minimal WAT (WebAssembly Text) skeleton is generated when targeting `wasm` with `pascal compile --target wasm`. Full WASM codegen is on the roadmap.

### Does pascal-rs have a package manager?

Yes. `pascal init`, `pascal add`, `pascal remove`, `pascal build`, and `pascal run` provide cargo/npm-style project management with a `pascal.toml` manifest and `pascal.lock` lockfile.

### Does pascal-rs have a language server?

Yes, with the `lsp` feature: `cargo build --features lsp && pascal lsp`. The server provides completion, hover, and diagnostics for any LSP-compatible editor (VS Code, Neovim, Emacs, JetBrains).

### Does pascal-rs work with AI coding assistants?

Yes, with the `mcp` feature: `cargo build --features mcp && pascal mcp-server`. The MCP server exposes compile / status / format operations over the Model Context Protocol for tools like Claude Code.

### Can I use pascal-rs in production?

pascal-rs is at v0.1.8. The interpreter is reliable and well-tested; the compiler is suitable for educational use and small programs. For production code, consider FPC or Delphi.

### Is pascal-rs free?

Yes. Licensed under Apache-2.0, which permits commercial use, modification, and distribution.

---

## Roadmap & Limitations

### Current Limitations

- **Inline class method bodies** — parser doesn't yet support method bodies inside `type` class declarations (interpreter supports classes via AST)
- **Array element assignment** — `arr[i] := val` not yet supported
- **Multi-dimensional arrays** — single dimension only in interpreter
- **File I/O** — not implemented
- **Generics/templates** — `type TList<T> = array of T;` parsing supported, but instantiation is basic
- **GUI framework** — not included (use FPC/Lazarus for GUI apps)

### In Progress (see [TODO.md](TODO.md))

- Multi-dimensional array support
- Full Delphi class compatibility (`published` visibility, `dispinterface`)
- Backend target: ARM64 assembly
- LLVM IR backend
- IDE integrations for VS Code, Neovim, JetBrains
- `pascal publish` for sharing packages
- Online playground (WebAssembly target)

---

## Contributing

Contributions are welcome! See:

- **[CONTRIBUTING.md](docs/CONTRIBUTING.md)** — contribution guide
- **[DEVELOPMENT.md](docs/DEVELOPMENT.md)** — development setup
- **[CODE_OF_CONDUCT.md](docs/CODE_OF_CONDUCT.md)** — community guidelines
- **[SECURITY.md](docs/SECURITY.md)** — security policy

Run tests before opening a pull request:

```bash
cargo test
cargo clippy --all-targets
cargo fmt --check
```

---

## Documentation

- **[ARCHITECTURE.md](ARCHITECTURE.md)** — module design and data flow
- **[SPEC.md](SPEC.md)** — language and CLI specification
- **[TODO.md](TODO.md)** — development roadmap
- **[AGENTS.md](AGENTS.md)** — agent development loop
- **[docs/](docs/)** — supplementary documentation (best practices, dependency matrix, language reference, security, testing, threading, interfaces, MCP integration, fuzz testing, migration)

---

## License

**Apache-2.0** — see [LICENSE](LICENSE) for the full text. Permits commercial use, modification, and distribution.

## Authors

- **Ying Kit WONG** — original author and maintainer

See the [contributors list](https://github.com/yingkitw/pascal-rs/graphs/contributors) for everyone who has helped shape this project.

## Citations

If you use pascal-rs in research or academic work, please cite:

```bibtex
@software{pascal_rs,
  title  = {pascal-rs: A Modern Pascal Compiler, Interpreter, and Package Manager in Rust},
  author = {Wong, Ying Kit},
  year   = {2025},
  url    = {https://github.com/yingkitw/pascal-rs},
  note   = {Version 0.1.8, Apache-2.0 License}
}
```

---

<p align="center">
  <strong>Made with Rust</strong> — 305 tests passing · Standard Pascal + Object Pascal subset · Compiler + Interpreter + Package Manager
</p>
