# pascal-rs — A Modern Pascal Compiler & Interpreter in Rust

**pascal-rs** is a Pascal compiler and tree-walking interpreter written in Rust. It supports standard Pascal and a growing subset of Object Pascal (classes, exceptions, inheritance, virtual dispatch), backed by 216 automated tests and 10 runnable example programs.

---

## Status

| Metric | Value |
|--------|-------|
| **Tests** | 216 passing (115 unit + 101 integration) |
| **Language** | Standard Pascal + Object Pascal subset |
| **Interpreter** | Full-featured tree-walking execution |
| **Compiler** | x86-64 assembly generation with optimizations |
| **Edition** | Rust 2024 |

---

## What's New

### Object Pascal in the Interpreter

The interpreter now supports core Object Pascal features at the AST/runtime level:

- **Classes** — `Value::Object` with field access, constructors, destructors, method dispatch
- **Inheritance** — single inheritance with field/method merging
- **Virtual/Override** — vtable-style dispatch via `find_method_in_hierarchy`
- **`is` / `as`** — runtime type checks walking the inheritance chain
- **`inherited`** — call parent class methods
- **Properties** — read/write resolution infrastructure
- **Exceptions** — `try/except/finally`, `raise`, `on E: Type do` matching, re-raise

### Interpreter Enhancements

- **Arrays** — `SetLength`, indexing with bounds checking, `high()`, `low()`, `length()`
- **Records** — field access and assignment via dot notation
- **String indexing** — `s[i]` (1-indexed, Pascal style)
- **`exit`** — early return from functions/procedures, with optional return value (`exit(42)`)
- **`break`** — loop termination
- **`with` statement** — pushes object/record fields into scope
- **`uses` clause** — loads and imports `.pas` unit files
- **Nested functions** — proper scoping for functions declared inside functions

### Parser Improvements

- **Source location tracking** — error messages now include `at line N, column M`
- **Error recovery** — `consume_or_skip` and `synchronize` for resilient parsing
- **`exit`/`break` tokens** — properly parsed as statements (not silently skipped)

### Bug Fixes

- **Recursive function scoping** — `set_local_variable` prevents recursive calls from clobbering parent scope return variables
- **`exit` inside `if`/`begin..end`** — `EarlyReturn` now propagates correctly through all control flow

### 10 Example Programs

Validated end-to-end (source → lexer → parser → interpreter):

| # | File | Features |
|---|------|----------|
| 01 | `01_basics.pas` | Variables, arithmetic, if/else, while, for, repeat/until, case |
| 02 | `02_functions.pas` | Factorial, Fibonacci, IsPrime, GCD, procedures |
| 03 | `03_strings.pas` | Reverse, palindrome, char counting, copy, pos, upcase |
| 04 | `04_arrays.pas` | SetLength, indexing, high/low/length |
| 05 | `05_classes.pas` | Shape area calculations with functions |
| 06 | `06_exceptions.pas` | try/except, try/finally, nested exceptions, raise |
| 07 | `07_nested_functions.pas` | exit() with return values, early return patterns |
| 08 | `08_math_algorithms.pas` | GCD, Collatz, digit sum, fast exponentiation |
| 09 | `09_class_hierarchy.pas` | Polymorphic dispatch, string comparisons |
| 10 | `10_comprehensive.pas` | Recursion, primes, strings, case, exceptions combined |

---

## Why pascal-rs?

### Compared to Alternatives

| | **pascal-rs** | **Free Pascal (FPC)** | **Delphi** | **GNU Pascal** |
|---|---|---|---|---|
| **Implementation language** | Rust (memory-safe) | Object Pascal/C | C++ | C |
| **Interpreter mode** | Built-in tree-walker | No | No | No |
| **Test suite** | 216 automated tests | Large but external | Proprietary | Minimal |
| **Object Pascal** | Subset (classes, exceptions, virtual) | Full | Full | Partial |
| **Trait-based design** | Yes — testable, extensible | No | No | No |
| **Error messages** | Line/column, colored | Basic | Good | Basic |
| **x86-64 codegen** | Yes, with optimizations | Mature, multi-target | Mature | Basic |
| **Active** | Yes | Yes | Yes | No (2006) |
| **Cross-platform** | Yes (Rust targets) | Yes | Windows-focused | Yes |
| **License** | Apache-2.0 | LGPL | Commercial | GPL |

### When to Use pascal-rs

- **Learning compiler construction** — clean Rust codebase, trait-based architecture, well-tested
- **Running Pascal programs quickly** — `pascal run program.pas` with no assembly/linking step
- **Research & experimentation** — easy to extend with new optimizations or language features
- **Small projects & algorithms** — full standard Pascal with functions, recursion, strings, arrays

### When to Use Something Else

- **Large production codebases** — FPC or Delphi have decades of maturity
- **Full Delphi compatibility** — pascal-rs covers a subset of Object Pascal
- **GUI applications** — FPC/Lazarus or Delphi have mature widget libraries
- **Multi-target compilation** — FPC supports ARM, MIPS, PowerPC, WebAssembly, etc.

---

## Quick Start

### Installation

```bash
git clone https://github.com/yingkitw/pascal-rs.git
cd pascal-rs
cargo build --release
```

### Run a Program

```bash
# Interpret directly (no assembly step)
./target/release/pascal run examples/01_basics.pas

# Compile to x86-64 assembly
./target/release/pascal compile examples/01_basics.pas -S
```

### Run All Tests

```bash
cargo test
```

```
test result: ok. 216 passed; 0 failed; 0 ignored
```

---

## Usage

### Commands

| Command | Description | Example |
|---------|-------------|---------|
| `run` | Interpret a Pascal program | `pascal run prog.pas` |
| `compile` | Compile to assembly | `pascal compile prog.pas -S` |
| `info` | Inspect a PPU file | `pascal info module.ppu` |
| `clean` | Remove build artifacts | `pascal clean` |

### Options

| Option | Description |
|--------|-------------|
| `-o, --output <DIR>` | Output directory |
| `-O, --optimize <LEVEL>` | Optimization level (0–3) |
| `-v, --verbose` | Verbose output |
| `-S, --assembly` | Generate assembly output |
| `-d, --debug` | Debug information |
| `-I, --include <DIR>` | Add unit search path |
| `--no-cache` | Disable PPU caching |

### Interpreter Mode

The interpreter runs Pascal programs directly without an assembly/linking step:

```bash
pascal run program.pas        # Run
pascal run program.pas -v     # Run with trace output
```

### Compilation Mode

```bash
pascal compile program.pas          # Compile
pascal compile program.pas -S       # Emit assembly
pascal compile program.pas -O2      # Optimize
pascal compile program.pas -o build # Output to directory
```

### Unit System

```bash
# Compile a unit
pascal compile MathUtils.pas -v

# Use it from a program
pascal run Calculator.pas    # uses MathUtils;
```

---

## Examples

### Recursive Fibonacci with Exit

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

### Prime Sieve with Early Return

```pascal
program Primes;
var
  i, count: integer;

function IsPrime(n: integer): boolean;
var
  i: integer;
begin
  if n < 2 then
  begin
    IsPrime := false;
    exit;
  end;
  i := 2;
  while i * i <= n do
  begin
    if n mod i = 0 then
    begin
      IsPrime := false;
      exit;
    end;
    inc(i);
  end;
  IsPrime := true;
end;

begin
  count := 0;
  for i := 2 to 100 do
    if IsPrime(i) then
      inc(count);
  writeln('Primes under 100: ', count);  { Output: 25 }
end.
```

### String Operations

```pascal
program Strings;
var
  s, rev: string;
  i, len: integer;

function ReverseStr(s: string): string;
var
  i, n: integer;
  r: string;
begin
  n := length(s);
  r := '';
  for i := n downto 1 do
    r := concat(r, copy(s, i, 1));
  ReverseStr := r;
end;

begin
  s := 'Hello, Pascal!';
  writeln('Original: ', s);
  writeln('Length: ', length(s));
  writeln('Upper: ', upcase(s));
  writeln('Reversed: ', ReverseStr(s));
  writeln('Substring: ', copy(s, 8, 6));  { Pascal }
  writeln('Char 1: ', s[1]);              { H }
end.
```

### Exception Handling

```pascal
program Exceptions;
begin
  try
    writeln('Before raise');
    raise Exception.Create('something went wrong');
    writeln('SHOULD NOT PRINT');
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

### Math Algorithms

```pascal
program Math;
var
  a, b: integer;

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

function Power(base, exp: integer): integer;
var
  result: integer;
begin
  result := 1;
  while exp > 0 do
  begin
    if exp mod 2 = 1 then
      result := result * base;
    base := base * base;
    exp := exp div 2;
  end;
  Power := result;
end;

begin
  writeln('GCD(48, 18) = ', GCD(48, 18));    { 6 }
  writeln('2^10 = ', Power(2, 10));           { 1024 }
end.
```

### Unit System

**MathUtils.pas:**
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

**Main.pas:**
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

See the `examples/` directory for all 10 validated example programs.

---

## Architecture

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

### Project Structure

```
pascal-rs/
├── src/
│   ├── lexer.rs                # Lexical analysis (logos-based, 100+ tokens)
│   ├── parser/                 # Recursive descent parser
│   │   ├── mod.rs              # Parser core with source location tracking
│   │   ├── expression.rs       # Expression parsing (precedence climbing)
│   │   ├── statement.rs        # Statement parsing (if, while, for, try, exit)
│   │   └── decl.rs             # Declaration parsing (var, type, class, function)
│   ├── ast.rs                  # AST node definitions
│   ├── interpreter.rs          # Tree-walking interpreter (Object Pascal support)
│   ├── type_checker.rs         # Type validation
│   ├── optimizer.rs            # Optimization passes
│   ├── unit_codegen.rs         # x86-64 code generation
│   ├── resolver.rs             # Symbol resolution
│   └── traits/                 # Trait abstractions for testability
├── tests/                      # 101 integration tests across 8 test files
├── examples/                   # 10 validated example programs
├── ARCHITECTURE.md
├── SPEC.md
└── TODO.md
```

### Built-in Functions & Procedures

**Math:** `abs`, `sqr`, `sqrt`, `sin`, `cos`, `ln`, `exp`, `round`, `trunc`

**String:** `length`, `concat`, `copy`, `pos`, `upcase`, `lowercase`, `inttostr`, `strtoint`

**Ordinal:** `ord`, `chr`, `odd`, `succ`, `pred`, `inc`, `dec`

**Array:** `length`, `high`, `low`, `setlength`

**I/O:** `write`, `writeln`, `readln`

**Control:** `exit`, `break`, `halt`, `random`

---

## Testing

```bash
cargo test                                        # All 216 tests
cargo test --lib                                  # 115 unit tests
cargo test --test run_example_tests               # 19 example pipeline tests
cargo test --test run_integration_tests           # 10 integration tests
cargo test --test run_compiler_tests              # 10 codegen tests
cargo test --test run_complex_validation_tests    # 9 complex validation tests
cargo test --test run_interpreter_tests           # 11 interpreter tests
cargo test --test run_simple_compiler_tests       # 18 parser tests
cargo test --test run_simple_interpreter_tests    # 13 simple interpreter tests
cargo test --test run_type_checker_tests          # 10 type checker tests
```

### Test Breakdown

```
Library (unit tests)        115
Example pipeline tests       19  ← NEW: validates 10 examples end-to-end
Compiler codegen tests       10
Complex validation            9
Integration tests            10
Interpreter tests            11
Simple compiler (parser)     18
Simple interpreter           13
Type checker                 10
Basic                         1
─────────────────────────────────
Total                       216
```

---

## Documentation

- **[ARCHITECTURE.md](ARCHITECTURE.md)** — module design and data flow
- **[SPEC.md](SPEC.md)** — language feature matrix (parser/interpreter/codegen status)
- **[TODO.md](TODO.md)** — development roadmap and completed phases
- **[TRAIT_ARCHITECTURE.md](TRAIT_ARCHITECTURE.md)** — trait-based design guide

---

## Current Limitations

- **Inline class method bodies** — parser doesn't yet support method bodies inside `type` class declarations (interpreter supports classes via AST)
- **Array element assignment** — `arr[i] := val` not yet supported
- **Multi-dimensional arrays** — single dimension only in interpreter
- **File I/O** — not implemented
- **Generics/templates** — not implemented
- **GUI framework** — not included (use FPC/Lazarus for GUI apps)

---

## License

Apache-2.0

---

**Made with Rust** — *216 tests passing | Standard Pascal + Object Pascal subset | Compiler + Interpreter*
