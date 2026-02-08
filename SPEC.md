# SPEC - pascal-rs Pascal Compiler

## Overview

pascal-rs is a Pascal compiler and interpreter written in Rust. It provides both compilation to x86-64 assembly and direct interpretation of Pascal programs.

## CLI Commands

| Command | Description |
|---------|-------------|
| `pascal compile <file>` | Compile a Pascal source file |
| `pascal run <file>` | Interpret/run a Pascal program |
| `pascal info <ppu>` | Show PPU file information |
| `pascal clean [dir]` | Remove compiled PPU files |

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

## Supported Pascal Constructs

### Data Types
- `integer` - 64-bit signed integer
- `real` - 64-bit floating point
- `boolean` - true/false
- `char` - single character
- `string` - string type
- `array` - static arrays with index ranges
- `record` - structured types
- `pointer` - pointer types

### Statements
- `assignment` - `:=` operator
- `if/then/else` - conditional
- `while/do` - pre-test loop
- `for/to/downto/do` - counted loop
- `repeat/until` - post-test loop
- `case/of` - multi-way branch
- `begin/end` - compound statement
- `procedure call` - with or without arguments

### Expressions
- Arithmetic: `+`, `-`, `*`, `/`, `div`, `mod`
- Comparison: `=`, `<>`, `<`, `>`, `<=`, `>=`
- Logical: `and`, `or`, `not`, `xor`
- Bitwise: `shl`, `shr`
- String concatenation: `+`
- Function calls
- Unary: `-`, `+`, `not`

### Declarations
- `program` header with optional `uses` clause
- `const` section
- `type` section
- `var` section (supports comma-separated names)
- `function` declarations with parameters and return type
- `procedure` declarations with parameters

## Interpreter Built-ins

### Procedures
- `write(args...)` - output without newline
- `writeln(args...)` - output with newline
- `readln(var)` - read line from stdin
- `inc(var [, amount])` - increment
- `dec(var [, amount])` - decrement
- `halt([code])` - exit program

### Functions
- Math: `abs`, `sqr`, `sqrt`, `sin`, `cos`, `ln`, `exp`
- Conversion: `round`, `trunc`, `ord`, `chr`, `inttostr`, `strtoint`
- String: `length`, `concat`, `copy`, `pos`, `upcase`, `lowercase`
- Ordinal: `odd`, `succ`, `pred`
- Other: `random`

## Compilation Pipeline

```
Source (.pas) → Lexer → Parser → AST → [Optimizer] → Code Generator → Assembly (.asm)
                                    ↘ Interpreter (direct execution)
```

## Module System

- Units with `interface` and `implementation` sections
- PPU (Precompiled Unit) files for caching
- Module dependency resolution
- Parallel compilation support via rayon
