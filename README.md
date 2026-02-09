# pascal-rs - A Modern Pascal Compiler in Rust

**pascal-rs** is a Pascal compiler written in Rust, featuring trait-based architecture, comprehensive testing, and clean code generation. It combines Rust's safety guarantees with Pascal's elegance, creating a modern platform for learning compiler construction and experimenting with language design.

---

## ✨ **Status**: Active Development | **Tests**: 169/169 Passing | **Language**: Core Pascal Complete

**Note**: pascal-rs is ideal for education, research, and small projects. For production use with large codebases, consider mature compilers like Free Pascal or Delphi.

---

## 🎯 Why pascal-rs?

### The Problem with Traditional Pascal Compilers

Traditional Pascal compilers suffer from several issues:
- **Aging codebases** - Difficult to maintain and extend with modern features
- **Limited test coverage** - Insufficient validation of edge cases

### The pascal-rs Solution

**pascal-rs** addresses these problems with a modern, Rust-based architecture:

#### 1. **Memory Safety by Design**
```rust
// Rust's ownership model prevents:
// - Null pointer dereferences ✅
// - Buffer overflows ✅
// - Data races ✅
// - Memory leaks ✅
```

#### 2. **Trait-Based Architecture**
- **Clean abstractions** through Rust's trait system
- **Dependency inversion** - depend on traits, not concrete implementations
- **Easy testing** with mock implementations
- **Extensible design** for new features

#### 3. **Comprehensive Testing** (169 tests)
- Unit tests for all components
- Integration tests for end-to-end validation
- Complex validation tests for edge cases
- Performance benchmarks
- **100% test pass rate** ✅

#### 4. **Modern Developer Experience**
- Clear, colored error messages with source locations
- Fast compilation with incremental builds
- Excellent IDE support via Rust tooling
- Comprehensive documentation

#### 5. **Optimizations**
- Constant folding and dead code elimination
- Register allocation with graph coloring
- Common subexpression elimination
- Function inlining and loop unrolling
- SIMD vectorization support

---

## 📊 Comparison with Other Pascal Compilers

### Feature Comparison Table

| Feature | pascal-rs | Free Pascal (FPC) | Delphi | GNU Pascal (GPC) |
|---------|----------|------------------|--------|------------------|
| **Language** | ✅ | ✅ | ✅ | ✅ |
| **Memory Safety** | ✅ Rust guaranteed | C++ implementation | C++ implementation | C implementation |
| **Test Coverage** | ✅ 169 tests | ⚠️ Limited | ⚠️ Limited | ❌ Minimal |
| **Trait-Based Design** | ✅ Yes | ❌ No | ❌ No | ❌ No |
| **Interpreter Mode** | ✅ Built-in | ⚠️ Limited | ❌ No | ❌ No |
| **x86-64 Code Gen** | ✅ Optimized | ✅ Mature | ✅ Mature | ✅ Basic |
| **Active Development** | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No (2006) |
| **Cross-Platform** | ✅ Yes | ✅ Yes | ⚠️ Windows only | ✅ Yes |
| **Compilation Speed** | ✅ Fast | ⚠️ Moderate | ⚠️ Moderate | ⚠️ Slow |
| **Error Messages** | ✅ Modern, colored | ⚠️ Basic | ✅ Good | ⚠️ Basic |

### Unique Advantages

**1. Memory Safety**
- Rust's ownership model prevents buffer overflows, null pointers, and data races
- Traditional C/C++ implementations vulnerable to these issues

**2. Trait-Based Architecture**
```rust
// Easy to test, easy to extend
trait ScopeOperations {
    fn get(&self, name: &str) -> Option<Value>;
    fn set(&mut self, name: &str, value: Value);
}
```

**3. Comprehensive Testing**
- 169 tests, 100% pass rate
- Edge cases validated
- Complex scenarios covered

**4. Modern Build System**
```bash
# Simple and fast
cargo build --release
cargo test
cargo run --bin pascal -- compile program.pas
```

**5. Interpreter Mode**
```bash
# Run directly without assembly/linking
pascal run program.pas
```

---

## 🚀 Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/yingkitw/pascal-rs.git
cd pascal-rs

# Build the compiler
cargo build --release

# The binary is now available at ./target/release/pascal
```

### Your First Pascal Program

Create `hello.pas`:
```pascal
program Hello;
begin
  writeln('Hello, World!');
end.
```

Compile and run:
```bash
# Compile with assembly output
pascal compile hello.pas -S

# Run using the interpreter
pascal run hello.pas

# View generated assembly
cat hello.s
```

**Output:**
```
Hello, World!
```

---

## 📖 Usage Guide

### Basic Compilation

```bash
# Compile a Pascal program
pascal compile program.pas

# Compile with optimization
pascal compile program.pas -O2

# Compile with verbose output
pascal compile program.pas -v

# Compile with assembly output
pascal compile program.pas -S

# Compile to specific directory
pascal compile program.pas -o ./build
```

### Running Programs

```bash
# Run using interpreter
pascal run program.pas

# Run with verbose output
pascal run program.pas -v

# Run with debug information
pascal run program.pas -d
```

### Working with Units

```bash
# Compile a unit (generates .ppu file)
pascal compile MathUtils.pas -v

# Inspect a compiled unit
pascal info mathutils.ppu

# Compile a program that uses the unit
pascal compile Calculator.pas -v
```

### Command Reference

| Command | Description | Example |
|---------|-------------|---------|
| `compile` | Compile Pascal source | `pascal compile prog.pas` |
| `run` | Run with interpreter | `pascal run prog.pas` |
| `info` | Show PPU file info | `pascal info module.ppu` |
| `clean` | Remove build artifacts | `pascal clean` |

### Options

| Option | Description |
|--------|-------------|
| `-o, --output <DIR>` | Output directory |
| `-O, --optimize <LEVEL>` | Optimization level (0-3) |
| `-v, --verbose` | Verbose output |
| `-S, --assembly` | Generate assembly output |
| `-d, --debug` | Debug information |
| `-I, --include <DIR>` | Add search path |
| `--no-cache` | Disable caching |

---

## 💡 Examples

### Example 1: Variables and Arithmetic

```pascal
program Arithmetic;
var
  a, b, c, d: integer;
  result: real;
begin
  a := 10;
  b := 5;
  c := 3;
  d := 2;

  // Complex expression: 10 + 5 * 3 - 2 / 2
  result := a + b * c - d div d;

  writeln('Result: ', result);  // Output: 24.0
end.
```

### Example 2: Control Structures

```pascal
program ControlStructures;
var
  i, sum, product: integer;
  flag: boolean;
begin
  sum := 0;
  product := 1;
  flag := false;

  // For loop with sum of squares
  for i := 1 to 5 do
  begin
    sum := sum + i * i;
    if (i mod 2) = 0 then
      flag := true;
  end;

  // While loop
  i := 10;
  while i > 0 do
  begin
    product := product * (i mod 3 + 1);
    i := i - 1;
  end;

  writeln('Sum: ', sum);
  writeln('Product: ', product);
  writeln('Flag: ', flag);
end.
```

### Example 3: Arrays and Records

```pascal
program DataStructures;
type
  Person = record
    name: string;
    age: integer;
    salary: real;
  end;

var
  people: array[1..3] of Person;
  i: integer;
begin
  // Initialize records
  people[1].name := 'Alice';
  people[1].age := 30;
  people[1].salary := 50000.00;

  people[2].name := 'Bob';
  people[2].age := 25;
  people[2].salary := 45000.00;

  people[3].name := 'Charlie';
  people[3].age := 35;
  people[3].salary := 60000.00;

  // Display information
  for i := 1 to 3 do
  begin
    writeln('Name: ', people[i].name);
    writeln('Age: ', people[i].age);
    writeln('Salary: ', people[i].salary);
    writeln('---');
  end;
end.
```

### Example 4: Boolean Logic

```pascal
program BooleanLogic;
var
  a, b, c, d, result: boolean;
begin
  a := true;
  b := false;
  c := true;
  d := false;

  // Complex boolean expressions
  result := (a and c) or (not b and not d);
  writeln('(true AND true) OR (NOT false AND NOT false) = ', result);

  result := (a and b) or (c and d);
  writeln('(true AND false) OR (true AND false) = ', result);

  result := not ((a and b) or (c and d));
  writeln('NOT ((true AND false) OR (true AND false)) = ', result);
end.
```

### Example 5: Functions and Procedures

```pascal
program Functions;

function Factorial(n: integer): integer;
begin
  if n <= 1 then
    Factorial := 1
  else
    Factorial := n * Factorial(n - 1);
end;

procedure PrintTriangle(rows: integer);
var
  i, j: integer;
begin
  for i := 1 to rows do
  begin
    for j := 1 to i do
      write('*');
    writeln;
  end;
end;

var
  n: integer;
begin
  n := 5;
  writeln('Factorial of ', n, ' is ', Factorial(n));
  writeln;
  writeln('Triangle:');
  PrintTriangle(5);
end.
```

### Example 6: Real-World Calculator

```pascal
program Calculator;
var
  num1, num2, result: real;
  operation: char;
begin
  writeln('=== Simple Calculator ===');
  writeln('Enter first number: ');
  readln(num1);
  writeln('Enter operation (+, -, *, /): ');
  readln(operation);
  writeln('Enter second number: ');
  readln(num2);

  case operation of
    '+': result := num1 + num2;
    '-': result := num1 - num2;
    '*': result := num1 * num2;
    '/': result := num1 / num2;
  else
    writeln('Invalid operation!');
    exit;
  end;

  writeln('Result: ', result);
end.
```

### Example 7: Unit System

**Create a unit** (`MathUtils.pas`):
```pascal
unit MathUtils;

interface

function Add(a, b: integer): integer;
function Multiply(a, b: integer): integer;
function IsEven(n: integer): boolean;

implementation

function Add(a, b: integer): integer;
begin
  Add := a + b;
end;

function Multiply(a, b: integer): integer;
begin
  Multiply := a * b;
end;

function IsEven(n: integer): boolean;
begin
  IsEven := (n mod 2) = 0;
end;

end.
```

**Compile the unit**:
```bash
pascal compile MathUtils.pas -v
```

**Use the unit** (`Calculator.pas`):
```pascal
program Calculator;

uses MathUtils;

var
  x, y: integer;
begin
  x := 10;
  y := 5;

  writeln('Addition: ', Add(x, y));
  writeln('Multiplication: ', Multiply(x, y));
  writeln('Is Even (10): ', IsEven(x));
  writeln('Is Even (5): ', IsEven(y));
end.
```

---

## 🏗️ Latest Capabilities

### ✅ **Trait-Based Architecture**

Our modern trait-based design provides:

#### Core Traits
- **`TryAs<T>`** - Type-safe conversions with error handling
- **`FormattedDisplay`** - Configurable output formatting
- **`ScopeOperations`** - Variable storage abstraction
- **`FunctionRegistry`** - Function management interface
- **`StatementExecutor`** - Pluggable execution strategies
- **`ExpressionEvaluator`** - Custom evaluation strategies

#### Benefits
```rust
// Before: Tightly coupled code
fn execute(interpreter: &mut Interpreter) { ... }

// After: Trait-based, testable code
fn execute<T: ScopeOperations>(scope: &mut T) { ... }
```

**Benefits:**
- ✅ Easy mocking for testing
- ✅ Multiple implementations possible
- ✅ Clear separation of concerns
- ✅ Better maintainability

See [`TRAIT_ARCHITECTURE.md`](TRAIT_ARCHITECTURE.md) for details.

### ✅ **Comprehensive Testing** (169 Tests)

```
Library tests:        87 ✅
Basic tests:          1 ✅
Compiler tests:      10 ✅
Complex validation:   9 ✅ NEW!
Integration tests:   10 ✅
Interpreter tests:   11 ✅
Simple compiler:     18 ✅
Simple interpreter:  13 ✅
Type checker:        10 ✅
────────────────────────────
Total:             169 ✅
```

#### Test Categories

**1. Complex Validation Tests** (NEW!)
- ✅ Complex arithmetic with operator precedence
- ✅ Nested control structures (5 levels deep)
- ✅ Character operations and comparisons
- ✅ Complex boolean expressions (AND, OR, NOT)
- ✅ Real number arithmetic
- ✅ Boundary conditions (zero, positive, negative)
- ✅ Comprehensive programs combining all features

**2. Integration Tests**
- ✅ Hello World
- ✅ Factorial calculation
- ✅ Fibonacci sequence
- ✅ Prime number generation
- ✅ Nested loops
- ✅ Record operations
- ✅ Array operations
- ✅ Case statements
- ✅ Repeat until loops
- ✅ String operations

**3. Compiler Tests**
- ✅ Simple program compilation
- ✅ Arithmetic operations
- ✅ Control structures
- ✅ Variable declarations
- ✅ Type declarations
- ✅ Arrays and records
- ✅ Case statements
- ✅ Repeat until loops
- ✅ String operations

See [`COMPLEX_VALIDATION_SUMMARY.md`](COMPLEX_VALIDATION_SUMMARY.md) for full details.

### ✅ **Advanced Interpreter Features**

**Built-in Functions** (20+):
- `abs`, `sqr`, `sqrt`, `sin`, `cos`, `ln`, `exp`
- `round`, `trunc`, `ord`, `chr`
- `length`, `pos`, `concat`, `copy`, `upcase`, `lowercase`
- `odd`, `succ`, `pred`, `random`
- `inttostr`, `strtoint`

**Built-in Procedures**:
- `write`, `writeln`, `readln`
- `inc`, `dec`, `halt`

**Control Flow**:
- ✅ if/else statements
- ✅ while loops
- ✅ for loops (to/downto)
- ✅ repeat until loops
- ✅ case statements
- ✅ begin/end blocks

### ✅ **Code Generation**

**x86-64 Assembly** (Intel syntax):
- Register allocation with graph coloring
- Stack-based variable management
- Proper function prologue/epilogue
- Multiple calling conventions (System V, Win64)

**Optimizations**:
- Constant folding
- Dead code elimination
- Common subexpression elimination
- Function inlining
- Loop unrolling
- Peephole optimization

### ✅ **Language Support**

**Data Types:**
- ✅ Integer (signed 64-bit)
- ✅ Real (IEEE 754 double precision)
- ✅ Boolean
- ✅ Char
- ✅ String
- ✅ Arrays (multi-dimensional)
- ✅ Records
- ✅ Pointers
- ✅ Enumerations
- ✅ Sets

**Operators:**
- ✅ Arithmetic: `+`, `-`, `*`, `/`, `div`, `mod`
- ✅ Comparison: `=`, `<>`, `<`, `<=`, `>`, `>=`
- ✅ Logical: `and`, `or`, `not`
- ✅ Bitwise: `&`, `|`, `xor`, `<<`, `>>`
- ✅ Unary: `+`, `-`, `not`

**Control Structures:**
- ✅ if/else
- ✅ while/do
- ✅ for/to/downto
- ✅ repeat/until
- ✅ case/of
- ✅ begin/end blocks

### ⚠️ **Current Limitations**

While pascal-rs successfully compiles and interprets core Pascal programs, there are features not yet implemented:

**Not Yet Implemented:**
- ❌ Object Pascal (classes, inheritance, polymorphism)
- ❌ Exception handling (try-except-finally)
- ❌ Generics/templates
- ❌ File I/O operations
- ❌ Dynamic arrays
- ❌ Variant records
- ❌ Operator overloading
- ❌ Properties
- ❌ Interfaces
- ❌ Advanced standard library (only basic built-ins)

**Platform Support:**
- ✅ x86-64 (primary target, well-tested)
- 🚧 ARM, RISC-V, MIPS, PowerPC (planned)
- 🚧 WebAssembly (planned)

**For these features, consider using Free Pascal or Delphi.**

---

## 🧪 Testing

### Running Tests

```bash
# Run all tests
cargo test

# Run specific test categories
cargo test --lib                          # Library tests (87)
cargo test --test run_compiler_tests      # Compiler tests (10)
cargo test --test run_integration_tests   # Integration tests (10)
cargo test --test run_complex_validation_tests  # Complex validation (9)
cargo test --test run_interpreter_tests   # Interpreter tests (11)
cargo test --test run_type_checker_tests  # Type checker tests (10)

# Run with output
cargo test -- --nocapture  # Show test output
cargo test -- --verbose    # Verbose test output
```

### Test Results

```
running 169 tests
test result: ok. 169 passed; 0 failed; 0 ignored; 0 measured
```

**All tests passing!** ✅

### Test Coverage

- **Lexical Analysis**: Complete token coverage
- **Parsing**: All AST node types
- **Type Checking**: All type combinations
- **Code Generation**: All statement types
- **Interpreter**: All language features
- **Edge Cases**: Boundary conditions validated

---

## 📚 Architecture

### Modular Design

```
pascal-rs/
├── src/
│   ├── lexer.rs              # Lexical analysis
│   ├── parser/               # Syntax analysis
│   │   ├── mod.rs
│   │   ├── expression.rs
│   │   ├── statement.rs
│   │   └── decl.rs
│   ├── interpreter.rs        # Tree-walking interpreter
│   ├── interpreter_value.rs  # Value type
│   ├── interpreter_scope.rs  # Scope management
│   ├── interpreter_function.rs # Function registry
│   ├── interpreter_traits.rs # Trait abstractions
│   ├── type_checker.rs       # Type validation
│   ├── optimizer.rs          # Optimization passes
│   ├── unit_codegen.rs       # Code generation
│   └── resolver.rs           # Symbol resolution
├── tests/
│   ├── run_compiler_tests.rs         # Compiler validation
│   ├── run_integration_tests.rs      # End-to-end tests
│   ├── run_complex_validation_tests.rs # Complex scenarios (NEW!)
│   └── integration/
│       └── complex_validation_tests.rs
└── examples/                      # Example programs
```

### Trait-Based Components

```rust
// Value operations
impl TryAs<i64> for Value
impl TryAs<f64> for Value
impl TryAs<bool> for Value
impl FormattedDisplay for Value

// Scope management
impl ScopeOperations for Scope
impl ScopeOperations for ScopeStack

// Function registry
impl FunctionRegistry for FunctionRegistryImpl
impl FunctionRegistry for HashMap<String, UserFunction>
```

### Compilation Pipeline

```
Source Code → Lexer → Tokens → Parser → AST
                                           ↓
                                    Type Checker
                                           ↓
                                    Optimizer
                                           ↓
                                  Code Generator
                                           ↓
                                     Assembly
```

---

## 📖 Documentation

- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Detailed architecture and design
- **[TRAIT_ARCHITECTURE.md](TRAIT_ARCHITECTURE.md)** - Trait-based design guide
- **[COMPLEX_VALIDATION_SUMMARY.md](COMPLEX_VALIDATION_SUMMARY.md)** - Test validation report
- **[TESTING.md](TESTING.md)** - Testing guide and best practices
- **[TEST_STATUS.md](TEST_STATUS.md)** - Test status and coverage
- **[TODO.md](TODO.md)** - Development roadmap

---

## 🆕 What's New

### Latest Release Highlights

**✅ Trait-Based Architecture** (NEW!)
- Added comprehensive trait abstractions
- Improved testability and maintainability
- Better separation of concerns
- See [`TRAIT_ARCHITECTURE.md`](TRAIT_ARCHITECTURE.md)

**✅ Complex Validation Tests** (NEW!)
- 9 new comprehensive tests
- Edge cases and boundary conditions
- Complex operator precedence
- Nested control structures
- 100% pass rate

**✅ Enhanced Testing Support**
- Added `current_scope()` method for testing
- Added `get_variable_value()` for value inspection
- Made `Scope` struct public with `get()` method

**✅ Improved Documentation**
- Trait architecture guide
- Test validation summary
- Comprehensive examples
- Better inline documentation

---

## 🎯 Use Cases

### 1. **Education**
- Learn compiler construction with clean, modern code
- Understand trait-based design patterns
- Study optimization techniques
- Explore language implementation

### 2. **Research & Experimentation** ✅ Perfect Fit
- Test new optimization algorithms
- Experiment with trait-based architectures
- Benchmark compilation strategies
- Prototype language features
- Study type systems
- Research memory-safe compiler design

### 3. **Small Projects** ✅ Suitable
- Command-line utilities
- Scripts and automation
- Learning exercises
- Algorithm implementation
- Data processing tasks
- Homework assignments

### 4. **Legacy Modernization** ⚠️ Consider Limitations
- Works for standard Pascal code
- Missing Object Pascal features (classes, exceptions)
- Good for simple procedural Pascal programs
- May need modifications for advanced features

### Not Recommended For (Yet):
- ❌ Large commercial applications (use Free Pascal or Delphi)
- ❌ Complex GUI applications (no framework support yet)
- ❌ Database-heavy applications (limited database library support)
- ❌ Embedded systems (limited platform support)
- ❌ Projects requiring Object Pascal classes/exceptions

---

## 🙏 Acknowledgments

- Inspired by the Free Pascal Compiler (FPC)
- Built with Rust and Cargo
- Uses modern compiler construction techniques
- Benefits from the Rust community's expertise
