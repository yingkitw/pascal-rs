# MiniPAS API Documentation

This document provides an overview of the MiniPAS compiler API and its crates.

## Crates

### Core Compilation Pipeline

#### `minipas-driver` - Compilation Driver
The main compilation orchestrator that manages the entire compilation process.

**Key Types:**
- `Compiler` - Main compiler struct
- `CompileOptions` - Configuration for compilation
- `CompileResult` - Result of compilation
- `CompilerError` - Error types

**Example:**
```rust
use minipas_driver::{Compiler, CompileOptions};

let options = CompileOptions::default();
let mut compiler = Compiler::new(options);
let result = compiler.compile_file("MyUnit.pas")?;
```

#### `minipas-module` - Module System
Implements Pascal's unit system with PPU file support.

**Key Types:**
- `Module` - Represents a compiled unit
- `ModuleManager` - Manages module dependencies
- `ModuleLoader` - Loads units from filesystem
- `PpuFile` - Precompiled unit file format

**Example:**
```rust
use minipas_module::{ModuleLoader, PpuFile};

let mut loader = ModuleLoader::new();
let unit = loader.load_from_ppu("MyUnit")?;
```

#### `minipas-parser` - Parser
Parses Pascal source code into an Abstract Syntax Tree.

**Key Types:**
- `Parser` - Main parser struct
- `ParseError` - Parse error types

**Example:**
```rust
use minipas_parser::Parser;

let source = "unit MyUnit; interface implementation end.";
let mut parser = Parser::new(source);
let unit = parser.parse_unit()?;
```

#### `minipas-ast` - Abstract Syntax Tree
Defines all AST node types for Pascal programs.

**Key Types:**
- `Unit` - Pascal unit
- `Program` - Pascal program
- `Expr` - Expressions
- `Stmt` - Statements
- `Type` - Type definitions

#### `minipas-lexer` - Lexer
Tokenizes Pascal source code.

**Key Types:**
- `Lexer` - Main lexer struct
- `Token` - Token types

**Example:**
```rust
use minipas_lexer::Lexer;

let source = "program Hello; begin end.";
let mut lexer = Lexer::new(source);
for token in lexer {
    println!("{:?}", token);
}
```

#### `minipas-codegen` - Code Generator
Generates target code from AST.

**Key Types:**
- `CodeGenerator` - Main code generator

### Command-Line Interface

#### `minipas-cli` - CLI Application
User-facing command-line interface.

**Commands:**
- `compile` - Compile Pascal source files
- `info` - Show PPU file information
- `clean` - Remove build artifacts

## Documentation Generation

Generate the full API documentation:

```bash
# Generate documentation for all crates
cargo doc --no-deps --workspace --open

# Generate documentation for a specific crate
cargo doc --no-deps -p minipas-driver --open
```

## Module Relationships

```
minipas-cli
    ↓
minipas-driver
    ↓
    ├── minipas-parser → minipas-ast
    │       ↓
    │   minipas-lexer
    │
    └── minipas-module → minipas-ast
            ↓
        minipas-codegen
```

## Key Concepts

### Compilation Flow

1. **Lexical Analysis** (`minipas-lexer`)
   - Tokenize source code
   - Handle comments and whitespace

2. **Parsing** (`minipas-parser`)
   - Build Abstract Syntax Tree
   - Validate syntax

3. **Module Management** (`minipas-module`)
   - Resolve dependencies
   - Load/save PPU files
   - Track compilation order

4. **Compilation** (`minipas-driver`)
   - Orchestrate compilation
   - Handle errors
   - Generate output

5. **Code Generation** (`minipas-codegen`)
   - Generate target code
   - Optimize output

### PPU Files

Precompiled Unit (PPU) files are binary files that store compiled unit information:

- **Format**: Binary serialization using bincode
- **Contents**: Complete AST with checksums
- **Purpose**: Fast incremental compilation
- **Location**: Same directory as source or output directory

### Error Handling

All crates use Result types for error handling:

```rust
pub type Result<T> = std::result::Result<T, Error>;
```

Errors include:
- Parse errors with source location
- Module errors (not found, circular dependencies)
- IO errors
- Compilation errors

## Testing

Run tests for all crates:

```bash
cargo test --workspace
```

Run tests for a specific crate:

```bash
cargo test -p minipas-driver
```

## Contributing

When adding new features:

1. Add rustdoc comments to public APIs
2. Include code examples in documentation
3. Add unit tests
4. Update this API documentation
5. Run `cargo doc` to verify documentation builds

## License

See the LICENSE file in the project root.
