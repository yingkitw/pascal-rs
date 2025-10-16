# MiniPAS User Guide

Welcome to MiniPAS, a modern Pascal compiler written in Rust!

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Writing Pascal Units](#writing-pascal-units)
4. [Compilation](#compilation)
5. [PPU Files](#ppu-files)
6. [Advanced Usage](#advanced-usage)
7. [Troubleshooting](#troubleshooting)

## Installation

### Prerequisites

- Rust 1.70 or later
- Cargo (comes with Rust)

### Building from Source

```bash
# Clone the repository
git clone https://github.com/yourusername/minipas.git
cd minipas

# Build the project
cargo build --release

# Install the compiler
cargo install --path crates/minipas-cli

# Verify installation
minipas --version
```

## Quick Start

### Your First Program

Create a file `hello.pas`:

```pascal
program Hello;
begin
  writeln('Hello, World!');
end.
```

Compile it:

```bash
minipas compile hello.pas
```

### Your First Unit

Create a file `mathutils.pas`:

```pascal
unit MathUtils;

interface

function Add(a, b: Integer): Integer;
function Multiply(a, b: Integer): Integer;

implementation

function Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function Multiply(a, b: Integer): Integer;
begin
  Result := a * b;
end;

end.
```

Compile it:

```bash
minipas compile mathutils.pas -v
```

Output:
```
Compiling: mathutils.pas
Success: Compiled module: MathUtils
  PPU file: mathutils.ppu
```

## Writing Pascal Units

### Unit Structure

A Pascal unit consists of:

1. **Unit Declaration**: `unit UnitName;`
2. **Interface Section**: Public declarations
3. **Implementation Section**: Private implementations
4. **Initialization** (optional): Runs when unit is loaded
5. **Finalization** (optional): Runs when program exits

Example:

```pascal
unit MyUnit;

interface

uses System, SysUtils;  // Interface dependencies

type
  TMyType = Integer;

const
  MyConst = 42;

var
  MyVar: Integer;

function MyFunction(x: Integer): Integer;

implementation

uses Classes;  // Implementation-only dependencies

function MyFunction(x: Integer): Integer;
begin
  Result := x * 2;
end;

initialization
  MyVar := MyConst;

finalization
  MyVar := 0;

end.
```

### Using Units

To use a unit in your program:

```pascal
program MyProgram;

uses MathUtils;

var
  result: Integer;
begin
  result := Add(10, 20);
  writeln('Result: ', result);
end.
```

## Compilation

### Basic Compilation

```bash
# Compile a unit
minipas compile MyUnit.pas

# Compile a program
minipas compile MyProgram.pas
```

### Compilation Options

#### Output Directory

```bash
minipas compile MyUnit.pas -o ./build
```

#### Search Paths

Add directories to search for units:

```bash
minipas compile MyProgram.pas -I /usr/lib/pascal -I ./lib
```

#### Optimization

Set optimization level (0-3):

```bash
# No optimization
minipas compile MyUnit.pas -O0

# Maximum optimization
minipas compile MyUnit.pas -O3
```

#### Debug Information

Include debug information:

```bash
minipas compile MyUnit.pas -d
```

#### Verbose Output

Show detailed compilation steps:

```bash
minipas compile MyUnit.pas -v
```

Output:
```
Compiling: MyUnit.pas
Configuration:
  Output directory: .
  Optimization: O0
  Debug info: false
  Generate PPU: true
  Use PPU cache: true
  Search paths: 1
Parsing and compiling...
Success: Compiled module: MyUnit
  PPU file: myunit.ppu
Compilation order:
  1. System
  2. MyUnit
```

### Disabling PPU

#### Don't Generate PPU Files

```bash
minipas compile MyUnit.pas --no-ppu
```

#### Don't Use PPU Cache

Force recompilation from source:

```bash
minipas compile MyUnit.pas --no-cache
```

### Complete Example

```bash
minipas compile MyProgram.pas \
  -o ./build \
  -I /usr/lib/pascal \
  -I ./lib \
  -O2 \
  -d \
  -v
```

## PPU Files

### What are PPU Files?

PPU (Precompiled Pascal Unit) files are binary files that store compiled unit information. They enable:

- **Fast Compilation**: Skip parsing for unchanged units
- **Incremental Builds**: Only recompile modified units
- **Dependency Caching**: Store resolved dependencies

### PPU File Format

- **Magic Number**: "MPU\0" (MiniPas Unit)
- **Version**: Format version number
- **Checksums**: Interface, implementation, and unit CRCs
- **Data**: Serialized AST using bincode

### Inspecting PPU Files

View information about a PPU file:

```bash
minipas info myunit.ppu
```

Output:
```
PPU File: myunit.ppu
Header:
  Version: 1
  Interface CRC: 0x12345678
  Implementation CRC: 0x87654321
  Unit CRC: 0xabcdef00
  Data size: 1024 bytes

Unit:
  Name: MyUnit
  Uses: ["System"]

Interface:
  Types: 2
  Constants: 5
  Variables: 3
  Functions: 10
  Procedures: 8

Implementation:
  Uses: []
  Types: 1
  Constants: 2
  Variables: 1
  Functions: 10
  Procedures: 8

✓ Checksums verified
```

### Managing PPU Files

#### Clean PPU Files

Remove all PPU files from a directory:

```bash
# Clean current directory
minipas clean

# Clean specific directory
minipas clean ./build
```

Output:
```
Cleaning: ./build
  Removed: ./build/myunit.ppu
  Removed: ./build/mathutils.ppu
Done: Removed 2 PPU file(s)
```

## Advanced Usage

### Project Structure

Organize your Pascal project:

```
myproject/
├── src/
│   ├── units/
│   │   ├── MathUtils.pas
│   │   └── StringUtils.pas
│   └── MyProgram.pas
├── lib/
│   └── external units...
└── build/
    └── compiled files...
```

Compile with:

```bash
minipas compile src/MyProgram.pas \
  -o build \
  -I src/units \
  -I lib
```

### Build Scripts

Create a build script `build.sh`:

```bash
#!/bin/bash

# Set options
OUTPUT_DIR="./build"
SEARCH_PATHS="-I ./src/units -I ./lib"
OPTIMIZATION="-O2"
DEBUG="-d"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Compile units
echo "Compiling units..."
minipas compile src/units/MathUtils.pas -o "$OUTPUT_DIR" $SEARCH_PATHS $OPTIMIZATION $DEBUG
minipas compile src/units/StringUtils.pas -o "$OUTPUT_DIR" $SEARCH_PATHS $OPTIMIZATION $DEBUG

# Compile program
echo "Compiling program..."
minipas compile src/MyProgram.pas -o "$OUTPUT_DIR" $SEARCH_PATHS $OPTIMIZATION $DEBUG

echo "Build complete!"
```

### Makefile Example

```makefile
.PHONY: all clean

OUTPUT_DIR := build
SEARCH_PATHS := -I src/units -I lib
OPTIONS := -O2 -d

all: $(OUTPUT_DIR)/myprogram

$(OUTPUT_DIR)/myprogram: src/MyProgram.pas src/units/*.pas
	@mkdir -p $(OUTPUT_DIR)
	minipas compile $< -o $(OUTPUT_DIR) $(SEARCH_PATHS) $(OPTIONS) -v

clean:
	minipas clean $(OUTPUT_DIR)
	rm -rf $(OUTPUT_DIR)
```

## Troubleshooting

### Common Issues

#### Unit Not Found

**Error**: `Unit not found: MyUnit`

**Solution**: Add the unit's directory to search paths:
```bash
minipas compile MyProgram.pas -I ./path/to/units
```

#### Circular Dependency

**Error**: `Circular dependency detected: UnitA`

**Solution**: Restructure your units to remove circular dependencies. Move shared code to a separate unit.

#### PPU Checksum Failed

**Error**: `PPU checksum verification failed`

**Solution**: The PPU file is corrupted. Delete it and recompile:
```bash
rm myunit.ppu
minipas compile MyUnit.pas
```

#### File Not Found

**Error**: `File not found: MyUnit.pas`

**Solution**: Check the file path and ensure the file exists:
```bash
ls -la MyUnit.pas
```

### Getting Help

```bash
# General help
minipas --help

# Command-specific help
minipas compile --help
minipas info --help
minipas clean --help
```

### Verbose Mode

Use verbose mode to see detailed compilation steps:

```bash
minipas compile MyUnit.pas -v
```

This shows:
- Configuration options
- Files being compiled
- Dependency resolution
- Compilation order
- Generated files

## Next Steps

- Read the [API Documentation](API.md) for library usage
- Check the [examples/](../examples/) directory for more examples
- See [ARCHITECTURE.md](../ARCHITECTURE.md) for compiler internals
- Visit the [GitHub repository](https://github.com/yourusername/minipas) for updates

## Support

- **Issues**: Report bugs on GitHub
- **Discussions**: Ask questions in GitHub Discussions
- **Documentation**: Check the docs/ directory

Happy coding with MiniPAS! 🎉
