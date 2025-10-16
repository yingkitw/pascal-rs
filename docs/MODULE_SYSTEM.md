# Module System Documentation

## Overview

The minipas module system implements Pascal's unit system, providing support for modular programming with interface/implementation separation and dependency management.

## Status

✅ **Implemented** (as of 2025-10-16):
- Module data structures and management
- Dependency tracking and resolution
- Parser integration for unit/interface/implementation
- Uses clause parsing
- Symbol resolution framework
- Comprehensive test coverage (41 tests passing)

## Architecture

### Core Components

The module system is implemented in the `minipas-module` crate with the following components:

1. **Module** - Represents a compiled Pascal unit
2. **ModuleManager** - Manages collections of modules and their dependencies
3. **ModuleLoader** - Loads units from the filesystem with caching
4. **ModuleResolver** - Resolves symbols across module boundaries

### Parser Integration

The `minipas-parser` crate now includes:
- `parse_unit()` - Parses complete Pascal unit files
- `parse_interface_section()` - Parses interface declarations
- `parse_implementation_section()` - Parses implementation code
- Uses clause parsing in both interface and implementation sections

### Module Structure

A Pascal unit consists of:

```pascal
unit MyUnit;

interface
  uses System, SysUtils;  // Interface dependencies
  
  type
    TMyType = record
      field: Integer;
    end;
  
  procedure DoSomething;
  function Calculate(x: Integer): Integer;

implementation
  uses InternalUnit;  // Implementation dependencies
  
  procedure DoSomething;
  begin
    // implementation
  end;
  
  function Calculate(x: Integer): Integer;
  begin
    Result := x * 2;
  end;

initialization
  // Optional initialization code

finalization
  // Optional finalization code

end.
```

## Features

### Dependency Management

The module system tracks two types of dependencies:

- **Interface dependencies** (`uses` in interface section) - Public dependencies
- **Implementation dependencies** (`uses` in implementation section) - Private dependencies

### Compilation Order

The `ModuleManager` computes the correct compilation order using topological sort:

```rust
let mut manager = ModuleManager::new();
// Register modules...
let compile_order = manager.compute_compile_order()?;
```

### Circular Dependency Detection

The system automatically detects circular dependencies:

```rust
match manager.check_circular_dependencies() {
    Ok(()) => println!("No circular dependencies"),
    Err(ModuleError::CircularDependency(module)) => {
        eprintln!("Circular dependency involving: {}", module);
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

### Module Loading

The `ModuleLoader` provides filesystem-based module loading with caching:

```rust
let mut loader = ModuleLoader::new();
loader.add_search_path("/usr/lib/pascal");
loader.add_search_path("./lib");

let (path, source) = loader.load_unit_source("MyUnit")?;
```

### Symbol Resolution

The `ModuleResolver` resolves symbols across module boundaries:

```rust
let mut resolver = ModuleResolver::new();
resolver.build_symbol_table(&module)?;

let symbol = resolver.resolve_symbol(
    "MyType",
    "CurrentModule",
    &["System", "SysUtils"]
)?;
```

## Usage Example

```rust
use minipas_module::{Module, ModuleManager, ModuleLoader};
use minipas_ast::Unit;

// Create module manager
let mut manager = ModuleManager::new();

// Load and register modules
let mut loader = ModuleLoader::new();
loader.add_search_path("./units");

for unit_name in &["System", "SysUtils", "MyApp"] {
    let (path, source) = loader.load_unit_source(unit_name)?;
    // Parse source to get Unit AST (using parser)
    let unit: Unit = parse_unit(&source)?;
    let module = Module::new(unit_name.to_string(), path, unit);
    manager.register_module(module)?;
}

// Compute compilation order
let order = manager.compute_compile_order()?;
println!("Compilation order: {:?}", order);

// Compile in order
for module_name in order {
    if manager.needs_recompilation(&module_name) {
        println!("Compiling: {}", module_name);
        // Compile module...
    }
}
```

## Implementation Details

### Module CRC Checksums

Each module maintains CRC checksums for:
- Interface section (for detecting interface changes)
- Implementation section (for detecting implementation changes)

This allows efficient incremental compilation.

### Module Search Paths

The loader searches for units in multiple directories:
1. Current directory (`.`)
2. User-specified search paths
3. System library paths (future)

### Symbol Visibility

Symbols declared in the interface section are public and can be used by other modules. Symbols in the implementation section are private to the module.

## Future Enhancements

### PPU File Format

Implement compiled unit files (PPU - Pascal Precompiled Unit) for faster loading:

```rust
pub struct PPUFile {
    header: PPUHeader,
    interface_symbols: SymbolTable,
    dependencies: Vec<String>,
    crc: u32,
}
```

### Incremental Compilation

Track file modification times and CRC checksums to avoid unnecessary recompilation:

```rust
if !manager.needs_recompilation("MyUnit") {
    println!("MyUnit is up to date");
    continue;
}
```

### Package System

Support for Pascal packages (collections of units):

```pascal
package MyPackage;

requires
  rtl,
  vcl;

contains
  Unit1,
  Unit2,
  Unit3;

end.
```

## Error Handling

The module system provides comprehensive error types:

```rust
pub enum ModuleError {
    ModuleNotFound(String),
    DuplicateModule(String),
    CircularDependency(String),
    LoadError(String, String),
    CompileError(String, String),
    CrcMismatch(String),
    NotCompiled(String),
    InvalidModuleName(String),
    UnresolvedDependencies(String, Vec<String>),
    IoError(String),
    ParseError(String, String),
}
```

## Testing

The module system includes comprehensive unit tests:

```bash
cargo test -p minipas-module
```

Test coverage includes:
- Module creation and dependency tracking
- Compilation order computation
- Circular dependency detection
- Module loading and caching
- Symbol resolution

## Integration with Parser

The parser will be extended to support unit parsing:

```rust
pub fn parse_unit(source: &str) -> Result<Unit, ParseError> {
    // Parse interface section
    // Parse implementation section
    // Parse initialization/finalization
}
```

## References

- [FPC Module System](https://www.freepascal.org/docs-html/prog/progsu1.html)
- [Pascal Units](https://www.freepascal.org/docs-html/ref/refsu2.html)
- [Module Dependencies](https://www.freepascal.org/docs-html/prog/progsu2.html)
