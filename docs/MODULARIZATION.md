# Code Modularization Plan - pascal-rs

## Overview

The pascal-rs codebase has several large files that would benefit from modularization. This document outlines the modularization plan and provides starter modules for the largest files.

## Files Analyzed

| File | Lines | Priority | Complexity |
|------|-------|----------|------------|
| src/interpreter.rs | 1084 | HIGH | High |
| src/enhanced_tokens.rs | 937 | MEDIUM | Medium |
| src/unit_codegen.rs | 707 | MEDIUM | High |
| src/simd.rs | 529 | LOW | Medium |
| src/ast.rs | 504 | LOW | Low |
| src/parallel.rs | 496 | LOW | Medium |

## 1. Interpreter Modularization (HIGH PRIORITY)

### Current Structure (1084 lines)

```
interpreter.rs
├── Value enum & conversions (60 lines)
├── Scope management (15 lines)
├── UserFunction struct (10 lines)
├── Interpreter main methods (40 lines)
├── Variable declaration (20 lines)
├── Function registration (40 lines)
├── Statement execution (270 lines)
├── Expression evaluation (280 lines)
└── Built-in functions (350 lines)
```

### Proposed Module Structure

```
src/interpreter/
├── mod.rs              # Main Interpreter struct & coordination
├── value.rs            # Value enum and conversions ✅ CREATED
├── scope.rs            # Scope management ✅ CREATED
└── function.rs         # UserFunction and registration ✅ CREATED
```

### Modules Created

#### ✅ src/interpreter/value.rs (90 lines)
- `Value` enum with all variants
- Conversion methods (`as_integer`, `as_real`, `as_boolean`)
- Display implementation
- Helper constructors

#### ✅ src/interpreter/scope.rs (55 lines)
- `Scope` struct with variable storage
- Get/set methods
- Variable lookup utilities

#### ✅ src/interpreter/function.rs (60 lines)
- `UserFunction` struct
- Parameter management
- Function vs procedure distinction

### Remaining Work

To complete the modularization:

1. **Create stmt_executor.rs** (~300 lines)
   - Extract all statement execution logic
   - Methods: `execute_stmt`, `execute_assignment`, `execute_if`, etc.
   - Helper methods for control flow

2. **Create expr_evaluator.rs** (~350 lines)
   - Extract all expression evaluation logic
   - Methods: `eval_expr`, `eval_binary_op`, `eval_unary_op`
   - Function call evaluation

3. **Create builtins.rs** (~400 lines)
   - Extract all built-in functions
   - Math functions (abs, sqrt, sin, cos, etc.)
   - String functions (length, copy, concat, etc.)
   - I/O functions (writeln, readln)

4. **Update mod.rs** (~150 lines)
   - Re-export all modules
   - Main `Interpreter` struct
   - Coordination methods

### Benefits

- ✅ Separation of concerns
- ✅ Easier to test individual components
- ✅ Better code organization
- ✅ Easier to add new features

## 2. Unit Codegen Modularization (MEDIUM PRIORITY)

### Current Structure (707 lines)

```
unit_codegen.rs
├── Struct definitions (50 lines)
├── Expression generation (200 lines)
├── Statement generation (250 lines)
├── Function generation (150 lines)
└── Utility methods (57 lines)
```

### Proposed Module Structure

```
src/codegen/
├── mod.rs              # Main codegen coordination
├── expr_gen.rs         # Expression code generation
├── stmt_gen.rs         # Statement code generation
├── function_gen.rs     # Function/procedure code generation
└── types.rs            # Codegen types & utilities
```

## 3. Enhanced Tokens Modularization (MEDIUM PRIORITY)

### Current Structure (937 lines)

```
enhanced_tokens.rs
├── Token definitions (400 lines)
├── Keyword mappings (200 lines)
├── Operator definitions (150 lines)
├── Utility functions (187 lines)
```

### Proposed Module Structure

```
src/tokens/
├── mod.rs              # Token re-exports
├── types.rs            # Token enum and definitions
├── keywords.rs         # Keyword constants and mappings
├── operators.rs        # Operator definitions
└── utils.rs            # Token utility functions
```

## Implementation Strategy

### Phase 1: Foundation (COMPLETED)
- ✅ Create module directories
- ✅ Create basic value, scope, function modules
- ✅ Document the plan

### Phase 2: Core Modules (PENDING)
- [ ] Create stmt_executor.rs
- [ ] Create expr_evaluator.rs
- [ ] Create builtins.rs
- [ ] Update mod.rs with re-exports

### Phase 3: Integration (PENDING)
- [ ] Update lib.rs imports
- [ ] Update tests for new module structure
- [ ] Verify all tests pass

### Phase 4: Additional Files (PENDING)
- [ ] Modularize unit_codegen.rs
- [ ] Modularize enhanced_tokens.rs
- [ ] Consider simd.rs modularization

## Benefits of Modularization

### Code Quality
- **Separation of Concerns**: Each module has a single, well-defined responsibility
- **Improved Readability**: Smaller files are easier to understand
- **Better Testing**: Individual components can be tested in isolation

### Maintainability
- **Easier Updates**: Changes to one area don't require reading entire file
- **Reduced Merge Conflicts**: Smaller files mean fewer conflicts
- **Clearer Ownership**: Each module has clear boundaries

### Performance
- **Faster Compilation**: Smaller files compile faster
- **Better Cache Utilization**: Related code is grouped together
- **Parallel Builds**: More files = better parallelization

## Module Design Principles

### 1. Single Responsibility
Each module should have one clear purpose:
- `value.rs` → Value representation only
- `scope.rs` → Variable scoping only
- `function.rs` → User functions only

### 2. Minimal Dependencies
Modules should depend on as few other modules as possible:
- Core modules (value, scope) have no dependencies on interpreter
- Higher-level modules can depend on core modules

### 3. Clear Interfaces
Each module exposes a clean, well-documented public API:
- Public structs and enums
- Public methods with clear purposes
- Internal implementation details hidden

### 4. Testability
Each module should be independently testable:
- Unit tests for each module
- Integration tests for module interactions
- Mock implementations for dependencies

## Next Steps

### Immediate Actions
1. ✅ Create foundational modules (value, scope, function) - COMPLETED
2. ⏳ Extract statement execution logic
3. ⏳ Extract expression evaluation logic
4. ⏳ Extract built-in functions
5. ⏳ Update main interpreter module

### Future Considerations
- Add benchmarks for modularized code
- Document module interactions
- Consider trait-based abstractions
- Evaluate performance impact

## Migration Guide

### For Developers Working on Interpreter

**Before:**
```rust
// All in one file: src/interpreter.rs
use crate::interpreter::{Value, Interpreter};
```

**After:**
```rust
// Use specific modules
use crate::interpreter::{value::Value, Interpreter};
use crate::interpreter::scope::Scope;
```

### Adding New Features

1. **New Value Type**: Modify `interpreter/value.rs`
2. **New Scope Feature**: Modify `interpreter/scope.rs`
3. **New Statement**: Add to `interpreter/stmt_executor.rs`
4. **New Expression**: Add to `interpreter/expr_evaluator.rs`
5. **New Builtin**: Add to `interpreter/builtins.rs`

## Status

- ✅ Plan created and documented
- ✅ Module structure designed
- ✅ Foundation modules created (value, scope, function)
- ⏳ Core modules extraction (in progress)
- ⏳ Integration and testing (pending)

---

**Created**: February 8, 2025
**Status**: Phase 1 Complete, Phase 2-4 Pending
**Impact**: Will improve code organization, maintainability, and testability
