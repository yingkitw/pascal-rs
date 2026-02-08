# Trait-Based Architecture

## Overview

The Pascal-Rs interpreter uses a trait-based architecture to improve maintainability, testability, and extensibility. This document explains the trait abstractions, their benefits, and how to use them.

## Core Traits

### 1. Value Operations

#### `TryAs<T>` Trait

Provides type-safe conversions between different value types.

```rust
pub trait TryAs<T> {
    fn try_as(&self) -> Result<T>;
}
```

**Benefits:**
- Type-safe conversions with explicit error handling
- Easy to extend for new types
- Clear intent in code

**Usage:**
```rust
let value = Value::Integer(42);
let int_value: i64 = value.try_as()?;  // Ok(42)
let float_value: f64 = value.try_as()?; // Ok(42.0)
let bool_value: bool = value.try_as()?; // Error (type mismatch)
```

**Implementations:**
- `TryAs<i64>` for `Value` - converts to integer
- `TryAs<f64>` for `Value` - converts to real
- `TryAs<bool>` for `Value` - converts to boolean

#### `FormattedDisplay` Trait

Provides formatted display with precision and width control.

```rust
pub trait FormattedDisplay {
    fn format_with(&self, precision: u32, width: u32) -> String;
}
```

**Benefits:**
- Consistent formatting across all value types
- Configurable output for different contexts
- Separates formatting from business logic

**Usage:**
```rust
let value = Value::Real(3.14159);
let formatted = value.format_with(2, 10); // "      3.14"
```

### 2. Scope Management

#### `ScopeOperations` Trait

Defines the interface for variable storage and retrieval.

```rust
pub trait ScopeOperations {
    fn get(&self, name: &str) -> Option<Value>;
    fn set(&mut self, name: &str, value: Value);
    fn contains(&self, name: &str) -> bool;
    fn variable_names(&self) -> Vec<String>;
}
```

**Benefits:**
- Dependency inversion: depend on abstraction, not concretion
- Easy to mock for testing
- Multiple implementations (single scope, nested scopes, persistent scopes)
- Clear interface between components

**Implementations:**

1. **Single Scope** - `Scope`
   ```rust
   let mut scope = Scope::new();
   scope.set("x", Value::Integer(42));
   let value = scope.get("x");  // Some(Value::Integer(42))
   ```

2. **Nested Scopes** - `ScopeStack`
   ```rust
   let mut stack = ScopeStack::with_global(global_scope);
   stack.push(Scope::new());  // Create local scope
   let value = stack.get("x"); // Searches local, then global
   ```

**Testing Example:**
```rust
#[cfg(test)]
mod tests {
    use super::*;

    struct MockScope {
        variables: HashMap<String, Value>,
    }

    impl ScopeOperations for MockScope {
        fn get(&self, name: &str) -> Option<Value> {
            self.variables.get(name).cloned()
        }
        fn set(&mut self, name: &str, value: Value) {
            self.variables.insert(name.to_string(), value);
        }
        fn contains(&self, name: &str) -> bool {
            self.variables.contains_key(name)
        }
        fn variable_names(&self) -> Vec<String> {
            self.variables.keys().cloned().collect()
        }
    }

    #[test]
    fn test_with_mock() {
        let mut scope = MockScope { variables: HashMap::new() };
        scope.set("test", Value::Integer(123));
        assert!(scope.contains("test"));
    }
}
```

### 3. Function Registry

#### `FunctionRegistry` Trait

Defines the interface for storing and retrieving functions.

```rust
pub trait FunctionRegistry {
    fn get_function(&self, name: &str) -> Option<&UserFunction>;
    fn register_function(&mut self, name: String, func: UserFunction);
    fn has_function(&self, name: &str) -> bool;
}
```

**Benefits:**
- Different storage strategies possible (HashMap, BTreeMap, database)
- Easy to mock for testing
- Supports function metadata and introspection
- Enables function hot-reloading in server environments

**Implementations:**

1. **HashMap-based Registry** - `FunctionRegistryImpl`
   ```rust
   let mut registry = FunctionRegistryImpl::new();
   registry.register_function("foo".to_string(), user_function);
   assert!(registry.has_function("foo"));
   ```

2. **Direct HashMap** - `HashMap<String, UserFunction>`
   ```rust
   let mut functions: HashMap<String, UserFunction> = HashMap::new();
   functions.insert("bar".to_string(), user_function);
   assert!(functions.has_function("bar"));
   ```

### 4. Statement Execution

#### `StatementExecutor` Trait

Defines how different statement types are executed.

```rust
pub trait StatementExecutor {
    fn execute(&mut self, statement: &Stmt) -> Result<ExecutionResult>;
}

pub enum ExecutionResult {
    Success,
    Break,
    Continue,
    Return(Value),
}
```

**Benefits:**
- Pluggable execution strategies (tree-walking, bytecode, JIT)
- Easy to add optimization passes
- Clear separation of concerns
- Supports different execution modes (debug, release)

### 5. Expression Evaluation

#### `ExpressionEvaluator` Trait

Defines how expressions are evaluated.

```rust
pub trait ExpressionEvaluator {
    fn evaluate(&mut self, expr: &Expr) -> Result<Value>;
    fn evaluate_binary(&mut self, op: &str, left: &Value, right: &Value) -> Result<Value>;
    fn evaluate_unary(&mut self, op: &str, operand: &Value) -> Result<Value>;
}
```

**Benefits:**
- Supports different evaluation strategies (lazy, eager, parallel)
- Easy to add optimization passes
- Clear separation of concerns
- Supports constant folding at evaluation time

### 6. Built-in Functions

#### `BuiltinFunction` Trait

Defines the interface for built-in functions.

```rust
pub trait BuiltinFunction {
    fn name(&self) -> &str;
    fn param_count(&self) -> usize;
    fn call(&self, args: &[Value]) -> Result<Value>;
}
```

**Benefits:**
- Adding new built-ins doesn't modify core interpreter
- Easy to extend with custom functions
- Supports dynamic function loading
- Clear function metadata interface

**Example Implementation:**
```rust
struct SqrtFunction;

impl BuiltinFunction for SqrtFunction {
    fn name(&self) -> &str { "sqrt" }
    fn param_count(&self) -> usize { 1 }
    fn call(&self, args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Real(r) => Ok(Value::Real(r.sqrt())),
            Value::Integer(n) => Ok(Value::Real((*n as f64).sqrt())),
            _ => bail!("sqrt requires numeric argument"),
        }
    }
}
```

## Architectural Benefits

### 1. Dependency Inversion

High-level modules don't depend on low-level modules. Both depend on abstractions.

**Before:**
```rust
pub struct Interpreter {
    scopes: Vec<Scope>,  // Concrete dependency
}
```

**After:**
```rust
pub struct Interpreter<T: ScopeOperations> {
    scopes: T,  // Abstract dependency
}
```

### 2. Open/Closed Principle

Open for extension, closed for modification.

**Example:** Adding a new optimization pass doesn't modify existing code:
```rust
struct ConstantFoldingPass;

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &str { "constant_folding" }
    fn optimize(&mut self, program: &Program) -> Result<Program> {
        // Implementation
    }
    fn is_enabled(&self, options: &OptimizationOptions) -> bool {
        options.constant_folding
    }
}
```

### 3. Liskov Substitution

Subtypes must be substitutable for their base types.

**Example:** Any `ScopeOperations` implementation can be used:
```rust
fn interpret_with_scope<T: ScopeOperations>(scope: &mut T) {
    scope.set("x", Value::Integer(42));
    // Works with Scope, ScopeStack, or MockScope
}
```

### 4. Interface Segregation

Clients shouldn't depend on interfaces they don't use.

Instead of one large `Interpreter` trait, we have focused traits:
- `ScopeOperations` - just variable storage
- `FunctionRegistry` - just function storage
- `StatementExecutor` - just statement execution
- `ExpressionEvaluator` - just expression evaluation

### 5. Single Responsibility

Each trait has one reason to change.

- `TryAs<T>` - changes when new types are added
- `ScopeOperations` - changes when variable storage changes
- `FunctionRegistry` - changes when function storage changes
- `StatementExecutor` - changes when statement execution changes

## Testing Benefits

### 1. Easy Mocking

```rust
struct MockScope {
    predefined: HashMap<String, Value>,
}

impl ScopeOperations for MockScope {
    fn get(&self, name: &str) -> Option<Value> {
        self.predefined.get(name).cloned()
    }
    fn set(&mut self, _name: &str, _value: Value) {
        // Do nothing in mock
    }
    fn contains(&self, name: &str) -> bool {
        self.predefined.contains_key(name)
    }
    fn variable_names(&self) -> Vec<String> {
        self.predefined.keys().cloned().collect()
    }
}
```

### 2. Test Isolation

Traits allow testing individual components without dependencies:
```rust
#[test]
fn test_binary_operations() {
    struct MockEvaluator;
    impl ExpressionEvaluator for MockEvaluator {
        fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
            match expr {
                Expr::Literal(Literal::Integer(n)) => Ok(Value::Integer(*n)),
                _ => bail!("Not implemented"),
            }
        }
        fn evaluate_binary(&mut self, op: &str, left: &Value, right: &Value) -> Result<Value> {
            match (op, left, right) {
                ("+", Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
                _ => bail!("Unsupported operation"),
            }
        }
        fn evaluate_unary(&mut self, _op: &str, _operand: &Value) -> Result<Value> {
            bail!("Not implemented")
        }
    }

    let mut evaluator = MockEvaluator;
    let left = Value::Integer(5);
    let right = Value::Integer(3);
    let result = evaluator.evaluate_binary("+", &left, &right).unwrap();
    assert_eq!(result, Value::Integer(8));
}
```

## Performance Considerations

### 1. Static Dispatch

Traits enable static dispatch with monomorphization, which is as fast as concrete types:
```rust
fn with_scope<T: ScopeOperations>(scope: &mut T) {
    // Monomorphized - no vtable overhead
}
```

### 2. Dynamic Dispatch

When runtime polymorphism is needed, use trait objects:
```rust
fn with_dyn_scope(scope: &mut dyn ScopeOperations) {
    // Vtable dispatch - slight overhead
}
```

### 3. Zero-Cost Abstractions

Rust's trait system provides abstractions without runtime cost when using static dispatch.

## Migration Guide

### Step 1: Use Traits Instead of Concrete Types

**Before:**
```rust
pub struct Interpreter {
    scopes: Vec<Scope>,
    functions: HashMap<String, UserFunction>,
}
```

**After:**
```rust
pub struct Interpreter {
    scopes: ScopeStack,
    functions: FunctionRegistryImpl,
}
```

### Step 2: Implement Traits for Your Types

```rust
impl ScopeOperations for ScopeStack {
    fn get(&self, name: &str) -> Option<Value> {
        // Implementation
    }
    // ... other methods
}
```

### Step 3: Use Trait Bounds in Generic Functions

```rust
fn execute_with_scope<T: ScopeOperations>(
    scope: &mut T,
    statement: &Stmt
) -> Result<ExecutionResult> {
    // Works with any ScopeOperations implementation
}
```

### Step 4: Write Tests with Mocks

```rust
#[test]
fn test_with_mock_scope() {
    let mut mock = MockScope::new();
    execute_with_scope(&mut mock, &stmt)?;
    // Assert behavior
}
```

## Future Enhancements

### 1. Async Interpreter

Traits enable async execution:
```rust
#[async_trait]
pub trait AsyncStatementExecutor {
    async fn execute(&mut self, statement: &Stmt) -> Result<ExecutionResult>;
}
```

### 2. Parallel Execution

Traits enable parallel evaluation:
```rust
pub trait ParallelExpressionEvaluator {
    fn evaluate_parallel(&mut self, exprs: &[Expr]) -> Result<Vec<Value>>;
}
```

### 3. Plugin System

Traits enable dynamic function loading:
```rust
pub trait Plugin {
    fn register_builtins(&self, registry: &mut dyn FunctionRegistry);
    fn register_optimizations(&self, pass_manager: &mut OptimizationPassManager);
}
```

## Summary

The trait-based architecture provides:
- **Better testability** through easy mocking
- **Higher maintainability** through clear abstractions
- **Greater extensibility** through open/closed principle
- **Type safety** through Rust's trait system
- **Zero-cost abstractions** through static dispatch
- **Clear separation of concerns** through focused traits

This architecture makes the codebase more maintainable, testable, and extensible while maintaining the performance of a concrete implementation.
