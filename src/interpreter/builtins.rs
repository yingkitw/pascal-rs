//! Built-in functions and procedures for Pascal interpreter

use crate::interpreter::value::Value;
use anyhow::Result;
use std::collections::HashMap;
use rand::Rng;

/// Built-in function implementation
pub type BuiltinFn = Box<dyn Fn(&[Value]) -> Result<Value>>;

/// Built-in function signature: (name, arity, function pointer)
pub type BuiltinFunction = (String, usize, BuiltinFn);

/// Built-in function registry
#[derive(Default)]
pub struct BuiltinRegistry {
    functions: HashMap<String, BuiltinFunction>,
}

impl BuiltinRegistry {
    /// Create a new built-in function registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a built-in function
    pub fn register(&mut self, name: String, arity: usize, func: BuiltinFn) {
        self.functions.insert(name.clone(), (name, arity, func));
    }

    /// Get a built-in function by name
    pub fn get_function(&self, name: &str) -> Option<&BuiltinFunction> {
        self.functions.get(name)
    }

    /// Check if a function exists
    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Get all function names
    pub fn function_names(&self) -> Vec<String> {
        self.functions.keys().cloned().collect()
    }

    /// Clear all functions
    pub fn clear(&mut self) {
        self.functions.clear();
    }
}

/// Built-in function implementations
pub struct Builtins;

impl Builtins {
    /// Register all built-in functions
    pub fn register_builtins(registry: &mut BuiltinRegistry) {
        // I/O functions
        registry.register(
            "write".to_string(),
            1,
            Box::new(Self::write),
        );
        
        registry.register(
            "writeln".to_string(),
            1,
            Box::new(Self::writeln),
        );
        
        registry.register(
            "read".to_string(),
            1,
            Box::new(Self::read),
        );
        
        registry.register(
            "readln".to_string(),
            1,
            Box::new(Self::readln),
        );

        // Mathematical functions
        registry.register(
            "abs".to_string(),
            1,
            Box::new(Self::abs),
        );
        
        registry.register(
            "sqrt".to_string(),
            1,
            Box::new(Self::sqrt),
        );
        
        registry.register(
            "sin".to_string(),
            1,
            Box::new(Self::sin),
        );
        
        registry.register(
            "cos".to_string(),
            1,
            Box::new(Self::cos),
        );
        
        registry.register(
            "exp".to_string(),
            1,
            Box::new(Self::exp),
        );
        
        registry.register(
            "ln".to_string(),
            1,
            Box::new(Self::ln),
        );

        // String functions
        registry.register(
            "length".to_string(),
            1,
            Box::new(Self::length),
        );
        
        registry.register(
            "copy".to_string(),
            3,
            Box::new(Self::copy_str),
        );
        
        registry.register(
            "pos".to_string(),
            2,
            Box::new(Self::pos),
        );

        // Type conversion functions
        registry.register(
            "str".to_string(),
            1,
            Box::new(Self::str_fn),
        );
        
        registry.register(
            "ord".to_string(),
            1,
            Box::new(Self::ord_fn),
        );
        
        registry.register(
            "chr".to_string(),
            1,
            Box::new(Self::chr_fn),
        );

        // Array functions
        registry.register(
            "low".to_string(),
            1,
            Box::new(Self::low),
        );
        
        registry.register(
            "high".to_string(),
            1,
            Box::new(Self::high),
        );

        // Internal indexing function (used by parser for arr[i] expressions)
        registry.register(
            "__index__".to_string(),
            2,
            Box::new(Self::__index__),
        );

        // Random functions
        registry.register(
            "random".to_string(),
            0,
            Box::new(Self::random),
        );
        
        registry.register(
            "randomize".to_string(),
            0,
            Box::new(Self::randomize),
        );

        // Reflection
        registry.register(
            "TypeName".to_string(),
            1,
            Box::new(|args| {
                if args.is_empty() {
                    return Err(anyhow::anyhow!("TypeName expects one argument"));
                }
                Ok(Value::String(crate::reflection::type_name(&args[0])))
            }),
        );
    }

    // I/O function implementations
    fn write(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::String(ref s) => {
                print!("{}", s);
                Ok(Value::Nil)
            },
            Value::Integer(i) => {
                print!("{}", i);
                Ok(Value::Nil)
            },
            Value::Boolean(b) => {
                print!("{}", b);
                Ok(Value::Nil)
            },
            Value::Real(f) => {
                print!("{}", f);
                Ok(Value::Nil)
            },
            Value::Char(c) => {
                print!("{}", c);
                Ok(Value::Nil)
            },
            _ => Err(anyhow::anyhow!("write: invalid argument type")),
        }
    }

    fn writeln(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::String(ref s) => {
                println!("{}", s);
                Ok(Value::Nil)
            },
            Value::Integer(i) => {
                println!("{}", i);
                Ok(Value::Nil)
            },
            Value::Boolean(b) => {
                println!("{}", b);
                Ok(Value::Nil)
            },
            Value::Real(f) => {
                println!("{}", f);
                Ok(Value::Nil)
            },
            Value::Char(c) => {
                println!("{}", c);
                Ok(Value::Nil)
            },
            _ => Err(anyhow::anyhow!("writeln: invalid argument type")),
        }
    }

    fn read(_args: &[Value]) -> Result<Value> {
        todo!("read: needs user input implementation")
    }

    fn readln(_args: &[Value]) -> Result<Value> {
        todo!("readln: needs user input implementation")
    }

    // Mathematical function implementations
    fn abs(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::Integer(i) => Ok(Value::Integer(i.abs())),
            Value::Real(f) => Ok(Value::Real(f.abs())),
            _ => Err(anyhow::anyhow!("abs: integer or float expected")),
        }
    }

    fn sqrt(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::Real(f) => Ok(Value::Real(f.sqrt())),
            Value::Integer(i) => Ok(Value::Real((i as f64).sqrt())),
            _ => Err(anyhow::anyhow!("sqrt: numeric expected")),
        }
    }

    fn sin(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::Real(f) => Ok(Value::Real(f.sin())),
            Value::Integer(i) => Ok(Value::Real((i as f64).sin())),
            _ => Err(anyhow::anyhow!("sin: numeric expected")),
        }
    }

    fn cos(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::Real(f) => Ok(Value::Real(f.cos())),
            Value::Integer(i) => Ok(Value::Real((i as f64).cos())),
            _ => Err(anyhow::anyhow!("cos: numeric expected")),
        }
    }

    fn exp(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::Real(f) => Ok(Value::Real(f.exp())),
            Value::Integer(i) => Ok(Value::Real((i as f64).exp())),
            _ => Err(anyhow::anyhow!("exp: numeric expected")),
        }
    }

    fn ln(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::Real(f) if f > 0.0 => Ok(Value::Real(f.ln())),
            Value::Integer(i) if i > 0 => Ok(Value::Real((i as f64).ln())),
            _ => Err(anyhow::anyhow!("ln: positive number expected")),
        }
    }

    // String function implementations
    fn length(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::String(s) => Ok(Value::Integer(s.len() as i64)),
            Value::Array { elements: arr, .. } => Ok(Value::Integer(arr.len() as i64)),
            _ => Err(anyhow::anyhow!("length: string or array expected")),
        }
    }

    fn copy_str(args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1], &args[2]) {
            (Value::String(s), Value::Integer(start), Value::Integer(count)) => {
                let start = (*start as usize).saturating_sub(1);
                let count = *count as usize;
                if start >= s.len() {
                    Ok(Value::String("".to_string()))
                } else {
                    let end = (start + count).min(s.len());
                    Ok(Value::String(s[start..end].to_string()))
                }
            },
            _ => Err(anyhow::anyhow!("copy: string, integer, integer expected")),
        }
    }

    fn pos(args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::String(sub), Value::String(s)) => {
                let pos = s.find(sub).map(|p| p as i64 + 1).unwrap_or(0);
                Ok(Value::Integer(pos))
            },
            _ => Err(anyhow::anyhow!("pos: string, string expected")),
        }
    }

    // Type conversion function implementations
    fn str_fn(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Integer(i) => Ok(Value::String(i.to_string())),
            Value::Real(f) => Ok(Value::String(f.to_string())),
            Value::Boolean(b) => Ok(Value::String(b.to_string())),
            Value::String(s) => Ok(Value::String(s.clone())),
            _ => Err(anyhow::anyhow!("str: type cannot be converted to string")),
        }
    }

    fn ord_fn(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::Char(c) => Ok(Value::Integer(c as i64)),
            Value::Boolean(b) => Ok(Value::Integer(if b { 1 } else { 0 })),
            _ => Err(anyhow::anyhow!("ord: char or boolean expected")),
        }
    }

    fn chr_fn(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::Integer(i) if (0..=255).contains(&i) => Ok(Value::Char(i as u8 as char)),
            _ => Err(anyhow::anyhow!("chr: integer between 0-255 expected")),
        }
    }

    // Array function implementations
    fn low(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Array { elements: _arr, .. } => Ok(Value::Integer(0)), // Pascal arrays are 1-based, but we return 0 for low
            _ => Err(anyhow::anyhow!("low: array expected")),
        }
    }

    fn high(args: &[Value]) -> Result<Value> {
        match args[0] {
            Value::Array { elements: ref arr, .. } => Ok(Value::Integer((arr.len() - 1) as i64)),
            _ => Err(anyhow::anyhow!("high: array expected")),
        }
    }

    fn __index__(args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Array { elements, lower_bound }, Value::Integer(idx)) => {
                let adjusted = (idx - *lower_bound) as usize;
                if adjusted >= elements.len() {
                    return Err(anyhow::anyhow!(
                        "Array index out of bounds: index {} (bounds: {}..{})",
                        idx, lower_bound, lower_bound + elements.len() as i64 - 1
                    ));
                }
                Ok(elements[adjusted].clone())
            },
            (Value::String(s), Value::Integer(idx)) => {
                let adjusted = (idx - 1) as usize; // Pascal strings are 1-indexed
                if adjusted >= s.len() {
                    return Err(anyhow::anyhow!(
                        "String index out of bounds: {} (length: {})", idx, s.len()
                    ));
                }
                Ok(Value::Char(s.chars().nth(adjusted).unwrap_or('\0')))
            },
            _ => Err(anyhow::anyhow!("__index__: array/string and integer expected")),
        }
    }

    // Random function implementations
    fn random(args: &[Value]) -> Result<Value> {
        match args[0] {
            // random() - returns random float between 0 and 1
            Value::Nil => {
                let _rng = rand::thread_rng();
                Ok(Value::Real(rand::random::<f64>()))
            },
            // random(n) - returns random integer between 0 and n-1
            Value::Integer(n) if n > 0 => {
                let mut rng = rand::thread_rng();
                Ok(Value::Integer(rng.gen_range(0..n)))
            },
            _ => Err(anyhow::anyhow!("random: no argument or positive integer expected")),
        }
    }

    fn randomize(_args: &[Value]) -> Result<Value> {
        // Initialize random number generator
        let _ = rand::thread_rng();
        Ok(Value::Nil)
    }
}

/// Helper to create a default built-in registry
pub fn create_default_registry() -> BuiltinRegistry {
    let mut registry = BuiltinRegistry::new();
    Builtins::register_builtins(&mut registry);
    registry
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_registry() {
        let mut registry = BuiltinRegistry::new();
        
        registry.register("test".to_string(), 1, Box::new(|_| Ok(Value::Integer(42))));
        
        assert!(registry.has_function("test"));
        assert_eq!(registry.function_names(), vec!["test".to_string()]);
    }

    #[test]
    fn test_builtins() {
        let registry = create_default_registry();
        
        // Test abs function
        if let Some((_, _, abs_func)) = registry.get_function("abs") {
            let result = abs_func(&[Value::Integer(-5)]).unwrap();
            assert_eq!(result, Value::Integer(5));
        }

        // Test sqrt function
        if let Some((_, _, sqrt_func)) = registry.get_function("sqrt") {
            let result = sqrt_func(&[Value::Integer(4)]).unwrap();
            assert_eq!(result, Value::Real(2.0));
        }

        // Test length function
        if let Some((_, _, len_func)) = registry.get_function("length") {
            let result = len_func(&[Value::Array { 
                elements: vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)], 
                lower_bound: 1 
            }]).unwrap();
            assert_eq!(result, Value::Integer(3));
        }
    }
}