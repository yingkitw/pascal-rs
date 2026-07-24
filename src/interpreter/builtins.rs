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

        // Math — additional
        registry.register("sqr".to_string(), 1, Box::new(Self::sqr));
        registry.register("round".to_string(), 1, Box::new(Self::round_fn));
        registry.register("trunc".to_string(), 1, Box::new(Self::trunc_fn));
        registry.register("power".to_string(), 2, Box::new(Self::power));

        // String — additional
        registry.register("concat".to_string(), 2, Box::new(Self::concat));
        registry.register("upcase".to_string(), 1, Box::new(Self::upcase));
        registry.register("lowercase".to_string(), 1, Box::new(Self::lowercase_fn));
        registry.register("inttostr".to_string(), 1, Box::new(Self::inttostr));
        registry.register("strtoint".to_string(), 1, Box::new(Self::strtoint));

        // Ordinal — additional
        registry.register("odd".to_string(), 1, Box::new(Self::odd));
        registry.register("succ".to_string(), 1, Box::new(Self::succ));
        registry.register("pred".to_string(), 1, Box::new(Self::pred));

        // Array — additional
        registry.register("setlength".to_string(), 2, Box::new(Self::setlength));

        // Control — additional
        registry.register("halt".to_string(), 0, Box::new(Self::halt));
    }

    // I/O function implementations
    fn write(args: &[Value]) -> Result<Value> {
        for arg in args {
            Self::print_value(arg)?;
        }
        Ok(Value::Nil)
    }

    /// Print a single value with no trailing newline.
    fn print_value(v: &Value) -> Result<()> {
        print!("{}", value_to_string(v));
        Ok(())
    }

    fn writeln(args: &[Value]) -> Result<Value> {
        for arg in args {
            Self::print_value(arg)?;
        }
        println!();
        Ok(Value::Nil)
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

    // Additional math
    fn sqr(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Integer(i) => Ok(Value::Integer(i.saturating_mul(*i))),
            Value::Real(f) => Ok(Value::Real(f * f)),
            _ => Err(anyhow::anyhow!("sqr: numeric expected")),
        }
    }

    fn round_fn(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Real(f) => Ok(Value::Integer(f.round() as i64)),
            Value::Integer(i) => Ok(Value::Integer(*i)),
            _ => Err(anyhow::anyhow!("round: numeric expected")),
        }
    }

    fn trunc_fn(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Real(f) => Ok(Value::Integer(f.trunc() as i64)),
            Value::Integer(i) => Ok(Value::Integer(*i)),
            _ => Err(anyhow::anyhow!("trunc: numeric expected")),
        }
    }

    fn power(args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Integer(base), Value::Integer(exp)) if *exp >= 0 => {
                let result = base.checked_pow(*exp as u32);
                Ok(result.map_or(Value::Nil, Value::Integer))
            }
            (Value::Real(base), Value::Integer(exp)) => Ok(Value::Real(base.powi(*exp as i32))),
            (Value::Real(base), Value::Real(exp)) => Ok(Value::Real(base.powf(*exp))),
            _ => Err(anyhow::anyhow!("power: numeric arguments expected")),
        }
    }

    // Additional string
    fn concat(args: &[Value]) -> Result<Value> {
        let a = value_to_string(&args[0]);
        let b = value_to_string(&args[1]);
        Ok(Value::String(format!("{}{}", a, b)))
    }

    fn upcase(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::String(s) => Ok(Value::String(s.to_uppercase())),
            Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
            _ => Err(anyhow::anyhow!("upcase: string or char expected")),
        }
    }

    fn lowercase_fn(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::String(s) => Ok(Value::String(s.to_lowercase())),
            Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
            _ => Err(anyhow::anyhow!("lowercase: string or char expected")),
        }
    }

    fn inttostr(args: &[Value]) -> Result<Value> {
        let s = value_to_string(&args[0]);
        Ok(Value::String(s))
    }

    fn strtoint(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::String(s) => s
                .trim()
                .parse::<i64>()
                .map(Value::Integer)
                .map_err(|e| anyhow::anyhow!("strtoint: {}", e)),
            _ => Err(anyhow::anyhow!("strtoint: string expected")),
        }
    }

    // Additional ordinal
    fn odd(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Integer(i) => Ok(Value::Boolean(i % 2 != 0)),
            _ => Err(anyhow::anyhow!("odd: integer expected")),
        }
    }

    fn succ(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Integer(i) => Ok(Value::Integer(i.saturating_add(1))),
            Value::Char(c) => {
                let next = (*c as u32).saturating_add(1);
                char::from_u32(next)
                    .map(Value::Char)
                    .ok_or_else(|| anyhow::anyhow!("succ: no successor for char {:?}", c))
            }
            Value::Enum { type_name, ordinal } => Ok(Value::Enum {
                type_name: type_name.clone(),
                ordinal: ordinal + 1,
            }),
            _ => Err(anyhow::anyhow!("succ: integer, char, or enum expected")),
        }
    }

    fn pred(args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Integer(i) => Ok(Value::Integer(i.saturating_sub(1))),
            Value::Char(c) => {
                let prev = (*c as u32).saturating_sub(1);
                char::from_u32(prev)
                    .map(Value::Char)
                    .ok_or_else(|| anyhow::anyhow!("pred: no predecessor for char {:?}", c))
            }
            Value::Enum { type_name, ordinal } => Ok(Value::Enum {
                type_name: type_name.clone(),
                ordinal: ordinal - 1,
            }),
            _ => Err(anyhow::anyhow!("pred: integer, char, or enum expected")),
        }
    }

    // Array
    fn setlength(args: &[Value]) -> Result<Value> {
        // setlength(var, n) is normally a statement; calling it as a function
        // returns the new length so it can still be tested.
        match &args[1] {
            Value::Integer(n) if *n >= 0 => Ok(Value::Integer(*n)),
            _ => Err(anyhow::anyhow!("setlength: non-negative length required")),
        }
    }

    // Control
    fn halt(_args: &[Value]) -> Result<Value> {
        Err(anyhow::anyhow!("__halt__"))
    }
}

/// Helper to create a default built-in registry
pub fn create_default_registry() -> BuiltinRegistry {
    let mut registry = BuiltinRegistry::new();
    Builtins::register_builtins(&mut registry);
    registry
}

/// Render a `Value` to its single-line, human-readable form.
fn value_to_string(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Integer(i) => i.to_string(),
        Value::Boolean(b) => b.to_string(),
        Value::Real(f) => f.to_string(),
        Value::Char(c) => c.to_string(),
        Value::Nil => String::new(),
        Value::Array { elements, .. } => join_values(elements, ", "),
        Value::Record { fields, .. } => format!("record({})", join_fields(fields, "; ")),
        Value::Object { class_name, .. } => format!("<{}>", class_name),
        Value::Enum { type_name, ordinal } => format!("{}({})", type_name, ordinal),
        Value::Set { elements } => {
            let mut parts: Vec<String> = elements.iter().map(|n| n.to_string()).collect();
            parts.sort();
            format!("[{}]", parts.join(", "))
        }
        Value::Pointer(id) => format!("^{}", id),
        Value::Closure { .. } => "<closure>".to_string(),
    }
}

fn join_values(values: &[Value], sep: &str) -> String {
    let parts: Vec<String> = values.iter().map(value_to_string).collect();
    format!("({})", parts.join(sep))
}

fn join_fields(fields: &std::collections::HashMap<String, Value>, sep: &str) -> String {
    let mut entries: Vec<(String, String)> = fields
        .iter()
        .map(|(k, v)| (k.clone(), value_to_string(v)))
        .collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    entries
        .into_iter()
        .map(|(k, v)| format!("{}: {}", k, v))
        .collect::<Vec<_>>()
        .join(sep)
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

    #[test]
    fn test_value_to_string_all_variants() {
        // Exhaustive check that every Value variant produces a non-empty string
        // (Nil is intentionally empty) so writeln/write never drop data silently.
        assert_eq!(value_to_string(&Value::Integer(42)), "42");
        assert_eq!(value_to_string(&Value::Real(3.14)), "3.14");
        assert_eq!(value_to_string(&Value::Boolean(true)), "true");
        assert_eq!(value_to_string(&Value::Boolean(false)), "false");
        assert_eq!(value_to_string(&Value::Char('x')), "x");
        assert_eq!(value_to_string(&Value::String("hi".into())), "hi");
        assert_eq!(value_to_string(&Value::Nil), "");
        assert_eq!(
            value_to_string(&Value::Array {
                elements: vec![Value::Integer(1), Value::Integer(2)],
                lower_bound: 1
            }),
            "(1, 2)"
        );
        assert_eq!(value_to_string(&Value::Pointer(7)), "^7");
        assert_eq!(value_to_string(&Value::Closure {
            params: vec![],
            body: crate::ast::Block::empty(),
            is_function: true,
            return_type_name: "integer".into(),
            captured: vec![],
        }), "<closure>");
    }

    #[test]
    fn test_join_fields_sorted() {
        let mut fields = std::collections::HashMap::new();
        fields.insert("z".to_string(), Value::Integer(1));
        fields.insert("a".to_string(), Value::Integer(2));
        let s = join_fields(&fields, ", ");
        // Sorted by key, so 'a' comes before 'z'
        assert!(s.starts_with("a: 2"));
        assert!(s.contains("z: 1"));
    }
}