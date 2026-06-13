//! Foreign function interface for calling external (C/Rust) symbols.

use crate::interpreter::Value;
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::sync::Arc;

pub type FfiCallback = Arc<dyn Fn(&[Value]) -> Result<Value> + Send + Sync>;

/// Registry of external functions callable from Pascal
#[derive(Default)]
pub struct FfiRegistry {
    functions: HashMap<String, FfiCallback>,
    aliases: HashMap<String, String>,
}

impl FfiRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register an external function by Pascal name
    pub fn register(&mut self, name: &str, callback: FfiCallback) {
        self.functions.insert(name.to_lowercase(), callback);
    }

    /// Map Pascal name to external symbol name (e.g. C linkage)
    pub fn alias(&mut self, pascal_name: &str, external_name: &str) {
        self.aliases
            .insert(pascal_name.to_lowercase(), external_name.to_string());
    }

    pub fn has(&self, name: &str) -> bool {
        self.functions.contains_key(&name.to_lowercase())
    }

    pub fn call(&self, name: &str, args: &[Value]) -> Result<Value> {
        let key = name.to_lowercase();
        let cb = self
            .functions
            .get(&key)
            .ok_or_else(|| anyhow!("External function '{}' not registered in FFI", name))?;
        cb(args)
    }

    pub fn external_name(&self, pascal_name: &str) -> String {
        self.aliases
            .get(&pascal_name.to_lowercase())
            .cloned()
            .unwrap_or_else(|| pascal_name.to_string())
    }
}

/// Create default FFI registry with common C runtime stubs (no-op for interpreter).
pub fn create_default_ffi_registry() -> FfiRegistry {
    let mut reg = FfiRegistry::new();
    reg.alias("WriteLn", "printf");
    reg
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ffi_register_and_call() {
        let mut reg = FfiRegistry::new();
        reg.register(
            "Double",
            Arc::new(|args| {
                let n = args[0].clone();
                match n {
                    Value::Integer(i) => Ok(Value::Integer(i * 2)),
                    _ => Err(anyhow!("expected integer")),
                }
            }),
        );
        let result = reg.call("Double", &[Value::Integer(21)]).unwrap();
        assert_eq!(result, Value::Integer(42));
    }
}
