//! Runtime values and scope for the Pascal interpreter

use anyhow::{anyhow, Result};
use std::collections::HashMap;

/// A Pascal exception that propagates through the interpreter
#[derive(Debug, Clone)]
pub struct PascalException {
    pub class_name: String,
    pub message: String,
}

impl std::fmt::Display for PascalException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.class_name, self.message)
    }
}

impl std::error::Error for PascalException {}

/// Early return signal (for exit from functions/procedures)
#[derive(Debug, Clone)]
pub struct EarlyReturn {
    pub value: Option<Value>,
}

impl std::fmt::Display for EarlyReturn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "EarlyReturn")
    }
}

impl std::error::Error for EarlyReturn {}

/// Runtime value
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Real(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Nil,
    Object {
        class_name: String,
        fields: HashMap<String, Value>,
    },
    Array {
        elements: Vec<Value>,
        lower_bound: i64,
    },
    Record {
        fields: HashMap<String, Value>,
    },
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Real(r) => write!(f, "{}", r),
            Value::Boolean(b) => write!(f, "{}", if *b { "TRUE" } else { "FALSE" }),
            Value::Char(c) => write!(f, "{}", c),
            Value::String(s) => write!(f, "{}", s),
            Value::Nil => write!(f, "nil"),
            Value::Object { class_name, .. } => write!(f, "<{}>", class_name),
            Value::Array { elements, .. } => {
                let items: Vec<String> = elements.iter().map(|v| format!("{}", v)).collect();
                write!(f, "({})", items.join(", "))
            }
            Value::Record { fields } => {
                let items: Vec<String> = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                write!(f, "record({})", items.join("; "))
            }
        }
    }
}

impl Value {
    pub(crate) fn as_integer(&self) -> Result<i64> {
        match self {
            Value::Integer(n) => Ok(*n),
            Value::Real(r) => Ok(*r as i64),
            Value::Boolean(b) => Ok(if *b { 1 } else { 0 }),
            Value::Char(c) => Ok(*c as i64),
            _ => Err(anyhow!("Cannot convert {:?} to integer", self)),
        }
    }

    pub(crate) fn as_real(&self) -> Result<f64> {
        match self {
            Value::Integer(n) => Ok(*n as f64),
            Value::Real(r) => Ok(*r),
            _ => Err(anyhow!("Cannot convert {:?} to real", self)),
        }
    }

    pub(crate) fn as_boolean(&self) -> Result<bool> {
        match self {
            Value::Boolean(b) => Ok(*b),
            Value::Integer(n) => Ok(*n != 0),
            _ => Err(anyhow!("Cannot convert {:?} to boolean", self)),
        }
    }

    pub(crate) fn is_real(&self) -> bool {
        matches!(self, Value::Real(_))
    }
}

/// Variable scope
#[derive(Debug, Clone)]
pub struct Scope {
    variables: HashMap<String, Value>,
}

impl Scope {
    pub(crate) fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    /// Get a variable value by name (for testing purposes)
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(&name.to_lowercase())
    }

    pub(crate) fn insert(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    pub(crate) fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.variables.get_mut(&name.to_lowercase())
    }

    pub(crate) fn contains_key(&self, name: &str) -> bool {
        self.variables.contains_key(&name.to_lowercase())
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.variables.iter()
    }
}
