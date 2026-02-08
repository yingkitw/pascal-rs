//! Runtime value type for the interpreter

use anyhow::{anyhow, Result};

/// Runtime value
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Real(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Nil,
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
        }
    }
}

impl Value {
    pub fn as_integer(&self) -> Result<i64> {
        match self {
            Value::Integer(n) => Ok(*n),
            Value::Real(r) => Ok(*r as i64),
            Value::Boolean(b) => Ok(if *b { 1 } else { 0 }),
            Value::Char(c) => Ok(*c as i64),
            _ => Err(anyhow!("Cannot convert {:?} to integer", self)),
        }
    }

    pub fn as_real(&self) -> Result<f64> {
        match self {
            Value::Integer(n) => Ok(*n as f64),
            Value::Real(r) => Ok(*r),
            _ => Err(anyhow!("Cannot convert {:?} to real", self)),
        }
    }

    pub fn as_boolean(&self) -> Result<bool> {
        match self {
            Value::Boolean(b) => Ok(*b),
            Value::Integer(n) => Ok(*n != 0),
            _ => Err(anyhow!("Cannot convert {:?} to boolean", self)),
        }
    }

    pub fn is_real(&self) -> bool {
        matches!(self, Value::Real(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn integer(n: i64) -> Self {
        Value::Integer(n)
    }

    pub fn real(r: f64) -> Self {
        Value::Real(r)
    }

    pub fn boolean(b: bool) -> Self {
        Value::Boolean(b)
    }

    pub fn string(s: impl Into<String>) -> Self {
        Value::String(s.into())
    }

    pub fn char(c: char) -> Self {
        Value::Char(c)
    }
}
