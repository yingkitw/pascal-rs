//! Runtime reflection — type introspection for interpreter values.

use crate::interpreter::Value;

/// Return a Pascal-style type name for a runtime value.
pub fn type_name(value: &Value) -> &'static str {
    match value {
        Value::Integer(_) => "Integer",
        Value::Real(_) => "Real",
        Value::Boolean(_) => "Boolean",
        Value::Char(_) => "Char",
        Value::String(_) => "String",
        Value::Nil => "Pointer",
        Value::Object { class_name, .. } => return leak_class_name(class_name),
        Value::Array { .. } => "Array",
        Value::Record { .. } => "Record",
        Value::Enum { type_name, .. } => return leak_class_name(type_name),
        Value::Set { .. } => "Set",
        Value::Pointer(_) => "Pointer",
        Value::Closure { .. } => "Function",
    }
}

fn leak_class_name(s: &str) -> &'static str {
    // Return dynamic names via a static leak for simplicity in builtins
    Box::leak(s.to_string().into_boxed_str())
}

/// Check if value is of a given type name (case-insensitive).
pub fn is_type(value: &Value, expected: &str) -> bool {
    expected.eq_ignore_ascii_case(match value {
        Value::Object { .. } => "Object",
        _ => type_name(value),
    }) || matches!(
        (value, expected.to_lowercase().as_str()),
        (Value::Integer(_), "integer") | (Value::Real(_), "real") | (Value::Boolean(_), "boolean")
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_name_integer() {
        assert_eq!(type_name(&Value::Integer(1)), "Integer");
    }
}
