//! Runtime reflection — type introspection for interpreter values.

use crate::interpreter::Value;

/// Return a Pascal-style type name for a runtime value.
pub fn type_name(value: &Value) -> String {
    match value {
        Value::Integer(_) => "Integer".to_string(),
        Value::Real(_) => "Real".to_string(),
        Value::Boolean(_) => "Boolean".to_string(),
        Value::Char(_) => "Char".to_string(),
        Value::String(_) => "String".to_string(),
        Value::Nil => "Pointer".to_string(),
        Value::Object { class_name, .. } => class_name.clone(),
        Value::Array { .. } => "Array".to_string(),
        Value::Record { type_name, .. } => {
            type_name.clone().unwrap_or_else(|| "Record".to_string())
        }
        Value::Enum { type_name, .. } => type_name.clone(),
        Value::Set { .. } => "Set".to_string(),
        Value::Pointer(_) => "Pointer".to_string(),
        Value::Closure { .. } => "Function".to_string(),
    }
}

/// Check if value is of a given type name (case-insensitive).
pub fn is_type(value: &Value, expected: &str) -> bool {
    let actual = match value {
        Value::Integer(_) => "Integer",
        Value::Real(_) => "Real",
        Value::Boolean(_) => "Boolean",
        Value::Char(_) => "Char",
        Value::String(_) => "String",
        Value::Nil => "Nil",
        Value::Object { .. } => "Object",
        Value::Array { .. } => "Array",
        Value::Record { .. } => "Record",
        Value::Enum { .. } => "Enum",
        Value::Set { .. } => "Set",
        Value::Pointer(_) => "Pointer",
        Value::Closure { .. } => "Function",
    };
    expected.eq_ignore_ascii_case(actual)
        || matches!(
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
