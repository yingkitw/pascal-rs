//! Interface type registry — tracks interface declarations and class conformance.

use crate::ast::{ClassDecl, InterfaceDecl, MethodDecl};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct InterfaceRegistry {
    interfaces: HashMap<String, InterfaceDecl>,
}

impl InterfaceRegistry {
    pub fn new() -> Self {
        Self {
            interfaces: HashMap::new(),
        }
    }

    pub fn register(&mut self, iface: InterfaceDecl) {
        self.interfaces.insert(iface.name.to_lowercase(), iface);
    }

    pub fn get(&self, name: &str) -> Option<&InterfaceDecl> {
        self.interfaces.get(&name.to_lowercase())
    }

    /// Check whether a class implements all methods required by an interface.
    pub fn class_implements(&self, class: &ClassDecl, interface_name: &str) -> bool {
        let Some(iface) = self.get(interface_name) else {
            return false;
        };
        for required in &iface.methods {
            if !class_has_method(class, &required.name) {
                return false;
            }
        }
        true
    }

    pub fn list_interfaces(&self) -> Vec<&str> {
        self.interfaces.keys().map(|s| s.as_str()).collect()
    }
}

fn class_has_method(class: &ClassDecl, method_name: &str) -> bool {
    let lower = method_name.to_lowercase();
    class.methods.iter().any(|m| m.name.to_lowercase() == lower)
        || class
            .parent
            .as_ref()
            .is_some_and(|_| false) // parent lookup would need class registry
}

impl Default for InterfaceRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::FieldVisibility;

    #[test]
    fn test_class_implements_interface() {
        let mut reg = InterfaceRegistry::new();
        reg.register(InterfaceDecl {
            name: "IComparable".to_string(),
            parent: None,
            methods: vec![MethodDecl {
                name: "Compare".to_string(),
                parameters: vec![],
                return_type: Some(crate::ast::Type::Integer),
                block: None,
                visibility: FieldVisibility::Public,
                is_class_method: false,
                is_virtual: true,
                is_abstract: true,
                is_override: false,
                is_overload: false,
                is_static: false,
                is_constructor: false,
                is_destructor: false,
            }],
            properties: vec![],
            visibility: FieldVisibility::Public,
        });

        let class = ClassDecl {
            name: "MyClass".to_string(),
            parent: None,
            interfaces: vec!["IComparable".to_string()],
            fields: vec![],
            methods: vec![MethodDecl {
                name: "Compare".to_string(),
                parameters: vec![],
                return_type: Some(crate::ast::Type::Integer),
                block: None,
                visibility: FieldVisibility::Public,
                is_class_method: false,
                is_virtual: true,
                is_abstract: false,
                is_override: true,
                is_overload: false,
                is_static: false,
                is_constructor: false,
                is_destructor: false,
            }],
            properties: vec![],
            visibility: FieldVisibility::Public,
            is_abstract: false,
            is_sealed: false,
        };

        assert!(reg.class_implements(&class, "IComparable"));
    }
}
