//! Compiler attribute metadata and directive parsing.

use std::collections::HashMap;

/// Attribute attached to a declaration (procedure, type, variable, etc.)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute {
    pub name: String,
    pub args: Vec<String>,
}

/// Map from symbol name to its attributes
#[derive(Debug, Clone, Default)]
pub struct AttributeMap {
    entries: HashMap<String, Vec<Attribute>>,
}

impl AttributeMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, symbol: &str, attr: Attribute) {
        self.entries
            .entry(symbol.to_lowercase())
            .or_default()
            .push(attr);
    }

    pub fn get(&self, symbol: &str) -> Option<&[Attribute]> {
        self.entries.get(&symbol.to_lowercase()).map(|v| v.as_slice())
    }

    pub fn has(&self, symbol: &str, attr_name: &str) -> bool {
        self.get(symbol)
            .map(|attrs| {
                attrs
                    .iter()
                    .any(|a| a.name.eq_ignore_ascii_case(attr_name))
            })
            .unwrap_or(false)
    }

    pub fn is_deprecated(&self, symbol: &str) -> bool {
        self.has(symbol, "deprecated")
    }
}

/// Parse `{$ATTR symbol attr [args...]}` directives from preprocessed source comments.
pub fn parse_attr_directives(source: &str) -> AttributeMap {
    let mut map = AttributeMap::new();
    for line in source.lines() {
        let trimmed = line.trim();
        if let Some(inner) = trimmed.strip_prefix("{$ATTR").and_then(|s| s.strip_suffix('}')) {
            let parts: Vec<&str> = inner.split_whitespace().collect();
            if parts.len() >= 2 {
                let symbol = parts[0].to_string();
                let name = parts[1].to_string();
                let args: Vec<String> = parts.iter().skip(2).map(|s| s.to_string()).collect();
                map.add(&symbol, Attribute { name, args });
            }
        }
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_attr_directives() {
        let src = "program P;\n{$ATTR MyProc deprecated}\nbegin\nend.";
        let map = parse_attr_directives(src);
        assert!(map.is_deprecated("MyProc"));
    }
}
