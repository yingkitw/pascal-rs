//! Simple memory pool for frequently allocated strings in the interpreter.

use std::collections::HashMap;

/// String interning pool to reduce duplicate allocations.
#[derive(Debug, Default)]
pub struct StringPool {
    strings: HashMap<String, String>,
    hits: usize,
    misses: usize,
}

impl StringPool {
    pub fn new() -> Self {
        Self::default()
    }

    /// Intern a string, returning a shared owned copy.
    pub fn intern(&mut self, s: &str) -> String {
        if let Some(existing) = self.strings.get(s) {
            self.hits += 1;
            return existing.clone();
        }
        self.misses += 1;
        let owned = s.to_string();
        self.strings.insert(owned.clone(), owned.clone());
        owned
    }

    pub fn stats(&self) -> (usize, usize) {
        (self.hits, self.misses)
    }

    pub fn len(&self) -> usize {
        self.strings.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_interning() {
        let mut pool = StringPool::new();
        let a = pool.intern("hello");
        let b = pool.intern("hello");
        assert_eq!(a, b);
        assert_eq!(pool.stats().0, 1);
    }
}
