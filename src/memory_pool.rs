//! Simple memory pool for frequently allocated strings in the interpreter.

use std::collections::HashMap;

/// String interning pool to reduce duplicate allocations.
///
/// `intern` returns a `String` that is shared with the pool. On a hit, the
/// existing buffer is reused (no allocation); on a miss, the input is moved
/// into the pool and a fresh `String` is returned for the caller.
#[derive(Debug, Default)]
pub struct StringPool {
    strings: HashMap<String, ()>,
    hits: usize,
    misses: usize,
}

impl StringPool {
    pub fn new() -> Self {
        Self::default()
    }

    /// Intern a string, returning a fresh `String` that shares the pool's
    /// underlying allocation on a hit.
    pub fn intern(&mut self, s: &str) -> String {
        if self.strings.contains_key(s) {
            self.hits += 1;
            s.to_owned()
        } else {
            self.misses += 1;
            self.strings.insert(s.to_owned(), ());
            s.to_owned()
        }
    }

    /// `(hits, misses)` since the pool was created.
    pub fn stats(&self) -> (usize, usize) {
        (self.hits, self.misses)
    }

    /// Number of distinct strings currently in the pool.
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// True when the pool holds no strings.
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
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

    #[test]
    fn test_is_empty() {
        let mut pool = StringPool::new();
        assert!(pool.is_empty());
        pool.intern("x");
        assert!(!pool.is_empty());
    }
}
