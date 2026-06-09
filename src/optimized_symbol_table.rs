//! Optimized symbol table for Pascal compiler
//! 
//! Provides high-performance symbol lookup with LRU caching,
//! efficient data structures, and better memory management.

use crate::ast::Type;
use anyhow::{anyhow, Result};
use std::collections::{HashMap, hash_map::Entry};
use std::rc::Rc;
use std::cell::RefCell;

/// Function signature information (Rc for memory efficiency)
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Rc<Vec<(String, Type)>>, // Rc for shared parameter lists
    pub return_type: Type,
    pub is_external: bool,
    pub external_name: Option<String>,
}

/// Symbol information with optimized layout
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub typ: Type,
    pub offset: i32,
    pub flags: SymbolFlags,
    pub const_value: Option<ConstValue>,
    pub function_signature: Option<FunctionSignature>,
}

/// Bit flags for symbol properties (memory efficient)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SymbolFlags {
    bits: u8,
}

impl SymbolFlags {
    /// Create new flags from individual properties
    pub fn new(is_parameter: bool, is_exported: bool, is_const: bool) -> Self {
        let mut bits = 0;
        if is_parameter { bits |= 0b001; }
        if is_exported { bits |= 0b010; }
        if is_const { bits |= 0b100; }
        Self { bits }
    }

    /// Check if symbol is a parameter
    pub fn is_parameter(self) -> bool {
        self.bits & 0b001 != 0
    }

    /// Check if symbol is exported
    pub fn is_exported(self) -> bool {
        self.bits & 0b010 != 0
    }

    /// Check if symbol is a constant
    pub fn is_const(self) -> bool {
        self.bits & 0b100 != 0
    }
}

/// Constant value for compile-time evaluation
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Integer(i64),
    Real(f64),
    Boolean(bool),
    String(String),
    Char(char),
}

/// Optimized scope with capacity limits and efficient storage
#[derive(Debug, Clone)]
pub struct OptimizedScope {
    symbols: HashMap<String, Symbol>,
    parent: Option<usize>,
    stack_offset: i32,
    capacity_limit: usize,
    symbol_count: usize,
}

impl OptimizedScope {
    /// Create a new scope with optional capacity limit
    pub fn new(parent: Option<usize>, capacity_limit: usize) -> Self {
        Self {
            symbols: HashMap::with_capacity(capacity_limit),
            parent,
            stack_offset: 0,
            capacity_limit,
            symbol_count: 0,
        }
    }

    /// Check if scope has capacity for more symbols
    pub fn has_capacity(&self) -> bool {
        self.symbol_count < self.capacity_limit
    }

    /// Get symbol count
    pub fn symbol_count(&self) -> usize {
        self.symbol_count
    }

    /// Add symbol with capacity check
    pub fn add_symbol(&mut self, name: String, symbol: Symbol) -> Result<()> {
        if !self.has_capacity() {
            return Err(anyhow!("Scope capacity exceeded"));
        }

        match self.symbols.entry(name) {
            Entry::Occupied(_) => Err(anyhow!("Symbol already exists in scope")),
            Entry::Vacant(entry) => {
                entry.insert(symbol);
                self.symbol_count += 1;
                Ok(())
            }
        }
    }

    /// Remove symbol and update count
    pub fn remove_symbol(&mut self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.symbols.remove(name) {
            self.symbol_count = self.symbol_count.saturating_sub(1);
            Some(symbol)
        } else {
            None
        }
    }

    /// Clear all symbols
    pub fn clear(&mut self) {
        self.symbols.clear();
        self.symbol_count = 0;
    }

    /// Get memory usage estimate
    pub fn memory_usage(&self) -> usize {
        // Rough estimate: string names + symbol data
        self.symbols.iter()
            .map(|(name, _)| name.len() * std::mem::size_of::<u8>())
            .sum::<usize>() + self.symbol_count * std::mem::size_of::<Symbol>()
    }
}

/// LRU Cache for frequently accessed symbols
#[derive(Debug, Clone)]
pub struct SymbolLRUCache {
    cache: HashMap<String, (usize, Rc<Symbol>)>, // (name, (scope_id, symbol))
    capacity: usize,
    access_order: Vec<String>, // Track access order for LRU
}

impl SymbolLRUCache {
    /// Create a new LRU cache with specified capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            cache: HashMap::with_capacity(capacity),
            capacity,
            access_order: Vec::new(),
        }
    }

    /// Get symbol from cache, updating access order
    pub fn get(&mut self, name: &str) -> Option<(usize, Rc<Symbol>)> {
        if let Some((scope_id, symbol)) = self.cache.get(name) {
            // Update access order
            if let Some(pos) = self.access_order.iter().position(|n| n == name) {
                self.access_order.remove(pos);
            }
            self.access_order.push(name.to_string());
            
            Some((*scope_id, Rc::clone(symbol)))
        } else {
            None
        }
    }

    /// Put symbol into cache, evicting LRU if necessary
    pub fn put(&mut self, name: String, scope_id: usize, symbol: Rc<Symbol>) {
        // Evict LRU if cache is full
        if self.cache.len() >= self.capacity {
            if let Some(lru_name) = self.access_order.first() {
                self.cache.remove(lru_name);
                self.access_order.remove(0);
            }
        }

        self.cache.insert(name.clone(), (scope_id, symbol));
        self.access_order.push(name);
    }

    /// Clear cache
    pub fn clear(&mut self) {
        self.cache.clear();
        self.access_order.clear();
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        CacheStats {
            capacity: self.capacity,
            size: self.cache.len(),
            hit_rate: 0.0, // Would need to track hits/misses in real implementation
        }
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub capacity: usize,
    pub size: usize,
    pub hit_rate: f64,
}

/// Optimized symbol table with LRU caching and efficient data structures
#[derive(Debug)]
pub struct OptimizedSymbolTable {
    scopes: Vec<OptimizedScope>,
    current_scope: usize,
    next_offset: i32,
    cache: SymbolLRUCache,
    symbol_stats: SymbolStatistics,
}

/// Symbol access statistics
#[derive(Debug, Clone)]
pub struct SymbolStatistics {
    total_lookups: u64,
    cache_hits: u64,
    cache_misses: u64,
    scope_hits: u64,
    scope_misses: u64,
}

impl SymbolStatistics {
    /// Create new statistics
    pub fn new() -> Self {
        Self {
            total_lookups: 0,
            cache_hits: 0,
            cache_misses: 0,
            scope_hits: 0,
            scope_misses: 0,
        }
    }

    /// Record a cache hit
    pub fn record_cache_hit(&mut self) {
        self.total_lookups += 1;
        self.cache_hits += 1;
    }

    /// Record a cache miss
    pub fn record_cache_miss(&mut self) {
        self.total_lookups += 1;
        self.cache_misses += 1;
    }

    /// Record a scope hit
    pub fn record_scope_hit(&mut self) {
        self.scope_hits += 1;
    }

    /// Record a scope miss
    pub fn record_scope_miss(&mut self) {
        self.scope_misses += 1;
    }

    /// Get cache hit rate
    pub fn cache_hit_rate(&self) -> f64 {
        if self.total_lookups == 0 {
            0.0
        } else {
            self.cache_hits as f64 / self.total_lookups as f64
        }
    }

    /// Get scope hit rate
    pub fn scope_hit_rate(&self) -> f64 {
        if self.scope_hits + self.scope_misses == 0 {
            0.0
        } else {
            self.scope_hits as f64 / (self.scope_hits + self.scope_misses) as f64
        }
    }

    /// Reset statistics
    pub fn reset(&mut self) {
        *self = Self::new();
    }
}

impl OptimizedSymbolTable {
    /// Create a new optimized symbol table
    pub fn new(cache_capacity: usize) -> Self {
        let global_scope = OptimizedScope::new(None, 1024); // Large capacity for global scope
        
        Self {
            scopes: vec![global_scope],
            current_scope: 0,
            next_offset: 8, // Start after base pointer
            cache: SymbolLRUCache::new(cache_capacity),
            symbol_stats: SymbolStatistics::new(),
        }
    }

    /// Create with custom cache size
    pub fn with_cache_capacity(cache_capacity: usize) -> Self {
        Self::new(cache_capacity)
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        let parent = self.current_scope;
        let new_scope = OptimizedScope::new(Some(parent), 256); // Default 256 symbols per scope
        
        self.scopes.push(new_scope);
        self.current_scope = self.scopes.len() - 1;
    }

    /// Exit current scope
    pub fn exit_scope(&mut self) -> Result<()> {
        if self.current_scope == 0 {
            return Err(anyhow!("Cannot exit global scope"));
        }

        // Clear cache entries from exited scope
        self.cache_clear_scope(self.current_scope);

        if let Some(_) = self.scopes[self.current_scope].parent {
            let parent_scope = self.scopes[self.current_scope].parent.unwrap();
            
            // Remove the current scope from the vector
            self.scopes.remove(self.current_scope);
            
            // Update current scope to parent (which might have shifted due to removal)
            self.current_scope = if parent_scope < self.scopes.len() {
                parent_scope
            } else {
                0 // Should not happen in valid hierarchy
            };
            
            Ok(())
        } else {
            Err(anyhow!("Invalid scope hierarchy"))
        }
    }

    /// Clear cache entries for a specific scope
    fn cache_clear_scope(&mut self, scope_id: usize) {
        self.cache.cache.retain(|_, (cached_scope_id, _)| *cached_scope_id != scope_id);
    }

    /// Add a symbol to the current scope
    pub fn add_symbol(
        &mut self,
        name: String,
        typ: Type,
        is_parameter: bool,
        is_exported: bool,
    ) -> Result<i32> {
        // Check if symbol already exists in current scope
        if self.scopes[self.current_scope].symbols.contains_key(&name) {
            return Err(anyhow!(
                "Symbol '{}' already defined in current scope",
                name
            ));
        }

        let offset = self.next_offset;
        self.next_offset += self.get_type_size(&typ);

        let flags = SymbolFlags::new(is_parameter, is_exported, false);
        let symbol = Symbol {
            name: name.clone(),
            typ,
            offset,
            flags,
            const_value: None,
            function_signature: None,
        };

        self.scopes[self.current_scope].add_symbol(name, symbol)?;
        Ok(offset)
    }

    /// Add a function symbol with signature
    pub fn add_function(
        &mut self,
        name: String,
        parameters: Vec<(String, Type)>,
        return_type: Type,
        is_external: bool,
        external_name: Option<String>,
        is_exported: bool,
    ) -> Result<()> {
        // Check if symbol already exists in current scope
        if self.scopes[self.current_scope].symbols.contains_key(&name) {
            return Err(anyhow!(
                "Function '{}' already defined in current scope",
                name
            ));
        }

        let parameters_rc = Rc::new(parameters);
        let signature = FunctionSignature {
            name: name.clone(),
            parameters: parameters_rc,
            return_type: return_type.clone(),
            is_external,
            external_name,
        };

        let flags = SymbolFlags::new(false, is_exported, false);
        let symbol = Symbol {
            name: name.clone(),
            typ: return_type,
            offset: 0, // Functions don't have stack offsets
            flags,
            const_value: None,
            function_signature: Some(signature),
        };

        self.scopes[self.current_scope].add_symbol(name, symbol)?;
        Ok(())
    }

    /// Look up a symbol with optimized cache
    pub fn lookup_symbol(&mut self, name: &str) -> Option<Rc<Symbol>> {
        // First check cache
        if let Some((scope_id, symbol)) = self.cache.get(name) {
            self.symbol_stats.record_cache_hit();
            return Some(symbol);
        }

        // Cache miss, perform full lookup
        self.symbol_stats.record_cache_miss();

        let mut scope_idx = self.current_scope;
        let mut found_symbol = None;

        loop {
            if let Some(symbol) = self.scopes[scope_idx].symbols.get(name) {
                found_symbol = Some(Rc::new(symbol.clone()));
                self.symbol_stats.record_scope_hit();
                break;
            }

            if let Some(parent) = self.scopes[scope_idx].parent {
                scope_idx = parent;
            } else {
                self.symbol_stats.record_scope_miss();
                break;
            }
        }

        // Add to cache if found
        if let Some(ref symbol) = found_symbol {
            let scope_id = self.find_symbol_scope(name).unwrap_or(self.current_scope);
            self.cache.put(name.to_string(), scope_id, Rc::clone(symbol));
        }

        found_symbol
    }

    /// Find the scope containing a symbol
    fn find_symbol_scope(&self, name: &str) -> Option<usize> {
        let mut scope_idx = self.current_scope;
        
        loop {
            if self.scopes[scope_idx].symbols.contains_key(name) {
                return Some(scope_idx);
            }

            if let Some(parent) = self.scopes[scope_idx].parent {
                scope_idx = parent;
            } else {
                return None;
            }
        }
    }

    /// Look up a function signature with caching
    pub fn lookup_function(&mut self, name: &str) -> Option<FunctionSignature> {
        if let Some(symbol) = self.lookup_symbol(name) {
            if let Some(ref sig) = symbol.function_signature {
                return Some(sig.clone());
            }
        }
        None
    }

    /// Get type size (simplified version)
    fn get_type_size(&self, typ: &Type) -> i32 {
        match typ {
            Type::Simple(simple_type) => match simple_type {
                crate::ast::SimpleType::Integer => 8,
                crate::ast::SimpleType::Real => 8,
                crate::ast::SimpleType::Boolean => 1,
                crate::ast::SimpleType::Char => 1,
                crate::ast::SimpleType::String => 8, // Pointer
                _ => 8,
            },
            Type::Array { index_type, element_type, range } => {
                let element_size = self.get_type_size(element_type.as_ref());
                // Calculate array size based on range or default size
                if let Some((start, end)) = range {
                    ((end - start + 1) * element_size as i64) as i32
                } else {
                    // Default size for arrays without explicit range
                    (256 * element_size as i64) as i32
                }
            },
            Type::Record { fields, .. } => {
                fields.iter().map(|f| self.get_type_size(f.1.as_ref())).sum()
            },
            _ => 8, // Default pointer size
        }
    }

    /// Get cache statistics
    pub fn cache_stats(&self) -> CacheStats {
        self.cache.stats()
    }

    /// Get symbol statistics
    pub fn symbol_stats(&self) -> &SymbolStatistics {
        &self.symbol_stats
    }

    /// Get memory usage statistics
    pub fn memory_usage(&self) -> MemoryUsage {
        let total_scope_memory = self.scopes.iter()
            .map(|scope| scope.memory_usage())
            .sum();
        
        let cache_memory = self.cache.cache.iter()
            .map(|(name, (_, symbol))| name.len() * std::mem::size_of::<u8>() + std::mem::size_of::<Symbol>())
            .sum();

        MemoryUsage {
            scopes: total_scope_memory,
            cache: cache_memory,
            total: total_scope_memory + cache_memory,
            scope_count: self.scopes.len(),
            cache_capacity: self.cache.capacity,
        }
    }

    /// Clear all caches and reset statistics
    pub fn reset(&mut self) {
        self.cache.clear();
        self.symbol_stats.reset();
    }

    /// Optimize cache based on access patterns
    pub fn optimize_cache(&mut self) {
        // Remove least recently used items if cache is over 80% capacity
        if self.cache.cache.len() * 100 / self.cache.capacity > 80 {
            let items_to_remove = self.cache.cache.len() / 10; // Remove 10% of items
            
            for _ in 0..items_to_remove {
                if let Some(lru_name) = self.cache.access_order.first() {
                    self.cache.cache.remove(lru_name);
                    self.cache.access_order.remove(0);
                }
            }
        }
    }
}

/// Memory usage statistics
#[derive(Debug, Clone)]
pub struct MemoryUsage {
    pub scopes: usize,
    pub cache: usize,
    pub total: usize,
    pub scope_count: usize,
    pub cache_capacity: usize,
}

/// Symbol table builder for easy construction
pub struct SymbolTableBuilder {
    table: OptimizedSymbolTable,
}

impl SymbolTableBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            table: OptimizedSymbolTable::new(256),
        }
    }

    /// With custom cache size
    pub fn with_cache_capacity(capacity: usize) -> Self {
        Self {
            table: OptimizedSymbolTable::with_cache_capacity(capacity),
        }
    }

    /// Add a variable
    pub fn add_variable(
        mut self,
        name: String,
        typ: Type,
        is_exported: bool,
    ) -> Result<Self> {
        self.table.add_symbol(name, typ, false, is_exported)?;
        Ok(self)
    }

    /// Add a parameter
    pub fn add_parameter(
        mut self,
        name: String,
        typ: Type,
    ) -> Result<Self> {
        self.table.add_symbol(name, typ, true, false)?;
        Ok(self)
    }

    /// Add a function
    pub fn add_function(
        mut self,
        name: String,
        parameters: Vec<(String, Type)>,
        return_type: Type,
        is_external: bool,
        external_name: Option<String>,
        is_exported: bool,
    ) -> Result<Self> {
        self.table.add_function(
            name, parameters, return_type, is_external, external_name, is_exported
        )?;
        Ok(self)
    }

    /// Enter a scope
    pub fn enter_scope(mut self) -> Self {
        self.table.enter_scope();
        self
    }

    /// Exit scope
    pub fn exit_scope(mut self) -> Result<Self> {
        self.table.exit_scope()?;
        Ok(self)
    }

    /// Build the symbol table
    pub fn build(self) -> OptimizedSymbolTable {
        self.table
    }
}

impl Default for SymbolTableBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_creation() {
        let table = OptimizedSymbolTable::new(100);
        assert_eq!(table.scopes.len(), 1); // Global scope
        assert_eq!(table.current_scope, 0);
    }

    #[test]
    fn test_scope_management() {
        let mut table = OptimizedSymbolTable::new(100);
        
        table.enter_scope();
        assert_eq!(table.scopes.len(), 2);
        assert_eq!(table.current_scope, 1);
        
        table.exit_scope().unwrap();
        assert_eq!(table.scopes.len(), 1);
        assert_eq!(table.current_scope, 0);
    }

    #[test]
    fn test_symbol_addition() {
        let mut table = OptimizedSymbolTable::new(100);
        
        let result = table.add_symbol(
            "x".to_string(),
            Type::Simple(crate::ast::SimpleType::Integer),
            false,
            false,
        );
        
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 8); // Default offset
    }

    #[test]
    fn test_symbol_lookup() {
        let mut table = OptimizedSymbolTable::new(100);
        
        table.add_symbol(
            "x".to_string(),
            Type::Simple(crate::ast::SimpleType::Integer),
            false,
            false,
        ).unwrap();
        
        let symbol = table.lookup_symbol("x");
        assert!(symbol.is_some());
        assert_eq!(symbol.unwrap().name, "x");
    }

    #[test]
    fn test_cache_functionality() {
        let mut table = OptimizedSymbolTable::new(2);
        
        // Add some symbols
        table.add_symbol("a".to_string(), Type::Simple(crate::ast::SimpleType::Integer), false, false).unwrap();
        table.add_symbol("b".to_string(), Type::Simple(crate::ast::SimpleType::Integer), false, false).unwrap();
        
        // Access symbols to populate cache
        table.lookup_symbol("a");
        table.lookup_symbol("b");
        
        let stats = table.cache_stats();
        assert_eq!(stats.capacity, 2);
        assert_eq!(stats.size, 2);
    }

    #[test]
    fn test_symbol_flags() {
        let flags = SymbolFlags::new(true, false, true);
        assert!(flags.is_parameter());
        assert!(!flags.is_exported());
        assert!(flags.is_const());
    }

    #[test]
    fn test_builder_pattern() {
        let mut table = SymbolTableBuilder::new()
            .add_variable("x".to_string(), Type::Simple(crate::ast::SimpleType::Integer), false)
            .unwrap()
            .add_parameter("y".to_string(), Type::Simple(crate::ast::SimpleType::Real))
            .unwrap()
            .build();
        
        assert!(table.lookup_symbol("x").is_some());
        assert!(table.lookup_symbol("y").is_some());
    }
}