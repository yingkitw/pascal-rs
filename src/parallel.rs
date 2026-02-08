//! Parallel compilation support using rayon
//!
//! This module provides multi-threaded compilation capabilities for:
//! - Parallel module compilation
//! - Parallel optimization passes
//! - Concurrent PPU file loading

use crate::{Module, ModuleResult};
use rayon::prelude::*;
use std::sync::{Arc, Mutex};

/// Configuration for parallel compilation
#[derive(Debug, Clone)]
pub struct ParallelConfig {
    /// Number of threads to use (0 = auto-detect)
    pub num_threads: usize,

    /// Enable parallel module compilation
    pub parallel_modules: bool,

    /// Enable parallel optimization passes
    pub parallel_optimization: bool,

    /// Minimum number of modules to enable parallelization
    pub min_modules_for_parallel: usize,
}

impl Default for ParallelConfig {
    fn default() -> Self {
        Self {
            num_threads: 0, // Auto-detect
            parallel_modules: true,
            parallel_optimization: true,
            min_modules_for_parallel: 2,
        }
    }
}

impl ParallelConfig {
    /// Create a new parallel configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the number of threads
    pub fn with_threads(mut self, num_threads: usize) -> Self {
        self.num_threads = num_threads;
        self
    }

    /// Enable or disable parallel module compilation
    pub fn with_parallel_modules(mut self, enabled: bool) -> Self {
        self.parallel_modules = enabled;
        self
    }

    /// Enable or disable parallel optimization
    pub fn with_parallel_optimization(mut self, enabled: bool) -> Self {
        self.parallel_optimization = enabled;
        self
    }

    /// Initialize the thread pool
    pub fn init_thread_pool(&self) -> Result<(), String> {
        if self.num_threads > 0 {
            rayon::ThreadPoolBuilder::new()
                .num_threads(self.num_threads)
                .build_global()
                .map_err(|e| format!("Failed to initialize thread pool: {}", e))?;
        }
        Ok(())
    }
}

/// Parallel module compiler
pub struct ParallelCompiler {
    config: ParallelConfig,
}

impl ParallelCompiler {
    /// Create a new parallel compiler
    pub fn new(config: ParallelConfig) -> Self {
        Self { config }
    }

    /// Compile multiple modules in parallel
    /// Returns compiled modules or errors
    pub fn compile_modules_parallel<F>(
        &self,
        module_names: Vec<String>,
        compile_fn: F,
    ) -> Vec<ModuleResult<Module>>
    where
        F: Fn(&str) -> ModuleResult<Module> + Sync + Send,
    {
        if !self.config.parallel_modules
            || module_names.len() < self.config.min_modules_for_parallel
        {
            // Sequential compilation for small workloads
            return module_names.iter().map(|name| compile_fn(name)).collect();
        }

        // Parallel compilation
        module_names
            .par_iter()
            .map(|name| compile_fn(name))
            .collect()
    }

    /// Process optimization passes in parallel
    pub fn optimize_parallel<T, F>(&self, items: Vec<T>, optimize_fn: F) -> Vec<T>
    where
        T: Send,
        F: Fn(T) -> T + Sync + Send,
    {
        if !self.config.parallel_optimization || items.len() < 2 {
            return items.into_iter().map(optimize_fn).collect();
        }

        items.into_par_iter().map(optimize_fn).collect()
    }

    /// Load multiple PPU files in parallel
    pub fn load_ppu_files_parallel<F>(
        &self,
        unit_names: Vec<String>,
        load_fn: F,
    ) -> Vec<ModuleResult<crate::ast::Unit>>
    where
        F: Fn(&str) -> ModuleResult<crate::ast::Unit> + Sync + Send,
    {
        if unit_names.len() < self.config.min_modules_for_parallel {
            return unit_names.iter().map(|name| load_fn(name)).collect();
        }

        unit_names.par_iter().map(|name| load_fn(name)).collect()
    }
}

/// Thread-safe compilation progress tracker
pub struct ProgressTracker {
    total: usize,
    completed: Arc<Mutex<usize>>,
    errors: Arc<Mutex<Vec<String>>>,
}

impl ProgressTracker {
    /// Create a new progress tracker
    pub fn new(total: usize) -> Self {
        Self {
            total,
            completed: Arc::new(Mutex::new(0)),
            errors: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Mark one item as completed
    pub fn complete_one(&self) {
        let mut completed = self.completed.lock().unwrap();
        *completed += 1;
    }

    /// Add an error
    pub fn add_error(&self, error: String) {
        let mut errors = self.errors.lock().unwrap();
        errors.push(error);
    }

    /// Get the current progress (0.0 to 1.0)
    pub fn progress(&self) -> f64 {
        let completed = *self.completed.lock().unwrap();
        if self.total == 0 {
            1.0
        } else {
            completed as f64 / self.total as f64
        }
    }

    /// Get the number of completed items
    pub fn completed(&self) -> usize {
        *self.completed.lock().unwrap()
    }

    /// Get all errors
    pub fn errors(&self) -> Vec<String> {
        self.errors.lock().unwrap().clone()
    }

    /// Check if all items are completed
    pub fn is_complete(&self) -> bool {
        *self.completed.lock().unwrap() >= self.total
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::thread;
    use crate::ModuleError;
    use std::time::Duration;

    // Helper function to create a test module
    fn create_test_module(name: &str) -> Module {
        Module {
            name: name.to_string(),
            unit: crate::ast::Unit {
                name: name.to_string(),
                uses: vec![],
                interface: crate::ast::UnitInterface {
                    uses: vec![],
                    types: vec![],
                    constants: vec![],
                    variables: vec![],
                    procedures: vec![],
                    functions: vec![],
                    classes: vec![],
                    interfaces: vec![],
                },
                implementation: crate::ast::UnitImplementation {
                    uses: vec![],
                    types: vec![],
                    constants: vec![],
                    variables: vec![],
                    procedures: vec![],
                    functions: vec![],
                    classes: vec![],
                    interfaces: vec![],
                    initialization: None,
                    finalization: None,
                },
            },
            dependencies: vec![],
        }
    }

    #[test]
    fn test_parallel_config_default() {
        let config = ParallelConfig::default();

        assert_eq!(config.num_threads, 0); // Auto-detect
        assert!(config.parallel_modules);
        assert!(config.parallel_optimization);
        assert_eq!(config.min_modules_for_parallel, 2);
    }

    #[test]
    fn test_parallel_config_builder() {
        let config = ParallelConfig::new()
            .with_threads(4)
            .with_parallel_modules(true)
            .with_parallel_optimization(false);

        assert_eq!(config.num_threads, 4);
        assert!(config.parallel_modules);
        assert!(!config.parallel_optimization);
    }

    #[test]
    fn test_parallel_config_chaining() {
        let config = ParallelConfig::new()
            .with_threads(8)
            .with_parallel_modules(false)
            .with_parallel_optimization(true);

        assert_eq!(config.num_threads, 8);
        assert!(!config.parallel_modules);
        assert!(config.parallel_optimization);
    }

    #[test]
    fn test_progress_tracker_basic() {
        let tracker = ProgressTracker::new(10);

        assert_eq!(tracker.completed(), 0);
        assert_eq!(tracker.progress(), 0.0);
        assert!(!tracker.is_complete());

        tracker.complete_one();
        assert_eq!(tracker.completed(), 1);
        assert_eq!(tracker.progress(), 0.1);

        tracker.add_error("Test error".to_string());
        assert_eq!(tracker.errors().len(), 1);
    }

    #[test]
    fn test_progress_tracker_completion() {
        let tracker = ProgressTracker::new(5);

        for _ in 0..5 {
            tracker.complete_one();
        }

        assert_eq!(tracker.completed(), 5);
        assert_eq!(tracker.progress(), 1.0);
        assert!(tracker.is_complete());
    }

    #[test]
    fn test_progress_tracker_zero_total() {
        let tracker = ProgressTracker::new(0);

        assert_eq!(tracker.progress(), 1.0);
        assert!(tracker.is_complete());
    }

    #[test]
    fn test_progress_tracker_multiple_errors() {
        let tracker = ProgressTracker::new(10);

        tracker.add_error("Error 1".to_string());
        tracker.add_error("Error 2".to_string());
        tracker.add_error("Error 3".to_string());

        let errors = tracker.errors();
        assert_eq!(errors.len(), 3);
        assert_eq!(errors[0], "Error 1");
        assert_eq!(errors[1], "Error 2");
        assert_eq!(errors[2], "Error 3");
    }

    #[test]
    fn test_progress_tracker_thread_safety() {
        let tracker = ProgressTracker::new(100);
        let mut handles = vec![];

        // Spawn 10 threads, each completing 10 items
        for _ in 0..10 {
            let tracker_clone = ProgressTracker {
                total: tracker.total,
                completed: Arc::clone(&tracker.completed),
                errors: Arc::clone(&tracker.errors),
            };

            let handle = thread::spawn(move || {
                for _ in 0..10 {
                    tracker_clone.complete_one();
                    thread::sleep(Duration::from_micros(1));
                }
            });
            handles.push(handle);
        }

        // Wait for all threads
        for handle in handles {
            handle.join().unwrap();
        }

        assert_eq!(tracker.completed(), 100);
        assert_eq!(tracker.progress(), 1.0);
    }

    #[test]
    fn test_parallel_compiler_basic() {
        let config = ParallelConfig::new();
        let compiler = ParallelCompiler::new(config);

        let items = vec![1, 2, 3, 4, 5];
        let results = compiler.optimize_parallel(items, |x| x * 2);

        assert_eq!(results, vec![2, 4, 6, 8, 10]);
    }

    #[test]
    fn test_parallel_compiler_large_dataset() {
        let config = ParallelConfig::new();
        let compiler = ParallelCompiler::new(config);

        let items: Vec<i32> = (1..=1000).collect();
        let results = compiler.optimize_parallel(items, |x| x * x);

        assert_eq!(results.len(), 1000);
        assert_eq!(results[0], 1);
        assert_eq!(results[999], 1000000);
    }

    #[test]
    fn test_parallel_compiler_sequential_fallback() {
        let config = ParallelConfig::new().with_parallel_optimization(false);
        let compiler = ParallelCompiler::new(config);

        let items = vec![1, 2, 3];
        let results = compiler.optimize_parallel(items, |x| x + 1);

        assert_eq!(results, vec![2, 3, 4]);
    }

    #[test]
    fn test_parallel_compiler_small_workload_fallback() {
        let config = ParallelConfig::new();
        let compiler = ParallelCompiler::new(config);

        // Single item should use sequential processing
        let items = vec![42];
        let results = compiler.optimize_parallel(items, |x| x * 2);

        assert_eq!(results, vec![84]);
    }

    #[test]
    fn test_compile_modules_parallel_success() {
        let config = ParallelConfig::new();
        let compiler = ParallelCompiler::new(config);

        let modules = vec![
            "Module1".to_string(),
            "Module2".to_string(),
            "Module3".to_string(),
        ];

        let results = compiler.compile_modules_parallel(modules, |name| {
            // Simulate successful compilation
            Ok(create_test_module(name))
        });

        assert_eq!(results.len(), 3);
        assert!(results.iter().all(|r| r.is_ok()));
    }

    #[test]
    fn test_compile_modules_parallel_with_errors() {
        let config = ParallelConfig::new();
        let compiler = ParallelCompiler::new(config);

        let modules = vec!["Good1".to_string(), "Bad".to_string(), "Good2".to_string()];

        let results = compiler.compile_modules_parallel(modules, |name| {
            if name == "Bad" {
                Err(ModuleError::LoadError(
                    name.to_string(),
                    "Simulated error".to_string(),
                ))
            } else {
                Ok(create_test_module(name))
            }
        });

        assert_eq!(results.len(), 3);
        assert!(results[0].is_ok());
        assert!(results[1].is_err());
        assert!(results[2].is_ok());
    }

    #[test]
    fn test_compile_modules_sequential_for_small_workload() {
        let config = ParallelConfig::new();
        let compiler = ParallelCompiler::new(config);

        // Single module should use sequential processing
        let modules = vec!["SingleModule".to_string()];
        let call_count = Arc::new(AtomicUsize::new(0));
        let call_count_clone = Arc::clone(&call_count);

        let results = compiler.compile_modules_parallel(modules, move |name| {
            call_count_clone.fetch_add(1, Ordering::SeqCst);
            Ok(create_test_module(name))
        });

        assert_eq!(results.len(), 1);
        assert_eq!(call_count.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_parallel_optimization_preserves_order() {
        let config = ParallelConfig::new();
        let compiler = ParallelCompiler::new(config);

        let items: Vec<usize> = (0..100).collect();
        let results = compiler.optimize_parallel(items, |x| x);

        // Verify order is preserved
        for (i, &val) in results.iter().enumerate() {
            assert_eq!(i, val);
        }
    }

    #[test]
    fn test_parallel_compiler_with_complex_computation() {
        let config = ParallelConfig::new();
        let compiler = ParallelCompiler::new(config);

        let items = vec![10, 20, 30, 40, 50];
        let results = compiler.optimize_parallel(items, |x| {
            // Simulate complex computation
            let mut sum = 0;
            for i in 0..x {
                sum += i;
            }
            sum
        });

        assert_eq!(results.len(), 5);
        assert!(results.iter().all(|&x| x >= 0));
    }
}
