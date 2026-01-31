//! Comprehensive threading tests for pascal-rs compiler
//! Tests parallel compilation, thread safety, and performance

use pascal::{ParallelConfig, ParallelCompiler, ProgressTracker, Module, ModuleError, ModuleResult};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::thread;

// Helper function to create a mock module
fn create_mock_module(name: &str) -> Module {
    Module {
        name: name.to_string(),
        // Add other required fields
        imports: Vec::new(),
        exports: Vec::new(),
        statements: Vec::new(),
        functions: Vec::new(),
        procedures: Vec::new(),
    }
}

// Helper function to simulate module compilation
fn compile_module_mock(name: &str) -> ModuleResult<Module> {
    // Simulate some work
    std::thread::sleep(Duration::from_millis(10));
    Ok(create_mock_module(name))
}

// Helper function to simulate failing compilation
fn compile_module_failing(name: &str) -> ModuleResult<Module> {
    if name == "fail" {
        Err(ModuleError::CompilationError {
            message: "Intentional failure".to_string(),
        })
    } else {
        compile_module_mock(name)
    }
}

#[test]
fn test_parallel_config_default() {
    let config = ParallelConfig::new();

    assert_eq!(config.num_threads, 0, "Default should be auto-detect");
    assert!(config.parallel_modules, "Parallel modules should be enabled by default");
    assert!(
        config.parallel_optimization,
        "Parallel optimization should be enabled by default"
    );
    assert_eq!(config.min_modules_for_parallel, 2, "Default min modules should be 2");
}

#[test]
fn test_parallel_config_with_threads() {
    let config = ParallelConfig::new().with_threads(4);

    assert_eq!(config.num_threads, 4);
}

#[test]
fn test_parallel_config_with_parallel_modules() {
    let config = ParallelConfig::new().with_parallel_modules(false);

    assert!(!config.parallel_modules);
}

#[test]
fn test_parallel_config_with_parallel_optimization() {
    let config = ParallelConfig::new().with_parallel_optimization(false);

    assert!(!config.parallel_optimization);
}

#[test]
fn test_parallel_config_builder_chain() {
    let config = ParallelConfig::new()
        .with_threads(8)
        .with_parallel_modules(true)
        .with_parallel_optimization(true);

    assert_eq!(config.num_threads, 8);
    assert!(config.parallel_modules);
    assert!(config.parallel_optimization);
}

#[test]
fn test_parallel_compiler_creation() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    // Just verify it's created successfully
    assert_eq!(compiler.config.num_threads, 0);
}

#[test]
fn test_parallel_compiler_with_config() {
    let config = ParallelConfig::new().with_threads(4);
    let compiler = ParallelCompiler::new(config);

    assert_eq!(compiler.config.num_threads, 4);
}

#[test]
fn test_progress_tracker_new() {
    let tracker = ProgressTracker::new(10);

    assert_eq!(tracker.completed(), 0);
    assert_eq!(tracker.total(), 10);
    assert_eq!(tracker.progress(), 0.0);
    assert!(tracker.errors().is_empty());
    assert!(!tracker.is_complete());
}

#[test]
fn test_progress_tracker_complete_one() {
    let tracker = ProgressTracker::new(10);

    tracker.complete_one();

    assert_eq!(tracker.completed(), 1);
    assert!((tracker.progress() - 0.1).abs() < 0.001);
    assert!(!tracker.is_complete());
}

#[test]
fn test_progress_tracker_complete_all() {
    let tracker = ProgressTracker::new(10);

    for _ in 0..10 {
        tracker.complete_one();
    }

    assert_eq!(tracker.completed(), 10);
    assert_eq!(tracker.progress(), 1.0);
    assert!(tracker.is_complete());
}

#[test]
fn test_progress_tracker_add_error() {
    let tracker = ProgressTracker::new(10);

    tracker.add_error("Error 1".to_string());
    tracker.add_error("Error 2".to_string());

    assert_eq!(tracker.errors().len(), 2);
    assert_eq!(tracker.errors()[0], "Error 1");
    assert_eq!(tracker.errors()[1], "Error 2");
}

#[test]
fn test_progress_tracker_with_completion_and_errors() {
    let tracker = ProgressTracker::new(10);

    // Complete 8 tasks
    for _ in 0..8 {
        tracker.complete_one();
    }

    // Add 2 errors
    tracker.add_error("Error 1".to_string());
    tracker.add_error("Error 2".to_string());

    assert_eq!(tracker.completed(), 8);
    assert_eq!(tracker.errors().len(), 2);
    assert!((tracker.progress() - 0.8).abs() < 0.001);
    assert!(!tracker.is_complete());
}

#[test]
fn test_progress_tracker_clone() {
    let tracker = Arc::new(ProgressTracker::new(10));

    // Simulate multiple threads updating progress
    let handles: Vec<_> = (0..5)
        .map(|_| {
            let tracker = Arc::clone(&tracker);
            thread::spawn(move || {
                for _ in 0..2 {
                    tracker.complete_one();
                    thread::sleep(Duration::from_millis(1));
                }
            })
        })
        .collect();

    // Wait for all threads
    for handle in handles {
        handle.join().unwrap();
    }

    assert_eq!(tracker.completed(), 10);
    assert!(tracker.is_complete());
}

#[test]
fn test_parallel_compilation_empty_modules() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let modules = Vec::new();
    let results = compiler.compile_modules_parallel(modules, compile_module_mock);

    assert!(results.is_empty());
}

#[test]
fn test_parallel_compilation_single_module() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let modules = vec!["module1".to_string()];
    let results = compiler.compile_modules_parallel(modules, compile_module_mock);

    assert_eq!(results.len(), 1);
    assert!(results[0].is_ok());
    assert_eq!(results[0].as_ref().unwrap().name, "module1");
}

#[test]
fn test_parallel_compilation_multiple_modules() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let modules = vec![
        "module1".to_string(),
        "module2".to_string(),
        "module3".to_string(),
    ];
    let results = compiler.compile_modules_parallel(modules, compile_module_mock);

    assert_eq!(results.len(), 3);
    assert!(results[0].is_ok());
    assert!(results[1].is_ok());
    assert!(results[2].is_ok());
}

#[test]
fn test_parallel_compilation_with_errors() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let modules = vec![
        "module1".to_string(),
        "fail".to_string(),
        "module3".to_string(),
    ];
    let results = compiler.compile_modules_parallel(modules, compile_module_failing);

    assert_eq!(results.len(), 3);
    assert!(results[0].is_ok());
    assert!(results[1].is_err());
    assert!(results[2].is_ok());
}

#[test]
fn test_parallel_compilation_sequential_below_threshold() {
    let config = ParallelConfig::new()
        .with_parallel_modules(true)
        .with_min_modules_for_parallel(5);
    let compiler = ParallelCompiler::new(config);

    let modules = vec!["module1".to_string(), "module2".to_string()];
    let results = compiler.compile_modules_parallel(modules, compile_module_mock);

    // Should still work, just sequentially
    assert_eq!(results.len(), 2);
    assert!(results[0].is_ok());
    assert!(results[1].is_ok());
}

#[test]
fn test_parallel_optimization_empty() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let items: Vec<i32> = Vec::new();
    let results = compiler.optimize_parallel(items, |x| x * 2);

    assert!(results.is_empty());
}

#[test]
fn test_parallel_optimization_single_item() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let items = vec![1];
    let results = compiler.optimize_parallel(items, |x| x * 2);

    assert_eq!(results.len(), 1);
    assert_eq!(results[0], 2);
}

#[test]
fn test_parallel_optimization_multiple_items() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let items = vec![1, 2, 3, 4, 5];
    let results = compiler.optimize_parallel(items, |x| x * 2);

    assert_eq!(results.len(), 5);
    assert_eq!(results, vec![2, 4, 6, 8, 10]);
}

#[test]
fn test_parallel_optimization_with_complex_function() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let items = vec![1, 2, 3, 4, 5];
    let results = compiler.optimize_parallel(items, |x| {
        // Simulate some work
        let mut result = x;
        for _ in 0..10 {
            result = result * 2 + 1;
        }
        result
    });

    assert_eq!(results.len(), 5);
}

#[test]
fn test_parallel_config_init_thread_pool_default() {
    let config = ParallelConfig::new();

    let result = config.init_thread_pool();
    assert!(result.is_ok());
}

#[test]
fn test_parallel_config_init_thread_pool_custom() {
    let config = ParallelConfig::new().with_threads(4);

    let result = config.init_thread_pool();
    // Might fail if thread pool already initialized
    // Just check it returns a Result
    let _ = result;
}

#[test]
fn test_parallel_compilation_thread_safety() {
    let config = ParallelConfig::new().with_threads(4);
    let compiler = ParallelCompiler::new(config);
    let tracker = Arc::new(ProgressTracker::new(100));

    let modules: Vec<String> = (0..100).map(|i| format!("module{}", i)).collect();
    let tracker_clone = Arc::clone(&tracker);

    let results = compiler.compile_modules_parallel(modules, |name| {
        tracker_clone.complete_one();
        compile_module_mock(name)
    });

    assert_eq!(results.len(), 100);
    assert!(tracker.is_complete());
}

#[test]
fn test_parallel_compilation_with_state() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    // Use a counter to verify all modules were processed
    let counter = Arc::new(Mutex::new(0));
    let modules: Vec<String> = (0..10).map(|i| format!("module{}", i)).collect();

    let counter_clone = Arc::clone(&counter);
    let results = compiler.compile_modules_parallel(modules, |name| {
        let mut count = counter_clone.lock().unwrap();
        *count += 1;
        drop(count);
        compile_module_mock(name)
    });

    assert_eq!(results.len(), 10);
    assert_eq!(*counter.lock().unwrap(), 10);
}

#[test]
fn test_parallel_compilation_ordering() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let modules = vec![
        "module1".to_string(),
        "module2".to_string(),
        "module3".to_string(),
    ];

    let results = compiler.compile_modules_parallel(modules, compile_module_mock);

    // Results should be in the same order as input
    assert_eq!(results.len(), 3);
    assert_eq!(results[0].as_ref().unwrap().name, "module1");
    assert_eq!(results[1].as_ref().unwrap().name, "module2");
    assert_eq!(results[2].as_ref().unwrap().name, "module3");
}

#[test]
fn test_parallel_compilation_disabled() {
    let config = ParallelConfig::new().with_parallel_modules(false);
    let compiler = ParallelCompiler::new(config);

    let modules = vec![
        "module1".to_string(),
        "module2".to_string(),
        "module3".to_string(),
    ];

    let results = compiler.compile_modules_parallel(modules, compile_module_mock);

    // Should still work, just sequentially
    assert_eq!(results.len(), 3);
    assert!(results[0].is_ok());
    assert!(results[1].is_ok());
    assert!(results[2].is_ok());
}

#[test]
fn test_parallel_optimization_disabled() {
    let config = ParallelConfig::new().with_parallel_optimization(false);
    let compiler = ParallelCompiler::new(config);

    let items = vec![1, 2, 3, 4, 5];
    let results = compiler.optimize_parallel(items, |x| x * 2);

    // Should still work
    assert_eq!(results.len(), 5);
}

#[test]
fn test_progress_tracker_thread_safety_concurrent_updates() {
    let tracker = Arc::new(ProgressTracker::new(1000));
    let handles: Vec<_> = (0..10)
        .map(|_| {
            let tracker = Arc::clone(&tracker);
            thread::spawn(move || {
                for _ in 0..100 {
                    tracker.complete_one();
                    tracker.add_error(format!("Error from thread"));
                }
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }

    assert_eq!(tracker.completed(), 1000);
    assert_eq!(tracker.errors().len(), 1000);
    assert!(tracker.is_complete());
}

#[test]
fn test_progress_tracker_zero_total() {
    let tracker = ProgressTracker::new(0);

    assert_eq!(tracker.total(), 0);
    assert_eq!(tracker.completed(), 0);
    assert!(tracker.is_complete());
}

#[test]
fn test_progress_tracker_fractional_progress() {
    let tracker = ProgressTracker::new(3);

    tracker.complete_one();

    assert!((tracker.progress() - 0.333).abs() < 0.01);

    tracker.complete_one();

    assert!((tracker.progress() - 0.666).abs() < 0.01);
}

#[test]
fn test_parallel_compilation_large_number_of_modules() {
    let config = ParallelConfig::new().with_threads(8);
    let compiler = ParallelCompiler::new(config);

    let modules: Vec<String> = (0..1000).map(|i| format!("module{}", i)).collect();
    let results = compiler.compile_modules_parallel(modules, compile_module_mock);

    assert_eq!(results.len(), 1000);

    // Verify all succeeded
    for result in &results {
        assert!(result.is_ok());
    }
}

#[test]
fn test_parallel_optimization_with_side_effects() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let items = vec![1, 2, 3, 4, 5];
    let results = compiler.optimize_parallel(items, |x| {
        // Simulate optimization pass
        if x % 2 == 0 {
            x * 2
        } else {
            x + 1
        }
    });

    assert_eq!(results.len(), 5);
    assert_eq!(results, vec![2, 4, 4, 8, 6]);
}

#[test]
fn test_parallel_config_min_modules_threshold() {
    let config = ParallelConfig::new()
        .with_parallel_modules(true)
        .with_min_modules_for_parallel(3);
    let compiler = ParallelCompiler::new(config);

    // Below threshold - should work sequentially
    let modules = vec!["module1".to_string(), "module2".to_string()];
    let results = compiler.compile_modules_parallel(modules, compile_module_mock);

    assert_eq!(results.len(), 2);

    // At threshold - should work in parallel
    let modules = vec![
        "module1".to_string(),
        "module2".to_string(),
        "module3".to_string(),
    ];
    let results = compiler.compile_modules_parallel(modules, compile_module_mock);

    assert_eq!(results.len(), 3);
}

#[test]
fn test_parallel_compilation_all_failures() {
    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    let modules = vec!["fail1".to_string(), "fail2".to_string(), "fail3".to_string()];

    let results = compiler.compile_modules_parallel(modules, |name| {
        Err(ModuleError::CompilationError {
            message: format!("Failed: {}", name),
        })
    });

    assert_eq!(results.len(), 3);
    assert!(results[0].is_err());
    assert!(results[1].is_err());
    assert!(results[2].is_err());
}

#[test]
fn test_progress_tracker_does_not_panic_on_over_completion() {
    let tracker = ProgressTracker::new(5);

    // Complete more than total
    for _ in 0..10 {
        tracker.complete_one();
    }

    // Should handle gracefully
    assert_eq!(tracker.completed(), 10);
    assert!(tracker.progress() > 1.0);
}

#[test]
fn test_parallel_config_debug_format() {
    let config = ParallelConfig::new()
        .with_threads(4)
        .with_parallel_modules(true)
        .with_parallel_optimization(false);

    // Just verify it can be formatted
    let debug_str = format!("{:?}", config);
    assert!(debug_str.contains("ParallelConfig"));
}

#[test]
fn test_parallel_compiler_send_sync() {
    // Verify ParallelCompiler implements Send and Sync
    fn assert_send_sync<T: Send + Sync>() {}

    assert_send_sync::<ParallelCompiler>();

    let config = ParallelConfig::new();
    let compiler = ParallelCompiler::new(config);

    // Verify it can be sent between threads
    let handle = thread::spawn(move || {
        // Use compiler in different thread
        let modules = vec!["test".to_string()];
        let _ = compiler.compile_modules_parallel(modules, compile_module_mock);
    });

    handle.join().unwrap();
}

#[test]
fn test_progress_tracker_send_sync() {
    // Verify ProgressTracker implements Send and Sync
    fn assert_send_sync<T: Send + Sync>() {}

    assert_send_sync::<ProgressTracker>();

    let tracker = Arc::new(ProgressTracker::new(10));

    let handle = thread::spawn(move || {
        tracker.complete_one();
        tracker.completed()
    });

    let completed = handle.join().unwrap();
    assert_eq!(completed, 1);
}
