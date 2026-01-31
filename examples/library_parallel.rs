//! Example: Using parallel compilation as a library
//! 
//! This example demonstrates how to use pascal-rs as a library
//! with parallel compilation capabilities.

use pascal::{
    ParallelConfig, ParallelCompiler, ProgressTracker,
    ModuleLoader, Module, ModuleResult,
};

fn main() {
    println!("=== Pascal Compiler - Library API Examples ===\n");

    // Example 1: Basic parallel configuration
    println!("1. Creating parallel configuration:");
    let config = ParallelConfig::new()
        .with_threads(4)
        .with_parallel_modules(true)
        .with_parallel_optimization(true);
    
    println!("   Threads: {}", if config.num_threads == 0 { 
        format!("{} (auto)", num_cpus::get()) 
    } else { 
        config.num_threads.to_string() 
    });
    println!("   Parallel modules: {}", config.parallel_modules);
    println!("   Parallel optimization: {}", config.parallel_optimization);
    println!();

    // Example 2: Initialize thread pool
    println!("2. Initializing thread pool:");
    match config.init_thread_pool() {
        Ok(_) => println!("   Thread pool initialized successfully"),
        Err(e) => println!("   Error: {}", e),
    }
    println!();

    // Example 3: Create parallel compiler
    println!("3. Creating parallel compiler:");
    let compiler = ParallelCompiler::new(config);
    println!("   Compiler created");
    println!();

    // Example 4: Parallel optimization
    println!("4. Running parallel optimization:");
    let data = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let optimized = compiler.optimize_parallel(data, |x| x * x);
    println!("   Input: [1..10]");
    println!("   Output: {:?}", optimized);
    println!();

    // Example 5: Progress tracking
    println!("5. Using progress tracker:");
    let tracker = ProgressTracker::new(100);
    
    for i in 0..100 {
        tracker.complete_one();
        if i % 20 == 19 {
            println!("   Progress: {:.0}%", tracker.progress() * 100.0);
        }
    }
    println!("   Completed: {}/{}", tracker.completed(), 100);
    println!("   Is complete: {}", tracker.is_complete());
    println!();

    // Example 6: Module loader with thread safety
    println!("6. Using thread-safe module loader:");
    let mut loader = ModuleLoader::new();
    loader.add_search_path("./stdlib");
    loader.add_search_path("./lib");

    println!("   Cached modules: {}", loader.cached_modules().len());
    
    // Clone for thread
    let loader_clone = loader.clone_for_thread();
    println!("   Cloned loader for thread (shares cache)");
    println!();

    // Example 7: Simulated parallel module compilation
    println!("7. Simulating parallel module compilation:");
    let module_names = vec![
        "System".to_string(),
        "SysUtils".to_string(),
        "Classes".to_string(),
    ];
    
    let results = compiler.compile_modules_parallel(
        module_names.clone(),
        |name| {
            // Simulate compilation work
            std::thread::sleep(std::time::Duration::from_millis(10));
            
            // Return mock result
            create_mock_module(name)
        }
    );
    
    let successful = results.iter().filter(|r| r.is_ok()).count();
    println!("   Modules attempted: {}", module_names.len());
    println!("   Successful: {}", successful);
    println!("   Failed: {}", results.len() - successful);
    println!();

    // Example 8: Large dataset parallel processing
    println!("8. Processing large dataset in parallel:");
    let large_data: Vec<i32> = (1..=1000).collect();
    let start = std::time::Instant::now();
    
    let processed = compiler.optimize_parallel(large_data, |x| {
        // Simulate complex computation
        (0..x).sum::<i32>()
    });
    
    let duration = start.elapsed();
    println!("   Items processed: {}", processed.len());
    println!("   Duration: {:?}", duration);
    println!("   Throughput: {:.0} items/sec", 
        1000.0 / duration.as_secs_f64());
    println!();

    // Example 9: Error handling in parallel compilation
    println!("9. Error handling in parallel compilation:");
    let modules_with_errors = vec![
        "Good1".to_string(),
        "Bad".to_string(),
        "Good2".to_string(),
    ];
    
    let results = compiler.compile_modules_parallel(
        modules_with_errors,
        |name| {
            if name == "Bad" {
                Err(pascal::ModuleError::LoadError(
                    name.to_string(),
                    "Simulated error".to_string()
                ))
            } else {
                create_mock_module(name)
            }
        }
    );
    
    for (i, result) in results.iter().enumerate() {
        match result {
            Ok(module) => println!("   [{}] Success: {}", i, module.name),
            Err(e) => println!("   [{}] Error: {}", i, e),
        }
    }
    println!();

    // Example 10: Performance comparison
    println!("10. Performance comparison (parallel vs sequential):");
    let test_data: Vec<i32> = (1..=100).collect();
    
    // Sequential
    let start = std::time::Instant::now();
    let _seq_result: Vec<i32> = test_data.iter().map(|&x| x * x).collect();
    let seq_duration = start.elapsed();
    
    // Parallel
    let start = std::time::Instant::now();
    let _par_result = compiler.optimize_parallel(test_data.clone(), |x| x * x);
    let par_duration = start.elapsed();
    
    println!("   Sequential: {:?}", seq_duration);
    println!("   Parallel: {:?}", par_duration);
    println!("   Speedup: {:.2}x", 
        seq_duration.as_secs_f64() / par_duration.as_secs_f64());
    println!();

    println!("=== Library API Examples Complete ===");
}

// Helper function to create a mock module for testing
fn create_mock_module(name: &str) -> ModuleResult<Module> {
    Ok(Module {
        name: name.to_string(),
        unit: pascal::Unit {
            name: name.to_string(),
            uses: vec![],
            interface: pascal::ast::UnitInterface {
                uses: vec![],
                types: std::collections::HashMap::new(),
                constants: std::collections::HashMap::new(),
                variables: std::collections::HashMap::new(),
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                interfaces: vec![],
            },
            implementation: pascal::ast::UnitImplementation {
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
    })
}
