//! Example: Using parallel compilation via MCP Server
//! 
//! This example demonstrates how to use the MCP server interface
//! for parallel compilation of Pascal modules.

use pascal::{McpServer, McpServerBuilder, CompileRequest, StatusRequest};

fn main() {
    println!("=== Pascal Compiler - MCP Server Examples ===\n");

    // Example 1: Create MCP server with default configuration
    println!("1. Creating MCP server with default configuration:");
    let server = McpServerBuilder::new().build();
    
    let status = server.handle_status(StatusRequest {});
    println!("   Version: {}", status.version);
    println!("   Parallel enabled: {}", status.parallel_enabled);
    println!("   Threads: {}", status.threads);
    println!("   Cached modules: {}", status.cached_modules.len());
    println!();

    // Example 2: Create MCP server with custom configuration
    println!("2. Creating MCP server with custom configuration:");
    let mut server = McpServerBuilder::new()
        .threads(4)
        .parallel_modules(true)
        .parallel_optimization(true)
        .build();
    println!("   Server created with 4 threads");
    println!();

    // Example 3: Compile single module (sequential)
    println!("3. Compiling single module (sequential):");
    let request = CompileRequest {
        modules: vec!["System".to_string()],
        parallel: false,
        threads: 1,
        search_paths: vec!["./stdlib".to_string()],
    };
    
    let response = server.handle_compile(request);
    println!("   Success: {}", response.success);
    println!("   Compiled: {:?}", response.compiled);
    println!("   Errors: {}", response.errors.len());
    println!("   Duration: {}ms", response.duration_ms);
    println!();

    // Example 4: Compile multiple modules in parallel
    println!("4. Compiling multiple modules in parallel:");
    let request = CompileRequest {
        modules: vec![
            "System".to_string(),
            "SysUtils".to_string(),
            "Classes".to_string(),
            "Math".to_string(),
        ],
        parallel: true,
        threads: 4,
        search_paths: vec!["./stdlib".to_string()],
    };
    
    let response = server.handle_compile(request);
    println!("   Success: {}", response.success);
    println!("   Compiled: {} modules", response.compiled.len());
    println!("   Errors: {}", response.errors.len());
    println!("   Duration: {}ms", response.duration_ms);
    
    for error in &response.errors {
        println!("   Error in {}: {}", error.module, error.message);
    }
    println!();

    // Example 5: Check cached modules
    println!("5. Checking cached modules:");
    let cached = server.get_cached_modules();
    println!("   Cached modules: {}", cached.len());
    for module in &cached {
        println!("     - {}", module);
    }
    println!();

    // Example 6: Clear cache
    println!("6. Clearing module cache:");
    server.clear_cache();
    let cached = server.get_cached_modules();
    println!("   Cached modules after clear: {}", cached.len());
    println!();

    // Example 7: Large batch compilation
    println!("7. Large batch compilation (parallel):");
    let modules: Vec<String> = (1..=20)
        .map(|i| format!("Module{}", i))
        .collect();
    
    let request = CompileRequest {
        modules,
        parallel: true,
        threads: 8,
        search_paths: vec!["./lib".to_string()],
    };
    
    let response = server.handle_compile(request);
    println!("   Compiled: {} modules", response.compiled.len());
    println!("   Errors: {}", response.errors.len());
    println!("   Duration: {}ms", response.duration_ms);
    println!("   Throughput: {:.2} modules/sec", 
        20.0 / (response.duration_ms as f64 / 1000.0));
    println!();

    println!("=== MCP Server Examples Complete ===");
}
