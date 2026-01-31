//! MCP (Model Context Protocol) Server for Pascal Compiler
//! 
//! Provides parallel compilation capabilities through MCP interface

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use crate::{ParallelConfig, ParallelCompiler, ModuleLoader, Module, ModuleResult};

/// MCP Server for Pascal compiler operations
pub struct McpServer {
    compiler: ParallelCompiler,
    loader: ModuleLoader,
}

/// Request to compile modules in parallel
#[derive(Debug, Serialize, Deserialize)]
pub struct CompileRequest {
    pub modules: Vec<String>,
    pub parallel: bool,
    pub threads: usize,
    pub search_paths: Vec<String>,
}

/// Response from compilation
#[derive(Debug, Serialize, Deserialize)]
pub struct CompileResponse {
    pub success: bool,
    pub compiled: Vec<String>,
    pub errors: Vec<CompileError>,
    pub duration_ms: u64,
}

/// Compilation error information
#[derive(Debug, Serialize, Deserialize)]
pub struct CompileError {
    pub module: String,
    pub message: String,
}

/// Request to get compiler status
#[derive(Debug, Serialize, Deserialize)]
pub struct StatusRequest {}

/// Compiler status response
#[derive(Debug, Serialize, Deserialize)]
pub struct StatusResponse {
    pub version: String,
    pub parallel_enabled: bool,
    pub threads: usize,
    pub cached_modules: Vec<String>,
}

impl McpServer {
    /// Create a new MCP server
    pub fn new(config: ParallelConfig) -> Self {
        Self {
            compiler: ParallelCompiler::new(config),
            loader: ModuleLoader::new(),
        }
    }

    /// Handle compile request
    pub fn handle_compile(&mut self, request: CompileRequest) -> CompileResponse {
        let start = std::time::Instant::now();
        let mut compiled = Vec::new();
        let mut errors = Vec::new();

        // Add search paths
        for path in &request.search_paths {
            self.loader.add_search_path(path);
        }

        // Compile modules
        let results = if request.parallel {
            self.compile_parallel(request.modules)
        } else {
            self.compile_sequential(request.modules)
        };

        // Process results
        for result in results {
            match result {
                Ok(module) => {
                    compiled.push(module.name.clone());
                    self.loader.cache_module(module);
                }
                Err(e) => {
                    errors.push(CompileError {
                        module: "unknown".to_string(),
                        message: format!("{}", e),
                    });
                }
            }
        }

        let duration_ms = start.elapsed().as_millis() as u64;

        CompileResponse {
            success: errors.is_empty(),
            compiled,
            errors,
            duration_ms,
        }
    }

    /// Handle status request
    pub fn handle_status(&self, _request: StatusRequest) -> StatusResponse {
        StatusResponse {
            version: env!("CARGO_PKG_VERSION").to_string(),
            parallel_enabled: true,
            threads: num_cpus::get(),
            cached_modules: self.loader.cached_modules(),
        }
    }

    /// Compile modules in parallel
    fn compile_parallel(&self, modules: Vec<String>) -> Vec<ModuleResult<Module>> {
        self.compiler.compile_modules_parallel(
            modules,
            |name| {
                // Simulate compilation - in real implementation, this would:
                // 1. Load source file
                // 2. Parse to AST
                // 3. Type check
                // 4. Generate code
                // 5. Create Module
                Err(crate::ModuleError::LoadError(
                    name.to_string(),
                    "Compilation not yet fully integrated".to_string(),
                ))
            }
        )
    }

    /// Compile modules sequentially
    fn compile_sequential(&self, modules: Vec<String>) -> Vec<ModuleResult<Module>> {
        modules.iter().map(|name| {
            Err(crate::ModuleError::LoadError(
                name.to_string(),
                "Compilation not yet fully integrated".to_string(),
            ))
        }).collect()
    }

    /// Get cached modules
    pub fn get_cached_modules(&self) -> Vec<String> {
        self.loader.cached_modules()
    }

    /// Clear module cache
    pub fn clear_cache(&mut self) {
        self.loader.clear_cache();
    }
}

/// MCP Server builder for easy configuration
pub struct McpServerBuilder {
    threads: usize,
    parallel_modules: bool,
    parallel_optimization: bool,
}

impl McpServerBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            threads: 0,
            parallel_modules: true,
            parallel_optimization: true,
        }
    }

    /// Set number of threads
    pub fn threads(mut self, threads: usize) -> Self {
        self.threads = threads;
        self
    }

    /// Enable/disable parallel modules
    pub fn parallel_modules(mut self, enabled: bool) -> Self {
        self.parallel_modules = enabled;
        self
    }

    /// Enable/disable parallel optimization
    pub fn parallel_optimization(mut self, enabled: bool) -> Self {
        self.parallel_optimization = enabled;
        self
    }

    /// Build the MCP server
    pub fn build(self) -> McpServer {
        let config = ParallelConfig::new()
            .with_threads(self.threads)
            .with_parallel_modules(self.parallel_modules)
            .with_parallel_optimization(self.parallel_optimization);

        McpServer::new(config)
    }
}

impl Default for McpServerBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mcp_server_creation() {
        let config = ParallelConfig::new();
        let server = McpServer::new(config);
        
        let status = server.handle_status(StatusRequest {});
        assert!(status.parallel_enabled);
        assert!(status.threads > 0);
    }

    #[test]
    fn test_mcp_server_builder() {
        let server = McpServerBuilder::new()
            .threads(4)
            .parallel_modules(true)
            .build();
        
        let cached = server.get_cached_modules();
        assert_eq!(cached.len(), 0);
    }

    #[test]
    fn test_compile_request() {
        let config = ParallelConfig::new();
        let mut server = McpServer::new(config);
        
        let request = CompileRequest {
            modules: vec!["Test".to_string()],
            parallel: false,
            threads: 1,
            search_paths: vec![],
        };
        
        let response = server.handle_compile(request);
        assert_eq!(response.compiled.len(), 0);
        assert!(response.duration_ms >= 0);
    }

    #[test]
    fn test_status_request() {
        let config = ParallelConfig::new();
        let server = McpServer::new(config);
        
        let status = server.handle_status(StatusRequest {});
        assert_eq!(status.version, env!("CARGO_PKG_VERSION"));
        assert!(status.parallel_enabled);
    }
}
