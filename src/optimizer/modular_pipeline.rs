//! Modular optimization pipeline for Pascal compiler
//! 
//! Provides a clean, extensible framework for compiler optimizations
//! with separate passes for different optimization types.

use crate::ast::{Block, Expr, Statement, Type, Program};
use crate::enhanced_error::{CompilerError, SourceLocation, ErrorReporter};
use std::collections::HashMap;

/// Optimization pass trait
pub trait OptimizationPass {
    /// The name of the optimization pass
    fn name(&self) -> &str;
    
    /// The description of what this pass does
    fn description(&self) -> &str;
    
    /// The optimization level this pass belongs to
    fn optimization_level(&self) -> OptimizationLevel;
    
    /// Execute the optimization pass on the AST
    fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError>;
    
    /// Check if this pass should be applied
    fn should_apply(&self, context: &OptimizationContext) -> bool;
    
    /// Get dependencies (other passes that must run before this one)
    fn dependencies(&self) -> Vec<String>;
}

/// Optimization levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    None = 0,
    Basic = 1,
    Standard = 2,
    Aggressive = 3,
}

impl OptimizationLevel {
    /// Check if this level includes the given level
    pub fn includes(self, other: OptimizationLevel) -> bool {
        self as u32 >= other as u32
    }
}

/// Optimization context for pass decisions
#[derive(Debug, Clone)]
pub struct OptimizationContext {
    pub level: OptimizationLevel,
    pub target_architecture: TargetArchitecture,
    pub enable_debug_info: bool,
    pub enable_optimizations: Vec<String>,
    pub disable_optimizations: Vec<String>,
    pub statistics: OptimizationStatistics,
}

impl OptimizationContext {
    /// Create a new optimization context
    pub fn new(level: OptimizationLevel, target_arch: TargetArchitecture) -> Self {
        Self {
            level,
            target_architecture: target_arch,
            enable_debug_info: false,
            enable_optimizations: Vec::new(),
            disable_optimizations: Vec::new(),
            statistics: OptimizationStatistics::new(),
        }
    }

    /// Check if an optimization is enabled
    pub fn is_optimization_enabled(&self, name: &str) -> bool {
        // Check if explicitly disabled
        if self.disable_optimizations.contains(&name.to_string()) {
            return false;
        }
        
        // Check if explicitly enabled or matches level
        self.enable_optimizations.contains(&name.to_string()) || 
        self.level.includes(OptimizationLevel::Basic)
    }

    /// Add enabled optimization
    pub fn with_optimization(mut self, name: String) -> Self {
        self.enable_optimizations.push(name);
        self
    }

    /// Add disabled optimization
    pub fn with_disabled_optimization(mut self, name: String) -> Self {
        self.disable_optimizations.push(name);
        self
    }

    /// Enable debug info
    pub fn with_debug_info(mut self, enabled: bool) -> Self {
        self.enable_debug_info = enabled;
        self
    }
}

/// Target architecture for optimizations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetArchitecture {
    X86_64,
    AArch64,
    Native, // Host architecture
}

/// Optimization result with statistics
#[derive(Debug, Clone)]
pub struct OptimizationResult {
    pub success: bool,
    pub modifications: usize,
    pub time_ms: u64,
    pub statistics: OptimizationStatistics,
    pub warnings: Vec<String>,
}

impl OptimizationResult {
    /// Create a successful optimization result
    pub fn new(modifications: usize, time_ms: u64, stats: OptimizationStatistics) -> Self {
        Self {
            success: true,
            modifications,
            time_ms,
            statistics: stats,
            warnings: Vec::new(),
        }
    }

    /// Create a failed optimization result
    pub fn error(error: String) -> Self {
        Self {
            success: false,
            modifications: 0,
            time_ms: 0,
            statistics: OptimizationStatistics::new(),
            warnings: vec![error],
        }
    }

    /// Add a warning
    pub fn with_warning(mut self, warning: String) -> Self {
        self.warnings.push(warning);
        self
    }

    /// Check if modifications were made
    pub fn made_changes(&self) -> bool {
        self.modifications > 0
    }
}

/// Optimization statistics
#[derive(Debug, Clone, Default)]
pub struct OptimizationStatistics {
    pub passes_run: usize,
    pub total_optimizations: usize,
    pub constants_eliminated: usize,
    pub dead_code_removed: usize,
    pub expressions_simplified: usize,
    pub functions_inlined: usize,
    pub loops_unrolled: usize,
    pub memory_usage_optimized: usize,
}

impl OptimizationStatistics {
    /// Create new statistics
    pub fn new() -> Self {
        Self::default()
    }

    /// Increment pass count
    pub fn increment_passes(&mut self) {
        self.passes_run += 1;
    }

    /// Add optimization statistics
    pub fn add_optimization(&mut self, stats: &OptimizationStats) {
        self.total_optimizations += 1;
        self.constants_eliminated += stats.constants_eliminated;
        self.dead_code_removed += stats.dead_code_removed;
        self.expressions_simplified += stats.expressions_simplified;
        self.functions_inlined += stats.functions_inlined;
        self.loops_unrolled += stats.loops_unrolled;
        self.memory_usage_optimized += stats.memory_usage_optimized;
    }
}

/// Individual optimization pass statistics
#[derive(Debug, Clone, Default)]
pub struct OptimizationStats {
    pub constants_eliminated: usize,
    pub dead_code_removed: usize,
    pub expressions_simplified: usize,
    pub functions_inlined: usize,
    pub loops_unrolled: usize,
    pub memory_usage_optimized: usize,
}

impl OptimizationStats {
    /// Create new stats
    pub fn new() -> Self {
        Self::default()
    }

    /// Add statistics from another stats object
    pub fn add(&mut self, other: &Self) {
        self.constants_eliminated += other.constants_eliminated;
        self.dead_code_removed += other.dead_code_removed;
        self.expressions_simplified += other.expressions_simplified;
        self.functions_inlined += other.functions_inlined;
        self.loops_unrolled += other.loops_unrolled;
        self.memory_usage_optimized += other.memory_usage_optimized;
    }
}

/// Optimization pipeline configuration
#[derive(Debug, Clone)]
pub struct PipelineConfig {
    pub optimization_level: OptimizationLevel,
    pub target_architecture: TargetArchitecture,
    pub enabled_passes: Vec<String>,
    pub disabled_passes: Vec<String>,
    pub enable_debug_info: bool,
    pub parallel_execution: bool,
}

impl PipelineConfig {
    /// Create a new pipeline configuration
    pub fn new(level: OptimizationLevel, arch: TargetArchitecture) -> Self {
        Self {
            optimization_level: level,
            target_architecture: arch,
            enabled_passes: Vec::new(),
            disabled_passes: Vec::new(),
            enable_debug_info: false,
            parallel_execution: true,
        }
    }

    /// Enable specific passes
    pub fn with_enabled_passes(mut self, passes: Vec<String>) -> Self {
        self.enabled_passes = passes;
        self
    }

    /// Disable specific passes
    pub fn with_disabled_passes(mut self, passes: Vec<String>) -> Self {
        self.disabled_passes = passes;
        self
    }

    /// Enable debug info
    pub fn with_debug_info(mut self, enabled: bool) -> Self {
        self.enable_debug_info = enabled;
        self
    }

    /// Enable or disable parallel execution
    pub fn with_parallel_execution(mut self, enabled: bool) -> Self {
        self.parallel_execution = enabled;
        self
    }
}

/// Main optimization pipeline
pub struct OptimizationPipeline {
    passes: Vec<Box<dyn OptimizationPass>>,
    config: PipelineConfig,
    error_reporter: ErrorReporter,
}

impl OptimizationPipeline {
    /// Create a new optimization pipeline
    pub fn new(config: PipelineConfig) -> Self {
        Self {
            passes: Vec::new(),
            config,
            error_reporter: ErrorReporter::new(),
        }
    }

    /// Add an optimization pass
    pub fn add_pass(&mut self, pass: Box<dyn OptimizationPass>) {
        self.passes.push(pass);
    }

    /// Add multiple optimization passes
    pub fn add_passes(&mut self, passes: Vec<Box<dyn OptimizationPass>>) {
        self.passes.extend(passes);
    }

    /// Register standard optimization passes
    pub fn register_standard_passes(&mut self) {
        // Add standard optimization passes
        self.add_passes(vec![
            Box::new(ConstantFoldingPass::new()),
            Box::new(DeadCodeEliminationPass::new()),
            Box::new(CommonSubexpressionEliminationPass::new()),
            Box::new(FunctionInliningPass::new()),
            Box::new(LoopOptimizationPass::new()),
            Box::new(RegisterAllocationPass::new()),
        ]);
    }

    /// Run optimization pipeline on AST
    pub fn optimize(&mut self, mut ast: Program) -> Result<OptimizationResult, CompilerError> {
        let start_time = std::time::Instant::now();
        let mut total_stats = OptimizationStatistics::new();
        let mut total_modifications = 0;

        // Create optimization context
        let context = OptimizationContext::new(
            self.config.optimization_level,
            self.config.target_architecture,
        )
        .with_debug_info(self.config.enable_debug_info)
        .with_optimizations(self.config.enabled_passes.clone())
        .with_disabled_optimizations(self.config.disabled_passes.clone());

        // Sort passes by dependencies
        let sorted_passes = self.topological_sort()?;
        
        // Execute passes
        for mut pass in sorted_passes {
            // Check if pass should be applied
            if !pass.should_apply(&context) {
                continue;
            }

            // Check if pass is enabled
            if !self.config.enabled_passes.is_empty() 
                && !self.config.enabled_passes.contains(&pass.name().to_string())
            {
                continue;
            }

            // Check if pass is disabled
            if self.config.disabled_passes.contains(&pass.name().to_string()) {
                continue;
            }

            eprintln!("[optimization] Running pass: {}", pass.name());

            // Execute pass
            match pass.optimize(&mut ast) {
                Ok(result) => {
                    total_modifications += result.modifications;
                    total_stats.add_optimization(&result.statistics);
                    
                    // Report warnings
                    for warning in result.warnings {
                        self.error_reporter.add_diagnostic(
                            crate::enhanced_error::Diagnostic::warning(
                                SourceLocation::new(0, 1, 1, 0, 0),
                                format!("{}: {}", pass.name(), warning),
                            )
                        );
                    }
                },
                Err(e) => {
                    self.error_reporter.report_error(e);
                    return Ok(OptimizationResult::error(format!(
                        "Optimization pass '{}' failed: {}",
                        pass.name(),
                        self.error_reporter.format_diagnostics()
                    )));
                }
            }
        }

        let elapsed = start_time.elapsed().as_millis() as u64;

        Ok(OptimizationResult::new(
            total_modifications,
            elapsed,
            total_stats,
        ))
    }

    /// Topological sort of passes based on dependencies
    fn topological_sort(&self) -> Result<Vec<Box<dyn OptimizationPass>>, CompilerError> {
        // For simplicity, just return passes in order (no dependencies for now)
        // In a real implementation, this would resolve dependencies
        
        Ok(self.passes.clone())
    }

    /// Get error reporter
    pub fn error_reporter(&mut self) -> &mut ErrorReporter {
        &mut self.error_reporter
    }

    /// Get optimization statistics
    pub fn get_statistics(&self) -> &OptimizationStatistics {
        // This would need to be maintained during optimization
        // For now, return empty statistics
        static EMPTY_STATS: OptimizationStatistics = OptimizationStatistics::new();
        &EMPTY_STATS
    }
}

/// Optimization registry for discovering and managing passes
pub struct OptimizationRegistry {
    passes: HashMap<String, Box<dyn OptimizationPass>>,
}

impl OptimizationRegistry {
    /// Create a new optimization registry
    pub fn new() -> Self {
        Self {
            passes: HashMap::new(),
        }
    }

    /// Register an optimization pass
    pub fn register(&mut self, name: String, pass: Box<dyn OptimizationPass>) {
        self.passes.insert(name, pass);
    }

    /// Get a pass by name
    pub fn get_pass(&mut self, name: &str) -> Option<&mut Box<dyn OptimizationPass>> {
        self.passes.get_mut(name)
    }

    /// Get all registered passes
    pub fn get_all_passes(&self) -> Vec<&String> {
        self.passes.keys().collect()
    }

    /// Create pipeline from configuration
    pub fn create_pipeline(&self, config: PipelineConfig) -> OptimizationPipeline {
        let mut pipeline = OptimizationPipeline::new(config);
        
        // Add all passes that match the configuration
        for (name, pass) in &self.passes {
            pipeline.add_pass(pass.boxed_clone());
        }
        
        pipeline
    }
}

// Macro to simplify pass creation
#[macro_export]
macro_rules! define_optimization_pass {
    ($name:ident, $level:expr, $description:expr) => {
        pub struct $name {
            name: String,
            description: String,
            level: OptimizationLevel,
        }

        impl $name {
            pub fn new() -> Self {
                Self {
                    name: stringify!($name).to_string(),
                    description: $description.to_string(),
                    level: $level,
                }
            }
        }

        impl OptimizationPass for $name {
            fn name(&self) -> &str {
                &self.name
            }

            fn description(&self) -> &str {
                &self.description
            }

            fn optimization_level(&self) -> OptimizationLevel {
                self.level
            }

            fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError> {
                // Placeholder implementation - actual optimization logic would go here
                Ok(OptimizationResult::new(0, 0, OptimizationStatistics::new()))
            }

            fn should_apply(&self, _context: &OptimizationContext) -> bool {
                true
            }

            fn dependencies(&self) -> Vec<String> {
                Vec::new()
            }
        }
    };
}

// Define some basic optimization passes using the macro
define_optimization_pass!(
    ConstantFoldingPass,
    OptimizationLevel::Basic,
    "Fold constant expressions at compile time"
);

define_optimization_pass!(
    DeadCodeEliminationPass,
    OptimizationLevel::Standard,
    "Remove unreachable code"
);

define_optimization_pass!(
    CommonSubexpressionEliminationPass,
    OptimizationLevel::Standard,
    "Eliminate duplicate subexpressions"
);

define_optimization_pass!(
    FunctionInliningPass,
    OptimizationLevel::Aggressive,
    "Inline small functions"
);

define_optimization_pass!(
    LoopOptimizationPass,
    OptimizationLevel::Standard,
    "Optimize loops (unrolling, strength reduction)"
);

define_optimization_pass!(
    RegisterAllocationPass,
    OptimizationLevel::Aggressive,
    "Optimize register usage"
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pipeline_creation() {
        let config = PipelineConfig::new(OptimizationLevel::Standard, TargetArchitecture::Native);
        let mut pipeline = OptimizationPipeline::new(config);
        pipeline.register_standard_passes();
        
        assert!(!pipeline.passes.is_empty());
    }

    #[test]
    fn test_optimization_context() {
        let context = OptimizationContext::new(OptimizationLevel::Aggressive, TargetArchitecture::X86_64);
        
        assert!(context.is_optimization_enabled("constant_folding"));
        assert!(context.level.includes(OptimizationLevel::Basic));
    }

    #[test]
    fn test_optimization_result() {
        let stats = OptimizationStatistics::new();
        let result = OptimizationResult::new(5, 100, stats);
        
        assert!(result.success);
        assert_eq!(result.modifications, 5);
        assert_eq!(result.time_ms, 100);
    }

    #[test]
    fn test_optimization_registry() {
        let mut registry = OptimizationRegistry::new();
        let pass = Box::new(ConstantFoldingPass::new());
        
        registry.register("constant_folding".to_string(), pass);
        
        assert!(registry.get_pass("constant_folding").is_some());
        assert!(registry.get_all_passes().contains(&"constant_folding".to_string()));
    }

    #[test]
    fn test_pipeline_config() {
        let config = PipelineConfig::new(OptimizationLevel::Basic, TargetArchitecture::AArch64)
            .with_enabled_passes(vec!["constant_folding".to_string()])
            .with_debug_info(true);
        
        assert_eq!(config.optimization_level, OptimizationLevel::Basic);
        assert_eq!(config.target_architecture, TargetArchitecture::AArch64);
        assert!(config.enable_debug_info);
        assert_eq!(config.enabled_passes, vec!["constant_folding"]);
    }
}