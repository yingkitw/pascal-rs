//! Modular optimization pipeline for Pascal compiler

use crate::ast::Program;
use crate::enhanced_error::{CompilerError, Diagnostic, ErrorReporter, SourceLocation};
use std::collections::HashMap;

/// Optimization pass trait
pub trait OptimizationPass {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn optimization_level(&self) -> OptimizationLevel;
    fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError>;
    fn should_apply(&self, context: &OptimizationContext) -> bool;
    fn dependencies(&self) -> Vec<String>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    None = 0,
    Basic = 1,
    Standard = 2,
    Aggressive = 3,
}

impl OptimizationLevel {
    pub fn includes(self, other: OptimizationLevel) -> bool {
        self as u32 >= other as u32
    }

    pub fn from_u8(level: u8) -> Self {
        match level {
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Basic,
            2 => OptimizationLevel::Standard,
            _ => OptimizationLevel::Aggressive,
        }
    }
}

#[derive(Debug, Clone)]
pub struct OptimizationContext {
    pub level: OptimizationLevel,
    pub target_architecture: TargetArchitecture,
    pub enable_debug_info: bool,
    pub enable_optimizations: Vec<String>,
    pub disable_optimizations: Vec<String>,
}

impl OptimizationContext {
    pub fn new(level: OptimizationLevel, target_arch: TargetArchitecture) -> Self {
        Self {
            level,
            target_architecture: target_arch,
            enable_debug_info: false,
            enable_optimizations: Vec::new(),
            disable_optimizations: Vec::new(),
        }
    }

    pub fn with_optimizations(mut self, names: Vec<String>) -> Self {
        self.enable_optimizations = names;
        self
    }

    pub fn with_disabled_optimizations(mut self, names: Vec<String>) -> Self {
        self.disable_optimizations = names;
        self
    }

    pub fn with_debug_info(mut self, enabled: bool) -> Self {
        self.enable_debug_info = enabled;
        self
    }

    pub fn is_pass_enabled(&self, pass_name: &str) -> bool {
        if self.disable_optimizations.iter().any(|n| n == pass_name) {
            return false;
        }
        if !self.enable_optimizations.is_empty() {
            return self.enable_optimizations.iter().any(|n| n == pass_name);
        }
        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetArchitecture {
    X86_64,
    AArch64,
    Native,
}

#[derive(Debug, Clone)]
pub struct OptimizationResult {
    pub success: bool,
    pub modifications: usize,
    pub time_ms: u64,
    pub stats: OptimizationStats,
    pub warnings: Vec<String>,
}

impl OptimizationResult {
    pub fn new(modifications: usize, time_ms: u64, stats: OptimizationStats) -> Self {
        Self {
            success: true,
            modifications,
            time_ms,
            stats,
            warnings: Vec::new(),
        }
    }

    pub fn error(error: String) -> Self {
        Self {
            success: false,
            modifications: 0,
            time_ms: 0,
            stats: OptimizationStats::new(),
            warnings: vec![error],
        }
    }

    pub fn made_changes(&self) -> bool {
        self.modifications > 0
    }
}

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
    pub const fn new() -> Self {
        Self {
            passes_run: 0,
            total_optimizations: 0,
            constants_eliminated: 0,
            dead_code_removed: 0,
            expressions_simplified: 0,
            functions_inlined: 0,
            loops_unrolled: 0,
            memory_usage_optimized: 0,
        }
    }

    pub fn add_pass(&mut self, stats: &OptimizationStats) {
        self.passes_run += 1;
        self.total_optimizations += stats.total();
        self.constants_eliminated += stats.constants_eliminated;
        self.dead_code_removed += stats.dead_code_removed;
        self.expressions_simplified += stats.expressions_simplified;
        self.functions_inlined += stats.functions_inlined;
        self.loops_unrolled += stats.loops_unrolled;
        self.memory_usage_optimized += stats.memory_usage_optimized;
    }
}

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
    pub fn new() -> Self {
        Self::default()
    }

    pub fn total(&self) -> usize {
        self.constants_eliminated
            + self.dead_code_removed
            + self.expressions_simplified
            + self.functions_inlined
            + self.loops_unrolled
            + self.memory_usage_optimized
    }
}

#[derive(Debug, Clone)]
pub struct PipelineConfig {
    pub optimization_level: OptimizationLevel,
    pub target_architecture: TargetArchitecture,
    pub enabled_passes: Vec<String>,
    pub disabled_passes: Vec<String>,
    pub enable_debug_info: bool,
}

impl PipelineConfig {
    pub fn new(level: OptimizationLevel, arch: TargetArchitecture) -> Self {
        Self {
            optimization_level: level,
            target_architecture: arch,
            enabled_passes: Vec::new(),
            disabled_passes: Vec::new(),
            enable_debug_info: false,
        }
    }

    pub fn with_enabled_passes(mut self, passes: Vec<String>) -> Self {
        self.enabled_passes = passes;
        self
    }

    pub fn with_disabled_passes(mut self, passes: Vec<String>) -> Self {
        self.disabled_passes = passes;
        self
    }

    pub fn with_debug_info(mut self, enabled: bool) -> Self {
        self.enable_debug_info = enabled;
        self
    }
}

pub struct OptimizationPipeline {
    passes: Vec<Box<dyn OptimizationPass>>,
    config: PipelineConfig,
    error_reporter: ErrorReporter,
    statistics: OptimizationStatistics,
}

impl OptimizationPipeline {
    pub fn new(config: PipelineConfig) -> Self {
        Self {
            passes: Vec::new(),
            config,
            error_reporter: ErrorReporter::new(),
            statistics: OptimizationStatistics::new(),
        }
    }

    pub fn add_pass(&mut self, pass: Box<dyn OptimizationPass>) {
        self.passes.push(pass);
    }

    pub fn register_standard_passes(&mut self) {
        use super::passes::{
            CommonSubexpressionEliminationPass, ConstantFoldingPass, DeadCodeEliminationPass,
            FunctionInliningPass, LoopOptimizationPass, RegisterAllocationPass,
        };

        self.add_pass(Box::new(ConstantFoldingPass::new()));
        self.add_pass(Box::new(DeadCodeEliminationPass::new()));
        self.add_pass(Box::new(CommonSubexpressionEliminationPass::new()));
        self.add_pass(Box::new(LoopOptimizationPass::new()));
        self.add_pass(Box::new(FunctionInliningPass::new()));
        self.add_pass(Box::new(RegisterAllocationPass::new()));
    }

    pub fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError> {
        let start_time = std::time::Instant::now();
        let mut total_modifications = 0;
        let mut aggregate_stats = OptimizationStats::new();

        let context = OptimizationContext::new(
            self.config.optimization_level,
            self.config.target_architecture,
        )
        .with_debug_info(self.config.enable_debug_info)
        .with_optimizations(self.config.enabled_passes.clone())
        .with_disabled_optimizations(self.config.disabled_passes.clone());

        for pass in &mut self.passes {
            if !pass.should_apply(&context) || !context.is_pass_enabled(pass.name()) {
                continue;
            }

            match pass.optimize(ast) {
                Ok(result) => {
                    total_modifications += result.modifications;
                    aggregate_stats.constants_eliminated += result.stats.constants_eliminated;
                    aggregate_stats.dead_code_removed += result.stats.dead_code_removed;
                    aggregate_stats.expressions_simplified += result.stats.expressions_simplified;
                    aggregate_stats.functions_inlined += result.stats.functions_inlined;
                    aggregate_stats.loops_unrolled += result.stats.loops_unrolled;
                    aggregate_stats.memory_usage_optimized += result.stats.memory_usage_optimized;
                    self.statistics.add_pass(&result.stats);

                    for warning in result.warnings {
                        self.error_reporter.add_diagnostic(Diagnostic::warning(
                            SourceLocation::new(0, 1, 1, 0, 0),
                            format!("{}: {}", pass.name(), warning),
                        ));
                    }
                }
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
            aggregate_stats,
        ))
    }

    pub fn statistics(&self) -> &OptimizationStatistics {
        &self.statistics
    }
}

pub struct OptimizationRegistry {
    pass_factories: HashMap<String, fn() -> Box<dyn OptimizationPass>>,
}

impl OptimizationRegistry {
    pub fn new() -> Self {
        Self {
            pass_factories: HashMap::new(),
        }
    }

    pub fn register(&mut self, name: String, factory: fn() -> Box<dyn OptimizationPass>) {
        self.pass_factories.insert(name, factory);
    }

    pub fn create_pipeline(&self, config: PipelineConfig) -> OptimizationPipeline {
        let mut pipeline = OptimizationPipeline::new(config);
        for factory in self.pass_factories.values() {
            pipeline.add_pass(factory());
        }
        pipeline
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Block;

    #[test]
    fn test_pipeline_creation() {
        let config = PipelineConfig::new(OptimizationLevel::Standard, TargetArchitecture::Native);
        let mut pipeline = OptimizationPipeline::new(config);
        pipeline.register_standard_passes();
        assert_eq!(pipeline.passes.len(), 6);
    }

    #[test]
    fn test_optimization_level_from_u8() {
        assert_eq!(OptimizationLevel::from_u8(0), OptimizationLevel::None);
        assert_eq!(OptimizationLevel::from_u8(2), OptimizationLevel::Standard);
        assert_eq!(OptimizationLevel::from_u8(9), OptimizationLevel::Aggressive);
    }

    #[test]
    fn test_optimize_program_constant_folding() {
        use crate::ast::{Expr, Literal, Program, Stmt};

        let mut program = Program {
            name: "Fold".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                statements: vec![Stmt::Assignment {
                    target: Expr::Variable("x".to_string()),
                    value: Expr::BinaryOp {
                        operator: "+".to_string(),
                        left: Box::new(Expr::Literal(Literal::Integer(2))),
                        right: Box::new(Expr::Literal(Literal::Integer(3))),
                    },
                }],
            },
        };

        let config = PipelineConfig::new(OptimizationLevel::Basic, TargetArchitecture::Native);
        let mut pipeline = OptimizationPipeline::new(config);
        pipeline.register_standard_passes();
        let result = pipeline.optimize(&mut program).unwrap();
        assert!(result.success);
        if let Stmt::Assignment { value, .. } = &program.block.statements[0] {
            assert_eq!(value, &Expr::Literal(Literal::Integer(5)));
        } else {
            panic!("expected assignment");
        }
    }
}
