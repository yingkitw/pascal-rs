//! Optimizer trait definitions

use anyhow::Result;

/// Core optimization capability
pub trait Optimizer {
    /// Optimize the generated assembly
    fn optimize(&mut self, assembly: &str) -> Result<String>;

    /// Get optimization statistics
    fn statistics(&self) -> OptimizationStats;
}

/// Optimization statistics
#[derive(Debug, Clone, Default)]
pub struct OptimizationStats {
    pub passes_run: usize,
    pub instructions_removed: usize,
    pub instructions_added: usize,
    pub registers_saved: usize,
    pub functions_inlined: usize,
    pub loops_unrolled: usize,
}

/// Constant folding optimization
pub trait ConstantFolding {
    /// Fold constant expressions
    fn fold_constants(&mut self, assembly: &str) -> Result<String>;

    /// Evaluate constant expression at compile time
    fn evaluate_constant(&self, expr: &str) -> Option<i64>;
}

/// Dead code elimination
pub trait DeadCodeElimination {
    /// Remove unreachable code
    fn remove_dead_code(&mut self, assembly: &str) -> Result<String>;

    /// Remove unused variables
    fn remove_unused_variables(&mut self, assembly: &str) -> Result<String>;

    /// Remove empty blocks
    fn remove_empty_blocks(&mut self, assembly: &str) -> Result<String>;
}

/// Common subexpression elimination
pub trait CommonSubexprElimination {
    /// Eliminate common subexpressions
    fn eliminate_common_subexprs(&mut self, assembly: &str) -> Result<String>;

    /// Find common subexpressions
    fn find_common_subexprs(&self, assembly: &str) -> Vec<String>;
}

/// Function inlining
pub trait FunctionInlining {
    /// Inline simple functions
    fn inline_functions(&mut self, assembly: &str) -> Result<String>;

    /// Check if function should be inlined
    fn should_inline(&self, func_name: &str, func_size: usize) -> bool;

    /// Get inlining threshold
    fn inlining_threshold(&self) -> usize;
}

/// Loop optimization
pub trait LoopOptimizer {
    /// Unroll loops
    fn unroll_loops(&mut self, assembly: &str) -> Result<String>;

    /// Vectorize loops (SIMD)
    fn vectorize_loops(&mut self, assembly: &str) -> Result<String>;

    /// Move loop-invariant code
    fn hoist_loop_invariants(&mut self, assembly: &str) -> Result<String>;

    /// Induction variable optimization
    fn optimize_induction_vars(&mut self, assembly: &str) -> Result<String>;
}

/// Register optimization
pub trait RegisterOptimizer {
    /// Optimize register usage
    fn optimize_registers(&mut self, assembly: &str) -> Result<String>;

    /// Allocate registers optimally
    fn allocate_registers(&mut self, assembly: &str) -> Result<String>;

    /// Spill registers to memory
    fn spill_registers(&mut self, assembly: &str) -> Result<String>;
}

/// Peephole optimization
pub trait PeepholeOptimizer {
    /// Apply peephole optimizations
    fn optimize_peephole(&mut self, assembly: &str) -> Result<String>;

    /// Apply specific peephole pattern
    fn apply_pattern(&mut self, pattern: &str, replacement: &str) -> Result<String>;

    /// Get peephole patterns
    fn peephole_patterns(&self) -> Vec<(String, String)>;
}

/// Strength reduction optimization
pub trait StrengthReduction {
    /// Replace expensive operations with cheaper ones
    fn reduce_strength(&mut self, assembly: &str) -> Result<String>;

    /// Replace multiplication with shifts
    fn replace_mul_with_shift(&mut self, assembly: &str) -> Result<String>;

    /// Replace division with shifts
    fn replace_div_with_shift(&mut self, assembly: &str) -> Result<String>;
}

/// Tail call optimization
pub trait TailCallOptimization {
    /// Optimize tail calls
    fn optimize_tail_calls(&mut self, assembly: &str) -> Result<String>;

    /// Detect tail call position
    fn is_tail_call(&self, instruction: &str) -> bool;
}

/// Inline assembly support
pub trait InlineAssembly {
    /// Parse inline assembly
    fn parse_inline_asm(&mut self, asm: &str) -> Result<Vec<String>>;

    /// Validate inline assembly
    fn validate_inline_asm(&mut self, asm: &str) -> Result<bool>;

    /// Integrate inline assembly
    fn integrate_inline_asm(&mut self, assembly: &str, inline: &str) -> Result<String>;
}
