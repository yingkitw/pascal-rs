//! Real optimization passes wired to `Optimizer` and `AdvancedOptimizer`.

use std::collections::HashMap;

use crate::ast::{Block, Expr, FunctionDecl, Literal, Program, Stmt};
use crate::enhanced_error::CompilerError;

use super::advanced::{AdvancedOptimizer, CSEOptimizer, FunctionInliner, LoopOptimizer};
use super::modular_pipeline::{
    OptimizationContext, OptimizationLevel, OptimizationPass, OptimizationResult,
    OptimizationStats,
};
use super::Optimizer;

fn optimize_expr_in_place(expr: &mut Expr, optimizer: &Optimizer) {
    *expr = optimizer.optimize_expr(expr);
}

fn fold_block(block: &mut Block, optimizer: &Optimizer) -> usize {
    let mut modifications = 0;

    for stmt in &mut block.statements {
        if optimize_stmt_tree(stmt, optimizer) {
            modifications += 1;
        }
    }

    for proc in &mut block.procedures {
        modifications += fold_block(&mut proc.block, optimizer);
    }
    for func in &mut block.functions {
        modifications += fold_block(&mut func.block, optimizer);
    }

    modifications
}

fn optimize_stmt_tree(stmt: &mut Stmt, optimizer: &Optimizer) -> bool {
    let mut changed = false;

    match stmt {
        Stmt::Assignment { value, .. } => {
            let before = format!("{value:?}");
            optimize_expr_in_place(value, optimizer);
            if format!("{value:?}") != before {
                changed = true;
            }
        }
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            optimize_expr_in_place(condition, optimizer);
            if let Expr::Literal(Literal::Boolean(val)) = &*condition {
                if *val {
                    if then_branch.len() == 1 {
                        *stmt = then_branch[0].clone();
                    } else {
                        *stmt = Stmt::Block(Block::with_statements(then_branch.clone()));
                    }
                } else if let Some(else_stmts) = else_branch {
                    if else_stmts.len() == 1 {
                        *stmt = else_stmts[0].clone();
                    } else {
                        *stmt = Stmt::Block(Block::with_statements(else_stmts.clone()));
                    }
                } else {
                    return false;
                }
                return true;
            }
            for s in then_branch {
                changed |= optimize_stmt_tree(s, optimizer);
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    changed |= optimize_stmt_tree(s, optimizer);
                }
            }
        }
        Stmt::While { condition, body } => {
            optimize_expr_in_place(condition, optimizer);
            if matches!(condition, Expr::Literal(Literal::Boolean(false))) {
                return false;
            }
            for s in body {
                changed |= optimize_stmt_tree(s, optimizer);
            }
        }
        Stmt::For { start, end, body, .. } => {
            optimize_expr_in_place(start, optimizer);
            optimize_expr_in_place(end, optimizer);
            for s in body {
                changed |= optimize_stmt_tree(s, optimizer);
            }
        }
        Stmt::Repeat {
            body,
            until_condition,
        } => {
            for s in body {
                changed |= optimize_stmt_tree(s, optimizer);
            }
            optimize_expr_in_place(until_condition, optimizer);
        }
        Stmt::Case {
            expression,
            branches,
            else_branch,
        } => {
            optimize_expr_in_place(expression, optimizer);
            for branch in branches {
                if let Some(guard) = &mut branch.guard {
                    optimize_expr_in_place(guard, optimizer);
                }
                for s in &mut branch.body {
                    changed |= optimize_stmt_tree(s, optimizer);
                }
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    changed |= optimize_stmt_tree(s, optimizer);
                }
            }
        }
        Stmt::Block(b) => {
            changed |= fold_block(b, optimizer) > 0;
        }
        Stmt::ProcedureCall { arguments, .. } => {
            for arg in arguments {
                optimize_expr_in_place(arg, optimizer);
            }
        }
        _ => {}
    }

    changed
}

fn apply_advanced_expr(block: &mut Block, advanced: &mut AdvancedOptimizer) -> usize {
    let mut modifications = 0;

    fn walk_expr(expr: &mut Expr, advanced: &mut AdvancedOptimizer, mods: &mut usize) {
        match expr {
            Expr::BinaryOp { left, right, .. } => {
                walk_expr(left, advanced, mods);
                walk_expr(right, advanced, mods);
            }
            Expr::UnaryOp { operand, .. } => walk_expr(operand, advanced, mods),
            Expr::FunctionCall { arguments, .. } => {
                for arg in arguments {
                    walk_expr(arg, advanced, mods);
                }
            }
            _ => {}
        }
        let before = format!("{expr:?}");
        *expr = advanced.optimize_expr(expr);
        if format!("{expr:?}") != before {
            *mods += 1;
        }
    }

    fn walk_stmt(stmt: &mut Stmt, advanced: &mut AdvancedOptimizer, mods: &mut usize) {
        match stmt {
            Stmt::Assignment { value, .. } => walk_expr(value, advanced, mods),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                walk_expr(condition, advanced, mods);
                for s in then_branch {
                    walk_stmt(s, advanced, mods);
                }
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        walk_stmt(s, advanced, mods);
                    }
                }
            }
            Stmt::While { condition, body } => {
                walk_expr(condition, advanced, mods);
                for s in body {
                    walk_stmt(s, advanced, mods);
                }
            }
            Stmt::For { start, end, body, .. } => {
                walk_expr(start, advanced, mods);
                walk_expr(end, advanced, mods);
                for s in body {
                    walk_stmt(s, advanced, mods);
                }
            }
            Stmt::Repeat {
                body,
                until_condition,
            } => {
                for s in body {
                    walk_stmt(s, advanced, mods);
                }
                walk_expr(until_condition, advanced, mods);
            }
            Stmt::Block(b) => walk_block(b, advanced, mods),
            _ => {}
        }
        let before = format!("{stmt:?}");
        *stmt = advanced.optimize_stmt(stmt);
        if format!("{stmt:?}") != before {
            *mods += 1;
        }
    }

    fn walk_block(block: &mut Block, advanced: &mut AdvancedOptimizer, mods: &mut usize) {
        for stmt in &mut block.statements {
            walk_stmt(stmt, advanced, mods);
        }
        for proc in &mut block.procedures {
            walk_block(&mut proc.block, advanced, mods);
        }
        for func in &mut block.functions {
            walk_block(&mut func.block, advanced, mods);
        }
    }

    walk_block(block, advanced, &mut modifications);
    modifications
}

fn inline_functions_in_block(block: &mut Block, inliner: &mut FunctionInliner) -> usize {
    let functions: HashMap<String, FunctionDecl> = block
        .functions
        .iter()
        .map(|f| (f.name.to_lowercase(), f.clone()))
        .collect();

    let mut modifications = 0;

    fn inline_stmt(stmt: &mut Stmt, functions: &HashMap<String, FunctionDecl>, inliner: &mut FunctionInliner, mods: &mut usize) {
        match stmt {
            Stmt::Assignment {
                target,
                value: Expr::FunctionCall { name, arguments },
            } if functions.contains_key(&name.to_lowercase()) => {
                if let Some(func) = functions.get(&name.to_lowercase()) {
                    if inliner.should_inline(func) {
                        let inlined = inliner.inline_call(func, arguments);
                        if let Expr::Variable(var) = target {
                            if let Some(last) = inlined.last() {
                                if let Stmt::Assignment { value, .. } = last {
                                    *stmt = Stmt::Assignment {
                                        target: Expr::Variable(var.clone()),
                                        value: value.clone(),
                                    };
                                    *mods += 1;
                                }
                            }
                        }
                    }
                }
            }
            Stmt::Block(b) => inline_block(b, functions, inliner, mods),
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                for s in then_branch {
                    inline_stmt(s, functions, inliner, mods);
                }
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        inline_stmt(s, functions, inliner, mods);
                    }
                }
            }
            Stmt::While { body, .. } | Stmt::For { body, .. } => {
                for s in body {
                    inline_stmt(s, functions, inliner, mods);
                }
            }
            _ => {}
        }
    }

    fn inline_block(block: &mut Block, functions: &HashMap<String, FunctionDecl>, inliner: &mut FunctionInliner, mods: &mut usize) {
        for stmt in &mut block.statements {
            inline_stmt(stmt, functions, inliner, mods);
        }
        for proc in &mut block.procedures {
            inline_block(&mut proc.block, functions, inliner, mods);
        }
        for func in &mut block.functions {
            inline_block(&mut func.block, functions, inliner, mods);
        }
    }

    inline_block(block, &functions, inliner, &mut modifications);
    modifications
}

macro_rules! define_pass {
    ($struct_name:ident, $pass_name:literal, $level:expr, $desc:literal) => {
        pub struct $struct_name;

        impl $struct_name {
            pub fn new() -> Self {
                Self
            }

            fn pass_name(&self) -> &str {
                $pass_name
            }

            fn pass_description(&self) -> &str {
                $desc
            }

            fn pass_level(&self) -> OptimizationLevel {
                $level
            }

            fn should_run(&self, context: &OptimizationContext) -> bool {
                context.level.includes($level)
            }
        }
    };
}

define_pass!(
    ConstantFoldingPass,
    "constant_folding",
    OptimizationLevel::Basic,
    "Fold constant expressions at compile time"
);

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &str {
        self.pass_name()
    }

    fn description(&self) -> &str {
        self.pass_description()
    }

    fn optimization_level(&self) -> OptimizationLevel {
        self.pass_level()
    }

    fn should_apply(&self, context: &OptimizationContext) -> bool {
        self.should_run(context)
    }

    fn dependencies(&self) -> Vec<String> {
        Vec::new()
    }

    fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError> {
        let optimizer = Optimizer::new();
        let modifications = fold_block(&mut ast.block, &optimizer);
        let mut stats = OptimizationStats::new();
        stats.expressions_simplified = modifications;
        Ok(OptimizationResult::new(modifications, 0, stats))
    }
}

define_pass!(
    DeadCodeEliminationPass,
    "dead_code_elimination",
    OptimizationLevel::Standard,
    "Remove unreachable code and uncalled routines"
);

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &str {
        self.pass_name()
    }

    fn description(&self) -> &str {
        self.pass_description()
    }

    fn optimization_level(&self) -> OptimizationLevel {
        self.pass_level()
    }

    fn should_apply(&self, context: &OptimizationContext) -> bool {
        self.should_run(context)
    }

    fn dependencies(&self) -> Vec<String> {
        Vec::new()
    }

    fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError> {
        let before_procs = ast.block.procedures.len();
        let before_funcs = ast.block.functions.len();
        Optimizer::eliminate_dead_procedures_and_functions(&mut ast.block);

        let optimizer = Optimizer::new();
        ast.block.statements = ast
            .block
            .statements
            .iter()
            .filter_map(|stmt| optimizer.optimize_stmt(stmt))
            .collect();

        let removed = (before_procs - ast.block.procedures.len())
            + (before_funcs - ast.block.functions.len());
        let mut stats = OptimizationStats::new();
        stats.dead_code_removed = removed;
        Ok(OptimizationResult::new(removed, 0, stats))
    }
}

define_pass!(
    CommonSubexpressionEliminationPass,
    "cse",
    OptimizationLevel::Standard,
    "Eliminate duplicate subexpressions"
);

impl OptimizationPass for CommonSubexpressionEliminationPass {
    fn name(&self) -> &str {
        self.pass_name()
    }

    fn description(&self) -> &str {
        self.pass_description()
    }

    fn optimization_level(&self) -> OptimizationLevel {
        self.pass_level()
    }

    fn should_apply(&self, context: &OptimizationContext) -> bool {
        self.should_run(context)
    }

    fn dependencies(&self) -> Vec<String> {
        Vec::new()
    }

    fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError> {
        let mut cse = CSEOptimizer::new();
        let mut modifications = 0;

        fn walk_exprs(block: &mut Block, cse: &mut CSEOptimizer, mods: &mut usize) {
            fn walk(expr: &mut Expr, cse: &mut CSEOptimizer, mods: &mut usize) {
                if let Expr::BinaryOp { left, right, .. } = expr {
                    walk(left, cse, mods);
                    walk(right, cse, mods);
                    let (_optimized, temps) = cse.optimize_expr(expr);
                    if !temps.is_empty() {
                        *mods += temps.len();
                    }
                }
            }
            for stmt in &mut block.statements {
                if let Stmt::Assignment { value, .. } = stmt {
                    walk(value, cse, mods);
                }
            }
            for func in &mut block.functions {
                walk_exprs(&mut func.block, cse, mods);
            }
        }

        walk_exprs(&mut ast.block, &mut cse, &mut modifications);
        let mut stats = OptimizationStats::new();
        stats.expressions_simplified = modifications;
        Ok(OptimizationResult::new(modifications, 0, stats))
    }
}

define_pass!(
    FunctionInliningPass,
    "function_inlining",
    OptimizationLevel::Aggressive,
    "Inline small functions"
);

impl OptimizationPass for FunctionInliningPass {
    fn name(&self) -> &str {
        self.pass_name()
    }

    fn description(&self) -> &str {
        self.pass_description()
    }

    fn optimization_level(&self) -> OptimizationLevel {
        self.pass_level()
    }

    fn should_apply(&self, context: &OptimizationContext) -> bool {
        self.should_run(context)
    }

    fn dependencies(&self) -> Vec<String> {
        Vec::new()
    }

    fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError> {
        let mut inliner = FunctionInliner::new(10);
        let modifications = inline_functions_in_block(&mut ast.block, &mut inliner);
        let mut stats = OptimizationStats::new();
        stats.functions_inlined = inliner.inlined_count();
        Ok(OptimizationResult::new(modifications, 0, stats))
    }
}

define_pass!(
    LoopOptimizationPass,
    "loop_optimization",
    OptimizationLevel::Standard,
    "Optimize loops via unrolling and strength reduction"
);

impl OptimizationPass for LoopOptimizationPass {
    fn name(&self) -> &str {
        self.pass_name()
    }

    fn description(&self) -> &str {
        self.pass_description()
    }

    fn optimization_level(&self) -> OptimizationLevel {
        self.pass_level()
    }

    fn should_apply(&self, context: &OptimizationContext) -> bool {
        self.should_run(context)
    }

    fn dependencies(&self) -> Vec<String> {
        Vec::new()
    }

    fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError> {
        let loop_opt = LoopOptimizer::new(4);
        let mut advanced = AdvancedOptimizer::new();
        let mut modifications = 0;

        fn walk_loops(block: &mut Block, loop_opt: &LoopOptimizer, mods: &mut usize) {
            block.statements = block
                .statements
                .iter()
                .map(|stmt| {
                    let optimized = loop_opt.optimize_loop(stmt);
                    if format!("{optimized:?}") != format!("{stmt:?}") {
                        *mods += 1;
                    }
                    optimized
                })
                .collect();
            for func in &mut block.functions {
                walk_loops(&mut func.block, loop_opt, mods);
            }
        }

        walk_loops(&mut ast.block, &loop_opt, &mut modifications);
        modifications += apply_advanced_expr(&mut ast.block, &mut advanced);

        let mut stats = OptimizationStats::new();
        stats.loops_unrolled = modifications;
        Ok(OptimizationResult::new(modifications, 0, stats))
    }
}

define_pass!(
    RegisterAllocationPass,
    "register_allocation",
    OptimizationLevel::Aggressive,
    "Analyze register pressure for generated code"
);

impl OptimizationPass for RegisterAllocationPass {
    fn name(&self) -> &str {
        self.pass_name()
    }

    fn description(&self) -> &str {
        self.pass_description()
    }

    fn optimization_level(&self) -> OptimizationLevel {
        self.pass_level()
    }

    fn should_apply(&self, context: &OptimizationContext) -> bool {
        self.should_run(context)
    }

    fn dependencies(&self) -> Vec<String> {
        Vec::new()
    }

    fn optimize(&mut self, ast: &mut Program) -> Result<OptimizationResult, CompilerError> {
        use crate::register_allocator::{LiveRangeAnalyzer, RegisterAllocator};

        let mut analyzer = LiveRangeAnalyzer::new();
        for var in &ast.block.vars {
            analyzer.define_variable(&var.name);
        }
        for stmt in &ast.block.statements {
            if let Stmt::Assignment {
                target: Expr::Variable(name),
                ..
            } = stmt
            {
                analyzer.use_variable(name);
            }
            analyzer.next_instruction();
        }

        let live_ranges = analyzer.get_live_ranges();
        let mut allocator = RegisterAllocator::new();
        if !live_ranges.is_empty() {
            allocator.build_interference_graph(live_ranges);
            allocator.allocate().map_err(|e| CompilerError::OptimizerError {
                location: None,
                message: e.to_string(),
            })?;
        }

        let modifications = allocator.get_allocations().len();
        let mut stats = OptimizationStats::new();
        stats.memory_usage_optimized = modifications;
        Ok(OptimizationResult::new(modifications, 0, stats))
    }
}
