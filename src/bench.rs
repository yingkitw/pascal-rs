//! Simple performance benchmarking for Pascal programs.

use crate::ast::Program;
use std::path::Path;
use std::time::Instant;

#[derive(Debug, Clone)]
pub struct BenchResult {
    pub iterations: u32,
    pub total_ms: u128,
    pub avg_us: u128,
}

impl BenchResult {
    pub fn format_report(&self) -> String {
        format!(
            "Iterations: {}\nTotal: {} ms\nAverage: {} µs/iter\n",
            self.iterations, self.total_ms, self.avg_us
        )
    }
}

/// Run a program repeatedly and measure interpreter time.
pub fn benchmark_program(program: &Program, iterations: u32, verbose: bool) -> BenchResult {
    let start = Instant::now();
    for i in 0..iterations {
        let mut interp = crate::interpreter::Interpreter::new(verbose && i == 0);
        let _ = interp.run_program(program);
    }
    let elapsed = start.elapsed();
    let total_ms = elapsed.as_millis();
    let avg_us = if iterations > 0 {
        elapsed.as_micros() / iterations as u128
    } else {
        0
    };
    BenchResult {
        iterations,
        total_ms,
        avg_us,
    }
}

/// Load and benchmark a Pascal source file.
pub fn benchmark_file(path: &Path, iterations: u32, verbose: bool) -> anyhow::Result<BenchResult> {
    let source = std::fs::read_to_string(path)?;
    let mut parser = crate::parser::Parser::new(&source);
    let program = parser.parse_program()?;
    Ok(benchmark_program(&program, iterations, verbose))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, Program, Stmt};

    #[test]
    fn test_benchmark_simple() {
        let program = Program {
            name: "B".to_string(),
            uses: vec![],
            block: Block::with_statements(vec![Stmt::Empty]),
        };
        let result = benchmark_program(&program, 10, false);
        assert_eq!(result.iterations, 10);
    }
}
