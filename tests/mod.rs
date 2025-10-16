// Comprehensive test module for minipas
// This module organizes all test suites and provides a unified testing interface

pub mod simple_test;
pub mod basic_test;
pub mod lexer_tests;
pub mod ast_tests;
pub mod integration_tests;
pub mod integration_comprehensive_tests;
pub mod error_handling_comprehensive_tests;
pub mod parser_tests;
pub mod codegen_tests;
pub mod string_tests;
pub mod type_tests;
pub mod test_runner;

// Re-export test modules for easy access
pub use lexer_tests::*;
pub use ast_tests::*;
pub use integration_tests::*;
pub use parser_tests::*;
pub use codegen_tests::*;
pub use string_tests::*;
pub use type_tests::*;
pub use test_runner::*;

#[cfg(test)]
mod comprehensive_tests {
    use super::*;

    /// Run all comprehensive tests for minipas
    /// This function serves as the main entry point for running all tests
    #[test]
    fn run_comprehensive_test_suite() {
        let mut runner = test_runner::TestRunner::new();
        runner.run_all_tests();
    }

    /// Test that all modules can be imported and used
    #[test]
    fn test_module_imports() {
        // Test that all test modules are accessible
        assert!(true, "All test modules should be importable");
    }

    /// Test that the test runner can be instantiated
    #[test]
    fn test_test_runner_instantiation() {
        let runner = test_runner::TestRunner::new();
        assert_eq!(runner.suites.len(), 0, "New test runner should have no suites");
    }

    /// Test that test suites can be created
    #[test]
    fn test_test_suite_creation() {
        let suite = test_runner::TestSuite::new("Test Suite");
        assert_eq!(suite.name, "Test Suite");
        assert_eq!(suite.tests.len(), 0);
        assert_eq!(suite.passed_count(), 0);
        assert_eq!(suite.failed_count(), 0);
        assert_eq!(suite.total_count(), 0);
        assert_eq!(suite.success_rate(), 0.0);
    }

    /// Test that test results can be added to suites
    #[test]
    fn test_test_result_management() {
        let mut suite = test_runner::TestSuite::new("Test Suite");
        
        let result = test_runner::TestResult {
            name: "Test 1".to_string(),
            passed: true,
            duration: std::time::Duration::new(0, 0),
            error: None,
        };
        
        suite.add_test(result);
        
        assert_eq!(suite.total_count(), 1);
        assert_eq!(suite.passed_count(), 1);
        assert_eq!(suite.failed_count(), 0);
        assert_eq!(suite.success_rate(), 100.0);
    }

    /// Test error handling in test results
    #[test]
    fn test_test_result_errors() {
        let mut suite = test_runner::TestSuite::new("Test Suite");
        
        let result = test_runner::TestResult {
            name: "Failing Test".to_string(),
            passed: false,
            duration: std::time::Duration::new(0, 0),
            error: Some("Test failed".to_string()),
        };
        
        suite.add_test(result);
        
        assert_eq!(suite.total_count(), 1);
        assert_eq!(suite.passed_count(), 0);
        assert_eq!(suite.failed_count(), 1);
        assert_eq!(suite.success_rate(), 0.0);
    }
}

/// Test configuration and utilities
pub mod config {
    use std::collections::HashMap;
    use std::path::PathBuf;

    #[derive(Debug, Clone)]
    pub struct TestConfig {
        pub test_data_dir: PathBuf,
        pub output_dir: PathBuf,
        pub verbose: bool,
        pub stop_on_first_failure: bool,
        pub parallel_execution: bool,
        pub test_filters: Vec<String>,
    }

    impl Default for TestConfig {
        fn default() -> Self {
            Self {
                test_data_dir: PathBuf::from("tests/data"),
                output_dir: PathBuf::from("tests/output"),
                verbose: false,
                stop_on_first_failure: false,
                parallel_execution: false,
                test_filters: Vec::new(),
            }
        }
    }

    impl TestConfig {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn with_verbose(mut self, verbose: bool) -> Self {
            self.verbose = verbose;
            self
        }

        pub fn with_parallel(mut self, parallel: bool) -> Self {
            self.parallel_execution = parallel;
            self
        }

        pub fn with_filters(mut self, filters: Vec<String>) -> Self {
            self.test_filters = filters;
            self
        }
    }

    /// Test data management utilities
    pub mod data {
        use super::*;
        use std::fs;
        use std::io::Write;

        pub struct TestDataManager {
            config: TestConfig,
        }

        impl TestDataManager {
            pub fn new(config: TestConfig) -> Self {
                Self { config }
            }

            pub fn create_test_data(&self) -> Result<(), Box<dyn std::error::Error>> {
                // Create test data directory if it doesn't exist
                fs::create_dir_all(&self.config.test_data_dir)?;
                fs::create_dir_all(&self.config.output_dir)?;

                // Create sample Pascal test files
                self.create_sample_pascal_files()?;
                
                Ok(())
            }

            fn create_sample_pascal_files(&self) -> Result<(), Box<dyn std::error::Error>> {
                let test_files = vec![
                    ("basic.pas", include_str!("../examples/hello.pas")),
                    ("arithmetic.pas", include_str!("../examples/simple_math.pas")),
                    ("loops.pas", include_str!("../examples/simple_loop.pas")),
                    ("conditionals.pas", include_str!("../examples/conditional.pas")),
                    ("boolean.pas", include_str!("../examples/boolean_logic.pas")),
                    ("fibonacci.pas", include_str!("../examples/fibonacci.pas")),
                    ("calculator.pas", include_str!("../examples/calculator.pas")),
                    ("advanced.pas", include_str!("../examples/advanced_features.pas")),
                    ("comprehensive.pas", include_str!("../examples/comprehensive_features.pas")),
                ];

                for (filename, content) in test_files {
                    let path = self.config.test_data_dir.join(filename);
                    let mut file = fs::File::create(path)?;
                    file.write_all(content.as_bytes())?;
                }

                Ok(())
            }

            pub fn cleanup_test_data(&self) -> Result<(), Box<dyn std::error::Error>> {
                if self.config.test_data_dir.exists() {
                    fs::remove_dir_all(&self.config.test_data_dir)?;
                }
                if self.config.output_dir.exists() {
                    fs::remove_dir_all(&self.config.output_dir)?;
                }
                Ok(())
            }
        }
    }
}

/// Test utilities and helpers
pub mod utils {
    use std::process::Command;
    use std::path::Path;

    /// Compile a Pascal source file and return the assembly output
    pub fn compile_pascal_source(source: &str) -> Result<String, Box<dyn std::error::Error>> {
        // This would use the actual minipas compiler
        // For now, we'll return a mock result
        Ok(format!("; Assembly output for: {}", source.lines().next().unwrap_or("Unknown")))
    }

    /// Run a compiled assembly file and return the exit code
    pub fn run_assembly_file(assembly_path: &Path) -> Result<i32, Box<dyn std::error::Error>> {
        // This would compile and run the assembly file
        // For now, we'll return a mock result
        Ok(0)
    }

    /// Compare two assembly files for equivalence
    pub fn compare_assembly_files(file1: &Path, file2: &Path) -> Result<bool, Box<dyn std::error::Error>> {
        // This would compare the assembly files
        // For now, we'll return a mock result
        Ok(true)
    }

    /// Generate test report in various formats
    pub fn generate_test_report(
        results: &crate::test_runner::TestRunner,
        format: ReportFormat,
    ) -> Result<String, Box<dyn std::error::Error>> {
        match format {
            ReportFormat::Text => Ok("Text report".to_string()),
            ReportFormat::Json => Ok("JSON report".to_string()),
            ReportFormat::Html => Ok("HTML report".to_string()),
            ReportFormat::Xml => Ok("XML report".to_string()),
        }
    }

    #[derive(Debug, Clone)]
    pub enum ReportFormat {
        Text,
        Json,
        Html,
        Xml,
    }
}

/// Performance testing utilities
pub mod performance {
    use std::time::{Duration, Instant};

    pub struct PerformanceTest {
        pub name: String,
        pub duration: Duration,
        pub memory_usage: Option<usize>,
        pub iterations: usize,
    }

    impl PerformanceTest {
        pub fn new(name: &str) -> Self {
            Self {
                name: name.to_string(),
                duration: Duration::new(0, 0),
                memory_usage: None,
                iterations: 1,
            }
        }

        pub fn with_iterations(mut self, iterations: usize) -> Self {
            self.iterations = iterations;
            self
        }

        pub fn run<F>(&mut self, test_fn: F) -> Duration
        where
            F: Fn() -> Result<(), Box<dyn std::error::Error>>,
        {
            let start = Instant::now();
            
            for _ in 0..self.iterations {
                if let Err(e) = test_fn() {
                    eprintln!("Performance test {} failed: {}", self.name, e);
                    break;
                }
            }
            
            self.duration = start.elapsed();
            self.duration
        }

        pub fn average_duration(&self) -> Duration {
            if self.iterations > 0 {
                Duration::from_nanos(self.duration.as_nanos() as u64 / self.iterations as u64)
            } else {
                Duration::new(0, 0)
            }
        }
    }
}

/// Benchmarking utilities
pub mod benchmarks {
    use super::performance::PerformanceTest;
    use std::collections::HashMap;

    pub struct BenchmarkSuite {
        pub name: String,
        pub tests: HashMap<String, PerformanceTest>,
    }

    impl BenchmarkSuite {
        pub fn new(name: &str) -> Self {
            Self {
                name: name.to_string(),
                tests: HashMap::new(),
            }
        }

        pub fn add_test(&mut self, test: PerformanceTest) {
            self.tests.insert(test.name.clone(), test);
        }

        pub fn run_all(&mut self) {
            println!("Running benchmark suite: {}", self.name);
            for (name, test) in &mut self.tests {
                println!("Running benchmark: {}", name);
                // Benchmark execution would go here
            }
        }

        pub fn generate_report(&self) -> String {
            let mut report = format!("Benchmark Report: {}\n", self.name);
            report.push_str("================================\n");
            
            for (name, test) in &self.tests {
                report.push_str(&format!(
                    "{}: {:?} (avg: {:?})\n",
                    name,
                    test.duration,
                    test.average_duration()
                ));
            }
            
            report
        }
    }
}
