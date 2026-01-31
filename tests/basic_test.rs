// Test module entry point for pascal-rs
// All comprehensive tests are organized in:
// - tests/unit/ - Unit tests for individual components
// - tests/integration/ - Integration tests for full pipeline
// - tests/performance/ - Performance and benchmark tests
// - tests/regression/ - Regression tests

// This file serves as the main test entry point

#[test]
fn test_compiler_exists() {
    // Basic smoke test to verify the compiler crate is accessible
    use pascal::Lexer;
    let source = "program test; begin end.";
    let _lexer = Lexer::new(source);
}
