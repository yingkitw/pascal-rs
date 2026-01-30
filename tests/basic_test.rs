use pascal::{CompileOptions, Compiler};

#[test]
fn test_basic_compilation() {
    let options = CompileOptions {
        output_dir: ".".into(),
    };

    let mut compiler = Compiler::new(options);

    // This should not panic
    let result = compiler.compile_file("test.pas");
    assert!(result.is_ok());

    // Test the actual compilation result
    let compile_result = result.unwrap();
    assert!(compile_result.success);
}
