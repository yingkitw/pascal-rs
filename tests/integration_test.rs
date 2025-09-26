use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

#[test]
fn test_compile_hello() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("minipas")?;
    
    // Create a temporary directory for the test
    let temp_dir = tempfile::tempdir()?;
    let output_path = temp_dir.path().join("output.s");
    
    // Run the compiler
    cmd.args([
        "-i", "examples/hello.pas",
        "-o", output_path.to_str().unwrap(),
        "--verbose"
    ])
    .assert()
    .success()
    .stdout(predicate::str::contains("Successfully compiled"));
    
    // Verify the output file was created
    assert!(output_path.exists());
    
    // Read the generated assembly
    let assembly = fs::read_to_string(&output_path)?;
    
    // Basic checks on the assembly output
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("mov eax, 42"));
    
    Ok(())
}
