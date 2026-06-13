//! WebAssembly code generation stub (WAT output).
//!
//! Full WASM backend is future work; this emits a minimal module skeleton.

use crate::ast::Program;
use anyhow::Result;

/// Generate WebAssembly Text Format (WAT) skeleton from a Pascal program.
pub fn compile_to_wat(program: &Program) -> Result<String> {
    Ok(format!(
        "(module\n  ;; Generated from Pascal program '{}'\n  (func $main (export \"main\")\n    ;; TODO: translate statements\n  )\n)\n",
        program.name
    ))
}

/// Generate WAT from Pascal source.
pub fn compile_source_to_wat(source: &str) -> Result<String> {
    let mut parser = crate::parser::Parser::new(source);
    let program = parser.parse_program()?;
    compile_to_wat(&program)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_to_wat_skeleton() {
        let src = "program Hello;\nbegin\nend.";
        let wat = compile_source_to_wat(src).unwrap();
        assert!(wat.contains("module"));
        assert!(wat.contains("Hello"));
    }
}
