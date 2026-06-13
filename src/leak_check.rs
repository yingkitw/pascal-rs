//! Memory leak detection for the Pascal interpreter.
//!
//! Tracks heap-allocated runtime values (arrays, objects, records) and reports
//! unreleased allocations after program execution.

use crate::interpreter::{Interpreter, Value};

/// Report from a leak-check run
#[derive(Debug, Clone, Default)]
pub struct LeakReport {
    pub heap_objects_in_scope: usize,
    pub scope_depth: usize,
    pub variables_with_heap: Vec<(String, String)>,
    pub total_variables: usize,
}

impl LeakReport {
    pub fn has_potential_leaks(&self) -> bool {
        self.heap_objects_in_scope > 0
    }

    pub fn format_report(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!("Scope depth: {}\n", self.scope_depth));
        out.push_str(&format!("Total variables: {}\n", self.total_variables));
        out.push_str(&format!(
            "Heap objects in scope: {}\n",
            self.heap_objects_in_scope
        ));
        if !self.variables_with_heap.is_empty() {
            out.push_str("\nVariables holding heap objects:\n");
            for (name, kind) in &self.variables_with_heap {
                out.push_str(&format!("  {} ({kind})\n", name));
            }
        }
        if self.has_potential_leaks() {
            out.push_str(
                "\nNote: Heap objects still referenced at program end may indicate leaks\n\
                 or may be intentional global state.\n",
            );
        } else {
            out.push_str("\nNo heap objects remain in interpreter scopes.\n");
        }
        out
    }
}

fn heap_kind(value: &Value) -> Option<&'static str> {
    match value {
        Value::Array { .. } => Some("array"),
        Value::Object { .. } => Some("object"),
        Value::Record { .. } => Some("record"),
        _ => None,
    }
}

/// Analyze interpreter state after execution for unreleased heap values.
pub fn analyze_interpreter(interp: &Interpreter) -> LeakReport {
    let mut report = LeakReport::default();
    report.scope_depth = interp.scope_count();

    for (name, value) in interp.all_scope_variables() {
        report.total_variables += 1;
        if let Some(kind) = heap_kind(&value) {
            report.heap_objects_in_scope += 1;
            report.variables_with_heap.push((name, kind.to_string()));
        }
    }

    report
}

/// Run a program and return a leak report (does not fail on runtime errors).
pub fn run_with_leak_check(
    program: &crate::ast::Program,
    verbose: bool,
) -> Result<(LeakReport, Result<(), anyhow::Error>), anyhow::Error> {
    let mut interp = Interpreter::new(verbose);
    let run_result = interp.run_program(program);
    let report = analyze_interpreter(&interp);
    Ok((report, run_result.map_err(|e| e.into())))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, Program, SimpleType, Stmt, Type, VariableDecl};

    #[test]
    fn test_leak_report_empty_after_simple_program() {
        let program = Program {
            name: "T".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![VariableDecl {
                    name: "x".to_string(),
                    variable_type: Type::Simple(SimpleType::Integer),
                    initial_value: None,
                    visibility: crate::ast::FieldVisibility::Public,
                    is_absolute: false,
                    absolute_address: None,
                }],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                statements: vec![Stmt::Assignment {
                    target: crate::ast::Expr::Variable("x".to_string()),
                    value: crate::ast::Expr::Literal(crate::ast::Literal::Integer(1)),
                }],
            },
        };

        let (report, run_result) = run_with_leak_check(&program, false).unwrap();
        assert!(run_result.is_ok());
        assert_eq!(report.heap_objects_in_scope, 0);
    }
}
