//! Interactive debugger with breakpoints and watch expressions
//!
//! Supports breaking on procedure/function entry and inspecting variables.

use crate::interpreter::Interpreter;
use crate::ast::Program;
use std::collections::HashSet;
use std::io::{self, BufRead, Write};

/// Breakpoint by procedure/function name
#[derive(Debug, Clone, Default)]
pub struct DebugSession {
    /// Procedure/function names to break on entry
    pub breakpoints: HashSet<String>,
    /// Variable names to watch (printed at each stop)
    pub watch_expressions: Vec<String>,
}

impl DebugSession {
    pub fn new() -> Self {
        Self {
            breakpoints: HashSet::new(),
            watch_expressions: Vec::new(),
        }
    }

    pub fn add_breakpoint(&mut self, name: &str) {
        self.breakpoints.insert(name.to_lowercase());
    }

    pub fn add_watch(&mut self, expr: &str) {
        self.watch_expressions.push(expr.to_string());
    }

    pub fn should_break(&self, procedure_name: &str) -> bool {
        self.breakpoints.contains(&procedure_name.to_lowercase())
    }
}

/// Run program with debugger; breaks on procedure entry and supports watch expressions
pub fn run_with_debugger(
    program: &Program,
    session: &mut DebugSession,
    verbose: bool,
) -> anyhow::Result<()> {
    let mut interp = Interpreter::new(verbose);
    let breakpoints = session.breakpoints.clone();
    let watch_exprs = session.watch_expressions.clone();

    interp.debug_breakpoint_check = Some(Box::new(move |name| breakpoints.contains(&name.to_lowercase())));
    interp.debug_breakpoint_handler = Some(Box::new(move |i| {
        debug_prompt_with_watch(i, &watch_exprs);
    }));

    interp.run_program(program)?;
    Ok(())
}

fn debug_prompt_with_watch(interp: &Interpreter, watch_expressions: &[String]) {
    let stdout = io::stdout();
    let mut out = stdout.lock();
    let stdin = io::stdin();
    let mut inp = stdin.lock();

    for expr in watch_expressions {
        if let Some(val) = interp.get_variable_value(expr) {
            let _ = writeln!(out, "  {} = {:?}", expr, val);
        }
    }

    loop {
        let _ = write!(out, "(debug) ");
        let _ = out.flush();
        let mut line = String::new();
        if inp.read_line(&mut line).is_err() || line.is_empty() {
            break;
        }
        let cmd = line.trim().to_lowercase();
        match cmd.as_str() {
            "c" | "continue" | "" => break,
            "q" | "quit" | "exit" => {
                let _ = writeln!(out, "Exiting debugger.");
                std::process::exit(0);
            }
            "vars" | "v" => {
                for (k, v) in interp.current_scope().iter() {
                    let _ = writeln!(out, "  {} = {:?}", k, v);
                }
            }
            "help" | "h" => {
                let _ = writeln!(out, "  c/continue - continue execution");
                let _ = writeln!(out, "  q/quit    - exit debugger");
                let _ = writeln!(out, "  vars      - print variables");
            }
            _ => {
                if let Some(val) = interp.get_variable_value(&line.trim()) {
                    let _ = writeln!(out, "{} = {:?}", line.trim(), val);
                } else if !cmd.is_empty() {
                    let _ = writeln!(out, "Unknown command. Try 'help'.");
                }
            }
        }
    }
}

/// Debugger REPL: print watch vars and prompt for next command
#[allow(dead_code)]
pub fn debug_prompt(interp: &Interpreter, session: &DebugSession) {
    let stdout = io::stdout();
    let mut out = stdout.lock();
    let stdin = io::stdin();
    let mut inp = stdin.lock();

    for expr in &session.watch_expressions {
        if let Some(val) = interp.get_variable_value(expr) {
            let _ = writeln!(out, "  {} = {:?}", expr, val);
        }
    }

    loop {
        let _ = write!(out, "(debug) ");
        let _ = out.flush();
        let mut line = String::new();
        if inp.read_line(&mut line).is_err() || line.is_empty() {
            break;
        }
        let cmd = line.trim().to_lowercase();
        match cmd.as_str() {
            "c" | "continue" | "" => break,
            "q" | "quit" | "exit" => {
                let _ = writeln!(out, "Exiting debugger.");
                std::process::exit(0);
            }
            "vars" | "v" => {
                for (k, v) in interp.current_scope().iter() {
                    let _ = writeln!(out, "  {} = {:?}", k, v);
                }
            }
            "help" | "h" => {
                let _ = writeln!(out, "  c/continue - continue execution");
                let _ = writeln!(out, "  q/quit    - exit debugger");
                let _ = writeln!(out, "  vars      - print variables");
            }
            _ => {
                // Try as variable name to print
                if let Some(val) = interp.get_variable_value(&line.trim()) {
                    let _ = writeln!(out, "{} = {:?}", line.trim(), val);
                } else if !cmd.is_empty() {
                    let _ = writeln!(out, "Unknown command. Try 'help'.");
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debug_session_breakpoints() {
        let mut session = DebugSession::new();
        session.add_breakpoint("MyProc");
        assert!(session.should_break("myproc"));
        assert!(session.should_break("MyProc"));
    }

    #[test]
    fn test_debug_session_watch() {
        let mut session = DebugSession::new();
        session.add_watch("x");
        assert_eq!(session.watch_expressions, ["x"]);
    }
}
