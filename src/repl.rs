//! Interactive REPL for Pascal with basic tab-completion hints.

use crate::ast::{Block, Program, Statement};
use crate::ide::completions_at;
use crate::interpreter::Interpreter;
use std::io::{self, BufRead, Write};

const PROMPT: &str = "pascal> ";

/// Run an interactive read-eval-print loop.
pub fn run_repl(verbose: bool) -> anyhow::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut statements: Vec<Statement> = Vec::new();
    let mut history: Vec<String> = Vec::new();

    writeln!(stdout, "Pascal REPL (type :quit to exit, :help for commands)")?;

    loop {
        write!(stdout, "{PROMPT}")?;
        stdout.flush()?;

        let mut line = String::new();
        if stdin.lock().read_line(&mut line).is_err() {
            break;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        match trimmed {
            ":quit" | ":q" | ":exit" => break,
            ":help" | ":h" => {
                print_help(&mut stdout)?;
                continue;
            }
            ":clear" => {
                statements.clear();
                writeln!(stdout, "Session cleared.")?;
                continue;
            }
            ":history" => {
                for (i, h) in history.iter().enumerate() {
                    writeln!(stdout, "  {}: {}", i + 1, h)?;
                }
                continue;
            }
            cmd if cmd.starts_with(":complete ") => {
                let prefix = cmd.strip_prefix(":complete ").unwrap_or("");
                let fake_src = format!("program Repl;\nbegin\n  {prefix}\nend.");
                let items = completions_at(&fake_src, 2, prefix.len());
                for item in items.iter().take(20) {
                    writeln!(stdout, "  {} ({:?})", item.label, item.kind)?;
                }
                continue;
            }
            _ => {}
        }

        history.push(trimmed.to_string());

        match parse_statement(trimmed) {
            Ok(stmt) => {
                statements.push(stmt);
                let program = build_repl_program(&statements);
                let mut interp = Interpreter::new(verbose);
                match interp.run_program(&program) {
                    Ok(()) => {}
                    Err(e) => writeln!(stdout, "Error: {}", e)?,
                }
            }
            Err(e) => writeln!(stdout, "Parse error: {}", e)?,
        }
    }

    Ok(())
}

fn build_repl_program(statements: &[Statement]) -> Program {
    Program {
        name: "ReplSession".to_string(),
        uses: vec![],
        block: Block {
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![],
            classes: vec![],
            statements: statements.to_vec(),
        },
    }
}

fn parse_statement(source: &str) -> Result<Statement, String> {
    let wrapped = format!(
        "program ReplStmt;\nbegin\n  {};\nend.",
        source.trim_end_matches(';')
    );
    let mut parser = crate::parser::Parser::new(&wrapped);
    let program = parser
        .parse_program()
        .map_err(|e| format!("{}", e))?;
    program
        .block
        .statements
        .into_iter()
        .next()
        .ok_or_else(|| "Empty statement".to_string())
}

fn print_help(out: &mut impl Write) -> io::Result<()> {
    writeln!(out, "Commands:")?;
    writeln!(out, "  :quit, :q     Exit the REPL")?;
    writeln!(out, "  :clear        Clear session statements")?;
    writeln!(out, "  :history      Show input history")?;
    writeln!(out, "  :complete X   Show completions for prefix X")?;
    writeln!(out, "  :help         Show this help")?;
    writeln!(out)?;
    writeln!(out, "Enter Pascal statements (e.g. writeln(42); x := 1;)")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_writeln_statement() {
        let stmt = parse_statement("writeln(42)").unwrap();
        assert!(matches!(stmt, Statement::ProcedureCall { .. }));
    }
}
