//! IDE support: completion symbols, hover info, and diagnostics helpers.
//!
//! Used by the LSP server and REPL. Feature-free so it can be unit-tested.

use crate::ast::{Block, ClassDecl, Program, Type};
use crate::interpreter::create_default_registry;
use std::collections::HashSet;

/// Kind of completion item for display
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompletionKind {
    Keyword,
    Variable,
    Constant,
    Type,
    Procedure,
    Function,
    Class,
    Builtin,
}

/// A completion candidate
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionCandidate {
    pub label: String,
    pub kind: CompletionKind,
    pub detail: Option<String>,
    pub documentation: Option<String>,
}

/// Pascal reserved words for completion
pub const PASCAL_KEYWORDS: &[&str] = &[
    "program", "var", "const", "type", "begin", "end", "if", "then", "else", "while", "do",
    "for", "to", "downto", "repeat", "until", "case", "of", "when", "with", "procedure",
    "function", "forward", "external", "inline", "uses", "unit", "interface", "implementation",
    "library", "package", "and", "or", "not", "xor", "mod", "div", "in", "nil", "true", "false",
    "class", "constructor", "destructor", "inherited", "override", "virtual", "abstract",
    "private", "protected", "public", "published", "property", "try", "except", "finally",
    "raise", "exit", "break", "continue", "asm", "absolute", "packed", "record", "set",
    "array", "string", "integer", "real", "boolean", "char", "writeln", "write", "read",
    "readln",
];

/// Collect completion candidates from source text at the given cursor position.
pub fn completions_at(source: &str, line: usize, character: usize) -> Vec<CompletionCandidate> {
    let prefix = word_prefix_at(source, line, character);
    let mut items = Vec::new();
    let mut seen = HashSet::new();

    let add = |items: &mut Vec<CompletionCandidate>, c: CompletionCandidate, seen: &mut HashSet<String>| {
        if seen.insert(c.label.to_lowercase()) {
            items.push(c);
        }
    };

    for kw in PASCAL_KEYWORDS {
        if prefix.is_empty() || kw.starts_with(&prefix) {
            add(
                &mut items,
                CompletionCandidate {
                    label: kw.to_string(),
                    kind: CompletionKind::Keyword,
                    detail: Some("keyword".to_string()),
                    documentation: None,
                },
                &mut seen,
            );
        }
    }

    if let Ok(program) = parse_program(source) {
        for sym in symbols_from_program(&program) {
            if prefix.is_empty() || sym.label.to_lowercase().starts_with(&prefix) {
                add(&mut items, sym, &mut seen);
            }
        }
    }

    let registry = create_default_registry();
    for name in registry.function_names() {
        if prefix.is_empty() || name.to_lowercase().starts_with(&prefix) {
            add(
                &mut items,
                CompletionCandidate {
                    label: name.clone(),
                    kind: CompletionKind::Builtin,
                    detail: Some("builtin".to_string()),
                    documentation: Some(format!("Built-in function `{}`", name)),
                },
                &mut seen,
            );
        }
    }

    items.sort_by(|a, b| a.label.to_lowercase().cmp(&b.label.to_lowercase()));
    items
}

/// Hover documentation for identifier at cursor, if any.
pub fn hover_at(source: &str, line: usize, character: usize) -> Option<String> {
    let word = word_at(source, line, character)?;
    let lower = word.to_lowercase();

    if let Some(kw) = PASCAL_KEYWORDS.iter().find(|k| k.to_lowercase() == lower) {
        return Some(format!("Pascal keyword `{}`", kw));
    }

    let registry = create_default_registry();
    if registry.has_function(&word) || registry.has_function(&lower) {
        return Some(format!("Built-in function `{}`", word));
    }

    if let Ok(program) = parse_program(source) {
        for sym in symbols_from_program(&program) {
            if sym.label.to_lowercase() == lower {
                let kind = match sym.kind {
                    CompletionKind::Variable => "variable",
                    CompletionKind::Constant => "constant",
                    CompletionKind::Type => "type",
                    CompletionKind::Procedure => "procedure",
                    CompletionKind::Function => "function",
                    CompletionKind::Class => "class",
                    _ => "symbol",
                };
                let detail = sym.detail.unwrap_or_else(|| kind.to_string());
                return Some(format!("**{}** — {}", sym.label, detail));
            }
        }
    }

    None
}

/// Parse diagnostics from source (parse errors).
pub fn parse_diagnostics(source: &str) -> Vec<(usize, usize, String)> {
    let mut parser = crate::parser::Parser::new(source);
    match parser.parse_program() {
        Ok(_) => Vec::new(),
        Err(e) => {
            let mut diags = vec![(1usize, 0usize, format!("{}", e))];
            for err in parser.errors() {
                diags.push((1, 0, err.to_string()));
            }
            diags
        }
    }
}

fn parse_program(source: &str) -> Result<Program, ()> {
    let mut parser = crate::parser::Parser::new(source);
    parser.parse_program().map_err(|_| ())
}

fn symbols_from_program(program: &Program) -> Vec<CompletionCandidate> {
    let mut out = Vec::new();
    collect_block_symbols(&program.block, &mut out);
    out
}

fn collect_block_symbols(block: &Block, out: &mut Vec<CompletionCandidate>) {
    for c in &block.consts {
        out.push(CompletionCandidate {
            label: c.name.clone(),
            kind: CompletionKind::Constant,
            detail: Some("constant".to_string()),
            documentation: None,
        });
    }
    for t in &block.types {
        out.push(CompletionCandidate {
            label: t.name.clone(),
            kind: CompletionKind::Type,
            detail: Some(type_detail(&t.type_definition)),
            documentation: None,
        });
    }
    for v in &block.vars {
        out.push(CompletionCandidate {
            label: v.name.clone(),
            kind: CompletionKind::Variable,
            detail: Some(type_detail(&v.variable_type)),
            documentation: None,
        });
    }
    for p in &block.procedures {
        out.push(CompletionCandidate {
            label: p.name.clone(),
            kind: CompletionKind::Procedure,
            detail: Some(format!("({} params)", p.parameters.len())),
            documentation: None,
        });
        collect_block_symbols(&p.block, out);
    }
    for f in &block.functions {
        out.push(CompletionCandidate {
            label: f.name.clone(),
            kind: CompletionKind::Function,
            detail: Some(type_detail(&f.return_type)),
            documentation: None,
        });
        collect_block_symbols(&f.block, out);
    }
    for ClassDecl { name, .. } in &block.classes {
        out.push(CompletionCandidate {
            label: name.clone(),
            kind: CompletionKind::Class,
            detail: Some("class".to_string()),
            documentation: None,
        });
    }
}

fn type_detail(typ: &Type) -> String {
    match typ {
        Type::Integer => "integer".to_string(),
        Type::Real => "real".to_string(),
        Type::Boolean => "boolean".to_string(),
        Type::Char => "char".to_string(),
        Type::String | Type::WideString => "string".to_string(),
        Type::Simple(s) => format!("{:?}", s).to_lowercase(),
        Type::Alias { name, .. } => name.clone(),
        Type::Array { .. } => "array".to_string(),
        Type::Record { .. } => "record".to_string(),
        Type::Enum { .. } => "enum".to_string(),
        Type::Set { .. } => "set".to_string(),
        _ => "type".to_string(),
    }
}

fn word_prefix_at(source: &str, line: usize, character: usize) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let Some(line_text) = lines.get(line) else {
        return String::new();
    };
    let col = character.min(line_text.len());
    let before = &line_text[..col];
    before
        .rsplit(|c: char| !c.is_alphanumeric() && c != '_')
        .next()
        .unwrap_or("")
        .to_lowercase()
}

fn word_at(source: &str, line: usize, character: usize) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    let line_text = lines.get(line)?;
    if line_text.is_empty() {
        return None;
    }
    let col = character.min(line_text.len().saturating_sub(1));
    if !is_ident_char(line_text.as_bytes().get(col).copied().map(|b| b as char)?) {
        return None;
    }
    let mut start = col;
    let mut end = col + 1;
    let chars: Vec<char> = line_text.chars().collect();
    while start > 0 && is_ident_char(chars[start - 1]) {
        start -= 1;
    }
    while end < chars.len() && is_ident_char(chars[end]) {
        end += 1;
    }
    Some(line_text.chars().skip(start).take(end - start).collect())
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_completions_include_keywords_and_vars() {
        let src = "program P;\nvar\n  count: integer;\nbegin\n  cou\nend.";
        let items = completions_at(src, 4, 5);
        let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"count"));
        assert!(labels.iter().any(|l| l.starts_with("co") || *l == "continue"));
    }

    #[test]
    fn test_hover_variable() {
        let src = "program P;\nvar\n  total: integer;\nbegin\n  total := 1;\nend.";
        let hover = hover_at(src, 4, 2);
        assert!(hover.is_some());
        assert!(hover.unwrap().contains("total"));
    }

    #[test]
    fn test_parse_diagnostics_on_error() {
        let src = "program P;\nbegin\n  if then\nend.";
        let diags = parse_diagnostics(src);
        assert!(!diags.is_empty());
    }
}
