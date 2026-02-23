//! Documentation generator for Pascal sources
//!
//! Generates Markdown and HTML documentation from parsed programs and units.

use crate::ast::{
    Block, ClassDecl, FunctionDecl, ProcedureDecl, Program, Type, Unit, UnitInterface,
    UnitImplementation,
};
use anyhow::Result;
use std::path::Path;

/// Format a Type for documentation
fn format_type(typ: &Type) -> String {
    match typ {
        Type::Simple(s) => format!("{:?}", s),
        Type::Integer => "integer".to_string(),
        Type::Real => "real".to_string(),
        Type::Boolean => "boolean".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::WideString => "widestring".to_string(),
        Type::Alias { name, .. } => name.clone(),
        Type::Array {
            element_type,
            range,
            ..
        } => {
            let range_str = range
                .map(|(l, u)| format!("[{}..{}]", l, u))
                .unwrap_or_else(|| "[]".to_string());
            format!("array{} of {}", range_str, format_type(element_type))
        }
        Type::Record { .. } => "record".to_string(),
        Type::Pointer(inner) => format!("^{}", format_type(inner)),
        Type::Generic { name, .. } => name.clone(),
        Type::GenericInstance {
            base_type,
            type_arguments,
        } => {
            let args: Vec<String> = type_arguments.iter().map(format_type).collect();
            format!("{}<{}>", base_type, args.join(", "))
        }
        _ => format!("{:?}", typ),
    }
}

/// Generate Markdown documentation for a program
pub fn generate_program_markdown(program: &Program, source_path: Option<&Path>) -> String {
    let mut md = String::new();

    md.push_str(&format!("# Program `{}`\n\n", program.name));

    if let Some(p) = source_path {
        md.push_str(&format!("*Source: {}*\n\n", p.display()));
    }

    if !program.uses.is_empty() {
        md.push_str("## Uses\n\n");
        for u in &program.uses {
            md.push_str(&format!("- `{}`\n", u));
        }
        md.push('\n');
    }

    md.push_str("## Declarations\n\n");
    generate_block_markdown(&program.block, &mut md);
    md
}

/// Generate Markdown documentation for a unit
pub fn generate_unit_markdown(unit: &Unit, source_path: Option<&Path>) -> String {
    let mut md = String::new();

    md.push_str(&format!("# Unit `{}`\n\n", unit.name));

    if let Some(p) = source_path {
        md.push_str(&format!("*Source: {}*\n\n", p.display()));
    }

    if !unit.uses.is_empty() {
        md.push_str("## Uses\n\n");
        for u in &unit.uses {
            md.push_str(&format!("- `{}`\n", u));
        }
        md.push('\n');
    }

    md.push_str("## Interface\n\n");
    generate_interface_markdown(&unit.interface, &mut md);

    md.push_str("\n## Implementation\n\n");
    generate_impl_markdown(&unit.implementation, &mut md);

    md
}

fn generate_impl_markdown(impl_: &UnitImplementation, md: &mut String) {
    generate_interface_like_markdown(
        &impl_.uses,
        &impl_.types,
        &impl_.constants,
        &impl_.variables,
        &impl_.procedures,
        &impl_.functions,
        &impl_.classes,
        &impl_.interfaces,
        md,
    );
}

fn generate_interface_markdown(iface: &UnitInterface, md: &mut String) {
    generate_interface_like_markdown(
        &iface.uses,
        &iface.types,
        &iface.constants,
        &iface.variables,
        &iface.procedures,
        &iface.functions,
        &iface.classes,
        &iface.interfaces,
        md,
    );
}

fn generate_interface_like_markdown(
    _uses: &[String],
    types: &[crate::ast::TypeDecl],
    constants: &[crate::ast::ConstDecl],
    variables: &[crate::ast::VariableDecl],
    procedures: &[ProcedureDecl],
    functions: &[FunctionDecl],
    classes: &[ClassDecl],
    interfaces: &[crate::ast::InterfaceDecl],
    md: &mut String,
) {
    if !constants.is_empty() {
        md.push_str("### Constants\n\n");
        for c in constants {
            md.push_str(&format!("- `{}` = …\n", c.name));
        }
        md.push('\n');
    }

    if !types.is_empty() {
        md.push_str("### Types\n\n");
        for t in types {
            md.push_str(&format!(
                "- `{}` : {}\n",
                t.name,
                format_type(&t.type_definition)
            ));
        }
        md.push('\n');
    }

    if !variables.is_empty() {
        md.push_str("### Variables\n\n");
        for v in variables {
            md.push_str(&format!(
                "- `{}` : {}\n",
                v.name,
                format_type(&v.variable_type)
            ));
        }
        md.push('\n');
    }

    if !procedures.is_empty() {
        md.push_str("### Procedures\n\n");
        for p in procedures {
            md.push_str(&format!("- `{}`\n", format_procedure_sig(p)));
        }
        md.push('\n');
    }

    if !functions.is_empty() {
        md.push_str("### Functions\n\n");
        for f in functions {
            md.push_str(&format!("- `{}`\n", format_function_sig(f)));
        }
        md.push('\n');
    }

    if !classes.is_empty() {
        md.push_str("### Classes\n\n");
        for c in classes {
            generate_class_markdown(c, md);
        }
        md.push('\n');
    }

    if !interfaces.is_empty() {
        md.push_str("### Interfaces\n\n");
        for i in interfaces {
            md.push_str(&format!("- `{}`\n", i.name));
        }
        md.push('\n');
    }
}

fn format_procedure_sig(p: &ProcedureDecl) -> String {
    let params: Vec<String> = p
        .parameters
        .iter()
        .map(|par| {
            let mut s = par.name.clone();
            if par.is_var {
                s.insert_str(0, "var ");
            }
            if par.is_const {
                s.insert_str(0, "const ");
            }
            if par.is_out {
                s.insert_str(0, "out ");
            }
            format!("{}: {}", s, format_type(&par.param_type))
        })
        .collect();
    format!("procedure {} ({})", p.name, params.join("; "))
}

fn format_function_sig(f: &FunctionDecl) -> String {
    let params: Vec<String> = f
        .parameters
        .iter()
        .map(|par| format!("{}: {}", par.name, format_type(&par.param_type)))
        .collect();
    format!(
        "function {} ({}): {}",
        f.name,
        params.join("; "),
        format_type(&f.return_type)
    )
}

fn generate_class_markdown(c: &ClassDecl, md: &mut String) {
    md.push_str(&format!("#### `{}`\n\n", c.name));
    if c.parent.is_some() || !c.interfaces.is_empty() {
        md.push_str(&format!("- Parent: `{}`\n", c.parent.as_deref().unwrap_or("-")));
        if !c.interfaces.is_empty() {
            md.push_str(&format!(
                "- Interfaces: {}\n",
                c.interfaces
                    .iter()
                    .map(|s| format!("`{}`", s))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
    }
    for f in &c.fields {
        md.push_str(&format!("  - `{}` : {}\n", f.name, format_type(&f.field_type)));
    }
    for m in &c.methods {
        let ret = m
            .return_type
            .as_ref()
            .map(|t| format!(": {}", format_type(t)))
            .unwrap_or_default();
        md.push_str(&format!("  - `{}`{}\n", m.name, ret));
    }
    md.push('\n');
}

fn generate_block_markdown(block: &Block, md: &mut String) {
    for c in &block.consts {
        md.push_str(&format!("- `{}` (const)\n", c.name));
    }
    for t in &block.types {
        md.push_str(&format!("- `{}` : type\n", t.name));
    }
    for v in &block.vars {
        md.push_str(&format!(
            "- `{}` : {}\n",
            v.name,
            format_type(&v.variable_type)
        ));
    }
    for p in &block.procedures {
        md.push_str(&format!("- `{}` (procedure)\n", p.name));
    }
    for f in &block.functions {
        md.push_str(&format!("- `{}` (function)\n", format_function_sig(f)));
    }
    for c in &block.classes {
        md.push_str(&format!("- `{}` (class)\n", c.name));
    }
}

/// Convert Markdown to HTML using pulldown-cmark
pub fn markdown_to_html(md: &str) -> String {
    use pulldown_cmark::{html, Options, Parser};

    let mut options = Options::empty();
    options.insert(Options::ENABLE_STRIKETHROUGH);
    options.insert(Options::ENABLE_TABLES);

    let parser = Parser::new_ext(md, options);
    let mut html_buf = String::new();
    html::push_html(&mut html_buf, parser);

    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Pascal Documentation</title>
  <style>
    body {{ font-family: system-ui, sans-serif; max-width: 800px; margin: 2rem auto; padding: 0 1rem; }}
    code {{ background: #f4f4f4; padding: 0.2em 0.4em; border-radius: 4px; }}
    pre {{ background: #f4f4f4; padding: 1rem; overflow-x: auto; }}
    h1 {{ border-bottom: 1px solid #ccc; }}
    h2, h3 {{ color: #333; }}
  </style>
</head>
<body>
{}
</body>
</html>
"#,
        html_buf
    )
}

/// Generate documentation (Markdown or HTML) from source file
pub fn generate_docs_from_source(
    source: &str,
    source_path: &Path,
    output_format: DocFormat,
) -> Result<String> {
    let mut parser_prog = crate::parser::Parser::new(source);
    let prog_result = parser_prog.parse_program();

    if let Ok(prog) = prog_result {
        let md = generate_program_markdown(&prog, Some(source_path));
        return Ok(match output_format {
            DocFormat::Markdown => md,
            DocFormat::Html => markdown_to_html(&md),
        });
    }

    let mut parser_unit = crate::parser::Parser::new(source);
    if let Ok(unit) = parser_unit.parse_unit() {
        let md = generate_unit_markdown(&unit, Some(source_path));
        return Ok(match output_format {
            DocFormat::Markdown => md,
            DocFormat::Html => markdown_to_html(&md),
        });
    }

    Err(anyhow::anyhow!(
        "Failed to parse as program or unit. Use 'pascal check' for details."
    ))
}

/// Output format for documentation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocFormat {
    Markdown,
    Html,
}

impl std::str::FromStr for DocFormat {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "md" | "markdown" => Ok(DocFormat::Markdown),
            "html" => Ok(DocFormat::Html),
            _ => Err(format!("Unknown format: {}. Use: markdown, html", s)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_program_markdown() {
        let prog = crate::ast::Program {
            name: "TestProg".to_string(),
            uses: vec!["Crt".to_string()],
            block: crate::ast::Block::empty(),
        };
        let md = generate_program_markdown(&prog, None);
        assert!(md.contains("# Program `TestProg`"));
        assert!(md.contains("## Uses"));
        assert!(md.contains("`Crt`"));
    }

    #[test]
    fn test_generate_docs_from_source_program() {
        let source = "program Foo; begin end.";
        let out = generate_docs_from_source(
            source,
            std::path::Path::new("foo.pas"),
            DocFormat::Markdown,
        )
        .unwrap();
        assert!(out.contains("# Program `Foo`"));
    }

    #[test]
    fn test_doc_format_parse() {
        assert!("markdown".parse::<DocFormat>().is_ok());
        assert!("html".parse::<DocFormat>().is_ok());
        assert!("md".parse::<DocFormat>().is_ok());
        assert!("HTML".parse::<DocFormat>().is_ok());
        assert!("unknown".parse::<DocFormat>().is_err());
    }

    #[test]
    fn test_markdown_to_html() {
        let html = markdown_to_html("# Hello\n*world*");
        assert!(html.contains("<h1>"));
        assert!(html.contains("Hello"));
        assert!(html.contains("<html"));
    }
}
