//! Conditional compilation preprocessor
//!
//! Supports {$IFDEF}, {$IFNDEF}, {$ENDIF}, {$DEFINE}, {$UNDEF} directives.
//! Run `preprocess(source, defines)` before parsing.

use std::collections::HashSet;

/// Preprocess source with conditional compilation directives.
/// `defines` is the initial set of defined symbols (e.g. from command line).
/// Returns the filtered source suitable for lexing/parsing.
pub fn preprocess(source: &str, defines: &HashSet<String>) -> String {
    let mut output = String::new();
    let mut defs = defines.clone();
    let mut stack: Vec<bool> = vec![]; // Whether we're including the current block
    let mut i = 0;
    let bytes = source.as_bytes();

    while i < bytes.len() {
        if i + 2 < bytes.len() && bytes[i] == b'{' && bytes[i + 1] == b'$' {
            let start = i;
            i += 2;
            let mut directive_end = i;
            while directive_end < bytes.len() && bytes[directive_end] != b'}' {
                directive_end += 1;
            }
            let directive_str =
                std::str::from_utf8(&bytes[i..directive_end]).unwrap_or("").trim();
            let end = if directive_end < bytes.len() {
                directive_end + 1
            } else {
                bytes.len()
            };

            let parts: Vec<&str> = directive_str.split_whitespace().collect();
            let currently_including = stack.is_empty() || stack.iter().all(|&x| x);
            let handled = match parts.as_slice() {
                ["IFDEF", sym] => {
                    let include = currently_including && defs.contains(&sym.to_string().to_uppercase());
                    stack.push(include);
                    true
                }
                ["IFNDEF", sym] => {
                    let include = currently_including && !defs.contains(&sym.to_string().to_uppercase());
                    stack.push(include);
                    true
                }
                ["ENDIF"] => {
                    stack.pop();
                    true
                }
                ["DEFINE", sym] => {
                    if currently_including {
                        defs.insert(sym.to_string().to_uppercase());
                    }
                    true
                }
                ["UNDEF", sym] => {
                    if currently_including {
                        defs.remove(&sym.to_string().to_uppercase());
                    }
                    true
                }
                _ => false,
            };

            if handled {
                i = end;
                if !stack.is_empty() && !stack.iter().all(|&x| x) {
                    // We're in a skipped block, don't output the directive
                } else if parts.first() == Some(&"DEFINE") || parts.first() == Some(&"UNDEF") {
                    // DEFINE/UNDEF don't produce output
                } else {
                    // Keep IFDEF/IFNDEF/ENDIF as comments or omit - we omit for clean output
                }
                continue;
            }
            i = start; // Re-parse from { for non-directive
        }

        // Output character if we're in an included block
        let include = stack.is_empty() || stack.iter().all(|&x| x);
        if include {
            output.push(bytes[i] as char);
        }
        i += 1;
    }

    output
}

/// Build a defines set from command-line args (e.g. -DDEBUG -DFOO=1)
pub fn defines_from_args(args: &[String]) -> HashSet<String> {
    let mut defs = HashSet::new();
    for arg in args {
        if let Some(sym) = arg.strip_prefix("-D") {
            let name = sym.split('=').next().unwrap_or(sym).to_uppercase();
            defs.insert(name);
        }
    }
    defs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ifdef_defined() {
        let mut defs = HashSet::new();
        defs.insert("DEBUG".to_string());
        let src = r#"program X;
{$IFDEF DEBUG}
var x: integer;
{$ENDIF}
begin end."#;
        let out = preprocess(src, &defs);
        assert!(out.contains("var x: integer;"));
    }

    #[test]
    fn test_ifdef_undefined() {
        let defs = HashSet::new();
        let src = r#"program X;
{$IFDEF DEBUG}
var x: integer;
{$ENDIF}
begin end."#;
        let out = preprocess(src, &defs);
        assert!(!out.contains("var x: integer;"));
    }

    #[test]
    fn test_ifndef_undefined() {
        let defs = HashSet::new();
        let src = r#"program X;
{$IFNDEF RELEASE}
var x: integer;
{$ENDIF}
begin end."#;
        let out = preprocess(src, &defs);
        assert!(out.contains("var x: integer;"));
    }

    #[test]
    fn test_define_in_file() {
        let defs = HashSet::new();
        let src = r#"program X;
{$DEFINE FOO}
{$IFDEF FOO}
var x: integer;
{$ENDIF}
begin end."#;
        let out = preprocess(src, &defs);
        assert!(out.contains("var x: integer;"));
    }
}
