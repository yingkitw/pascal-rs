//! Error message suggestions using Levenshtein distance ("did you mean X?")

use strsim::levenshtein;
use std::cmp::Ordering;

/// Find the closest matching string from candidates using Levenshtein distance.
/// Returns `Some((suggestion, distance))` if a close match exists, `None` otherwise.
pub fn suggest_similar(typo: &str, candidates: &[impl AsRef<str>], max_distance: usize) -> Option<(String, usize)> {
    let typo_lower = typo.to_lowercase();
    let mut best: Option<(String, usize)> = None;

    for c in candidates {
        let s = c.as_ref();
        let s_lower = s.to_lowercase();
        let dist = levenshtein(&typo_lower, &s_lower);

        if dist <= max_distance && dist < typo.len() {
            let replace = match best {
                Some((_, d)) if dist < d => Some((s.to_string(), dist)),
                Some(_) => best,
                None => Some((s.to_string(), dist)),
            };
            best = replace;
        }
    }
    best
}

/// Format "did you mean X?" suggestion for inclusion in error messages
pub fn did_you_mean(_typo: &str, suggestion: &str) -> String {
    format!("did you mean `{}`?", suggestion)
}

/// Find best suggestion from identifier-like candidates (e.g. variable names, procedure names)
pub fn suggest_identifier(typo: &str, candidates: &[String]) -> Option<String> {
    // Prefer shorter distances; for similar distances prefer exact prefix match
    let mut scored: Vec<(String, usize)> = candidates
        .iter()
        .filter_map(|c| {
            let d = levenshtein(typo, c);
            if d <= typo.len().min(3) + 1 {
                Some((c.clone(), d))
            } else {
                None
            }
        })
        .collect();

    scored.sort_by(|a, b| {
        a.1.cmp(&b.1).then_with(|| {
            let a_prefix = c_starts_with(&a.0, typo);
            let b_prefix = c_starts_with(&b.0, typo);
            match (a_prefix, b_prefix) {
                (true, false) => Ordering::Less,
                (false, true) => Ordering::Greater,
                _ => Ordering::Equal,
            }
        })
    });

    scored.first().map(|(s, _)| s.clone())
}

fn c_starts_with(s: &str, prefix: &str) -> bool {
    s.to_lowercase().starts_with(&prefix.to_lowercase())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_suggest_similar() {
        let words = ["writeln", "readln", "write", "read"];
        let (s, _) = suggest_similar("writlen", &words, 3).unwrap();
        assert_eq!(s, "writeln");
    }

    #[test]
    fn test_suggest_identifier() {
        let names = ["foo", "bar", "foobar", "baz"];
        let s = suggest_identifier("foobr", &names.iter().map(|x| x.to_string()).collect::<Vec<_>>());
        assert_eq!(s.as_deref(), Some("foobar"));
    }
}
