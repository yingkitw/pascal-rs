//! Diagnostic and error reporting trait definitions

use std::fmt;

/// Diagnostic severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Hint,
    Info,
    Warning,
    Error,
    Fatal,
}

/// Source location for diagnostics
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub span: std::ops::Range<usize>,
}

/// Diagnostic message
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub location: DiagnosticLocation,
    pub hints: Vec<String>,
    pub code: Option<String>,
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let severity_str = match self.severity {
            Severity::Hint => "hint",
            Severity::Info => "info",
            Severity::Warning => "warning",
            Severity::Error => "error",
            Severity::Fatal => "fatal error",
        };

        write!(
            f,
            "{}:{}:{}: {}: {}",
            self.location.file,
            self.location.line,
            self.location.column,
            severity_str,
            self.message
        )?;

        if let Some(code) = &self.code {
            write!(f, " [{}]", code)?;
        }

        Ok(())
    }
}

/// Diagnostic emitter trait
pub trait DiagnosticEmitter {
    /// Emit a diagnostic message
    fn emit(&mut self, diagnostic: Diagnostic);

    /// Emit an error
    fn emit_error(&mut self, location: DiagnosticLocation, message: &str) {
        self.emit(Diagnostic {
            severity: Severity::Error,
            message: message.to_string(),
            location,
            hints: vec![],
            code: None,
        });
    }

    /// Emit a warning
    fn emit_warning(&mut self, location: DiagnosticLocation, message: &str) {
        self.emit(Diagnostic {
            severity: Severity::Warning,
            message: message.to_string(),
            location,
            hints: vec![],
            code: None,
        });
    }

    /// Emit a hint
    fn emit_hint(&mut self, location: DiagnosticLocation, message: &str) {
        self.emit(Diagnostic {
            severity: Severity::Hint,
            message: message.to_string(),
            location,
            hints: vec![],
            code: None,
        });
    }

    /// Check if any errors were emitted
    fn has_errors(&self) -> bool;

    /// Get error count
    fn error_count(&self) -> usize;

    /// Get warning count
    fn warning_count(&self) -> usize;

    /// Clear all diagnostics
    fn clear(&mut self);

    /// Get all diagnostics
    fn diagnostics(&self) -> &[Diagnostic];
}

/// Diagnostic rendering trait
pub trait DiagnosticRenderer {
    /// Render diagnostics to string
    fn render(&self, diagnostic: &Diagnostic) -> String;

    /// Render with source code context
    fn render_with_context(&self, diagnostic: &Diagnostic, source: &str) -> String;

    /// Render multiple diagnostics
    fn render_all(&self, diagnostics: &[Diagnostic]) -> String;

    /// Color output (if supported)
    fn enable_colors(&mut self, enabled: bool);
}

/// Error reporting trait
pub trait DiagnosticErrorReporter {
    /// Report error with message
    fn report_error(&mut self, message: &str);

    /// Report error with location
    fn report_error_with_location(&mut self, location: DiagnosticLocation, message: &str);

    /// Report warning
    fn report_warning(&mut self, message: &str);

    /// Report fatal error (usually exits)
    fn report_fatal(&mut self, message: &str) -> !;

    /// Check if errors occurred
    fn has_errors(&self) -> bool;
}

/// Source code caching for diagnostics
pub trait SourceCache {
    /// Add source file to cache
    fn add_source(&mut self, file: String, source: String);

    /// Get source file from cache
    fn get_source(&self, file: &str) -> Option<&str>;

    /// Get line from source
    fn get_line(&self, file: &str, line: usize) -> Option<&str>;

    /// Remove source from cache
    fn remove_source(&mut self, file: &str);
}
