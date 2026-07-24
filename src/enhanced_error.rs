//! Enhanced error handling for Pascal compiler
//! 
//! Provides comprehensive error reporting with source location tracking,
//! proper error hierarchy, and diagnostic information.

use std::path::PathBuf;
use thiserror::Error;

/// Source location information for error reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    /// File identifier (index into source manager)
    pub file_id: usize,
    /// Line number (1-based)
    pub line: usize,
    /// Column number (1-based)
    pub column: usize,
    /// Byte offset from start of file
    pub offset: usize,
    /// Length of the token/construct in bytes
    pub length: usize,
}

impl SourceLocation {
    /// Create a new source location
    pub fn new(file_id: usize, line: usize, column: usize, offset: usize, length: usize) -> Self {
        Self {
            file_id,
            line,
            column,
            offset,
            length,
        }
    }

    /// Format the location for display
    pub fn format(&self) -> String {
        format!("{}:{}:{}", self.line, self.column, self.length)
    }

    /// Check if this location is valid (not synthetic)
    pub fn is_valid(&self) -> bool {
        self.line > 0 && self.column > 0
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

/// Source manager for tracking file information
#[derive(Default)]
pub struct SourceManager {
    files: Vec<PathBuf>,
    file_contents: Vec<String>,
}

impl SourceManager {
    /// Create a new source manager
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a source file to the manager
    pub fn add_file(&mut self, path: PathBuf, content: String) -> usize {
        let file_id = self.files.len();
        self.files.push(path);
        self.file_contents.push(content);
        file_id
    }

    /// Get file path by ID
    pub fn get_file_path(&self, file_id: usize) -> Option<&PathBuf> {
        self.files.get(file_id)
    }

    /// Get file content by ID
    pub fn get_file_content(&self, file_id: usize) -> Option<&str> {
        self.file_contents.get(file_id).map(|s| s.as_str())
    }

    /// Get the number of files managed
    pub fn file_count(&self) -> usize {
        self.files.len()
    }

    /// Resolve a location to line and column from offset
    pub fn resolve_location(&self, file_id: usize, offset: usize) -> SourceLocation {
        if let Some(content) = self.get_file_content(file_id) {
            let mut line = 1;
            let mut column = 1;
            let mut current_offset = 0;
            
            for (i, c) in content.chars().enumerate() {
                if i >= offset {
                    break;
                }
                
                if c == '\n' {
                    line += 1;
                    column = 1;
                } else {
                    column += 1;
                }
                current_offset = i + 1;
            }
            
            SourceLocation {
                file_id,
                line,
                column,
                offset: current_offset,
                length: 1, // Default length
            }
        } else {
            SourceLocation {
                file_id,
                line: 1,
                column: 1,
                offset: 0,
                length: 0,
            }
        }
    }
}

/// Suggestion for error recovery
#[derive(Debug, Clone)]
pub struct ErrorSuggestion {
    pub message: String,
    pub code_snippet: Option<String>,
    pub replacement: Option<String>,
}

impl ErrorSuggestion {
    /// Create a new suggestion
    pub fn new(message: String) -> Self {
        Self {
            message,
            code_snippet: None,
            replacement: None,
        }
    }

    /// Create a suggestion with code snippet
    pub fn with_snippet(message: String, snippet: String) -> Self {
        Self {
            message,
            code_snippet: Some(snippet),
            replacement: None,
        }
    }

    /// Create a suggestion with replacement
    pub fn with_replacement(message: String, replacement: String) -> Self {
        Self {
            message,
            code_snippet: None,
            replacement: Some(replacement),
        }
    }
}

/// Severity level for diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
    Hint,
}

impl DiagnosticSeverity {
    /// Get the severity level as a string
    pub fn as_str(&self) -> &'static str {
        match self {
            DiagnosticSeverity::Error => "error",
            DiagnosticSeverity::Warning => "warning",
            DiagnosticSeverity::Info => "info",
            DiagnosticSeverity::Hint => "hint",
        }
    }

    /// Check if this severity should be reported
    pub fn should_report(&self) -> bool {
        matches!(self, DiagnosticSeverity::Error | DiagnosticSeverity::Warning)
    }
}

/// Diagnostic information for compiler messages
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: DiagnosticSeverity,
    pub location: SourceLocation,
    pub message: String,
    pub code: Option<String>,
    pub suggestions: Vec<ErrorSuggestion>,
    pub related_diagnostics: Vec<Diagnostic>,
}

impl Diagnostic {
    /// Create a new diagnostic
    pub fn new(severity: DiagnosticSeverity, location: SourceLocation, message: String) -> Self {
        Self {
            severity,
            location,
            message,
            code: None,
            suggestions: Vec::new(),
            related_diagnostics: Vec::new(),
        }
    }

    /// Create an error diagnostic
    pub fn error(location: SourceLocation, message: String) -> Self {
        Self::new(DiagnosticSeverity::Error, location, message)
    }

    /// Create a warning diagnostic
    pub fn warning(location: SourceLocation, message: String) -> Self {
        Self::new(DiagnosticSeverity::Warning, location, message)
    }

    /// Create an info diagnostic
    pub fn info(location: SourceLocation, message: String) -> Self {
        Self::new(DiagnosticSeverity::Info, location, message)
    }

    /// Create a hint diagnostic
    pub fn hint(location: SourceLocation, message: String) -> Self {
        Self::new(DiagnosticSeverity::Hint, location, message)
    }

    /// Add an error code
    pub fn with_code(mut self, code: String) -> Self {
        self.code = Some(code);
        self
    }

    /// Add a suggestion
    pub fn with_suggestion(mut self, suggestion: ErrorSuggestion) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    /// Add multiple suggestions
    pub fn with_suggestions(mut self, suggestions: Vec<ErrorSuggestion>) -> Self {
        self.suggestions.extend(suggestions);
        self
    }

    /// Add a related diagnostic
    pub fn with_related(mut self, related: Diagnostic) -> Self {
        self.related_diagnostics.push(related);
        self
    }

    /// Format the diagnostic for display
    pub fn format(&self, source_manager: &SourceManager) -> String {
        let file_path = if let Some(path) = source_manager.get_file_path(self.location.file_id) {
            path.to_string_lossy().to_string()
        } else {
            format!("<file {}>", self.location.file_id)
        };

        let location_str = if self.location.is_valid() {
            format!("{}:{}", file_path, self.location.format())
        } else {
            file_path
        };

        let mut output = format!("{}: {}: {}", location_str, self.severity.as_str(), self.message);

        if let Some(code) = &self.code {
            output.push_str(&format!(" (code: {})", code));
        }

        // Add suggestions if any
        for suggestion in &self.suggestions {
            output.push_str(&format!("\n  = note: {}", suggestion.message));
        }

        // Add related diagnostics
        for related in &self.related_diagnostics {
            output.push_str(&format!("\n  = {}: {}", related.severity.as_str(), related.message));
        }

        output
    }
}

/// Enhanced compiler error types with proper hierarchy
#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("IO error: {0}")]
    IoError(String),

    #[error("Lexical error at {location}: {message}")]
    LexerError {
        location: SourceLocation,
        message: String,
    },

    #[error("Syntax error at {location}: {message}")]
    SyntaxError {
        location: SourceLocation,
        message: String,
        expected_tokens: Vec<String>,
    },

    #[error("Semantic error at {location}: {message}")]
    SemanticError {
        location: SourceLocation,
        message: String,
        suggestion: Option<String>,
    },

    #[error("Type error at {location}: {message}")]
    TypeError {
        location: SourceLocation,
        message: String,
        expected: String,
        found: String,
    },

    #[error("Code generation error: {message}")]
    CodegenError {
        location: Option<SourceLocation>,
        message: String,
    },

    #[error("Optimizer error: {message}")]
    OptimizerError {
        location: Option<SourceLocation>,
        message: String,
    },

    #[error("Runtime error: {message}")]
    RuntimeError {
        location: Option<SourceLocation>,
        message: String,
    },

    #[error("Internal compiler error: {message}")]
    InternalError {
        location: Option<SourceLocation>,
        message: String,
    },

    #[error("Configuration error: {message}")]
    ConfigError {
        message: String,
    },
}

impl CompilerError {
    /// Create a lexer error
    pub fn lexer_error(location: SourceLocation, message: String) -> Self {
        Self::LexerError { location, message }
    }

    /// Create a syntax error
    pub fn syntax_error(location: SourceLocation, message: String, expected_tokens: Vec<String>) -> Self {
        Self::SyntaxError { location, message, expected_tokens }
    }

    /// Create a semantic error
    pub fn semantic_error(location: SourceLocation, message: String, suggestion: Option<String>) -> Self {
        Self::SemanticError { location, message, suggestion }
    }

    /// Create a type error
    pub fn type_error(location: SourceLocation, message: String, expected: String, found: String) -> Self {
        Self::TypeError { location, message, expected, found }
    }

    /// Create a code generation error
    pub fn codegen_error(location: Option<SourceLocation>, message: String) -> Self {
        Self::CodegenError { location, message }
    }

    /// Create a runtime error
    pub fn runtime_error(location: Option<SourceLocation>, message: String) -> Self {
        Self::RuntimeError { location, message }
    }

    /// Create an internal error
    pub fn internal_error(location: Option<SourceLocation>, message: String) -> Self {
        Self::InternalError { location, message }
    }

    /// Create a configuration error
    pub fn config_error(message: String) -> Self {
        Self::ConfigError { message }
    }

    /// Get the severity of this error
    pub fn severity(&self) -> DiagnosticSeverity {
        match self {
            CompilerError::IoError(_) => DiagnosticSeverity::Error,
            CompilerError::LexerError { .. } => DiagnosticSeverity::Error,
            CompilerError::SyntaxError { .. } => DiagnosticSeverity::Error,
            CompilerError::SemanticError { .. } => DiagnosticSeverity::Error,
            CompilerError::TypeError { .. } => DiagnosticSeverity::Error,
            CompilerError::CodegenError { .. } => DiagnosticSeverity::Error,
            CompilerError::OptimizerError { .. } => DiagnosticSeverity::Error,
            CompilerError::RuntimeError { .. } => DiagnosticSeverity::Error,
            CompilerError::InternalError { .. } => DiagnosticSeverity::Error,
            CompilerError::ConfigError { .. } => DiagnosticSeverity::Error,
        }
    }

    /// Get the source location of this error
    pub fn location(&self) -> Option<&SourceLocation> {
        match self {
            CompilerError::LexerError { location, .. } => Some(location),
            CompilerError::SyntaxError { location, .. } => Some(location),
            CompilerError::SemanticError { location, .. } => Some(location),
            CompilerError::TypeError { location, .. } => Some(location),
            CompilerError::CodegenError { location, .. } => location.as_ref(),
            CompilerError::OptimizerError { location, .. } => location.as_ref(),
            CompilerError::RuntimeError { location, .. } => location.as_ref(),
            CompilerError::InternalError { location, .. } => location.as_ref(),
            CompilerError::IoError(_) | CompilerError::ConfigError { .. } => None,
        }
    }

    /// Format the error as a diagnostic
    pub fn to_diagnostic(&self, _source_manager: &SourceManager) -> Diagnostic {
        let _severity = self.severity();
        let location = self.location().copied().unwrap_or_else(|| SourceLocation::new(0, 0, 0, 0, 0));
        
        let message = self.to_string();
        
        match self {
            CompilerError::SyntaxError { expected_tokens, .. } => {
                let suggestions = expected_tokens
                    .iter()
                    .map(|token| {
                        let mut suggestion = ErrorSuggestion::new(format!("Did you mean '{}'?", token));
                        suggestion.code_snippet = Some(token.clone());
                        suggestion
                    })
                    .collect();
                
                Diagnostic::error(location, message)
                    .with_suggestions(suggestions)
            },
            CompilerError::SemanticError { suggestion, .. } => {
                let suggestions = if let Some(suggestion) = suggestion {
                    vec![ErrorSuggestion::new(suggestion.clone())]
                } else {
                    Vec::new()
                };
                
                Diagnostic::error(location, message)
                    .with_suggestions(suggestions)
            },
            CompilerError::TypeError { expected, found, .. } => {
                let suggestion = ErrorSuggestion::new(format!("Expected type '{}', found type '{}'", expected, found));
                Diagnostic::error(location, message)
                    .with_suggestion(suggestion)
            },
            _ => Diagnostic::error(location, message),
        }
    }
}

/// Error reporter for collecting and formatting diagnostics
pub struct ErrorReporter {
    diagnostics: Vec<Diagnostic>,
    source_manager: SourceManager,
}

impl ErrorReporter {
    /// Create a new error reporter
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            source_manager: SourceManager::new(),
        }
    }

    /// Add a diagnostic
    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Report an error
    pub fn report_error(&mut self, error: CompilerError) {
        let diagnostic = error.to_diagnostic(&self.source_manager);
        self.add_diagnostic(diagnostic);
    }

    /// Add a source file
    pub fn add_file(&mut self, path: PathBuf, content: String) -> usize {
        self.source_manager.add_file(path, content)
    }

    /// Get all diagnostics
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Get error diagnostics only
    pub fn errors(&self) -> Vec<&Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Error)
            .collect()
    }

    /// Get warning diagnostics only
    pub fn warnings(&self) -> Vec<&Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Warning)
            .collect()
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        self.errors().is_empty()
    }

    /// Check if there are any warnings
    pub fn has_warnings(&self) -> bool {
        self.warnings().is_empty()
    }

    /// Get the number of errors
    pub fn error_count(&self) -> usize {
        self.errors().len()
    }

    /// Get the number of warnings
    pub fn warning_count(&self) -> usize {
        self.warnings().len()
    }

    /// Format all diagnostics
    pub fn format_diagnostics(&self) -> String {
        let mut output = String::new();
        
        for diagnostic in &self.diagnostics {
            if !output.is_empty() {
                output.push('\n');
            }
            output.push_str(&diagnostic.format(&self.source_manager));
        }
        
        output
    }

    /// Clear all diagnostics
    pub fn clear(&mut self) {
        self.diagnostics.clear();
    }
}

impl Default for ErrorReporter {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper functions for error reporting
pub mod error_utils {
    use super::*;

    /// Create a "did you mean" suggestion for identifiers
    pub fn did_you_mean(identifier: &str, candidates: &[&str]) -> Option<String> {
        if candidates.is_empty() {
            return None;
        }

        // Simple similarity check
        let best_candidate = candidates.iter()
            .map(|c| (c, strsim::jaro_winkler(identifier, c)))
            .max_by(|(_, sim1), (_, sim2)| sim1.partial_cmp(sim2).unwrap_or(std::cmp::Ordering::Less))?;

        if best_candidate.1 > 0.7 { // Similarity threshold
            Some(format!("Did you mean '{}'?", best_candidate.0))
        } else {
            None
        }
    }

    /// Create a suggestion for missing semicolon
    pub fn suggest_semicolon() -> ErrorSuggestion {
        ErrorSuggestion::with_replacement(
            "Missing semicolon".to_string(),
            ";".to_string(),
        )
    }

    /// Create a suggestion for missing closing bracket
    pub fn suggest_closing_bracket(opening: char) -> ErrorSuggestion {
        let closing = match opening {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            _ => '?',
        };
        
        ErrorSuggestion::with_replacement(
            format!("Missing closing '{}'", closing),
            closing.to_string(),
        )
    }

    /// Create a suggestion for missing begin/end
    pub fn suggest_begin_end() -> ErrorSuggestion {
        ErrorSuggestion::new(
            "Consider using 'begin' and 'end' to group statements".to_string(),
        )
    }
}

// Re-export common error types for convenience
pub use error_utils::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_location() {
        let location = SourceLocation::new(1, 10, 5, 100, 10);
        assert_eq!(location.file_id, 1);
        assert_eq!(location.line, 10);
        assert_eq!(location.column, 5);
        assert_eq!(location.offset, 100);
        assert_eq!(location.length, 10);
        assert!(location.is_valid());
    }

    #[test]
    fn test_diagnostic_creation() {
        let location = SourceLocation::new(0, 1, 1, 0, 1);
        let diagnostic = Diagnostic::error(location, "Test error message".to_string());
        
        assert_eq!(diagnostic.severity, DiagnosticSeverity::Error);
        assert_eq!(diagnostic.location, location);
        assert_eq!(diagnostic.message, "Test error message");
        assert!(diagnostic.suggestions.is_empty());
    }

    #[test]
    fn test_error_reporter() {
        let mut reporter = ErrorReporter::new();
        
        let location = SourceLocation::new(0, 1, 1, 0, 1);
        let diagnostic = Diagnostic::error(location, "Test error".to_string());
        reporter.add_diagnostic(diagnostic);
        
        assert_eq!(reporter.error_count(), 1);
        assert_eq!(reporter.warning_count(), 0);
        assert!(!reporter.has_errors());
        assert!(reporter.has_warnings());
    }

    #[test]
    fn test_error_utils() {
        // Test did_you_mean function
        let candidates = ["variable", "function", "procedure", "begin"];
        let suggestion = error_utils::did_you_mean("varible", &candidates);
        assert!(suggestion.is_some());
        assert!(suggestion.unwrap().contains("variable"));
    }
}