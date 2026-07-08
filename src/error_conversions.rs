//! Error conversion utilities for Pascal compiler
//! 
//! Provides utilities to convert between different error formats and maintain backward compatibility.

use crate::error::CompilerError as OldCompilerError;
use crate::enhanced_error::{CompilerError, SourceLocation, ErrorReporter};

/// Convert legacy compiler error to enhanced error format
pub fn legacy_to_enhanced(error: OldCompilerError) -> CompilerError {
    match error {
        OldCompilerError::IoError(msg) => CompilerError::config_error(msg),
        OldCompilerError::ParseError(msg) => {
            // Use synthetic location for legacy errors
            let location = SourceLocation::new(0, 1, 1, 0, msg.len());
            CompilerError::semantic_error(location, msg, None)
        },
        OldCompilerError::UnexpectedToken(msg) => {
            let location = SourceLocation::new(0, 1, 1, 0, msg.len());
            CompilerError::syntax_error(location, msg, Vec::new())
        },
        OldCompilerError::CompilationError(msg) => {
            let location = SourceLocation::new(0, 1, 1, 0, msg.len());
            CompilerError::semantic_error(location, msg, None)
        },
    }
}

/// Convert enhanced compiler error to legacy format
pub fn enhanced_to_legacy(error: CompilerError) -> OldCompilerError {
    match error {
        CompilerError::IoError(msg) => OldCompilerError::IoError(msg),
        CompilerError::LexerError { location: _, message } => {
            OldCompilerError::ParseError(message)
        },
        CompilerError::SyntaxError { location: _, message, expected_tokens: _ } => {
            OldCompilerError::UnexpectedToken(message)
        },
        CompilerError::SemanticError { location: _, message, suggestion: _ } => {
            OldCompilerError::ParseError(message)
        },
        CompilerError::TypeError { location: _, message, .. } => {
            OldCompilerError::ParseError(message)
        },
        CompilerError::CodegenError { location: _, message } => {
            OldCompilerError::CompilationError(message)
        },
        CompilerError::OptimizerError { location: _, message } => {
            OldCompilerError::CompilationError(message)
        },
        CompilerError::RuntimeError { location: _, message } => {
            OldCompilerError::CompilationError(message)
        },
        CompilerError::InternalError { location: _, message } => {
            OldCompilerError::CompilationError(message)
        },
        CompilerError::ConfigError { message } => {
            OldCompilerError::IoError(message)
        },
    }
}

/// Add error reporting to existing error types
pub trait ErrorReporting {
    fn report_to(&self, reporter: &mut ErrorReporter);
}

impl ErrorReporting for OldCompilerError {
    fn report_to(&self, reporter: &mut ErrorReporter) {
        let enhanced = legacy_to_enhanced(self.clone());
        reporter.report_error(enhanced);
    }
}

/// Enhanced result type for better error handling
pub type Result<T> = std::result::Result<T, CompilerError>;

/// Legacy result type for backward compatibility
pub type LegacyResult<T> = std::result::Result<T, OldCompilerError>;

/// Conversion trait for enhanced results
pub trait ResultExt<T> {
    /// Convert legacy result to enhanced result
    fn to_enhanced(self) -> Result<T>;
    
    /// Report error to reporter on failure
    fn report_on_error(self, reporter: &mut ErrorReporter) -> Result<T>;
}

impl<T> ResultExt<T> for LegacyResult<T> {
    fn to_enhanced(self) -> Result<T> {
        self.map_err(legacy_to_enhanced)
    }
    
    fn report_on_error(self, reporter: &mut ErrorReporter) -> Result<T> {
        match self {
            Ok(value) => Ok(value),
            Err(legacy_error) => {
                legacy_error.report_to(reporter);
                Err(legacy_to_enhanced(legacy_error))
            }
        }
    }
}

/// Helper to create formatted error messages
pub fn format_error_message(error: &CompilerError, _source_manager: &ErrorReporter) -> String {
    match error {
        CompilerError::LexerError { location, message } => {
            format!("Lexical error at {}: {}", location.format(), message)
        },
        CompilerError::SyntaxError { location, message, expected_tokens } => {
            let expected = if expected_tokens.is_empty() {
                "token".to_string()
            } else {
                expected_tokens.join(", ")
            };
            format!("Syntax error at {}: Expected {}, found {}", 
                   location.format(), expected, message)
        },
        CompilerError::SemanticError { location, message, suggestion } => {
            let suggestion_text = if let Some(s) = suggestion {
                format!(" ({})", s)
            } else {
                "".to_string()
            };
            format!("Semantic error at {}: {}{}", location.format(), message, suggestion_text)
        },
        CompilerError::TypeError { location, message: _, expected, found } => {
            format!("Type error at {}: Expected {}, found {}", 
                   location.format(), expected, found)
        },
        CompilerError::CodegenError { location, message } => {
            if let Some(loc) = location {
                format!("Code generation error at {}: {}", loc.format(), message)
            } else {
                format!("Code generation error: {}", message)
            }
        },
        CompilerError::OptimizerError { location, message } => {
            if let Some(loc) = location {
                format!("Optimizer error at {}: {}", loc.format(), message)
            } else {
                format!("Optimizer error: {}", message)
            }
        },
        CompilerError::RuntimeError { location, message } => {
            if let Some(loc) = location {
                format!("Runtime error at {}: {}", loc.format(), message)
            } else {
                format!("Runtime error: {}", message)
            }
        },
        CompilerError::InternalError { location, message } => {
            if let Some(loc) = location {
                format!("Internal error at {}: {}", loc.format(), message)
            } else {
                format!("Internal error: {}", message)
            }
        },
        CompilerError::IoError(message) | CompilerError::ConfigError { message } => {
            format!("{}: {}", error.severity().as_str(), message)
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::enhanced_error::SourceLocation;

    #[test]
    fn test_error_conversion() {
        // Test legacy to enhanced conversion
        let legacy_error = OldCompilerError::ParseError("Test error".to_string());
        let enhanced_error = legacy_to_enhanced(legacy_error);
        
        assert!(matches!(enhanced_error, CompilerError::SemanticError { .. }));
    }

    #[test]
    fn test_enhanced_to_legacy() {
        // Test enhanced to legacy conversion
        let location = SourceLocation::new(0, 1, 1, 0, 0);
        let enhanced_error = CompilerError::syntax_error(location, "Test error".to_string(), Vec::new());
        let legacy_error = enhanced_to_legacy(enhanced_error);
        
        assert!(matches!(legacy_error, OldCompilerError::UnexpectedToken(..)));
    }

    #[test]
    fn test_error_reporting() {
        let mut reporter = ErrorReporter::new();
        let legacy_error = OldCompilerError::ParseError("Test error".to_string());
        
        legacy_error.report_to(&mut reporter);
        
        assert_eq!(reporter.error_count(), 1);
        assert_eq!(reporter.diagnostics().len(), 1);
    }

    #[test]
    fn test_result_extensions() {
        let mut reporter = ErrorReporter::new();
        let legacy_result1: LegacyResult<i32> = Err(OldCompilerError::ParseError("Test".to_string()));
        let legacy_result2 = legacy_result1.clone();
        
        let enhanced_result = legacy_result1.to_enhanced();
        assert!(enhanced_result.is_err());
        
        let result_with_reporting = legacy_result2.report_on_error(&mut reporter);
        assert!(result_with_reporting.is_err());
        assert_eq!(reporter.error_count(), 1);
    }
}