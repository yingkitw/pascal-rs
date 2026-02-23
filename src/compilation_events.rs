//! Event-driven compilation phases
//!
//! Allows subscribers to react to compilation events without tight coupling.

use std::sync::Arc;

/// Compilation events emitted during the build pipeline
#[derive(Debug, Clone)]
pub enum CompilationEvent {
    /// Lexing started for a source file
    LexStart { path: String },
    /// Lexing finished
    LexEnd { path: String, token_count: usize },
    /// Parsing started
    ParseStart { path: String },
    /// Program parsed successfully
    ParseProgramEnd { name: String },
    /// Unit parsed successfully
    ParseUnitEnd { name: String, path: String },
    /// Type check started
    TypeCheckStart { unit: String },
    /// Type check finished
    TypeCheckEnd { unit: String },
    /// Code generation started
    CodegenStart { unit: String },
    /// Code generation finished
    CodegenEnd { unit: String, output_size: usize },
    /// Build finished successfully
    BuildComplete { units: usize },
    /// Build failed
    BuildFailed { message: String },
}

/// Handler for compilation events (e.g. progress UI, logging, metrics)
pub trait EventHandler: Send + Sync {
    fn on_event(&self, event: &CompilationEvent);
}

impl<F> EventHandler for F
where
    F: Fn(&CompilationEvent) + Send + Sync,
{
    fn on_event(&self, event: &CompilationEvent) {
        (self)(event)
    }
}

/// Emits compilation events to registered handlers
#[derive(Clone, Default)]
pub struct EventEmitter {
    handlers: Vec<Arc<dyn EventHandler>>,
}

impl EventEmitter {
    pub fn new() -> Self {
        Self {
            handlers: Vec::new(),
        }
    }

    /// Subscribe to compilation events
    pub fn subscribe(&mut self, handler: Arc<dyn EventHandler>) {
        self.handlers.push(handler);
    }

    /// Emit an event to all subscribers
    pub fn emit(&self, event: CompilationEvent) {
        for h in &self.handlers {
            h.on_event(&event);
        }
    }

    /// Emit and return (convenience for chaining)
    pub fn emit_and<T>(&self, event: CompilationEvent, value: T) -> T {
        self.emit(event);
        value
    }
}
