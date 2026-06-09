//! Visual Framework Module
//! 
//! Provides a comprehensive Delphi-like visual application framework with components,
//! forms, events, and themes.

pub mod components;
pub mod forms;
pub mod events;
pub mod themes;

// Re-export main types
pub use components::*;
pub use forms::*;
pub use events::*;
pub use themes::*;

/// Main application entry point for visual applications
pub struct VisualApplication {
    name: String,
    width: u32,
    height: u32,
    forms: Vec<Box<dyn Form>>,
    main_form_id: Option<String>,
    current_theme: Option<Box<dyn Theme>>,
}

impl VisualApplication {
    /// Create a new visual application
    pub fn new(name: &str, width: u32, height: u32) -> Self {
        Self {
            name: name.to_string(),
            width,
            height,
            forms: Vec::new(),
            main_form_id: None,
            current_theme: None,
        }
    }
    
    /// Add a form to the application
    pub fn add_form(&mut self, form: Box<dyn Form>) {
        self.forms.push(form);
    }
    
    /// Get a form by ID
    pub fn get_form(&self, form_id: &str) -> Option<&dyn Form> {
        self.forms.iter()
            .find(|f| f.id() == form_id)
            .map(|f| &**f as &dyn Form)
    }
    
    /// Set the main form
    pub fn set_main_form(&mut self, form_id: &str) {
        self.main_form_id = Some(form_id.to_string());
    }
    
    /// Apply a theme to the application
    pub fn apply_theme(&mut self, theme: Box<dyn Theme>) {
        self.current_theme = Some(theme);
    }
    
    /// Get the number of forms in the application
    pub fn form_count(&self) -> usize {
        self.forms.len()
    }
    
    /// Run the application (placeholder for actual GUI implementation)
    pub fn run(&self) {
        println!("Running application: {}", self.name);
        println!("Window size: {}x{}", self.width, self.height);
        println!("Number of forms: {}", self.form_count());
        
        if let Some(ref form_id) = self.main_form_id {
            if let Some(form) = self.get_form(form_id) {
                println!("Main form: {}", form.title());
            }
        }
        
        if let Some(ref theme) = self.current_theme {
            println!("Theme applied: {}", theme.name());
        }
    }
}

/// Trait for all visual components
pub trait Component {
    /// Get the component ID
    fn id(&self) -> &str;
    
    /// Get the component name
    fn name(&self) -> &str;
    
    /// Get the position and size
    fn bounds(&self) -> (i32, i32, u32, u32);
    
    /// Set the position and size
    fn set_bounds(&mut self, x: i32, y: i32, width: u32, height: u32);
    
    /// Get the parent form
    fn parent_form(&self) -> Option<String>;
    
    /// Set the parent form
    fn set_parent_form(&mut self, form_id: String);
    
    /// Handle events
    fn handle_event(&mut self, event: &Event) -> EventResult;
    
    /// Render the component (placeholder)
    fn render(&self) -> String {
        format!("Component: {}", self.name())
    }
}

/// Trait for all forms
pub trait Form {
    /// Get the form ID
    fn id(&self) -> &str;
    
    /// Get the form title
    fn title(&self) -> &str;
    
    /// Set the form title
    fn set_title(&mut self, title: &str);
    
    /// Get the form position and size
    fn bounds(&self) -> (i32, i32, u32, u32);
    
    /// Set the form position and size
    fn set_bounds(&mut self, x: i32, y: i32, width: u32, height: u32);
    
    /// Add a component to the form
    fn add_component(&mut self, component: Box<dyn Component>);
    
    /// Remove a component from the form
    fn remove_component(&mut self, component_id: &str) -> bool;
    
    /// Get all components
    fn components(&self) -> &Vec<Box<dyn Component>>;
    
    /// Handle events
    fn handle_event(&mut self, event: &Event) -> EventResult;
}

/// Result type for event handling
#[derive(Debug, Clone, PartialEq)]
pub enum EventResult {
    Handled,
    NotHandled,
    Propagate,
}

impl Default for EventResult {
    fn default() -> Self {
        EventResult::NotHandled
    }
}