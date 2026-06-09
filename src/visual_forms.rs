//! Visual Forms Module
//! 
//! Provides form management and form designer functionality

use crate::visual_framework::{Component, Form, Event, EventResult};

/// Form implementation
pub struct VisualForm {
    id: String,
    title: String,
    x: i32,
    y: i32,
    width: u32,
    height: u32,
    components: Vec<Box<dyn Component>>,
}

impl VisualForm {
    /// Create a new form
    pub fn new(id: &str, x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            id: id.to_string(),
            title: String::new(),
            x,
            y,
            width,
            height,
            components: Vec::new(),
        }
    }
    
    /// Get form ID
    pub fn id(&self) -> &str {
        &self.id
    }
    
    /// Get form title
    pub fn title(&self) -> &str {
        &self.title
    }
    
    /// Set form title
    pub fn set_title(&mut self, title: &str) {
        self.title = title.to_string();
    }
    
    /// Get form position and size
    pub fn bounds(&self) -> (i32, i32, u32, u32) {
        (self.x, self.y, self.width, self.height)
    }
    
    /// Set form position and size
    pub fn set_bounds(&mut self, x: i32, y: i32, width: u32, height: u32) {
        self.x = x;
        self.y = y;
        self.width = width;
        self.height = height;
    }
    
    /// Add a component to the form
    pub fn add_component(&mut self, mut component: Box<dyn Component>) {
        component.set_parent_form(self.id.clone());
        self.components.push(component);
    }
    
    /// Get all components
    pub fn components(&self) -> &Vec<Box<dyn Component>> {
        &self.components
    }
    
    /// Remove a component by ID
    pub fn remove_component(&mut self, component_id: &str) -> bool {
        let initial_len = self.components.len();
        self.components.retain(|c| c.id() != component_id);
        self.components.len() < initial_len
    }
    
    /// Find a component by ID
    pub fn find_component(&self, component_id: &str) -> Option<&dyn Component> {
        self.components.iter()
            .find(|c| c.id() == component_id)
            .map(|c| &**c as &dyn Component)
    }
    
    /// Handle form events
    pub fn handle_event(&mut self, event: &Event) -> EventResult {
        // Route events to components
        for component in &mut self.components {
            let result = component.handle_event(event);
            if result != EventResult::NotHandled {
                return result;
            }
        }
        EventResult::NotHandled
    }
}

/// Form designer for visual development
pub struct FormDesigner {
    current_form: Option<Box<dyn Form>>,
    tool_palette: ToolPalette,
    property_inspector: PropertyInspector,
}

impl FormDesigner {
    /// Create a new form designer
    pub fn new() -> Self {
        Self {
            current_form: None,
            tool_palette: ToolPalette::new(),
            property_inspector: PropertyInspector::new(),
        }
    }
    
    /// Set the current form for design
    pub fn set_current_form(&mut self, form: Box<dyn Form>) {
        self.current_form = Some(form);
    }
    
    /// Get the current form
    pub fn current_form(&self) -> Option<&dyn Form> {
        self.current_form.as_ref().map(|f| &**f as &dyn Form)
    }
    
    /// Get the tool palette
    pub fn tool_palette(&self) -> &ToolPalette {
        &self.tool_palette
    }
    
    /// Get the property inspector
    pub fn property_inspector(&self) -> &PropertyInspector {
        &self.property_inspector
    }
    
    /// Add a component to the current form
    pub fn add_component(&mut self, component_type: ComponentType, x: i32, y: i32) -> Result<(), String> {
        if let Some(ref mut form) = self.current_form {
            let component = match component_type {
                ComponentType::Button => {
                    let mut button = crate::visual_components::Button::new("btn", x, y, 100, 30);
                    button.set_text("Button");
                    Box::new(button) as Box<dyn Component>
                }
                ComponentType::Label => {
                    let mut label = crate::visual_components::Label::new("lbl", x, y, 100, 20);
                    label.set_text("Label");
                    Box::new(label) as Box<dyn Component>
                }
                ComponentType::Edit => {
                    let mut edit = crate::visual_components::Edit::new("edit", x, y, 150, 25);
                    Box::new(edit) as Box<dyn Component>
                }
                ComponentType::ListBox => {
                    let mut listbox = crate::visual_components::ListBox::new("list", x, y, 200, 100);
                    Box::new(listbox) as Box<dyn Component>
                }
                ComponentType::Panel => {
                    let mut panel = crate::visual_components::Panel::new("panel", x, y, 200, 150);
                    Box::new(panel) as Box<dyn Component>
                }
            };
            
            form.add_component(component);
            Ok(())
        } else {
            Err("No current form for design".to_string())
        }
    }
    
    /// Remove selected component
    pub fn remove_selected_component(&mut self) -> Result<(), String> {
        if let Some(ref mut form) = self.current_form {
            if let Some(selected_id) = self.property_inspector.selected_component() {
                if form.remove_component(selected_id) {
                    Ok(())
                } else {
                    Err("Selected component not found".to_string())
                }
            } else {
                Err("No component selected".to_string())
            }
        } else {
            Err("No current form for design".to_string())
        }
    }
}

/// Tool palette for form designer
pub struct ToolPalette {
    components: Vec<(ComponentType, String)>,
}

impl ToolPalette {
    /// Create a new tool palette
    pub fn new() -> Self {
        let components = vec![
            (ComponentType::Button, String::from("Button")),
            (ComponentType::Label, String::from("Label")),
            (ComponentType::Edit, String::from("Edit Box")),
            (ComponentType::ListBox, String::from("List Box")),
            (ComponentType::Panel, String::from("Panel")),
        ];
        
        Self { components }
    }
    
    /// Get available components
    pub fn components(&self) -> &[(ComponentType, String)] {
        &self.components
    }
}

/// Property inspector for form designer
pub struct PropertyInspector {
    selected_component_id: Option<String>,
    properties: std::collections::HashMap<String, String>,
}

impl PropertyInspector {
    /// Create a new property inspector
    pub fn new() -> Self {
        Self {
            selected_component_id: None,
            properties: std::collections::HashMap::new(),
        }
    }
    
    /// Select a component
    pub fn select_component(&mut self, component_id: &str) {
        self.selected_component_id = Some(component_id.to_string());
    }
    
    /// Get selected component ID
    pub fn selected_component(&self) -> Option<&str> {
        self.selected_component_id.as_deref()
    }
    
    /// Update a property
    pub fn set_property(&mut self, key: &str, value: &str) {
        self.properties.insert(key.to_string(), value.to_string());
    }
    
    /// Get a property
    pub fn get_property(&self, key: &str) -> Option<&str> {
        self.properties.get(key).map(|s| s.as_str())
    }
    
    /// Get all properties
    pub fn properties(&self) -> &std::collections::HashMap<String, String> {
        &self.properties
    }
}

impl Form for VisualForm {
    fn id(&self) -> &str {
        &self.id
    }
    
    fn title(&self) -> &str {
        &self.title
    }
    
    fn set_title(&mut self, title: &str) {
        self.title = title.to_string();
    }
    
    fn bounds(&self) -> (i32, i32, u32, u32) {
        (self.x, self.y, self.width, self.height)
    }
    
    fn set_bounds(&mut self, x: i32, y: i32, width: u32, height: u32) {
        self.x = x;
        self.y = y;
        self.width = width;
        self.height = height;
    }
    
    fn add_component(&mut self, component: Box<dyn Component>) {
        self.components.push(component);
    }
    
    fn remove_component(&mut self, component_id: &str) -> bool {
        let index = self.components.iter().position(|c| c.id() == component_id);
        if let Some(pos) = index {
            self.components.remove(pos);
            true
        } else {
            false
        }
    }
    
    fn components(&self) -> &Vec<Box<dyn Component>> {
        &self.components
    }
    
    fn handle_event(&mut self, event: &Event) -> EventResult {
        // Handle form-level events and propagate to components
        for component in &mut self.components {
            let result = component.handle_event(event);
            if result == EventResult::Handled {
                return EventResult::Handled;
            }
        }
        EventResult::NotHandled
    }
}

/// Component types for the form designer
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComponentType {
    Button,
    Label,
    Edit,
    ListBox,
    Panel,
}

/// Form templates for quick creation
pub struct FormTemplates;

impl FormTemplates {
    /// Create a basic empty form
    pub fn basic_form(id: &str) -> Box<dyn Form> {
        let mut form = Box::new(VisualForm::new(id, 100, 100, 400, 300));
        form.set_title("Basic Form");
        form
    }
    
    /// Create a form with basic controls
    pub fn form_with_controls(id: &str) -> Box<dyn Form> {
        let mut form = VisualForm::new(id, 100, 100, 600, 400);
        form.set_title("Form with Controls");
        
        // Add some basic controls
        let mut button = crate::visual_components::Button::new("save_btn", 50, 50, 100, 30);
        button.set_text("Save");
        button.set_parent_form(id.to_string());
        
        let mut label = crate::visual_components::Label::new("title_label", 50, 100, 200, 20);
        label.set_text("Data Entry Form");
        label.set_alignment(crate::visual_components::LabelAlignment::Center);
        label.set_parent_form(id.to_string());
        
        let mut edit = crate::visual_components::Edit::new("name_edit", 50, 140, 200, 25);
        edit.set_text("Enter name here...");
        edit.set_parent_form(id.to_string());
        
        let mut listbox = crate::visual_components::ListBox::new("data_list", 50, 200, 200, 100);
        listbox.add_item("Item 1");
        listbox.add_item("Item 2");
        listbox.add_item("Item 3");
        listbox.set_parent_form(id.to_string());
        
        let mut panel = crate::visual_components::Panel::new("main_panel", 300, 50, 250, 300);
        panel.set_title("Data Panel");
        panel.set_parent_form(id.to_string());
        
        form.add_component(Box::new(button));
        form.add_component(Box::new(label));
        form.add_component(Box::new(edit));
        form.add_component(Box::new(listbox));
        form.add_component(Box::new(panel));
        
        Box::new(form)
    }
    
    /// Create a MDI (Multiple Document Interface) template
    pub fn mdi_template(id: &str) -> Box<dyn Form> {
        let mut form = VisualForm::new(id, 50, 50, 800, 600);
        form.set_title("MDI Application");
        
        // Add MDI client area and menu bar (simplified)
        let mut panel = crate::visual_components::Panel::new("mdi_client", 50, 100, 700, 450);
        panel.set_title("MDI Client Area");
        panel.set_parent_form(id.to_string());
        
        let mut status_bar = crate::visual_components::Panel::new("status_bar", 50, 550, 700, 30);
        status_bar.set_title("Status Bar");
        status_bar.set_border_width(0);
        status_bar.set_parent_form(id.to_string());
        
        form.add_component(Box::new(panel));
        form.add_component(Box::new(status_bar));
        
        Box::new(form)
    }
}

/// Form state management
#[derive(Debug, Clone)]
pub enum FormWindowState {
    Normal,
    Minimized,
    Maximized,
    Hidden,
}

/// Form manager for managing multiple forms
pub struct FormManager {
    forms: std::collections::HashMap<String, Box<dyn Form>>,
    active_form_id: Option<String>,
}

impl FormManager {
    /// Create a new form manager
    pub fn new() -> Self {
        Self {
            forms: std::collections::HashMap::new(),
            active_form_id: None,
        }
    }
    
    /// Add a form to the manager
    pub fn add_form(&mut self, form: Box<dyn Form>) {
        let form_id = form.id().to_string();
        self.forms.insert(form_id.clone(), form);
        self.set_active_form(&form_id);
    }
    
    /// Remove a form from the manager
    pub fn remove_form(&mut self, form_id: &str) -> bool {
        let removed = self.forms.remove(form_id).is_some();
        if self.active_form_id == Some(form_id.to_string()) {
            self.active_form_id = self.forms.keys().next().map(|s| s.clone());
        }
        removed
    }
    
    /// Get a form by ID
    pub fn get_form(&self, form_id: &str) -> Option<&dyn Form> {
        self.forms.get(form_id).map(|f| &**f as &dyn Form)
    }
    
    /// Set the active form
    pub fn set_active_form(&mut self, form_id: &str) {
        if self.forms.contains_key(form_id) {
            self.active_form_id = Some(form_id.to_string());
        }
    }
    
    /// Get the active form
    pub fn active_form(&self) -> Option<&dyn Form> {
        if let Some(ref form_id) = self.active_form_id {
            self.get_form(form_id)
        } else {
            None
        }
    }
    
    /// Get all form IDs
    pub fn form_ids(&self) -> Vec<String> {
        self.forms.keys().cloned().collect()
    }
    
    /// Count the number of forms
    pub fn form_count(&self) -> usize {
        self.forms.len()
    }
}