//! Forms Module
//! 
//! Defines form types and implementations for the Pascal visual framework

pub mod base_form;
pub mod main_form;
pub mod modal_form;

// Re-export main form types
pub use base_form::*;
pub use main_form::*;
pub use modal_form::*;

use crate::visual_framework::{Component, Form, EventResult};
use crate::visual_framework::events::Event;
use std::collections::HashMap;

/// Base form implementation
pub struct BaseForm {
    id: String,
    title: String,
    x: i32,
    y: i32,
    width: u32,
    height: u32,
    components: Vec<Box<dyn Component>>,
    component_map: HashMap<String, Box<dyn Component>>,
}

impl BaseForm {
    pub fn new(id: &str, title: &str, x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            id: id.to_string(),
            title: title.to_string(),
            x,
            y,
            width,
            height,
            components: Vec::new(),
            component_map: HashMap::new(),
        }
    }
    
    pub fn get_component(&self, id: &str) -> Option<&dyn Component> {
        self.component_map.get(id).map(|c| &**c as &dyn Component)
    }
}

impl Form for BaseForm {
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
        let id = component.id().to_string();
        self.components.push(component);
        // Note: In a real implementation, we'd need to handle the component_map properly
        // For now, we'll just store a reference if possible
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
        // Handle form-level events
        EventResult::NotHandled
    }
}