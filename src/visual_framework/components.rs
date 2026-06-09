//! Components Module
//! 
//! Defines visual components for the Pascal visual framework

pub mod button;
pub mod label;
pub mod edit;
pub mod list_box;
pub mod combo_box;
pub mod panel;
pub mod menu;

// Re-export main component types
pub use button::*;
pub use label::*;
pub use edit::*;
pub use list_box::*;
pub use combo_box::*;
pub use panel::*;
pub use menu::*;

use crate::visual_framework::{Component, EventResult};
use crate::visual_framework::events::Event;

/// Base component implementation
pub struct BaseComponent {
    id: String,
    name: String,
    x: i32,
    y: i32,
    width: u32,
    height: u32,
    parent_form_id: Option<String>,
}

impl BaseComponent {
    pub fn new(id: &str, name: &str, x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            x,
            y,
            width,
            height,
            parent_form_id: None,
        }
    }
}

impl Component for BaseComponent {
    fn id(&self) -> &str {
        &self.id
    }
    
    fn name(&self) -> &str {
        &self.name
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
    
    fn parent_form(&self) -> Option<String> {
        self.parent_form_id.clone()
    }
    
    fn set_parent_form(&mut self, form_id: String) {
        self.parent_form_id = Some(form_id);
    }
    
    fn handle_event(&mut self, event: &Event) -> EventResult {
        EventResult::NotHandled
    }
    
    fn render(&self) -> String {
        format!("Component: {} at ({}, {}), size: {}x{}", self.name, self.x, self.y, self.width, self.height)
    }
}