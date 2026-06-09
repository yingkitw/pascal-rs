//! Visual Components Module
//! 
//! Provides basic UI components for visual applications

use crate::visual_framework::{Component, EventResult, Event};

/// Button component
pub struct Button {
    id: String,
    name: String,
    x: i32,
    y: i32,
    width: u32,
    height: u32,
    parent_form: Option<String>,
    text: String,
    is_enabled: bool,
    on_click: Option<Box<dyn Fn(&Event)>>,
}

impl Button {
    /// Create a new button
    pub fn new(id: &str, x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            id: id.to_string(),
            name: format!("Button_{}", id),
            x,
            y,
            width,
            height,
            parent_form: None,
            text: String::new(),
            is_enabled: true,
            on_click: None,
        }
    }
    
    /// Set the button text
    pub fn set_text(&mut self, text: &str) {
        self.text = text.to_string();
    }
    
    /// Set the enabled state
    pub fn set_enabled(&mut self, enabled: bool) {
        self.is_enabled = enabled;
    }
    
    /// Set the click handler
    pub fn set_on_click(&mut self, handler: Box<dyn Fn(&Event)>) {
        self.on_click = Some(handler);
    }
}

impl Component for Button {
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
        self.parent_form.clone()
    }
    
    fn set_parent_form(&mut self, form_id: String) {
        self.parent_form = Some(form_id);
    }
    
    fn handle_event(&mut self, event: &Event) -> EventResult {
        if let Event::Mouse(mouse_event) = event {
            if let crate::visual_framework::events::MouseEvent::Click { x, y, .. } = mouse_event {
                // Check if click is within component bounds
                if *x >= self.x && *x <= self.x + self.width as i32 && 
                   *y >= self.y && *y <= self.y + self.height as i32 {
                    if self.is_enabled {
                        if let Some(ref handler) = self.on_click {
                            handler(event);
                            return EventResult::Handled;
                        }
                    }
                }
            }
        }
        EventResult::NotHandled
    }
    
    fn render(&self) -> String {
        if self.is_enabled {
            format!("Button: {} at ({}, {}) [{}x{}]", self.text, self.x, self.y, self.width, self.height)
        } else {
            format!("Button (disabled): {} at ({}, {}) [{}x{}]", self.text, self.x, self.y, self.width, self.height)
        }
    }
}

/// Label component
pub struct Label {
    id: String,
    name: String,
    x: i32,
    y: i32,
    width: u32,
    height: u32,
    parent_form: Option<String>,
    text: String,
    alignment: LabelAlignment,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LabelAlignment {
    Left,
    Center,
    Right,
}

impl Label {
    /// Create a new label
    pub fn new(id: &str, x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            id: id.to_string(),
            name: format!("Label_{}", id),
            x,
            y,
            width,
            height,
            parent_form: None,
            text: String::new(),
            alignment: LabelAlignment::Left,
        }
    }
    
    /// Set the label text
    pub fn set_text(&mut self, text: &str) {
        self.text = text.to_string();
    }
    
    /// Set the label alignment
    pub fn set_alignment(&mut self, alignment: LabelAlignment) {
        self.alignment = alignment;
    }
}

impl Component for Label {
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
        self.parent_form.clone()
    }
    
    fn set_parent_form(&mut self, form_id: String) {
        self.parent_form = Some(form_id);
    }
    
    fn handle_event(&mut self, event: &Event) -> EventResult {
        // Labels typically don't handle events
        EventResult::NotHandled
    }
    
    fn render(&self) -> String {
        format!("Label: '{}' at ({}, {}) [{}x{}]", self.text, self.x, self.y, self.width, self.height)
    }
}

/// Edit control component
pub struct Edit {
    id: String,
    name: String,
    x: i32,
    y: i32,
    width: u32,
    height: u32,
    parent_form: Option<String>,
    text: String,
    is_readonly: bool,
    multiline: bool,
}

impl Edit {
    /// Create a new edit control
    pub fn new(id: &str, x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            id: id.to_string(),
            name: format!("Edit_{}", id),
            x,
            y,
            width,
            height,
            parent_form: None,
            text: String::new(),
            is_readonly: false,
            multiline: false,
        }
    }
    
    /// Set the edit text
    pub fn set_text(&mut self, text: &str) {
        self.text = text.to_string();
    }
    
    /// Get the edit text
    pub fn text(&self) -> &str {
        &self.text
    }
    
    /// Set the readonly state
    pub fn set_readonly(&mut self, readonly: bool) {
        self.is_readonly = readonly;
    }
    
    /// Set multiline mode
    pub fn set_multiline(&mut self, multiline: bool) {
        self.multiline = multiline;
    }
}

impl Component for Edit {
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
        self.parent_form.clone()
    }
    
    fn set_parent_form(&mut self, form_id: String) {
        self.parent_form = Some(form_id);
    }
    
    fn handle_event(&mut self, event: &Event) -> EventResult {
        // Handle text input events (simplified)
        if let Event::Keyboard(key_event) = event {
            if let crate::visual_framework::events::KeyboardEvent::Char(c) = key_event {
                if !self.is_readonly {
                    self.text.push(*c);
                    return EventResult::Handled;
                }
            }
            if let crate::visual_framework::events::KeyboardEvent::KeyDown { key, .. } = key_event {
                match key {
                    crate::visual_framework::events::KeyCode::Backspace => {
                        if !self.is_readonly && !self.text.is_empty() {
                            self.text.pop();
                            return EventResult::Handled;
                        }
                    }
                    _ => {}
                }
            }
        }
        EventResult::NotHandled
    }
    
    fn render(&self) -> String {
        format!("Edit: '{}' at ({}, {}) [{}x{}] {}{}", 
                self.text, self.x, self.y, self.width, self.height,
                if self.multiline { "(multiline)" } else { "" },
                if self.is_readonly { " (readonly)" } else { "" })
    }
}

/// ListBox component
pub struct ListBox {
    id: String,
    name: String,
    x: i32,
    y: i32,
    width: u32,
    height: u32,
    parent_form: Option<String>,
    items: Vec<String>,
    selected_index: Option<usize>,
}

impl ListBox {
    /// Create a new list box
    pub fn new(id: &str, x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            id: id.to_string(),
            name: format!("ListBox_{}", id),
            x,
            y,
            width,
            height,
            parent_form: None,
            items: Vec::new(),
            selected_index: None,
        }
    }
    
    /// Add an item to the list
    pub fn add_item(&mut self, item: &str) {
        self.items.push(item.to_string());
    }
    
    /// Clear all items
    pub fn clear(&mut self) {
        self.items.clear();
        self.selected_index = None;
    }
    
    /// Set the selected index
    pub fn set_selected_index(&mut self, index: Option<usize>) {
        self.selected_index = index;
    }
    
    /// Get the selected index
    pub fn selected_index(&self) -> Option<usize> {
        self.selected_index
    }
    
    /// Get the selected item
    pub fn selected_item(&self) -> Option<&str> {
        self.selected_index.and_then(|i| self.items.get(i).map(|s| s.as_str()))
    }
}

impl Component for ListBox {
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
        self.parent_form.clone()
    }
    
    fn set_parent_form(&mut self, form_id: String) {
        self.parent_form = Some(form_id);
    }
    
    fn handle_event(&mut self, event: &Event) -> EventResult {
        // Handle list selection events (simplified)
        if let Event::Mouse(mouse_event) = event {
            if let crate::visual_framework::events::MouseEvent::Click { x, y, .. } = mouse_event {
                // Check if click is within component bounds
                if *x >= self.x && *x <= self.x + self.width as i32 && 
                   *y >= self.y && *y <= self.y + self.height as i32 {
                    // Simple item selection based on y position
                    let item_height = self.height as i32 / self.items.len().max(1) as i32;
                    let clicked_index = ((*y - self.y) / item_height) as usize;
                    
                    if clicked_index < self.items.len() {
                        self.selected_index = Some(clicked_index);
                        return EventResult::Handled;
                    }
                }
            }
        }
        EventResult::NotHandled
    }
    
    fn render(&self) -> String {
        let items_display: Vec<String> = self.items.iter().enumerate().map(|(i, item)| {
            if Some(i) == self.selected_index {
                format!("> {}", item)
            } else {
                format!("  {}", item)
            }
        }).collect();
        
        format!("ListBox at ({}, {}) [{}x{}]:\n{}", 
                self.x, self.y, self.width, self.height,
                items_display.join("\n"))
    }
}

/// Panel component
pub struct Panel {
    id: String,
    name: String,
    x: i32,
    y: i32,
    width: u32,
    height: u32,
    parent_form: Option<String>,
    title: String,
    border_width: u32,
}

impl Panel {
    /// Create a new panel
    pub fn new(id: &str, x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            id: id.to_string(),
            name: format!("Panel_{}", id),
            x,
            y,
            width,
            height,
            parent_form: None,
            title: String::new(),
            border_width: 1,
        }
    }
    
    /// Set the panel title
    pub fn set_title(&mut self, title: &str) {
        self.title = title.to_string();
    }
    
    /// Set the border width
    pub fn set_border_width(&mut self, width: u32) {
        self.border_width = width;
    }
}

impl Component for Panel {
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
        self.parent_form.clone()
    }
    
    fn set_parent_form(&mut self, form_id: String) {
        self.parent_form = Some(form_id);
    }
    
    fn handle_event(&mut self, event: &Event) -> EventResult {
        // Panels typically don't handle events unless they have special functionality
        EventResult::NotHandled
    }
    
    fn render(&self) -> String {
        format!("Panel: '{}' at ({}, {}) [{}x{}] with border width {}", 
                self.title, self.x, self.y, self.width, self.height, self.border_width)
    }
}