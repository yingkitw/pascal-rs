//! Visual Events Module
//! 
//! Provides event system for visual applications

use crate::visual_framework::{EventResult};

/// Base event structure
pub struct BaseEvent {
    event_type: String,
    timestamp: std::time::Instant,
    component_id: String,
}

impl BaseEvent {
    /// Create a new base event
    pub fn new(event_type: String) -> Self {
        Self {
            event_type,
            timestamp: std::time::Instant::now(),
            component_id: String::new(),
        }
    }
    
    /// Create a new base event with component ID
    pub fn with_component(event_type: String, component_id: String) -> Self {
        Self {
            event_type,
            timestamp: std::time::Instant::now(),
            component_id,
        }
    }
    
    /// Get event type
    pub fn event_type(&self) -> &str {
        &self.event_type
    }
    
    /// Get timestamp
    pub fn timestamp(&self) -> std::time::Instant {
        self.timestamp
    }
    
    /// Get component ID
    pub fn component_id(&self) -> &str {
        &self.component_id
    }
    
    /// Set component ID
    pub fn set_component_id(&mut self, component_id: String) {
        self.component_id = component_id;
    }
}

/// Main event enum for visual applications
pub enum Event {
    /// Mouse events
    Mouse(MouseEvent),
    /// Keyboard events
    Keyboard(KeyboardEvent),
    /// Focus events
    Focus(FocusEvent),
    /// Form events
    Form(FormEvent),
    /// Custom events
    Custom(CustomEvent),
    /// Paint/Draw events
    Paint(PaintEvent),
    /// Timer events
    Timer(TimerEvent),
    /// Drag and drop events
    DragDrop(DragDropEvent),
}

/// Mouse event structure
pub struct MouseEvent {
    pub component_id: String,
    pub mouse_type: MouseType,
    pub x: i32,
    pub y: i32,
    pub button: MouseButton,
    pub click_count: u32,
    pub modifiers: KeyModifiers,
}

impl MouseEvent {
    /// Create a new mouse event
    pub fn new(component_id: String, mouse_type: MouseType, x: i32, y: i32, button: MouseButton) -> Self {
        Self {
            component_id,
            mouse_type,
            x,
            y,
            button,
            click_count: 1,
            modifiers: KeyModifiers::default(),
        }
    }
    
    /// Create a new mouse event with modifiers
    pub fn with_modifiers(component_id: String, mouse_type: MouseType, x: i32, y: i32, button: MouseButton, modifiers: KeyModifiers) -> Self {
        Self {
            component_id,
            mouse_type,
            x,
            y,
            button,
            click_count: 1,
            modifiers,
        }
    }
    
    /// Create a new mouse event with click count
    pub fn with_click_count(component_id: String, mouse_type: MouseType, x: i32, y: i32, button: MouseButton, click_count: u32) -> Self {
        Self {
            component_id,
            mouse_type,
            x,
            y,
            button,
            click_count,
            modifiers: KeyModifiers::default(),
        }
    }
}

/// Mouse event types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MouseType {
    Move,
    Down,
    Up,
    Click,
    DoubleClick,
    RightClick,
    Enter,
    Leave,
    Wheel,
    DragStart,
    Drag,
    DragEnd,
    Hover,
}

/// Mouse button types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MouseButton {
    Left,
    Right,
    Middle,
    Back,
    Forward,
    Other(u8),
}

/// Keyboard event structure
pub struct KeyboardEvent {
    pub component_id: String,
    pub key_type: KeyType,
    pub key: Key,
    pub modifiers: KeyModifiers,
    pub is_repeat: bool,
}

impl KeyboardEvent {
    /// Create a new keyboard event
    pub fn new(component_id: String, key_type: KeyType, key: Key) -> Self {
        Self {
            component_id,
            key_type,
            key,
            modifiers: KeyModifiers::default(),
            is_repeat: false,
        }
    }
    
    /// Create a new keyboard event with modifiers
    pub fn with_modifiers(component_id: String, key_type: KeyType, key: Key, modifiers: KeyModifiers) -> Self {
        Self {
            component_id,
            key_type,
            key,
            modifiers,
            is_repeat: false,
        }
    }
    
    /// Create a new keyboard event with repeat flag
    pub fn with_repeat(component_id: String, key_type: KeyType, key: Key, is_repeat: bool) -> Self {
        Self {
            component_id,
            key_type,
            key,
            modifiers: KeyModifiers::default(),
            is_repeat,
        }
    }
}

/// Keyboard event types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KeyType {
    Press,
    Release,
    Character,
    Backspace,
    Tab,
    Enter,
    Escape,
    Function(u8),
}

/// Key representation
#[derive(Debug, Clone, PartialEq)]
pub enum Key {
    Character(char),
    Special(SpecialKey),
    Unknown,
}

/// Special keys
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecialKey {
    Backspace,
    Tab,
    Enter,
    Escape,
    Space,
    ArrowUp,
    ArrowDown,
    ArrowLeft,
    ArrowRight,
    Home,
    End,
    PageUp,
    PageDown,
    Insert,
    Delete,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    Control,
    Alt,
    Shift,
    CapsLock,
    NumLock,
    ScrollLock,
}

/// Key modifiers
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct KeyModifiers {
    pub shift: bool,
    pub ctrl: bool,
    pub alt: bool,
    pub cmd: bool,
}

impl KeyModifiers {
    /// Create default key modifiers
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Create key modifiers with flags
    pub fn with_flags(shift: bool, ctrl: bool, alt: bool, cmd: bool) -> Self {
        Self { shift, ctrl, alt, cmd }
    }
    
    /// Check if shift is pressed
    pub fn shift(&self) -> bool {
        self.shift
    }
    
    /// Check if control is pressed
    pub fn ctrl(&self) -> bool {
        self.ctrl
    }
    
    /// Check if alt is pressed
    pub fn alt(&self) -> bool {
        self.alt
    }
    
    /// Check if command/meta key is pressed
    pub fn cmd(&self) -> bool {
        self.cmd
    }
}

/// Focus event structure
pub struct FocusEvent {
    pub component_id: String,
    pub focus_type: FocusType,
}

impl FocusEvent {
    /// Create a new focus event
    pub fn new(component_id: String, focus_type: FocusType) -> Self {
        Self {
            component_id,
            focus_type,
        }
    }
}

/// Focus event types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FocusType {
    Enter,
    Leave,
    Gain,
    Lost,
}

/// Form event structure
pub struct FormEvent {
    pub form_id: String,
    pub event_type: FormEventType,
    pub parameter: Option<String>,
}

impl FormEvent {
    /// Create a new form event
    pub fn new(form_id: String, event_type: FormEventType) -> Self {
        Self {
            form_id,
            event_type,
            parameter: None,
        }
    }
    
    /// Create a new form event with parameter
    pub fn with_parameter(form_id: String, event_type: FormEventType, parameter: String) -> Self {
        Self {
            form_id,
            event_type,
            parameter: Some(parameter),
        }
    }
}

/// Form event types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FormEventType {
    Load,
    Show,
    Hide,
    Close,
    Resize,
    Move,
    Activate,
    Deactivate,
    Maximize,
    Minimize,
    Restore,
}

/// Paint event structure
pub struct PaintEvent {
    pub component_id: String,
    pub paint_type: PaintType,
    pub bounds: (i32, i32, u32, u32),
    pub graphics: GraphicsContext,
}

impl PaintEvent {
    /// Create a new paint event
    pub fn new(component_id: String, paint_type: PaintType, bounds: (i32, i32, u32, u32)) -> Self {
        Self {
            component_id,
            paint_type,
            bounds,
            graphics: GraphicsContext::new(),
        }
    }
}

/// Paint event types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PaintType {
    Paint,
    Repaint,
    Update,
    Print,
}

/// Graphics context for painting
pub struct GraphicsContext {
    pub canvas: Vec<u8>, // Simplified representation
    pub width: u32,
    pub height: u32,
}

impl GraphicsContext {
    /// Create a new graphics context
    pub fn new() -> Self {
        Self {
            canvas: Vec::new(),
            width: 0,
            height: 0,
        }
    }
    
    /// Create a graphics context with specific size
    pub fn with_size(width: u32, height: u32) -> Self {
        Self {
            canvas: vec![0; (width * height * 4) as usize], // RGBA
            width,
            height,
        }
    }
}

/// Timer event structure
pub struct TimerEvent {
    pub timer_id: u32,
    pub interval: u32,
    pub repeats: bool,
}

impl TimerEvent {
    /// Create a new timer event
    pub fn new(timer_id: u32, interval: u32, repeats: bool) -> Self {
        Self {
            timer_id,
            interval,
            repeats,
        }
    }
}

/// Drag and drop event structure
pub struct DragDropEvent {
    pub component_id: String,
    pub event_type: DragDropType,
    pub data: DragData,
    pub position: (i32, i32),
}

impl DragDropEvent {
    /// Create a new drag and drop event
    pub fn new(component_id: String, event_type: DragDropType, data: DragData, position: (i32, i32)) -> Self {
        Self {
            component_id,
            event_type,
            data,
            position,
        }
    }
}

/// Drag and drop event types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DragDropType {
    Start,
    Drag,
    Enter,
    Leave,
    Drop,
    End,
}

/// Drag data structure
#[derive(Debug, Clone)]
pub struct DragData {
    pub data_type: String,
    pub data: String,
}

impl DragData {
    /// Create new drag data
    pub fn new(data_type: String, data: String) -> Self {
        Self { data_type, data }
    }
}

/// Custom event structure
pub struct CustomEvent {
    pub event_type: String,
    pub source: String,
    pub data: serde_json::Value,
}

impl CustomEvent {
    /// Create a new custom event
    pub fn new(event_type: String, source: String) -> Self {
        Self {
            event_type,
            source,
            data: serde_json::json!(null),
        }
    }
    
    /// Create a new custom event with data
    pub fn with_data(event_type: String, source: String, data: serde_json::Value) -> Self {
        Self {
            event_type,
            source,
            data,
        }
    }
    
    /// Set event data
    pub fn set_data(&mut self, data: serde_json::Value) {
        self.data = data;
    }
    
    /// Get event data
    pub fn data(&self) -> &serde_json::Value {
        &self.data
    }
}

/// Event dispatcher for managing event routing
pub struct EventDispatcher {
    handlers: std::collections::HashMap<String, Vec<Box<dyn EventHandler>>>,
}

impl EventDispatcher {
    /// Create a new event dispatcher
    pub fn new() -> Self {
        Self {
            handlers: std::collections::HashMap::new(),
        }
    }
    
    /// Add an event handler
    pub fn add_handler(&mut self, event_type: &str, handler: Box<dyn EventHandler>) {
        self.handlers.entry(event_type.to_string()).or_insert(Vec::new()).push(handler);
    }
    
    /// Remove event handlers
    pub fn remove_handlers(&mut self, event_type: &str) {
        self.handlers.remove(event_type);
    }
    
    /// Dispatch an event
    pub fn dispatch(&mut self, event: &Event) -> EventResult {
        let event_type = self.get_event_type(event);
        
        if let Some(handlers) = self.handlers.get_mut(&event_type) {
            for handler in handlers {
                let result = handler.handle(event);
                if result != EventResult::NotHandled {
                    return result;
                }
            }
        }
        EventResult::NotHandled
    }
    
    fn get_event_type(&self, event: &Event) -> String {
        match event {
            Event::Mouse(_) => "mouse".to_string(),
            Event::Keyboard(_) => "keyboard".to_string(),
            Event::Focus(_) => "focus".to_string(),
            Event::Form(_) => "form".to_string(),
            Event::Custom(custom_event) => custom_event.event_type.clone(),
            Event::Paint(_) => "paint".to_string(),
            Event::Timer(_) => "timer".to_string(),
            Event::DragDrop(_) => "dragdrop".to_string(),
        }
    }
}

/// Event handler trait
pub trait EventHandler {
    /// Handle an event
    fn handle(&mut self, event: &Event) -> EventResult;
    
    /// Get the handler priority
    fn priority(&self) -> u32;
    
    /// Check if this handler can handle the event type
    fn can_handle(&self, event: &Event) -> bool;
}

/// Priority levels for event handlers
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EventPriority {
    Lowest = 0,
    Low = 1,
    Normal = 2,
    High = 3,
    Highest = 4,
}

/// Event filter for filtering events
pub struct EventFilter {
    event_types: Vec<String>,
    component_ids: Vec<String>,
    conditions: Vec<Box<dyn Fn(&Event) -> bool>>,
}

impl EventFilter {
    /// Create a new event filter
    pub fn new() -> Self {
        Self {
            event_types: Vec::new(),
            component_ids: Vec::new(),
            conditions: Vec::new(),
        }
    }
    
    /// Add event types to filter
    pub fn add_event_types(&mut self, event_types: Vec<String>) {
        self.event_types.extend(event_types);
    }
    
    /// Add component IDs to filter
    pub fn add_component_ids(&mut self, component_ids: Vec<String>) {
        self.component_ids.extend(component_ids);
    }
    
    /// Add a condition filter
    pub fn add_condition(&mut self, condition: Box<dyn Fn(&Event) -> bool>) {
        self.conditions.push(condition);
    }
    
    /// Check if an event passes the filter
    pub fn matches(&self, event: &Event) -> bool {
        // Check event type
        let event_type = match event {
            Event::Mouse(_) => "mouse",
            Event::Keyboard(_) => "keyboard",
            Event::Focus(_) => "focus",
            Event::Form(_) => "form",
            Event::Custom(custom_event) => &custom_event.event_type,
            Event::Paint(_) => "paint",
            Event::Timer(_) => "timer",
            Event::DragDrop(_) => "dragdrop",
        };
        
        if !self.event_types.is_empty() && !self.event_types.contains(&event_type.to_string()) {
            return false;
        }
        
        // Check component ID
        if !self.component_ids.is_empty() {
            let component_id = match event {
                Event::Mouse(mouse_event) => &mouse_event.component_id,
                Event::Keyboard(keyboard_event) => &keyboard_event.component_id,
                Event::Focus(focus_event) => &focus_event.component_id,
                Event::Form(form_event) => &form_event.form_id,
                Event::Custom(custom_event) => &custom_event.source,
                Event::Paint(paint_event) => &paint_event.component_id,
                Event::Timer(_) => "",
                Event::DragDrop(drag_event) => &drag_event.component_id,
            };
            
            // Convert empty string to empty String for comparison
            let component_id_str = if component_id.is_empty() {
                String::new()
            } else {
                component_id.to_string()
            };
            
            if !self.component_ids.contains(&component_id_str) {
                return false;
            }
        }
        
        // Check conditions
        for condition in &self.conditions {
            if !condition(event) {
                return false;
            }
        }
        
        true
    }
}