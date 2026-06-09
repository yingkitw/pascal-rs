//! Events Module
//! 
//! Defines event types and handling for the Pascal visual framework

pub mod mouse_events;
pub mod keyboard_events;
pub mod system_events;

use std::collections::HashMap;

// Re-export main event types
pub use mouse_events::*;
pub use keyboard_events::*;
pub use system_events::*;

/// Base event type
#[derive(Debug, Clone)]
pub enum Event {
    Mouse(MouseEvent),
    Keyboard(KeyboardEvent),
    System(SystemEvent),
    Custom(String),
}

/// Mouse event types
#[derive(Debug, Clone)]
pub enum MouseEvent {
    Click { x: i32, y: i32, button: MouseButton },
    DoubleClick { x: i32, y: i32, button: MouseButton },
    Move { x: i32, y: i32 },
    Drag { x: i32, y: i32, button: MouseButton },
    Wheel { delta: f32 },
    Enter,
    Leave,
}

/// Mouse button types
#[derive(Debug, Clone)]
pub enum MouseButton {
    Left,
    Right,
    Middle,
    Other(u8),
}

/// Keyboard event types
#[derive(Debug, Clone)]
pub enum KeyboardEvent {
    KeyDown { key: KeyCode, modifiers: KeyModifiers },
    KeyUp { key: KeyCode, modifiers: KeyModifiers },
    Char(char),
}

/// System event types
#[derive(Debug, Clone)]
pub enum SystemEvent {
    Resize { width: u32, height: u32 },
    Close,
    Focus,
    Unfocus,
    Timer,
}

/// Key codes
#[derive(Debug, Clone, PartialEq)]
pub enum KeyCode {
    Unknown,
    Backspace,
    Tab,
    Enter,
    Shift,
    Control,
    Alt,
    Pause,
    CapsLock,
    Escape,
    Space,
    PageUp,
    PageDown,
    End,
    Home,
    Left,
    Up,
    Right,
    Down,
    Select,
    Print,
    Execute,
    Snapshot,
    Insert,
    Delete,
    Help,
    Num0,
    Num1,
    Num2,
    Num3,
    Num4,
    Num5,
    Num6,
    Num7,
    Num8,
    Num9,
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
}

/// Key modifiers
#[derive(Debug, Clone, PartialEq)]
pub struct KeyModifiers {
    pub shift: bool,
    pub control: bool,
    pub alt: bool,
    pub meta: bool,
}

impl Default for KeyModifiers {
    fn default() -> Self {
        Self {
            shift: false,
            control: false,
            alt: false,
            meta: false,
        }
    }
}

/// Event dispatcher
pub struct EventDispatcher {
    listeners: HashMap<String, Vec<Box<dyn Fn(Event) -> bool>>>,
}

impl EventDispatcher {
    pub fn new() -> Self {
        Self {
            listeners: HashMap::new(),
        }
    }
    
    pub fn add_listener(&mut self, event_type: String, listener: Box<dyn Fn(Event) -> bool>) {
        self.listeners.entry(event_type).or_insert_with(Vec::new).push(listener);
    }
    
    pub fn dispatch(&mut self, event: Event) {
        let event_type = match &event {
            Event::Mouse(_) => "mouse".to_string(),
            Event::Keyboard(_) => "keyboard".to_string(),
            Event::System(_) => "system".to_string(),
            Event::Custom(s) => s.clone(),
        };
        
        if let Some(listeners) = self.listeners.get_mut(&event_type) {
            for listener in listeners.iter_mut() {
                listener(event.clone());
            }
        }
    }
}