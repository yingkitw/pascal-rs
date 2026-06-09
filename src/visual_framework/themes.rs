//! Themes Module
//! 
//! Defines theme types and styling for the Pascal visual framework

pub mod light_theme;
pub mod dark_theme;
pub mod component_styles;

// Re-export main theme types
pub use light_theme::*;
pub use dark_theme::*;
pub use component_styles::*;

use std::collections::HashMap;

/// Theme trait
pub trait Theme {
    fn name(&self) -> &str;
    fn background_color(&self) -> Color;
    fn foreground_color(&self) -> Color;
    fn border_color(&self) -> Color;
    fn component_styles(&self) -> &HashMap<String, ComponentStyle>;
    fn apply(&self) {
        // Theme application logic would go here
    }
}

/// Color structure
#[derive(Debug, Clone, Copy)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    pub fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }
    
    pub fn from_hex(hex: &str) -> Result<Self, String> {
        let hex = hex.trim_start_matches('#');
        if hex.len() != 6 {
            return Err("Invalid hex color length".to_string());
        }
        
        let r = u8::from_str_radix(&hex[0..2], 16)
            .map_err(|_| "Invalid red component".to_string())?;
        let g = u8::from_str_radix(&hex[2..4], 16)
            .map_err(|_| "Invalid green component".to_string())?;
        let b = u8::from_str_radix(&hex[4..6], 16)
            .map_err(|_| "Invalid blue component".to_string())?;
        
        Ok(Self::new(r, g, b, 255))
    }
    
    pub fn to_hex(&self) -> String {
        format!("#{:02x}{:02x}{:02x}", self.r, self.g, self.b)
    }
}

/// Base component style
#[derive(Debug, Clone)]
pub struct ComponentStyle {
    pub background: Option<Color>,
    pub foreground: Option<Color>,
    pub border: Option<Color>,
    pub border_width: f32,
    pub border_radius: f32,
    pub padding: (f32, f32, f32, f32),
    pub margin: (f32, f32, f32, f32),
    pub font_size: f32,
    pub font_weight: FontWeight,
    pub text_alignment: TextAlignment,
    pub opacity: f32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FontWeight {
    Normal,
    Bold,
    Light,
    Thin,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TextAlignment {
    Left,
    Center,
    Right,
    Justify,
}

impl Default for ComponentStyle {
    fn default() -> Self {
        Self {
            background: None,
            foreground: None,
            border: None,
            border_width: 0.0,
            border_radius: 0.0,
            padding: (0.0, 0.0, 0.0, 0.0),
            margin: (0.0, 0.0, 0.0, 0.0),
            font_size: 12.0,
            font_weight: FontWeight::Normal,
            text_alignment: TextAlignment::Left,
            opacity: 1.0,
        }
    }
}

/// Style helper for creating custom themes
pub struct ThemeBuilder {
    name: String,
    background: Color,
    foreground: Color,
    border: Color,
    component_styles: HashMap<String, ComponentStyle>,
}

impl ThemeBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            background: Color::new(255, 255, 255, 255),
            foreground: Color::new(0, 0, 0, 255),
            border: Color::new(0, 0, 0, 255),
            component_styles: HashMap::new(),
        }
    }
    
    pub fn background(mut self, color: Color) -> Self {
        self.background = color;
        self
    }
    
    pub fn foreground(mut self, color: Color) -> Self {
        self.foreground = color;
        self
    }
    
    pub fn border(mut self, color: Color) -> Self {
        self.border = color;
        self
    }
    
    pub fn add_component_style(mut self, component_type: &str, style: ComponentStyle) -> Self {
        self.component_styles.insert(component_type.to_string(), style);
        self
    }
    
    pub fn build(self) -> Box<dyn Theme> {
        Box::new(CustomTheme {
            name: self.name,
            background: self.background,
            foreground: self.foreground,
            border: self.border,
            component_styles: self.component_styles,
        })
    }
}

/// Custom theme implementation
pub struct CustomTheme {
    name: String,
    background: Color,
    foreground: Color,
    border: Color,
    component_styles: HashMap<String, ComponentStyle>,
}

impl Theme for CustomTheme {
    fn name(&self) -> &str {
        &self.name
    }
    
    fn background_color(&self) -> Color {
        self.background
    }
    
    fn foreground_color(&self) -> Color {
        self.foreground
    }
    
    fn border_color(&self) -> Color {
        self.border
    }
    
    fn component_styles(&self) -> &HashMap<String, ComponentStyle> {
        &self.component_styles
    }
}