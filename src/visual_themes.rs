//! Visual Themes Module
//! 
//! Defines theme types and styling for Pascal visual applications

use std::collections::HashMap;
use serde::{Deserialize, Serialize};

/// Color structure
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FontWeight {
    Normal,
    Bold,
    Light,
    Thin,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

/// Component styles container
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComponentStyles {
    pub button: ComponentStyle,
    pub label: ComponentStyle,
    pub edit: ComponentStyle,
    pub listbox: ComponentStyle,
    pub combobox: ComponentStyle,
    pub form: ComponentStyle,
    pub scrollbar: ComponentStyle,
    pub progress_bar: ComponentStyle,
}

/// Color scheme
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ColorScheme {
    pub primary: Color,
    pub secondary: Color,
    pub background: Color,
    pub surface: Color,
    pub text: Color,
    pub text_secondary: Color,
    pub accent: Color,
    pub success: Color,
    pub warning: Color,
    pub error: Color,
    pub border: Color,
    pub shadow: Color,
    pub highlight: Color,
    pub disabled: Color,
    pub hover: Color,
    pub active: Color,
    pub selected: Color,
}

/// Font scheme
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FontScheme {
    pub default: FontDefinition,
    pub heading: FontDefinition,
    pub body: FontDefinition,
    pub caption: FontDefinition,
    pub code: FontDefinition,
    pub monospace: FontDefinition,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FontDefinition {
    pub family: String,
    pub size: f32,
    pub weight: FontWeight,
    pub style: FontStyle,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FontStyle {
    Normal,
    Italic,
    Oblique,
}

impl Default for FontDefinition {
    fn default() -> Self {
        Self {
            family: "Segoe UI".to_string(),
            size: 12.0,
            weight: FontWeight::Normal,
            style: FontStyle::Normal,
        }
    }
}

/// Animation settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnimationSettings {
    pub enabled: bool,
    pub duration: f32,
    pub easing: EasingFunction,
    pub hover_duration: f32,
    pub click_duration: f32,
    pub transition_duration: f32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EasingFunction {
    Linear,
    EaseIn,
    EaseOut,
    EaseInOut,
    EaseInCubic,
    EaseOutCubic,
    EaseInOutCubic,
    EaseInQuart,
    EaseOutQuart,
    EaseInOutQuart,
    EaseInQuint,
    EaseOutQuint,
    EaseInOutQuint,
    EaseInSine,
    EaseOutSine,
    EaseInOutSine,
    EaseInExpo,
    EaseOutExpo,
    EaseInOutExpo,
    EaseInCirc,
    EaseOutCirc,
    EaseInOutCirc,
    Spring,
}

/// Theme trait
pub trait Theme {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn author(&self) -> &str;
    fn version(&self) -> &str;
    fn apply(&self) -> Result<(), String>;
    fn colors(&self) -> &ColorScheme;
    fn fonts(&self) -> &FontScheme;
    fn component_styles(&self) -> &ComponentStyles;
    fn animations(&self) -> &AnimationSettings;
}

/// Light theme implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LightTheme {
    pub name: String,
    pub description: String,
    pub author: String,
    pub version: String,
    pub colors: ColorScheme,
    pub fonts: FontScheme,
    pub component_styles: ComponentStyles,
    pub animations: AnimationSettings,
}

impl LightTheme {
    pub fn new() -> Self {
        Self {
            name: "Light".to_string(),
            description: "Clean and modern light theme".to_string(),
            author: "pascal-rs".to_string(),
            version: "1.0.0".to_string(),
            colors: ColorScheme {
                primary: Color::from_hex("#2196F3").unwrap(),
                secondary: Color::from_hex("#FFC107").unwrap(),
                background: Color::from_hex("#FFFFFF").unwrap(),
                surface: Color::from_hex("#F5F5F5").unwrap(),
                text: Color::from_hex("#212121").unwrap(),
                text_secondary: Color::from_hex("#757575").unwrap(),
                accent: Color::from_hex("#00BCD4").unwrap(),
                success: Color::from_hex("#4CAF50").unwrap(),
                warning: Color::from_hex("#FF9800").unwrap(),
                error: Color::from_hex("#F44336").unwrap(),
                border: Color::from_hex("#E0E0E0").unwrap(),
                shadow: Color::from_hex("#000000").unwrap(),
                highlight: Color::from_hex("#BBDEFB").unwrap(),
                disabled: Color::from_hex("#BDBDBD").unwrap(),
                hover: Color::from_hex("#E3F2FD").unwrap(),
                active: Color::from_hex("#2196F3").unwrap(),
                selected: Color::from_hex("#1976D2").unwrap(),
            },
            fonts: FontScheme {
                default: FontDefinition {
                    family: "Segoe UI".to_string(),
                    size: 12.0,
                    ..Default::default()
                },
                heading: FontDefinition {
                    family: "Segoe UI".to_string(),
                    size: 18.0,
                    weight: FontWeight::Bold,
                    ..Default::default()
                },
                body: FontDefinition {
                    family: "Segoe UI".to_string(),
                    size: 14.0,
                    ..Default::default()
                },
                caption: FontDefinition {
                    family: "Segoe UI".to_string(),
                    size: 10.0,
                    weight: FontWeight::Light,
                    ..Default::default()
                },
                code: FontDefinition {
                    family: "Consolas".to_string(),
                    size: 12.0,
                    ..Default::default()
                },
                monospace: FontDefinition {
                    family: "Consolas".to_string(),
                    size: 13.0,
                    ..Default::default()
                },
            },
            component_styles: ComponentStyles {
                button: ComponentStyle {
                    background: Some(Color::from_hex("#2196F3").unwrap()),
                    foreground: Some(Color::new(255, 255, 255, 255)),
                    border: Some(Color::from_hex("#1976D2").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (8.0, 16.0, 8.0, 16.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Bold,
                    text_alignment: TextAlignment::Center,
                    opacity: 1.0,
                },
                label: ComponentStyle {
                    background: None,
                    foreground: Some(Color::from_hex("#212121").unwrap()),
                    border: None,
                    border_width: 0.0,
                    border_radius: 0.0,
                    padding: (0.0, 0.0, 0.0, 0.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                edit: ComponentStyle {
                    background: Some(Color::new(255, 255, 255, 255)),
                    foreground: Some(Color::from_hex("#212121").unwrap()),
                    border: Some(Color::from_hex("#BDBDBD").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (4.0, 8.0, 4.0, 8.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                listbox: ComponentStyle {
                    background: Some(Color::new(255, 255, 255, 255)),
                    foreground: Some(Color::from_hex("#212121").unwrap()),
                    border: Some(Color::from_hex("#BDBDBD").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (4.0, 8.0, 4.0, 8.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                combobox: ComponentStyle {
                    background: Some(Color::new(255, 255, 255, 255)),
                    foreground: Some(Color::from_hex("#212121").unwrap()),
                    border: Some(Color::from_hex("#BDBDBD").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (4.0, 8.0, 4.0, 8.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                form: ComponentStyle {
                    background: Some(Color::from_hex("#FFFFFF").unwrap()),
                    foreground: Some(Color::from_hex("#212121").unwrap()),
                    border: Some(Color::from_hex("#BDBDBD").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (8.0, 8.0, 8.0, 8.0),
                    margin: (0.0, 0.0, 0.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                scrollbar: ComponentStyle {
                    background: Some(Color::from_hex("#F5F5F5").unwrap()),
                    foreground: None,
                    border: Some(Color::from_hex("#E0E0E0").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (0.0, 0.0, 0.0, 0.0),
                    margin: (0.0, 0.0, 0.0, 0.0),
                    font_size: 12.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                progress_bar: ComponentStyle {
                    background: Some(Color::from_hex("#E0E0E0").unwrap()),
                    foreground: Some(Color::from_hex("#2196F3").unwrap()),
                    border: Some(Color::from_hex("#BDBDBD").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (0.0, 0.0, 0.0, 0.0),
                    margin: (0.0, 0.0, 0.0, 0.0),
                    font_size: 12.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
            },
            animations: AnimationSettings {
                enabled: true,
                duration: 0.2,
                easing: EasingFunction::EaseInOut,
                hover_duration: 0.1,
                click_duration: 0.1,
                transition_duration: 0.15,
            },
        }
    }
}

impl Theme for LightTheme {
    fn name(&self) -> &str {
        &self.name
    }
    
    fn description(&self) -> &str {
        &self.description
    }
    
    fn author(&self) -> &str {
        &self.author
    }
    
    fn version(&self) -> &str {
        &self.version
    }
    
    fn apply(&self) -> Result<(), String> {
        println!("Applying Light Theme");
        Ok(())
    }
    
    fn colors(&self) -> &ColorScheme {
        &self.colors
    }
    
    fn fonts(&self) -> &FontScheme {
        &self.fonts
    }
    
    fn component_styles(&self) -> &ComponentStyles {
        &self.component_styles
    }
    
    fn animations(&self) -> &AnimationSettings {
        &self.animations
    }
}

/// Dark theme implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DarkTheme {
    pub name: String,
    pub description: String,
    pub author: String,
    pub version: String,
    pub colors: ColorScheme,
    pub fonts: FontScheme,
    pub component_styles: ComponentStyles,
    pub animations: AnimationSettings,
}

impl DarkTheme {
    pub fn new() -> Self {
        Self {
            name: "Dark".to_string(),
            description: "Modern dark theme for reduced eye strain".to_string(),
            author: "pascal-rs".to_string(),
            version: "1.0.0".to_string(),
            colors: ColorScheme {
                primary: Color::from_hex("#BB86FC").unwrap(),
                secondary: Color::from_hex("#03DAC6").unwrap(),
                background: Color::from_hex("#121212").unwrap(),
                surface: Color::from_hex("#1E1E1E").unwrap(),
                text: Color::from_hex("#FFFFFF").unwrap(),
                text_secondary: Color::from_hex("#B0B0B0").unwrap(),
                accent: Color::from_hex("#03DAC6").unwrap(),
                success: Color::from_hex("#4CAF50").unwrap(),
                warning: Color::from_hex("#FF9800").unwrap(),
                error: Color::from_hex("#F44336").unwrap(),
                border: Color::from_hex("#333333").unwrap(),
                shadow: Color::from_hex("#000000").unwrap(),
                highlight: Color::from_hex("#333333").unwrap(),
                disabled: Color::from_hex("#666666").unwrap(),
                hover: Color::from_hex("#2A2A2A").unwrap(),
                active: Color::from_hex("#BB86FC").unwrap(),
                selected: Color::from_hex("#6200EA").unwrap(),
            },
            fonts: FontScheme {
                default: FontDefinition {
                    family: "Segoe UI".to_string(),
                    size: 12.0,
                    ..Default::default()
                },
                heading: FontDefinition {
                    family: "Segoe UI".to_string(),
                    size: 18.0,
                    weight: FontWeight::Bold,
                    ..Default::default()
                },
                body: FontDefinition {
                    family: "Segoe UI".to_string(),
                    size: 14.0,
                    ..Default::default()
                },
                caption: FontDefinition {
                    family: "Segoe UI".to_string(),
                    size: 10.0,
                    weight: FontWeight::Light,
                    ..Default::default()
                },
                code: FontDefinition {
                    family: "Consolas".to_string(),
                    size: 12.0,
                    ..Default::default()
                },
                monospace: FontDefinition {
                    family: "Consolas".to_string(),
                    size: 13.0,
                    ..Default::default()
                },
            },
            component_styles: ComponentStyles {
                button: ComponentStyle {
                    background: Some(Color::from_hex("#6200EA").unwrap()),
                    foreground: Some(Color::new(255, 255, 255, 255)),
                    border: Some(Color::from_hex("#BB86FC").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (8.0, 16.0, 8.0, 16.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Bold,
                    text_alignment: TextAlignment::Center,
                    opacity: 1.0,
                },
                label: ComponentStyle {
                    background: None,
                    foreground: Some(Color::from_hex("#FFFFFF").unwrap()),
                    border: None,
                    border_width: 0.0,
                    border_radius: 0.0,
                    padding: (0.0, 0.0, 0.0, 0.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                edit: ComponentStyle {
                    background: Some(Color::from_hex("#1E1E1E").unwrap()),
                    foreground: Some(Color::from_hex("#FFFFFF").unwrap()),
                    border: Some(Color::from_hex("#333333").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (4.0, 8.0, 4.0, 8.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                listbox: ComponentStyle {
                    background: Some(Color::from_hex("#1E1E1E").unwrap()),
                    foreground: Some(Color::from_hex("#FFFFFF").unwrap()),
                    border: Some(Color::from_hex("#333333").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (4.0, 8.0, 4.0, 8.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                combobox: ComponentStyle {
                    background: Some(Color::from_hex("#1E1E1E").unwrap()),
                    foreground: Some(Color::from_hex("#FFFFFF").unwrap()),
                    border: Some(Color::from_hex("#333333").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (4.0, 8.0, 4.0, 8.0),
                    margin: (0.0, 0.0, 4.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                form: ComponentStyle {
                    background: Some(Color::from_hex("#121212").unwrap()),
                    foreground: Some(Color::from_hex("#FFFFFF").unwrap()),
                    border: Some(Color::from_hex("#333333").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (8.0, 8.0, 8.0, 8.0),
                    margin: (0.0, 0.0, 0.0, 0.0),
                    font_size: 14.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                scrollbar: ComponentStyle {
                    background: Some(Color::from_hex("#1E1E1E").unwrap()),
                    foreground: None,
                    border: Some(Color::from_hex("#666666").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (0.0, 0.0, 0.0, 0.0),
                    margin: (0.0, 0.0, 0.0, 0.0),
                    font_size: 12.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
                progress_bar: ComponentStyle {
                    background: Some(Color::from_hex("#333333").unwrap()),
                    foreground: Some(Color::from_hex("#BB86FC").unwrap()),
                    border: Some(Color::from_hex("#666666").unwrap()),
                    border_width: 1.0,
                    border_radius: 4.0,
                    padding: (0.0, 0.0, 0.0, 0.0),
                    margin: (0.0, 0.0, 0.0, 0.0),
                    font_size: 12.0,
                    font_weight: FontWeight::Normal,
                    text_alignment: TextAlignment::Left,
                    opacity: 1.0,
                },
            },
            animations: AnimationSettings {
                enabled: true,
                duration: 0.3,
                easing: EasingFunction::EaseInOut,
                hover_duration: 0.15,
                click_duration: 0.1,
                transition_duration: 0.2,
            },
        }
    }
}

impl Theme for DarkTheme {
    fn name(&self) -> &str {
        &self.name
    }
    
    fn description(&self) -> &str {
        &self.description
    }
    
    fn author(&self) -> &str {
        &self.author
    }
    
    fn version(&self) -> &str {
        &self.version
    }
    
    fn apply(&self) -> Result<(), String> {
        println!("Applying Dark Theme");
        Ok(())
    }
    
    fn colors(&self) -> &ColorScheme {
        &self.colors
    }
    
    fn fonts(&self) -> &FontScheme {
        &self.fonts
    }
    
    fn component_styles(&self) -> &ComponentStyles {
        &self.component_styles
    }
    
    fn animations(&self) -> &AnimationSettings {
        &self.animations
    }
}