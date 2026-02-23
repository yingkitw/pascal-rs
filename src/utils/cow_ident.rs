//! Memory-efficient identifier representation using Cow
//!
//! Allows identifiers to be either borrowed from source (&str) or owned (String),
//! reducing allocations when parsing from a string that outlives the AST.

use std::borrow::Cow;

/// Identifier that can be borrowed or owned (memory-efficient)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CowIdent<'a>(Cow<'a, str>);

impl<'a> CowIdent<'a> {
    pub fn borrowed(s: &'a str) -> Self {
        Self(Cow::Borrowed(s))
    }

    pub fn owned(s: String) -> Self {
        Self(Cow::Owned(s))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    pub fn into_owned(self) -> String {
        self.0.into_owned()
    }
}

impl<'a> From<&'a str> for CowIdent<'a> {
    fn from(s: &'a str) -> Self {
        Self::borrowed(s)
    }
}

impl From<String> for CowIdent<'static> {
    fn from(s: String) -> Self {
        Self(Cow::Owned(s))
    }
}

impl<'a> std::fmt::Display for CowIdent<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
