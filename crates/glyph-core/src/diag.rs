use serde::{Deserialize, Serialize};

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Severity {
    Error,
    Warning,
    Note,
    Help,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Diagnostic {
    pub message: String,
    pub severity: Severity,
    pub span: Option<Span>,
    pub module_id: Option<String>,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>, severity: Severity, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            severity,
            span,
            module_id: None,
        }
    }

    pub fn error(message: impl Into<String>, span: Option<Span>) -> Self {
        Self::new(message, Severity::Error, span)
    }

    pub fn warning(message: impl Into<String>, span: Option<Span>) -> Self {
        Self::new(message, Severity::Warning, span)
    }

    pub fn note(message: impl Into<String>, span: Option<Span>) -> Self {
        Self::new(message, Severity::Note, span)
    }

    pub fn help(message: impl Into<String>, span: Option<Span>) -> Self {
        Self::new(message, Severity::Help, span)
    }

    pub fn with_module_id(mut self, module_id: impl Into<String>) -> Self {
        self.module_id = Some(module_id.into());
        self
    }
}
