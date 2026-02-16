use glyph_core::diag::{self, Severity};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

use crate::position::byte_to_lsp_position;

pub fn glyph_diagnostic_to_lsp(
    diag: &dyn GlyphDiagnostic,
    source: &str,
    line_starts: &[u32],
) -> Diagnostic {
    let range = match diag.span() {
        Some(span) => {
            let start = byte_to_lsp_position(span.start, source, line_starts);
            let end = byte_to_lsp_position(span.end, source, line_starts);
            Range { start, end }
        }
        None => Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        },
    };

    let severity = match diag.severity() {
        Severity::Error => Some(DiagnosticSeverity::ERROR),
        Severity::Warning => Some(DiagnosticSeverity::WARNING),
        Severity::Note => Some(DiagnosticSeverity::INFORMATION),
        Severity::Help => Some(DiagnosticSeverity::HINT),
    };

    Diagnostic {
        range,
        severity,
        source: Some("glyph".to_string()),
        message: diag.message().to_string(),
        ..Default::default()
    }
}

pub trait GlyphDiagnostic {
    fn message(&self) -> &str;
    fn severity(&self) -> &Severity;
    fn span(&self) -> Option<glyph_core::span::Span>;
}

impl GlyphDiagnostic for diag::Diagnostic {
    fn message(&self) -> &str {
        &self.message
    }

    fn severity(&self) -> &Severity {
        &self.severity
    }

    fn span(&self) -> Option<glyph_core::span::Span> {
        self.span
    }
}
