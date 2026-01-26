use std::collections::HashMap;
use std::path::PathBuf;

use glyph_core::diag::{Diagnostic, Severity};

pub struct SourceInfo {
    pub path: PathBuf,
    pub source: String,
    pub line_starts: Vec<u32>,
}

impl SourceInfo {
    pub fn new(path: PathBuf, source: String) -> Self {
        let line_starts = build_line_starts(&source);
        Self {
            path,
            source,
            line_starts,
        }
    }
}

pub fn build_line_starts(source: &str) -> Vec<u32> {
    let mut starts = Vec::new();
    starts.push(0);
    for (idx, b) in source.as_bytes().iter().enumerate() {
        if *b == b'\n' {
            let next = idx + 1;
            if let Ok(pos) = u32::try_from(next) {
                starts.push(pos);
            } else {
                break;
            }
        }
    }
    starts
}

pub fn line_from_span(span_start: u32, line_starts: &[u32]) -> usize {
    if line_starts.is_empty() {
        return 1;
    }
    match line_starts.binary_search(&span_start) {
        Ok(idx) => idx + 1,
        Err(idx) => idx,
    }
}

pub fn format_diagnostic(diag: &Diagnostic, sources: &HashMap<String, SourceInfo>) -> String {
    let (label, line) = match diag.module_id.as_ref() {
        Some(module_id) => match sources.get(module_id) {
            Some(source_info) => {
                let line = diag
                    .span
                    .map(|span| line_from_span(span.start, &source_info.line_starts));
                (source_info.path.display().to_string(), line)
            }
            None => (module_id.clone(), None),
        },
        None => ("<unknown>".to_string(), None),
    };

    let severity = severity_label(&diag.severity);

    match line {
        Some(line) => format!("{}:{}: {}: {}", label, line, severity, diag.message),
        None => format!("{}: {}: {}", label, severity, diag.message),
    }
}

fn severity_label(severity: &Severity) -> &'static str {
    match severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Note => "note",
        Severity::Help => "help",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_starts_handle_empty() {
        let starts = build_line_starts("");
        assert_eq!(starts, vec![0]);
        assert_eq!(line_from_span(0, &starts), 1);
    }

    #[test]
    fn line_starts_handle_single_line() {
        let starts = build_line_starts("hello");
        assert_eq!(starts, vec![0]);
        assert_eq!(line_from_span(0, &starts), 1);
        assert_eq!(line_from_span(4, &starts), 1);
    }

    #[test]
    fn line_starts_handle_multi_line() {
        let source = "a\nbee\nsee\n";
        let starts = build_line_starts(source);
        assert_eq!(starts, vec![0, 2, 6, 10]);
        assert_eq!(line_from_span(0, &starts), 1);
        assert_eq!(line_from_span(2, &starts), 2);
        assert_eq!(line_from_span(5, &starts), 2);
        assert_eq!(line_from_span(6, &starts), 3);
        assert_eq!(line_from_span(9, &starts), 3);
        assert_eq!(line_from_span(10, &starts), 4);
    }
}
