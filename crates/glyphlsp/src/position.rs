use tower_lsp::lsp_types::Position;

/// Convert a 1-based line number from `line_from_span` into the actual line index.
/// `line_from_span` returns 1-based lines; LSP uses 0-based.
fn line_from_span(span_start: u32, line_starts: &[u32]) -> usize {
    if line_starts.is_empty() {
        return 1;
    }
    match line_starts.binary_search(&span_start) {
        Ok(idx) => idx + 1,
        Err(idx) => idx,
    }
}

pub fn byte_to_lsp_position(byte_offset: u32, source: &str, line_starts: &[u32]) -> Position {
    let line = line_from_span(byte_offset, line_starts);
    // line is 1-based from line_from_span; convert to 0-based index into line_starts
    let line_idx = line.saturating_sub(1);
    let line_start = line_starts.get(line_idx).copied().unwrap_or(0);
    let end = (byte_offset as usize).min(source.len());
    let start = (line_start as usize).min(end);
    let prefix = &source[start..end];
    let character = prefix.encode_utf16().count() as u32;
    Position {
        line: line_idx as u32,
        character,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::build_line_starts;

    #[test]
    fn single_line() {
        let src = "hello world";
        let ls = build_line_starts(src);
        let pos = byte_to_lsp_position(6, src, &ls);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 6);
    }

    #[test]
    fn multi_line() {
        let src = "ab\ncde\nfg";
        let ls = build_line_starts(src);
        // 'c' is at byte 3, line 2 (0-based line 1), col 0
        let pos = byte_to_lsp_position(3, src, &ls);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);
        // 'e' is at byte 5, line 2 (0-based line 1), col 2
        let pos = byte_to_lsp_position(5, src, &ls);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 2);
        // 'f' is at byte 7, line 3 (0-based line 2), col 0
        let pos = byte_to_lsp_position(7, src, &ls);
        assert_eq!(pos.line, 2);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn start_of_file() {
        let src = "fn main() {}";
        let ls = build_line_starts(src);
        let pos = byte_to_lsp_position(0, src, &ls);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);
    }
}
