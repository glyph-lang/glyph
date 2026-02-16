pub struct DocumentState {
    pub source: String,
    pub line_starts: Vec<u32>,
    pub version: i32,
}

impl DocumentState {
    pub fn new(source: String, version: i32) -> Self {
        let line_starts = build_line_starts(&source);
        Self {
            source,
            line_starts,
            version,
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
