use serde::{Deserialize, Serialize};

pub type BytePos = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub start: BytePos,
    pub end: BytePos,
}

impl Span {
    pub fn new(start: BytePos, end: BytePos) -> Self {
        assert!(start <= end, "span start must be <= end");
        Self { start, end }
    }

    pub fn len(&self) -> BytePos {
        self.end - self.start
    }

    pub fn join(self, other: Span) -> Span {
        Span::new(self.start.min(other.start), self.end.max(other.end))
    }
}
