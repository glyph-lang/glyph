use glyph_core::{
    diag::Diagnostic,
    span::Span,
    token::{Token, TokenKind},
};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct LexOutput {
    pub tokens: Vec<Token>,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn lex(source: &str) -> LexOutput {
    let mut toks = Vec::new();
    let mut diags = Vec::new();
    let bytes = source.as_bytes();
    let mut i = 0usize;

    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }

        let start = i as u32;
        match b {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let end = consume_ident(bytes, i);
                let text = &source[i..end];
                let kind = keyword_or_ident(text);
                toks.push(Token::new(kind, Span::new(start, end as u32)));
                i = end;
            }
            b'0'..=b'9' => {
                let (end, kind) = consume_number(bytes, i);
                toks.push(Token::new(kind, Span::new(start, end as u32)));
                i = end;
            }
            b'"' => match consume_string(bytes, i) {
                Ok(end) => {
                    toks.push(Token::new(TokenKind::Str, Span::new(start, end as u32)));
                    i = end;
                }
                Err(pos) => {
                    diags.push(Diagnostic::error(
                        "unterminated string literal",
                        Some(Span::new(start, pos as u32)),
                    ));
                    break;
                }
            },
            _ => {
                if let Some((kind, end)) = consume_operator(bytes, i) {
                    toks.push(Token::new(kind, Span::new(start, end as u32)));
                    i = end;
                } else {
                    diags.push(Diagnostic::error(
                        "unknown token",
                        Some(Span::new(start, (i + 1) as u32)),
                    ));
                    i += 1;
                }
            }
        }
    }

    toks.push(Token::new(
        TokenKind::Eof,
        Span::new(bytes.len() as u32, bytes.len() as u32),
    ));

    LexOutput {
        tokens: toks,
        diagnostics: diags,
    }
}

fn consume_ident(bytes: &[u8], start: usize) -> usize {
    let mut i = start + 1;
    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_alphanumeric() || b == b'_' {
            i += 1;
        } else {
            break;
        }
    }
    i
}

fn keyword_or_ident(text: &str) -> TokenKind {
    use TokenKind::*;
    match text {
        "fn" => Fn,
        "let" => Let,
        "mut" => Mut,
        "type" => Type,
        "struct" => Struct,
        "interface" => Interface,
        "enum" => Enum,
        "impl" => Impl,
        "use" => Use,
        "pub" => Pub,
        "if" => If,
        "else" => Else,
        "for" => For,
        "in" => In,
        "while" => While,
        "match" => Match,
        "break" => Break,
        "cont" => Cont,
        "ret" => Ret,
        "true" | "false" => Bool,
        _ => Ident,
    }
}

fn consume_number(bytes: &[u8], start: usize) -> (usize, TokenKind) {
    let mut i = start;
    while i < bytes.len() && bytes[i].is_ascii_digit() {
        i += 1;
    }
    let mut kind = TokenKind::Int;
    if i < bytes.len() && bytes[i] == b'.' && i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit() {
        kind = TokenKind::Float;
        i += 1; // consume '.'
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            i += 1;
        }
    }
    (i, kind)
}

fn consume_string(bytes: &[u8], start: usize) -> Result<usize, usize> {
    let mut i = start + 1; // skip opening quote
    while i < bytes.len() {
        match bytes[i] {
            b'\\' => {
                i += 2; // skip escape
            }
            b'"' => return Ok(i + 1),
            _ => i += 1,
        }
    }
    Err(i)
}

fn consume_operator(bytes: &[u8], start: usize) -> Option<(TokenKind, usize)> {
    use TokenKind::*;
    let two = if start + 1 < bytes.len() {
        Some((bytes[start], bytes[start + 1]))
    } else {
        None
    };

    if let Some((a, b)) = two {
        match (a, b) {
            (b'-', b'>') => return Some((Arrow, start + 2)),
            (b'=', b'>') => return Some((FatArrow, start + 2)),
            (b'=', b'=') => return Some((EqEq, start + 2)),
            (b':', b':') => return Some((ColonColon, start + 2)),
            (b'!', b'=') => return Some((BangEq, start + 2)),
            (b'<', b'=') => return Some((Le, start + 2)),
            (b'>', b'=') => return Some((Ge, start + 2)),
            (b'&', b'&') => return Some((AmpAmp, start + 2)),
            (b'|', b'|') => return Some((PipePipe, start + 2)),
            (b'.', b'.') => return Some((DotDot, start + 2)),
            _ => {}
        }
    }

    let ch = bytes[start];
    let kind = match ch {
        b'(' => LParen,
        b')' => RParen,
        b'{' => LBrace,
        b'}' => RBrace,
        b'[' => LBracket,
        b']' => RBracket,
        b',' => Comma,
        b':' => Colon,
        b';' => Semicolon,
        b'.' => Dot,
        b'+' => Plus,
        b'-' => Minus,
        b'*' => Star,
        b'/' => Slash,
        b'%' => Percent,
        b'=' => Eq,
        b'!' => Bang,
        b'<' => Lt,
        b'>' => Gt,
        b'&' => Amp,
        b'?' => Question,
        _ => Unknown,
    };

    Some((kind, start + 1))
}

#[cfg(test)]
mod tests {
    use super::*;
    use glyph_core::diag::Severity;
    use glyph_core::token::TokenKind;
    use insta::assert_debug_snapshot;
    use proptest::prelude::*;

    #[test]
    fn lexes_simple_function() {
        let src = "fn add(a: i, b: i) { ret a + b }";
        let out = lex(src);
        assert!(out.diagnostics.is_empty());
        assert_debug_snapshot!(out.tokens);
    }

    #[test]
    fn reports_unterminated_string() {
        let src = "let s = \"oops";
        let out = lex(src);
        assert!(!out.diagnostics.is_empty());
        assert_eq!(out.diagnostics[0].severity, Severity::Error);
    }

    proptest! {
        #[test]
        fn lexer_always_ends_with_eof(s in ".*") {
            let out = lex(&s);
            prop_assert!(!out.tokens.is_empty());
            let last = out.tokens.last().unwrap();
            prop_assert_eq!(last.kind.clone(), TokenKind::Eof);
            for w in out.tokens.windows(2) {
                prop_assert!(w[0].span.end <= w[1].span.end);
            }
        }
    }
}
