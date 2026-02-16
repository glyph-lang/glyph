use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_import(&mut self) -> Option<Import> {
        if self.at(TokenKind::From) {
            let from_tok = self.consume(TokenKind::From, "expected 'from'")?;
            let start = from_tok.span.start;
            let path = self.parse_import_path()?;

            self.consume(TokenKind::Import, "expected 'import' after module path")?;

            let mut items = Vec::new();

            let first_tok = self.consume(TokenKind::Ident, "expected symbol name")?;
            let mut end_span = first_tok.span.end;
            let mut alias = None;
            if self.at(TokenKind::As) {
                self.advance();
                let alias_tok = self.consume(TokenKind::Ident, "expected alias name")?;
                end_span = alias_tok.span.end;
                alias = Some(Ident(self.slice(&alias_tok)));
            }

            items.push(ImportItem {
                name: Ident(self.slice(&first_tok)),
                alias,
                span: first_tok.span,
            });

            while self.at(TokenKind::Comma) {
                self.advance();
                let name_tok = self.consume(TokenKind::Ident, "expected symbol name")?;
                let name = Ident(self.slice(&name_tok));
                end_span = name_tok.span.end;

                let alias = if self.at(TokenKind::As) {
                    self.advance();
                    let alias_tok = self.consume(TokenKind::Ident, "expected alias name")?;
                    end_span = alias_tok.span.end;
                    Some(Ident(self.slice(&alias_tok)))
                } else {
                    None
                };

                items.push(ImportItem {
                    name,
                    alias,
                    span: name_tok.span,
                });
            }

            return Some(Import {
                kind: ImportKind::Selective { items },
                path,
                span: Span::new(start, end_span),
            });
        }

        let import_tok = self.consume(TokenKind::Import, "expected 'import'")?;
        let start = import_tok.span.start;

        // Determine if this is a selective import by looking ahead for "from"
        let is_selective = self.lookahead_for_from();

        let (kind, path, end) = if is_selective {
            // Parse: import foo, bar as baz from file_b
            let mut items = Vec::new();

            loop {
                let name_tok = self.consume(TokenKind::Ident, "expected symbol name")?;
                let name = Ident(self.slice(&name_tok));
                let item_span = name_tok.span;

                let alias = if self.at(TokenKind::As) {
                    self.advance();
                    let alias_tok = self.consume(TokenKind::Ident, "expected alias name")?;
                    Some(Ident(self.slice(&alias_tok)))
                } else {
                    None
                };

                items.push(ImportItem {
                    name,
                    alias,
                    span: item_span,
                });

                if self.at(TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            self.consume(TokenKind::From, "expected 'from' after import list")?;
            let path = self.parse_import_path()?;
            let end = path.span.end;
            (ImportKind::Selective { items }, path, end)
        } else {
            // Parse: import file_b
            let path = self.parse_import_path()?;
            let end = path.span.end;
            (ImportKind::Wildcard, path, end)
        };

        Some(Import {
            kind,
            path,
            span: Span::new(start, end),
        })
    }

    pub(super) fn parse_import_path(&mut self) -> Option<ImportPath> {
        // Check for ".." at the start (parent directory import)
        if self.at(TokenKind::DotDot) {
            let tok = self.peek()?;
            self.diagnostics.push(Diagnostic::error(
                "parent directory imports not allowed (..)",
                Some(tok.span),
            ));
            return None;
        }

        let first_tok = self.consume(TokenKind::Ident, "expected module name")?;
        let start = first_tok.span.start;
        let mut segments = vec![self.slice(&first_tok)];
        let mut end = first_tok.span.end;

        // Handle paths like "subdir/module"
        while self.at(TokenKind::Slash) {
            self.advance();

            // Check for ".." in path segments
            if self.at(TokenKind::DotDot) {
                let tok = self.peek()?;
                self.diagnostics.push(Diagnostic::error(
                    "parent directory imports not allowed (..)",
                    Some(tok.span),
                ));
                return None;
            }

            let seg_tok = self.consume(TokenKind::Ident, "expected path segment")?;
            segments.push(self.slice(&seg_tok));
            end = seg_tok.span.end;
        }

        Some(ImportPath {
            segments,
            span: Span::new(start, end),
        })
    }

    pub(super) fn lookahead_for_from(&self) -> bool {
        let mut pos = self.pos;
        while pos < self.tokens.len() {
            match self.tokens[pos].kind {
                TokenKind::From => return true,
                TokenKind::Semicolon
                | TokenKind::Eof
                | TokenKind::Const
                | TokenKind::Struct
                | TokenKind::Fn
                | TokenKind::Interface
                | TokenKind::Impl => return false,
                _ => pos += 1,
            }
        }
        false
    }
}
