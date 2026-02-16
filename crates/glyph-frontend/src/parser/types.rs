use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_type_expr(&mut self) -> Option<TypeExpr> {
        // Tuple type: (T1, T2, ...) or parenthesized type: (T)
        if self.at(TokenKind::LParen) {
            let lp = self.advance().unwrap();

            // Empty tuple: ()
            if self.at(TokenKind::RParen) {
                let rp = self.advance().unwrap();
                return Some(TypeExpr::Tuple {
                    elements: vec![],
                    span: Span::new(lp.span.start, rp.span.end),
                });
            }

            let first = self.parse_type_expr()?;

            // Check if it's a tuple (has comma) or just parenthesized
            if self.at(TokenKind::Comma) {
                let mut elements = vec![first];
                while self.at(TokenKind::Comma) {
                    self.advance();
                    if self.at(TokenKind::RParen) {
                        break;
                    }
                    elements.push(self.parse_type_expr()?);
                }
                let rp = self.consume(TokenKind::RParen, "expected `)`")?;
                return Some(TypeExpr::Tuple {
                    elements,
                    span: Span::new(lp.span.start, rp.span.end),
                });
            } else {
                // Parenthesized type
                self.consume(TokenKind::RParen, "expected `)`")?;
                return Some(first);
            }
        }

        // Array type: [T; N]
        if self.at(TokenKind::LBracket) {
            let lb = self.advance().unwrap();
            let elem = self.parse_type_expr()?;
            self.consume(TokenKind::Semicolon, "expected `;` in array type")?;
            let size_tok = self.consume(TokenKind::Int, "expected array size")?;
            let size: usize = self.slice(size_tok).parse().ok()?;
            let rb = self.consume(TokenKind::RBracket, "expected `]")?;
            return Some(TypeExpr::Array {
                elem: Box::new(elem),
                size,
                span: Span::new(lb.span.start, rb.span.end),
            });
        }

        // Reference type: &T or &mut T
        if self.at(TokenKind::Amp) {
            let amp = self.advance().unwrap();
            let mutability = if self.at(TokenKind::Mut) {
                self.advance();
                Mutability::Mutable
            } else {
                Mutability::Immutable
            };
            let inner = self.parse_type_expr()?;
            let span_end = match &inner {
                TypeExpr::Path { span, .. }
                | TypeExpr::App { span, .. }
                | TypeExpr::Ref { span, .. }
                | TypeExpr::Array { span, .. }
                | TypeExpr::Tuple { span, .. } => span.end,
            };
            return Some(TypeExpr::Ref {
                mutability,
                inner: Box::new(inner),
                span: Span::new(amp.span.start, span_end),
            });
        }

        // Base path (with optional qualifiers)
        let first = self.consume(TokenKind::Ident, "expected type")?;
        let mut segments = vec![self.slice(first)];
        let mut end_span = first.span.end;
        while self.at(TokenKind::ColonColon) {
            self.advance();
            if let Some(seg) = self.consume(TokenKind::Ident, "expected identifier after `::`") {
                segments.push(self.slice(seg));
                end_span = seg.span.end;
            } else {
                break;
            }
        }
        let mut base = TypeExpr::Path {
            segments,
            span: Span::new(first.span.start, end_span),
        };

        // Generic application
        if self.at(TokenKind::Lt) {
            let lt = self.advance().unwrap();
            let mut args = Vec::new();
            while !self.at(TokenKind::Gt) && !self.at(TokenKind::Eof) {
                let arg = self.parse_type_expr()?;
                args.push(arg);
                if self.at(TokenKind::Comma) {
                    self.advance();
                    continue;
                }
            }
            let gt = self.consume(TokenKind::Gt, "expected `>` after type arguments")?;
            base = TypeExpr::App {
                base: Box::new(base),
                args,
                span: Span::new(lt.span.start.min(first.span.start), gt.span.end),
            };
        }

        Some(base)
    }
}
