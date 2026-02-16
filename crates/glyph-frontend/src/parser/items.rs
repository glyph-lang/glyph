use super::*;
use crate::method_symbols::{inherent_method_symbol, interface_method_symbol};

impl<'a> Parser<'a> {
    pub(super) fn parse_function(&mut self) -> Option<Function> {
        let fn_tok = self.consume(TokenKind::Fn, "expected `fn`")?;
        let name_tok = self.consume(TokenKind::Ident, "expected function name")?;
        let name = Ident(self.slice(name_tok));

        self.consume(TokenKind::LParen, "expected `(` after function name")?;
        let params = self.parse_params();
        self.consume(TokenKind::RParen, "expected `)` after parameters")?;

        let mut ret_type = None;
        if self.at(TokenKind::Arrow) {
            self.advance();
            ret_type = self.parse_type_expr();
        }

        let body = self.parse_block()?;

        Some(Function {
            name,
            params,
            ret_type,
            span: Span::new(fn_tok.span.start, body.span.end),
            body,
        })
    }

    pub(super) fn parse_const(&mut self) -> Option<ConstDef> {
        let const_tok = self.consume(TokenKind::Const, "expected `const`")?;
        let name_tok = self.consume(TokenKind::Ident, "expected constant name")?;

        if !self.at(TokenKind::Colon) {
            let span = self.peek().map(|t| t.span).or(Some(name_tok.span));
            self.diagnostics.push(Diagnostic::error(
                "top-level const requires explicit type annotation",
                span,
            ));
            return None;
        }
        self.advance();
        let ty = self.parse_type_expr()?;

        self.consume(TokenKind::Eq, "expected `=` after const type")?;
        let value = self.parse_expr()?;

        if self.at(TokenKind::Semicolon) {
            self.advance();
        }

        let end = self.expr_end(&value);
        Some(ConstDef {
            name: Ident(self.slice(name_tok)),
            ty,
            value,
            span: Span::new(const_tok.span.start, end),
        })
    }

    pub(super) fn parse_extern_function(&mut self) -> Option<ExternFunctionDecl> {
        let extern_tok = self.consume(TokenKind::Extern, "expected `extern`")?;
        let abi = if self.at(TokenKind::Str) {
            let abi_tok = self.advance().unwrap();
            let raw = self.slice(abi_tok);
            Some(raw.trim_matches('"').to_string())
        } else {
            None
        };

        let _fn_tok = self.consume(TokenKind::Fn, "expected `fn` after `extern`")?;
        let name_tok = self.consume(TokenKind::Ident, "expected function name")?;
        let name = Ident(self.slice(name_tok));

        self.consume(TokenKind::LParen, "expected `(` after function name")?;
        let params = self.parse_params();
        self.consume(TokenKind::RParen, "expected `)` after parameters")?;

        let mut ret_type = None;
        if self.at(TokenKind::Arrow) {
            self.advance();
            ret_type = self.parse_type_expr();
        }

        // Validate ABI (v0: only "C" allowed if provided)
        if let Some(ref abi_str) = abi {
            if abi_str != "C" {
                let span = self.peek().map(|t| t.span).or(Some(extern_tok.span));
                self.diagnostics.push(Diagnostic::error(
                    format!("unsupported ABI '{}'; only \"C\" is supported", abi_str),
                    span,
                ));
                return None;
            }
        }

        // Extern declarations must end with semicolon; bodies are not allowed.
        if self.at(TokenKind::LBrace) {
            let span = self.peek().map(|t| t.span);
            self.diagnostics.push(Diagnostic::error(
                "extern functions cannot have a body; use `;` to terminate the declaration",
                span,
            ));
            return None;
        }
        let end = self.consume(
            TokenKind::Semicolon,
            "expected `;` after extern function declaration",
        )?;

        Some(ExternFunctionDecl {
            abi,
            name,
            params,
            ret_type,
            link_name: None,
            span: Span::new(extern_tok.span.start, end.span.end),
        })
    }

    pub(super) fn parse_interface(&mut self) -> Option<InterfaceDef> {
        let iface_tok = self.consume(TokenKind::Interface, "expected `interface`")?;
        let name_tok = self.consume(TokenKind::Ident, "expected interface name")?;
        let name = Ident(self.slice(name_tok));
        self.consume(TokenKind::LBrace, "expected `{` to start interface body")?;
        let mut methods = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            if self.at(TokenKind::Pub) {
                self.advance();
            }
            if self.at(TokenKind::Fn) {
                if let Some(method) = self.parse_interface_method() {
                    methods.push(method);
                } else {
                    break;
                }
            } else {
                let span = self.peek().map(|t| t.span);
                self.diagnostics.push(Diagnostic::error(
                    "interfaces may only contain method signatures",
                    span,
                ));
                self.synchronize();
                break;
            }
        }
        let end = self.consume(TokenKind::RBrace, "expected `}` to end interface body")?;
        Some(InterfaceDef {
            name,
            methods,
            span: Span::new(iface_tok.span.start, end.span.end),
        })
    }

    pub(super) fn parse_interface_method(&mut self) -> Option<InterfaceMethod> {
        let fn_tok = self.consume(TokenKind::Fn, "expected `fn`")?;
        let name_tok = self.consume(TokenKind::Ident, "expected method name")?;
        let name = Ident(self.slice(name_tok));
        self.consume(TokenKind::LParen, "expected `(` after method name")?;
        let params = self.parse_params();
        self.consume(TokenKind::RParen, "expected `)` after parameters")?;
        let mut ret_type = None;
        if self.at(TokenKind::Arrow) {
            self.advance();
            ret_type = self.parse_type_expr();
        }

        self.consume(
            TokenKind::Semicolon,
            "expected `;` after interface method signature",
        )?;
        Some(InterfaceMethod {
            name,
            params,
            ret_type,
            span: Span::new(fn_tok.span.start, name_tok.span.end),
        })
    }

    pub(super) fn parse_struct(&mut self) -> Option<StructDef> {
        let struct_tok = self.consume(TokenKind::Struct, "expected `struct`")?;
        let name_tok = self.consume(TokenKind::Ident, "expected struct name")?;
        let name = Ident(self.slice(name_tok));

        let generic_params = self.parse_generic_params();

        let mut interfaces = Vec::new();
        if self.at(TokenKind::Colon) {
            self.advance();
            loop {
                let iface_tok = self.consume(TokenKind::Ident, "expected interface name")?;
                interfaces.push(Ident(self.slice(iface_tok)));
                if self.at(TokenKind::Comma) {
                    self.advance();
                    continue;
                }
                break;
            }
        }

        self.consume(TokenKind::LBrace, "expected `{` after struct name")?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut inline_impls = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            if self.at(TokenKind::Pub) {
                self.advance();
            }
            if self.at(TokenKind::Fn) {
                if let Some(func) = self.parse_function() {
                    methods.push(func);
                } else {
                    break;
                }
                continue;
            }

            if self.at(TokenKind::Impl) {
                if let Some(block) = self.parse_inline_impl(&name) {
                    inline_impls.push(block);
                } else {
                    break;
                }
                continue;
            }

            let field_name_tok = match self.consume(TokenKind::Ident, "expected field name") {
                Some(tok) => tok,
                None => break,
            };
            self.consume(TokenKind::Colon, "expected `:` after field name")?;
            let ty = match self.parse_type_expr() {
                Some(t) => t,
                None => break,
            };

            let end_span = match &ty {
                TypeExpr::Path { span, .. }
                | TypeExpr::App { span, .. }
                | TypeExpr::Ref { span, .. }
                | TypeExpr::Array { span, .. }
                | TypeExpr::Tuple { span, .. } => span.end,
            };

            let field = FieldDef {
                name: Ident(self.slice(field_name_tok)),
                ty,
                span: Span::new(field_name_tok.span.start, end_span),
            };
            fields.push(field);

            // Optional comma or just newline separator
            if self.at(TokenKind::Comma) {
                self.advance();
            }
        }

        let end = self.consume(TokenKind::RBrace, "expected `}` to end struct")?;

        for method in &methods {
            self.schedule_method_function(&name, method, None);
        }

        Some(StructDef {
            name,
            generic_params,
            fields,
            interfaces,
            methods,
            inline_impls,
            span: Span::new(struct_tok.span.start, end.span.end),
        })
    }

    pub(super) fn parse_enum(&mut self) -> Option<EnumDef> {
        let enum_tok = self.consume(TokenKind::Enum, "expected `enum`")?;
        let name_tok = self.consume(TokenKind::Ident, "expected enum name")?;
        let name = Ident(self.slice(name_tok));

        let generic_params = self.parse_generic_params();

        self.consume(TokenKind::LBrace, "expected `{` after enum name")?;

        let mut variants = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let variant_tok = match self.consume(TokenKind::Ident, "expected variant name") {
                Some(tok) => tok,
                None => break,
            };
            let variant_name = Ident(self.slice(variant_tok));
            let mut payload = None;
            let mut end = variant_tok.span.end;

            if self.at(TokenKind::LParen) {
                self.advance();
                payload = self.parse_type_expr();
                let close =
                    self.consume(TokenKind::RParen, "expected `)` after variant payload")?;
                end = close.span.end;
            }

            if self.at(TokenKind::Comma) {
                let comma = self.advance().unwrap();
                end = comma.span.end;
            }

            variants.push(EnumVariantDef {
                name: variant_name,
                payload,
                span: Span::new(variant_tok.span.start, end),
            });
        }

        let end_tok = self.consume(TokenKind::RBrace, "expected `}` after enum variants")?;

        Some(EnumDef {
            name,
            generic_params,
            variants,
            span: Span::new(enum_tok.span.start, end_tok.span.end),
        })
    }

    pub(super) fn parse_inline_impl(&mut self, struct_name: &Ident) -> Option<InlineImpl> {
        let impl_tok = self.consume(TokenKind::Impl, "expected `impl`")?;
        let iface_tok = self.consume(TokenKind::Ident, "expected interface name")?;
        self.consume(
            TokenKind::LBrace,
            "expected `{` to start interface implementation block",
        )?;
        let mut methods = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            if self.at(TokenKind::Pub) {
                self.advance();
            }
            if self.at(TokenKind::Fn) {
                if let Some(func) = self.parse_function() {
                    methods.push(func);
                } else {
                    break;
                }
            } else {
                let span = self.peek().map(|t| t.span);
                self.diagnostics.push(Diagnostic::error(
                    "only functions are allowed inside impl blocks",
                    span,
                ));
                self.synchronize();
                break;
            }
        }
        let end = self.consume(TokenKind::RBrace, "expected `}` to end impl block")?;
        let interface_ident = Ident(self.slice(iface_tok));
        for method in &methods {
            self.schedule_method_function(struct_name, method, Some(&interface_ident));
        }
        Some(InlineImpl {
            interface: interface_ident,
            methods,
            span: Span::new(impl_tok.span.start, end.span.end),
        })
    }

    pub(super) fn parse_impl_block(&mut self) -> Option<ImplBlock> {
        let impl_tok = self.consume(TokenKind::Impl, "expected `impl`")?;
        let iface_tok = self.consume(TokenKind::Ident, "expected interface name")?;
        self.consume(TokenKind::For, "expected `for` in impl block")?;
        let target_tok = self.consume(TokenKind::Ident, "expected struct name in impl block")?;
        self.consume(TokenKind::LBrace, "expected `{` to start impl body")?;
        let mut methods = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            if self.at(TokenKind::Pub) {
                self.advance();
            }
            if self.at(TokenKind::Fn) {
                if let Some(func) = self.parse_function() {
                    methods.push(func);
                } else {
                    break;
                }
            } else {
                let span = self.peek().map(|t| t.span);
                self.diagnostics.push(Diagnostic::error(
                    "only functions are allowed inside impl blocks",
                    span,
                ));
                self.synchronize();
                break;
            }
        }
        let end = self.consume(TokenKind::RBrace, "expected `}` to end impl block")?;
        let interface_ident = Ident(self.slice(iface_tok));
        let target_ident = Ident(self.slice(target_tok));
        for method in &methods {
            self.schedule_method_function(&target_ident, method, Some(&interface_ident));
        }
        Some(ImplBlock {
            interface: interface_ident,
            target: target_ident,
            methods,
            span: Span::new(impl_tok.span.start, end.span.end),
        })
    }

    fn schedule_method_function(
        &mut self,
        struct_name: &Ident,
        method: &Function,
        interface: Option<&Ident>,
    ) {
        if let Some(func) = self.prepare_method_function(struct_name, method, interface) {
            self.pending_items.push(Item::Function(func));
        }
    }

    fn prepare_method_function(
        &mut self,
        struct_name: &Ident,
        method: &Function,
        interface: Option<&Ident>,
    ) -> Option<Function> {
        if method.params.is_empty() {
            self.diagnostics.push(Diagnostic::error(
                "methods must declare a `self` parameter",
                Some(method.span),
            ));
            return None;
        }

        let mut func = method.clone();
        let first = func.params.get_mut(0).expect("self param exists");
        if first.name.0 != "self" {
            self.diagnostics.push(Diagnostic::error(
                "first parameter of a method must be named `self`",
                Some(first.span),
            ));
            return None;
        }

        // Canonicalize the self parameter type to match the struct and preserve
        // reference variants for downstream resolution/codegen.

        let canonical_self_ty = match first.ty.as_ref() {
            Some(ty) => {
                let normalized = render_type_expr(ty).replace(' ', "");
                let value_ty = struct_name.0.as_str();
                let ref_ty = format!("&{}", value_ty);
                let mutref_ty = format!("&mut {}", value_ty);

                if normalized == value_ty {
                    TypeExpr::Path {
                        segments: vec![struct_name.0.clone()],
                        span: first.span,
                    }
                } else if normalized == ref_ty {
                    TypeExpr::Ref {
                        mutability: Mutability::Immutable,
                        inner: Box::new(TypeExpr::Path {
                            segments: vec![struct_name.0.clone()],
                            span: first.span,
                        }),
                        span: first.span,
                    }
                } else if normalized == mutref_ty {
                    TypeExpr::Ref {
                        mutability: Mutability::Mutable,
                        inner: Box::new(TypeExpr::Path {
                            segments: vec![struct_name.0.clone()],
                            span: first.span,
                        }),
                        span: first.span,
                    }
                } else {
                    self.diagnostics.push(Diagnostic::error(
                        format!(
                            "self parameter type must be '{}', '&{}', or '&mut {}'",
                            struct_name.0, struct_name.0, struct_name.0
                        ),
                        Some(first.span),
                    ));
                    return None;
                }
            }
            None => TypeExpr::Path {
                segments: vec![struct_name.0.clone()],
                span: first.span,
            },
        };

        first.ty = Some(canonical_self_ty);

        let symbol = match interface {
            Some(iface) => interface_method_symbol(&struct_name.0, &iface.0, &method.name.0),
            None => inherent_method_symbol(&struct_name.0, &method.name.0),
        };
        func.name = Ident(symbol);
        Some(func)
    }

    pub(super) fn parse_generic_params(&mut self) -> Vec<Ident> {
        if !self.at(TokenKind::Lt) {
            return Vec::new();
        }
        let mut params = Vec::new();
        self.advance();
        while !self.at(TokenKind::Gt) && !self.at(TokenKind::Eof) {
            if let Some(tok) = self.consume(TokenKind::Ident, "expected generic parameter") {
                params.push(Ident(self.slice(tok)));
            } else {
                break;
            }
            if self.at(TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        if let Some(gt) = self.consume(TokenKind::Gt, "expected `>` to close generic parameters") {
            let _ = gt;
        }
        params
    }

    pub(super) fn parse_params(&mut self) -> Vec<Param> {
        let mut params = Vec::new();
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let start_tok = match self.consume(TokenKind::Ident, "expected parameter name") {
                Some(tok) => tok,
                None => break,
            };
            let mut ty = None;
            let mut end_span = start_tok.span;
            if self.at(TokenKind::Colon) {
                self.advance();
                if let Some(parsed_ty) = self.parse_type_expr() {
                    let current_pos = if let Some(tok) = self.peek() {
                        tok.span.start
                    } else {
                        start_tok.span.end
                    };
                    end_span = Span::new(start_tok.span.start, current_pos);
                    ty = Some(parsed_ty);
                }
            }
            let span = Span::new(start_tok.span.start, end_span.end);
            params.push(Param {
                name: Ident(self.slice(start_tok)),
                ty,
                span,
            });
            if self.at(TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        params
    }
}
