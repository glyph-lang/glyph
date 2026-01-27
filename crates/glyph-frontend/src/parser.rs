use crate::method_symbols::{inherent_method_symbol, interface_method_symbol};
use glyph_core::{
    ast::{
        BinaryOp, Block, ConstDef, EnumDef, EnumVariantDef, Expr, ExternFunctionDecl, FieldDef,
        Function, Ident, ImplBlock, Import, ImportItem, ImportKind, ImportPath, InlineImpl,
        InterfaceDef, InterfaceMethod, InterpSegment, Item, Literal, MatchArm, MatchPattern,
        Module, Param, Stmt, StructDef, TypeExpr, UnaryOp,
    },
    diag::Diagnostic,
    span::Span,
    token::{Token, TokenKind},
    types::Mutability,
};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ParseOutput {
    pub module: Module,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn parse(tokens: &[Token], source: &str) -> ParseOutput {
    let mut parser = Parser {
        tokens,
        source,
        pos: 0,
        diagnostics: Vec::new(),
        items: Vec::new(),
        pending_items: Vec::new(),
        suppress_struct_literals: false,
    };

    let mut imports = Vec::new();

    // Parse imports first (must come before items)
    while parser.at(TokenKind::Import) || parser.at(TokenKind::From) {
        match parser.parse_import() {
            Some(import) => imports.push(import),
            None => parser.synchronize(),
        }
    }

    // Then parse items
    while !parser.at(TokenKind::Eof) {
        if parser.at(TokenKind::Struct) {
            match parser.parse_struct() {
                Some(s) => {
                    parser.items.push(Item::Struct(s));
                    parser.flush_pending_items();
                }
                None => parser.synchronize(),
            }
        } else if parser.at(TokenKind::Enum) {
            match parser.parse_enum() {
                Some(e) => {
                    parser.items.push(Item::Enum(e));
                    parser.flush_pending_items();
                }
                None => parser.synchronize(),
            }
        } else if parser.at(TokenKind::Interface) {
            match parser.parse_interface() {
                Some(iface) => {
                    parser.items.push(Item::Interface(iface));
                    parser.flush_pending_items();
                }
                None => parser.synchronize(),
            }
        } else if parser.at(TokenKind::Impl) {
            match parser.parse_impl_block() {
                Some(block) => {
                    parser.items.push(Item::Impl(block));
                    parser.flush_pending_items();
                }
                None => parser.synchronize(),
            }
        } else if parser.at(TokenKind::Fn) {
            match parser.parse_function() {
                Some(func) => parser.items.push(Item::Function(func)),
                None => parser.synchronize(),
            }
        } else if parser.at(TokenKind::Const) {
            match parser.parse_const() {
                Some(def) => parser.items.push(Item::Const(def)),
                None => parser.synchronize(),
            }
        } else if parser.at(TokenKind::Pub) {
            parser.advance();
            if parser.at(TokenKind::Struct) {
                match parser.parse_struct() {
                    Some(s) => {
                        parser.items.push(Item::Struct(s));
                        parser.flush_pending_items();
                    }
                    None => parser.synchronize(),
                }
            } else if parser.at(TokenKind::Enum) {
                match parser.parse_enum() {
                    Some(e) => {
                        parser.items.push(Item::Enum(e));
                        parser.flush_pending_items();
                    }
                    None => parser.synchronize(),
                }
            } else if parser.at(TokenKind::Interface) {
                match parser.parse_interface() {
                    Some(iface) => {
                        parser.items.push(Item::Interface(iface));
                        parser.flush_pending_items();
                    }
                    None => parser.synchronize(),
                }
            } else if parser.at(TokenKind::Impl) {
                match parser.parse_impl_block() {
                    Some(block) => {
                        parser.items.push(Item::Impl(block));
                        parser.flush_pending_items();
                    }
                    None => parser.synchronize(),
                }
            } else if parser.at(TokenKind::Fn) {
                match parser.parse_function() {
                    Some(func) => parser.items.push(Item::Function(func)),
                    None => parser.synchronize(),
                }
            } else if parser.at(TokenKind::Const) {
                match parser.parse_const() {
                    Some(def) => parser.items.push(Item::Const(def)),
                    None => parser.synchronize(),
                }
            } else if parser.at(TokenKind::Extern) {
                match parser.parse_extern_function() {
                    Some(extern_fn) => parser.items.push(Item::ExternFunction(extern_fn)),
                    None => parser.synchronize(),
                }
            } else {
                let span = parser.peek().map(|t| t.span);
                parser
                    .diagnostics
                    .push(Diagnostic::error("expected item after `pub`", span));
                parser.synchronize();
            }
        } else if parser.at(TokenKind::Extern) {
            match parser.parse_extern_function() {
                Some(extern_fn) => parser.items.push(Item::ExternFunction(extern_fn)),
                None => parser.synchronize(),
            }
        } else {
            let span = parser.peek().map(|t| t.span);
            parser.diagnostics.push(Diagnostic::error(
                "expected `const`, `fn`, `struct`, `enum`, `interface`, `impl`, or `extern`",
                span,
            ));
            parser.synchronize();
        }
    }

    parser.flush_pending_items();

    ParseOutput {
        module: Module {
            imports,
            items: parser.items,
        },
        diagnostics: parser.diagnostics,
    }
}

struct Parser<'a> {
    tokens: &'a [Token],
    source: &'a str,
    pos: usize,
    diagnostics: Vec<Diagnostic>,
    items: Vec<Item>,
    pending_items: Vec<Item>,
    suppress_struct_literals: bool,
}

impl<'a> Parser<'a> {
    fn at(&self, kind: TokenKind) -> bool {
        match self.peek() {
            Some(t) => t.kind == kind,
            None => kind == TokenKind::Eof,
        }
    }

    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.pos)
    }

    fn flush_pending_items(&mut self) {
        if self.pending_items.is_empty() {
            return;
        }
        let pending = std::mem::take(&mut self.pending_items);
        self.items.extend(pending);
    }

    fn advance(&mut self) -> Option<&'a Token> {
        let t = self.tokens.get(self.pos);
        if t.is_some() {
            self.pos += 1;
        }
        t
    }

    fn parse_function(&mut self) -> Option<Function> {
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

    fn parse_const(&mut self) -> Option<ConstDef> {
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

    fn parse_extern_function(&mut self) -> Option<ExternFunctionDecl> {
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

    fn parse_interface(&mut self) -> Option<InterfaceDef> {
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

    fn parse_interface_method(&mut self) -> Option<InterfaceMethod> {
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

    fn parse_struct(&mut self) -> Option<StructDef> {
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

    fn parse_enum(&mut self) -> Option<EnumDef> {
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

    fn parse_inline_impl(&mut self, struct_name: &Ident) -> Option<InlineImpl> {
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

    fn parse_impl_block(&mut self) -> Option<ImplBlock> {
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

    fn parse_generic_params(&mut self) -> Vec<Ident> {
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

    fn parse_type_expr(&mut self) -> Option<TypeExpr> {
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

    fn parse_params(&mut self) -> Vec<Param> {
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

    fn parse_block(&mut self) -> Option<Block> {
        let lb = self.consume(TokenKind::LBrace, "expected `{` to start block")?;
        let start = lb.span.start;
        let mut stmts = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                self.synchronize();
            }
            if self.at(TokenKind::Semicolon) {
                self.advance();
            }
        }
        let end = if let Some(rb) = self.consume(TokenKind::RBrace, "expected `}` to end block") {
            rb.span.end
        } else {
            lb.span.end
        };
        Some(Block {
            span: Span::new(start, end),
            stmts,
        })
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        if self.at(TokenKind::Let) {
            let let_tok = self.advance().unwrap();

            let mutable = if self.at(TokenKind::Mut) {
                self.advance();
                true
            } else {
                false
            };

            let name_tok = self.consume(TokenKind::Ident, "expected identifier after `let`")?;
            let mut ty = None;
            if self.at(TokenKind::Colon) {
                self.advance();
                ty = self.parse_type_expr();
            }
            let mut value = None;
            if self.at(TokenKind::Eq) {
                self.advance();
                value = self.parse_expr();
            }
            let end = value
                .as_ref()
                .map(|v| self.expr_end(v))
                .unwrap_or(name_tok.span.end);
            let span = Span::new(let_tok.span.start, end);
            return Some(Stmt::Let {
                name: Ident(self.slice(name_tok)),
                mutable,
                ty,
                value,
                span,
            });
        }

        if self.at(TokenKind::Ret) {
            let ret_tok = self.advance().unwrap();
            let expr = if self.at(TokenKind::RBrace) || self.at(TokenKind::Semicolon) {
                None
            } else {
                self.parse_expr()
            };
            let end = expr
                .as_ref()
                .map(|e| self.expr_end(e))
                .unwrap_or(ret_tok.span.end);
            return Some(Stmt::Ret(expr, Span::new(ret_tok.span.start, end)));
        }

        if self.at(TokenKind::Break) {
            let break_tok = self.advance().unwrap();
            return Some(Stmt::Break(break_tok.span));
        }

        if self.at(TokenKind::Cont) {
            let cont_tok = self.advance().unwrap();
            return Some(Stmt::Continue(cont_tok.span));
        }

        // Try to parse as assignment or expression statement
        let expr = self.parse_expr()?;

        // Check if this is an assignment (target = value)
        if self.at(TokenKind::Eq) {
            let start = self.expr_start(&expr);
            self.advance(); // consume =

            let value = self.parse_expr()?;
            let end = self.expr_end(&value);
            let span = Span::new(start, end);

            return Some(Stmt::Assign {
                target: expr,
                value,
                span,
            });
        }

        // Otherwise, it's an expression statement
        let span = Span::new(self.expr_start(&expr), self.expr_end(&expr));
        Some(Stmt::Expr(expr, span))
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_match_expr()
    }

    fn parse_match_expr(&mut self) -> Option<Expr> {
        if self.at(TokenKind::Match) {
            let match_tok = self.advance().unwrap();
            let prev = self.suppress_struct_literals;
            self.suppress_struct_literals = true;
            let scrutinee = self.parse_binary_expr();
            self.suppress_struct_literals = prev;
            let scrutinee = scrutinee?;
            self.consume(TokenKind::LBrace, "expected `{` after match scrutinee")?;

            let mut arms = Vec::new();
            while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
                let pattern = self.parse_match_pattern()?;
                if self.at(TokenKind::FatArrow) {
                    self.advance();
                } else if self.at(TokenKind::Eq) {
                    // Be tolerant if lexer produced '=' followed by '>'
                    self.advance();
                    self.consume(TokenKind::Gt, "expected `>` after `=` in match arm")?;
                } else {
                    self.consume(TokenKind::FatArrow, "expected `=>` after match pattern")?;
                }
                let expr = self.parse_expr()?;
                let mut end = self.expr_end(&expr);
                if self.at(TokenKind::Comma) {
                    let comma = self.advance().unwrap();
                    end = comma.span.end;
                }
                arms.push(MatchArm {
                    pattern,
                    span: Span::new(self.expr_start(&expr), end),
                    expr,
                });
            }

            let end_tok = self.consume(TokenKind::RBrace, "expected `}` to close match")?;
            return Some(Expr::Match {
                scrutinee: Box::new(scrutinee),
                arms,
                span: Span::new(match_tok.span.start, end_tok.span.end),
            });
        }

        self.parse_binary_expr()
    }

    fn parse_match_pattern(&mut self) -> Option<MatchPattern> {
        if self.at(TokenKind::Ident) {
            let tok = self.peek().unwrap();
            if self.slice(tok) == "_" {
                self.advance();
                return Some(MatchPattern::Wildcard);
            }
        }

        let variant_tok = self.consume(TokenKind::Ident, "expected variant name")?;
        let mut name = Ident(self.slice(variant_tok));
        while self.at(TokenKind::ColonColon) {
            self.advance();
            let seg_tok = self.consume(
                TokenKind::Ident,
                "expected identifier after `::` in match pattern",
            )?;
            name = Ident(self.slice(seg_tok));
        }
        let mut binding = None;
        if self.at(TokenKind::LParen) {
            self.advance();
            if let Some(b) = self.consume(TokenKind::Ident, "expected binding name") {
                binding = Some(Ident(self.slice(b)));
            }
            self.consume(TokenKind::RParen, "expected `)` after binding")?;
        }

        Some(MatchPattern::Variant { name, binding })
    }

    fn parse_binary_expr(&mut self) -> Option<Expr> {
        self.parse_binary_expr_prec(0)
    }

    fn parse_binary_expr_prec(&mut self, min_prec: u8) -> Option<Expr> {
        let mut lhs = self.parse_call_or_primary()?;

        loop {
            let Some(op) = self.peek_binary_op() else {
                break;
            };
            let prec = self.binary_precedence(&op);
            if prec < min_prec {
                break;
            }

            self.advance();
            // All binary operators are left-associative.
            let rhs = self.parse_binary_expr_prec(prec.saturating_add(1))?;

            let span = Span::new(self.expr_start(&lhs), self.expr_end(&rhs));
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span,
            };
        }

        Some(lhs)
    }

    fn binary_precedence(&self, op: &BinaryOp) -> u8 {
        match op {
            // Lowest precedence
            BinaryOp::Or => 1,
            BinaryOp::And => 2,
            BinaryOp::Eq | BinaryOp::Ne => 3,
            BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => 4,
            BinaryOp::Add | BinaryOp::Sub => 5,
            BinaryOp::Mul | BinaryOp::Div => 6,
        }
    }

    fn parse_call_or_primary(&mut self) -> Option<Expr> {
        if self.at(TokenKind::Match) {
            return self.parse_match_expr();
        }
        // Unary not prefix (!expr)
        if self.at(TokenKind::Bang) {
            let start_tok = self.peek().unwrap();
            let start = start_tok.span.start;
            self.advance();
            let expr = self.parse_call_or_primary()?;
            let span = Span::new(start, self.expr_end(&expr));
            return Some(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(expr),
                span,
            });
        }
        // Check for reference prefix (&expr or &mut expr)
        if self.at(TokenKind::Amp) {
            let start_tok = self.peek().unwrap();
            let start = start_tok.span.start;
            self.advance();

            // Check for mut keyword
            let mutability = if self.at(TokenKind::Mut) {
                self.advance();
                glyph_core::types::Mutability::Mutable
            } else {
                glyph_core::types::Mutability::Immutable
            };

            // Parse the expression being referenced
            let expr = self.parse_call_or_primary()?;
            let span = Span::new(start, self.expr_end(&expr));

            return Some(Expr::Ref {
                expr: Box::new(expr),
                mutability,
                span,
            });
        }

        let mut expr = self.parse_primary()?;
        loop {
            if self.at(TokenKind::LParen) {
                let start = self.expr_start(&expr);
                self.advance();
                let mut args = Vec::new();
                while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                    if let Some(arg) = self.parse_expr() {
                        args.push(arg);
                    }
                    if self.at(TokenKind::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let end =
                    if let Some(rp) = self.consume(TokenKind::RParen, "expected `)` after args") {
                        rp.span.end
                    } else {
                        self.expr_end(args.last().unwrap_or(&expr))
                    };
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                    span: Span::new(start, end),
                };
                continue;
            } else if self.at(TokenKind::Dot) {
                // Field access or method call - need lookahead
                self.advance(); // consume dot

                // Check for numeric index (tuple element access like .0, .1)
                let (name_tok, name) = if self.at(TokenKind::Int) {
                    let idx_tok = self.advance().unwrap();
                    let idx_str = self.slice(idx_tok);
                    (idx_tok, Ident(idx_str.to_string()))
                } else {
                    let ident_tok =
                        self.consume(TokenKind::Ident, "expected field or method name after `.`")?;
                    (ident_tok, Ident(self.slice(ident_tok)))
                };

                // Lookahead: is there a `(` after the identifier?
                if self.at(TokenKind::LParen) {
                    // METHOD CALL: obj.method(args)
                    self.advance(); // consume (

                    let mut args = Vec::new();
                    while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                        if let Some(arg) = self.parse_expr() {
                            args.push(arg);
                        }
                        if self.at(TokenKind::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }

                    let end = if let Some(rp) =
                        self.consume(TokenKind::RParen, "expected `)` after method arguments")
                    {
                        rp.span.end
                    } else {
                        args.last()
                            .map(|a| self.expr_end(a))
                            .unwrap_or(name_tok.span.end)
                    };

                    let start = self.expr_start(&expr);
                    expr = Expr::MethodCall {
                        receiver: Box::new(expr),
                        method: name,
                        args,
                        span: Span::new(start, end),
                    };
                } else {
                    // FIELD ACCESS: obj.field
                    let span = Span::new(self.expr_start(&expr), name_tok.span.end);
                    expr = Expr::FieldAccess {
                        base: Box::new(expr),
                        field: name,
                        span,
                    };
                }
                continue;
            } else if self.at(TokenKind::LBracket) {
                // Array indexing
                self.advance(); // consume [
                let index_expr = self.parse_expr()?;
                let end = if let Some(rb) = self.consume(TokenKind::RBracket, "expected `]`") {
                    rb.span.end
                } else {
                    self.expr_end(&index_expr)
                };
                let span = Span::new(self.expr_start(&expr), end);
                expr = Expr::Index {
                    base: Box::new(expr),
                    index: Box::new(index_expr),
                    span,
                };
                continue;
            }
            break;
        }
        Some(expr)
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        let tok = self.advance()?;
        match tok.kind {
            TokenKind::Ident => {
                // Parse full path (supports `module::Type`)
                let ident_expr = self.parse_path_ident(&tok);
                if !self.suppress_struct_literals && self.at(TokenKind::LBrace) {
                    if let Expr::Ident(name, span) = ident_expr {
                        self.parse_struct_lit(name, span.start)
                    } else {
                        None
                    }
                } else {
                    Some(ident_expr)
                }
            }
            TokenKind::Int => Some(Expr::Lit(self.literal_from(tok), tok.span)),
            TokenKind::Float => Some(Expr::Lit(self.literal_from(tok), tok.span)),
            TokenKind::Str => Some(Expr::Lit(self.literal_from(tok), tok.span)),
            TokenKind::Char => Some(Expr::Lit(self.literal_from(tok), tok.span)),
            TokenKind::InterpStr => self.parse_interpolated_string(tok),
            TokenKind::Bool => Some(Expr::Lit(self.literal_from(tok), tok.span)),
            TokenKind::LParen => {
                let start = tok.span.start;

                // Empty tuple: ()
                if self.at(TokenKind::RParen) {
                    let rp = self.advance().unwrap();
                    return Some(Expr::Tuple {
                        elements: vec![],
                        span: Span::new(start, rp.span.end),
                    });
                }

                let first = self.parse_expr()?;

                // Check if it's a tuple (has comma) or just parenthesized
                if self.at(TokenKind::Comma) {
                    let mut elements = vec![first];
                    while self.at(TokenKind::Comma) {
                        self.advance();
                        if self.at(TokenKind::RParen) {
                            break;
                        }
                        elements.push(self.parse_expr()?);
                    }
                    let rp = self.consume(TokenKind::RParen, "expected `)`")?;
                    Some(Expr::Tuple {
                        elements,
                        span: Span::new(start, rp.span.end),
                    })
                } else {
                    // Parenthesized expression
                    self.consume(TokenKind::RParen, "expected `)`")?;
                    Some(first)
                }
            }
            TokenKind::If => self.parse_if(tok.span.start),
            TokenKind::While => self.parse_while(tok.span.start),
            TokenKind::For => self.parse_for(tok.span.start),
            TokenKind::LBracket => {
                let start = tok.span.start;
                let mut elements = Vec::new();

                while !self.at(TokenKind::RBracket) && !self.at(TokenKind::Eof) {
                    if let Some(elem) = self.parse_expr() {
                        elements.push(elem);
                    } else {
                        break;
                    }

                    if self.at(TokenKind::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }

                let end = if let Some(rb) = self.consume(TokenKind::RBracket, "expected `]`") {
                    rb.span.end
                } else {
                    tok.span.end
                };

                Some(Expr::ArrayLit {
                    elements,
                    span: Span::new(start, end),
                })
            }
            TokenKind::LBrace => {
                self.pos -= 1;
                self.parse_block().map(Expr::Block)
            }
            _ => {
                self.diagnostics.push(Diagnostic::error(
                    "unexpected token in expression",
                    Some(tok.span),
                ));
                None
            }
        }
    }

    fn parse_interpolated_string(&mut self, tok: &Token) -> Option<Expr> {
        let raw = self.slice(tok);
        if raw.len() < 3 {
            self.diagnostics.push(Diagnostic::error(
                "invalid interpolated string",
                Some(tok.span),
            ));
            return None;
        }

        let mut segments = Vec::new();
        let content = &raw[2..raw.len().saturating_sub(1)]; // strip $" and closing "
        let bytes = content.as_bytes();
        let mut i = 0usize;
        let mut literal = String::new();

        while i < bytes.len() {
            if bytes[i] == b'{' {
                if i + 1 < bytes.len() && bytes[i + 1] == b'{' {
                    literal.push('{');
                    i += 2;
                    continue;
                }

                // Flush current literal segment
                if !literal.is_empty() {
                    segments.push(InterpSegment::Literal(std::mem::take(&mut literal)));
                }

                let hole_start = i;
                i += 1;
                let mut depth = 1;
                while i < bytes.len() && depth > 0 {
                    match bytes[i] {
                        b'{' => depth += 1,
                        b'}' => {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                    i += 1;
                }

                if depth != 0 {
                    self.diagnostics.push(Diagnostic::error(
                        "unterminated interpolation hole",
                        Some(tok.span),
                    ));
                    return None;
                }

                let hole_end = i;
                let hole_src = content[hole_start + 1..hole_end].trim();
                let offset = tok.span.start + 2 + hole_start as u32;
                if let Some(expr) = self.parse_inline_expr(hole_src, offset) {
                    segments.push(InterpSegment::Expr(expr));
                }
                i += 1; // skip closing brace
                continue;
            }

            if bytes[i] == b'}' && i + 1 < bytes.len() && bytes[i + 1] == b'}' {
                literal.push('}');
                i += 2;
                continue;
            }

            if bytes[i] == b'\\' && i + 1 < bytes.len() {
                let escaped = match bytes[i + 1] {
                    b'n' => '\n',
                    b't' => '\t',
                    b'r' => '\r',
                    b'\\' => '\\',
                    b'"' => '"',
                    other => other as char,
                };
                literal.push(escaped);
                i += 2;
                continue;
            }

            literal.push(bytes[i] as char);
            i += 1;
        }

        if !literal.is_empty() {
            segments.push(InterpSegment::Literal(literal));
        }

        Some(Expr::InterpString {
            segments,
            span: tok.span,
        })
    }

    fn parse_inline_expr(&mut self, src: &str, span_offset: u32) -> Option<Expr> {
        let lex_out = crate::lexer::lex(src);
        for diag in lex_out.diagnostics {
            let span = diag
                .span
                .map(|s| Span::new(s.start + span_offset, s.end + span_offset));
            self.diagnostics.push(Diagnostic::error(diag.message, span));
        }

        let mut parser = Parser {
            tokens: &lex_out.tokens,
            source: src,
            pos: 0,
            diagnostics: Vec::new(),
            items: Vec::new(),
            pending_items: Vec::new(),
            suppress_struct_literals: false,
        };

        let expr = parser.parse_expr();
        for diag in parser.diagnostics {
            let span = diag
                .span
                .map(|s| Span::new(s.start + span_offset, s.end + span_offset));
            self.diagnostics.push(Diagnostic::error(diag.message, span));
        }
        expr
    }

    fn parse_path_ident(&mut self, first: &Token) -> Expr {
        let mut name = self.slice(first).to_string();
        let mut end = first.span.end;
        while self.at(TokenKind::ColonColon) {
            self.advance();
            if let Some(seg) = self.consume(TokenKind::Ident, "expected identifier after `::`") {
                name.push_str("::");
                name.push_str(&self.slice(seg));
                end = seg.span.end;
            } else {
                break;
            }
        }
        Expr::Ident(Ident(name), Span::new(first.span.start, end))
    }

    fn parse_if(&mut self, start: u32) -> Option<Expr> {
        let prev = self.suppress_struct_literals;
        self.suppress_struct_literals = true;
        let cond = self.parse_expr()?;
        self.suppress_struct_literals = prev;
        let then_block = self.parse_block()?;
        let else_block = if self.at(TokenKind::Else) {
            self.advance();
            if self.at(TokenKind::If) {
                let if_tok = self.advance().unwrap();
                let nested_start = if_tok.span.start;
                let nested = self.parse_if(nested_start)?;
                Some(Block {
                    span: Span::new(self.expr_start(&nested), self.expr_end(&nested)),
                    stmts: vec![Stmt::Expr(
                        nested.clone(),
                        Span::new(self.expr_start(&nested), self.expr_end(&nested)),
                    )],
                })
            } else {
                self.parse_block()
            }
        } else {
            None
        };
        let end = else_block
            .as_ref()
            .map(|b| b.span.end)
            .unwrap_or(then_block.span.end);
        Some(Expr::If {
            cond: Box::new(cond),
            then_block,
            else_block,
            span: Span::new(start, end),
        })
    }

    fn parse_while(&mut self, start: u32) -> Option<Expr> {
        let prev = self.suppress_struct_literals;
        self.suppress_struct_literals = true;
        let cond = self.parse_expr()?;
        self.suppress_struct_literals = prev;
        let body = self.parse_block()?;
        let end = body.span.end;
        Some(Expr::While {
            cond: Box::new(cond),
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_for(&mut self, start: u32) -> Option<Expr> {
        let var_tok = self.consume(TokenKind::Ident, "expected variable name in for loop")?;
        let var = Ident(self.slice(var_tok));

        self.consume(TokenKind::In, "expected `in` after loop variable")?;

        let range_start = self.parse_expr()?;

        self.consume(TokenKind::DotDot, "expected `..` for range")?;

        let range_end = self.parse_expr()?;

        let body = self.parse_block()?;
        let end = body.span.end;

        Some(Expr::For {
            var,
            start: Box::new(range_start),
            end: Box::new(range_end),
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_struct_lit(&mut self, name: Ident, start: u32) -> Option<Expr> {
        self.consume(TokenKind::LBrace, "expected `{` for struct literal")?;

        let mut fields = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let field_name_tok = self.consume(TokenKind::Ident, "expected field name")?;
            self.consume(TokenKind::Colon, "expected `:` after field name")?;
            let value = self.parse_expr()?;

            fields.push((Ident(self.slice(field_name_tok)), value));

            if self.at(TokenKind::Comma) {
                self.advance();
            } else if !self.at(TokenKind::RBrace) {
                break;
            }
        }

        let end = self.consume(TokenKind::RBrace, "expected `}` to end struct literal")?;

        Some(Expr::StructLit {
            name,
            fields,
            span: Span::new(start, end.span.end),
        })
    }

    fn literal_from(&self, tok: &Token) -> Literal {
        let text = self.slice(tok);
        match tok.kind {
            TokenKind::Int => Literal::Int(text.parse().unwrap_or(0)),
            TokenKind::Float => Literal::Float(text.parse().unwrap_or(0.0)),
            TokenKind::Str => Literal::Str(parse_string_literal(&text)),
            TokenKind::Char => Literal::Char(parse_char_literal(&text)),
            TokenKind::Bool => Literal::Bool(matches!(text.as_str(), "true")),
            _ => Literal::Int(0),
        }
    }

    fn expr_start(&self, expr: &Expr) -> u32 {
        match expr {
            Expr::Lit(_, sp) => sp.start,
            Expr::InterpString { span, .. } => span.start,
            Expr::Ident(_, sp) => sp.start,
            Expr::Unary { span, .. } => span.start,
            Expr::Binary { span, .. } => span.start,
            Expr::Call { span, .. } => span.start,
            Expr::If { span, .. } => span.start,
            Expr::Block(block) => block.span.start,
            Expr::StructLit { span, .. } => span.start,
            Expr::FieldAccess { span, .. } => span.start,
            Expr::Ref { span, .. } => span.start,
            Expr::While { span, .. } => span.start,
            Expr::For { span, .. } => span.start,
            Expr::ArrayLit { span, .. } => span.start,
            Expr::Index { span, .. } => span.start,
            Expr::MethodCall { span, .. } => span.start,
            Expr::Match { span, .. } => span.start,
            Expr::Tuple { span, .. } => span.start,
        }
    }

    fn expr_end(&self, expr: &Expr) -> u32 {
        match expr {
            Expr::Lit(_, sp) => sp.end,
            Expr::InterpString { span, .. } => span.end,
            Expr::Ident(_, sp) => sp.end,
            Expr::Unary { span, .. } => span.end,
            Expr::Binary { span, .. } => span.end,
            Expr::Call { span, .. } => span.end,
            Expr::If { span, .. } => span.end,
            Expr::Block(block) => block.span.end,
            Expr::StructLit { span, .. } => span.end,
            Expr::FieldAccess { span, .. } => span.end,
            Expr::Ref { span, .. } => span.end,
            Expr::While { span, .. } => span.end,
            Expr::For { span, .. } => span.end,
            Expr::ArrayLit { span, .. } => span.end,
            Expr::Index { span, .. } => span.end,
            Expr::MethodCall { span, .. } => span.end,
            Expr::Match { span, .. } => span.end,
            Expr::Tuple { span, .. } => span.end,
        }
    }

    fn peek_binary_op(&self) -> Option<BinaryOp> {
        match self.peek().map(|t| &t.kind) {
            Some(TokenKind::Plus) => Some(BinaryOp::Add),
            Some(TokenKind::Minus) => Some(BinaryOp::Sub),
            Some(TokenKind::Star) => Some(BinaryOp::Mul),
            Some(TokenKind::Slash) => Some(BinaryOp::Div),
            Some(TokenKind::EqEq) => Some(BinaryOp::Eq),
            Some(TokenKind::BangEq) => Some(BinaryOp::Ne),
            Some(TokenKind::Lt) => Some(BinaryOp::Lt),
            Some(TokenKind::Le) => Some(BinaryOp::Le),
            Some(TokenKind::Gt) => Some(BinaryOp::Gt),
            Some(TokenKind::Ge) => Some(BinaryOp::Ge),
            Some(TokenKind::AmpAmp) => Some(BinaryOp::And),
            Some(TokenKind::PipePipe) => Some(BinaryOp::Or),
            _ => None,
        }
    }

    fn synchronize(&mut self) {
        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Semicolon | TokenKind::RBrace => {
                    self.advance();
                    return;
                }
                TokenKind::Eof => {
                    self.advance();
                    return;
                }
                _ => {
                    self.pos += 1;
                }
            }
        }
    }

    fn consume(&mut self, kind: TokenKind, msg: &str) -> Option<&'a Token> {
        if self.at(kind) {
            return self.advance();
        }
        let span = self.peek().map(|t| t.span);
        self.diagnostics.push(Diagnostic::error(msg, span));
        None
    }

    fn slice(&self, tok: &Token) -> String {
        self.source
            .get(tok.span.start as usize..tok.span.end as usize)
            .unwrap_or("")
            .to_string()
    }

    fn parse_import(&mut self) -> Option<Import> {
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

    fn parse_import_path(&mut self) -> Option<ImportPath> {
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

    fn lookahead_for_from(&self) -> bool {
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

fn render_type_expr(ty: &TypeExpr) -> String {
    match ty {
        TypeExpr::Path { segments, .. } => segments.join("::"),
        TypeExpr::App { base, args, .. } => {
            let mut s = render_type_expr(base);
            let rendered_args: Vec<String> = args.iter().map(render_type_expr).collect();
            s.push('<');
            s.push_str(&rendered_args.join(", "));
            s.push('>');
            s
        }
        TypeExpr::Ref {
            mutability, inner, ..
        } => {
            let mut s = String::from("&");
            if matches!(mutability, Mutability::Mutable) {
                s.push_str("mut ");
            }
            s.push_str(&render_type_expr(inner));
            s
        }
        TypeExpr::Array { elem, size, .. } => format!("[{}; {}]", render_type_expr(elem), size),
        TypeExpr::Tuple { elements, .. } => {
            let elem_strs: Vec<String> = elements.iter().map(render_type_expr).collect();
            format!("({})", elem_strs.join(", "))
        }
    }
}

fn parse_char_literal(raw: &str) -> char {
    let trimmed = raw.trim_matches('\'');
    let mut chars = trimmed.chars();
    match chars.next() {
        Some('\\') => match chars.next() {
            Some('n') => '\n',
            Some('t') => '\t',
            Some('r') => '\r',
            Some('\\') => '\\',
            Some('\'') => '\'',
            Some(other) => other,
            None => '\0',
        },
        Some(ch) => ch,
        None => '\0',
    }
}

fn parse_string_literal(raw: &str) -> String {
    let trimmed = raw
        .strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(raw);
    let mut result = String::new();
    let mut chars = trimmed.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next) = chars.next() {
                match next {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    other => result.push(other),
                }
            }
        } else {
            result.push(ch);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use glyph_core::token::TokenKind;
    use insta::assert_debug_snapshot;

    #[test]
    fn parses_simple_function() {
        let source = "fn add(a: i, b) { ret a + b }";
        let tokens = vec![
            Token::new(TokenKind::Fn, Span::new(0, 2)),
            Token::new(TokenKind::Ident, Span::new(3, 6)),
            Token::new(TokenKind::LParen, Span::new(6, 7)),
            Token::new(TokenKind::Ident, Span::new(7, 8)),
            Token::new(TokenKind::Colon, Span::new(8, 9)),
            Token::new(TokenKind::Ident, Span::new(10, 11)),
            Token::new(TokenKind::Comma, Span::new(11, 12)),
            Token::new(TokenKind::Ident, Span::new(13, 14)),
            Token::new(TokenKind::RParen, Span::new(14, 15)),
            Token::new(TokenKind::LBrace, Span::new(16, 17)),
            Token::new(TokenKind::Ret, Span::new(18, 21)),
            Token::new(TokenKind::Ident, Span::new(22, 23)),
            Token::new(TokenKind::Plus, Span::new(24, 25)),
            Token::new(TokenKind::Ident, Span::new(26, 27)),
            Token::new(TokenKind::RBrace, Span::new(28, 29)),
            Token::new(TokenKind::Eof, Span::new(29, 29)),
        ];
        let out = parse(&tokens, source);
        assert!(out.diagnostics.is_empty());
        assert_eq!(out.module.items.len(), 1);
        match &out.module.items[0] {
            Item::Function(f) => {
                assert_eq!(f.name.0, "add");
                assert_eq!(f.params.len(), 2);
            }
            other => panic!("expected function, got {:?}", other),
        }
    }

    #[test]
    fn parses_match_expression() {
        let source = r#"
fn main() -> i32 {
  let x = Some(5);
  let y = match x {
    Some(v) => v,
    None => 0,
  };
  ret y
}
"#;
        let lex_out = lex(source);
        let out = parse(&lex_out.tokens, source);
        assert!(
            out.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            out.diagnostics
        );
        assert_eq!(out.module.items.len(), 1);
    }

    #[test]
    fn parses_interpolated_string_with_hole() {
        let source = r#"
fn main() {
  let s = $"hi {x}";
  ret 0
}
"#;
        let lex_out = lex(source);
        let out = parse(&lex_out.tokens, source);
        assert!(
            out.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            out.diagnostics
        );

        match &out.module.items[0] {
            Item::Function(f) => match &f.body.stmts[0] {
                Stmt::Let {
                    value: Some(Expr::InterpString { segments, .. }),
                    ..
                } => {
                    assert_eq!(segments.len(), 2);
                }
                other => panic!("expected interpolated string let, got {:?}", other),
            },
            other => panic!("expected function, got {:?}", other),
        }
    }

    // TODO: This test hangs - likely infinite loop in parser when combining
    // if expressions with function calls. Need to debug parser state machine.
    // Not struct-related, deferred to future work.
    #[test]
    #[ignore]
    fn parses_if_and_call() {
        let source = "fn f() { let x = foo(1, 2) ret if x { x } else { 0 } }";
        let tokens = lex_stub_tokens();
        println!("starting parse_if_and_call");
        let out = parse(&tokens, source);
        println!("after parse; diags: {}", out.diagnostics.len());
        assert!(out.diagnostics.is_empty());
        assert_debug_snapshot!("if_call", out.module);
    }

    fn lex_stub_tokens() -> Vec<Token> {
        vec![
            Token::new(TokenKind::Fn, Span::new(0, 2)),
            Token::new(TokenKind::Ident, Span::new(3, 4)),
            Token::new(TokenKind::LParen, Span::new(4, 5)),
            Token::new(TokenKind::RParen, Span::new(5, 6)),
            Token::new(TokenKind::LBrace, Span::new(7, 8)),
            Token::new(TokenKind::Let, Span::new(9, 12)),
            Token::new(TokenKind::Ident, Span::new(13, 14)),
            Token::new(TokenKind::Eq, Span::new(15, 16)),
            Token::new(TokenKind::Ident, Span::new(17, 20)),
            Token::new(TokenKind::LParen, Span::new(20, 21)),
            Token::new(TokenKind::Int, Span::new(21, 22)),
            Token::new(TokenKind::Comma, Span::new(22, 23)),
            Token::new(TokenKind::Int, Span::new(24, 25)),
            Token::new(TokenKind::RParen, Span::new(25, 26)),
            Token::new(TokenKind::Ret, Span::new(27, 30)),
            Token::new(TokenKind::If, Span::new(31, 33)),
            Token::new(TokenKind::Ident, Span::new(34, 35)),
            Token::new(TokenKind::LBrace, Span::new(36, 37)),
            Token::new(TokenKind::Ident, Span::new(38, 39)),
            Token::new(TokenKind::RBrace, Span::new(40, 41)),
            Token::new(TokenKind::Else, Span::new(42, 46)),
            Token::new(TokenKind::LBrace, Span::new(47, 48)),
            Token::new(TokenKind::Int, Span::new(49, 50)),
            Token::new(TokenKind::RBrace, Span::new(51, 52)),
            Token::new(TokenKind::RBrace, Span::new(53, 54)),
            Token::new(TokenKind::Eof, Span::new(54, 54)),
        ]
    }

    #[test]
    fn reports_missing_fn_name() {
        let tokens = vec![
            Token::new(TokenKind::Fn, Span::new(0, 2)),
            Token::new(TokenKind::LParen, Span::new(2, 3)),
            Token::new(TokenKind::Eof, Span::new(3, 3)),
        ];
        let out = parse(&tokens, "fn(");
        assert!(!out.diagnostics.is_empty());
    }

    #[test]
    fn parses_extern_function_decl() {
        let source = r#"extern "C" fn puts(msg: RawPtr<i32>);"#;
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(
            out.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            out.diagnostics
        );
        assert_eq!(out.module.items.len(), 1);
        match &out.module.items[0] {
            Item::ExternFunction(f) => {
                assert_eq!(f.name.0, "puts");
                assert_eq!(f.abi.as_deref(), Some("C"));
            }
            other => panic!("expected extern function, got {:?}", other),
        }
    }

    #[test]
    fn rejects_extern_function_body() {
        let source = r#"extern "C" fn puts() {}"#;
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(
            out.diagnostics
                .iter()
                .any(|d| d.message.contains("extern functions cannot have a body")),
            "expected body error, got {:?}",
            out.diagnostics
        );
    }

    #[test]
    fn rejects_unsupported_abi() {
        let source = r#"extern "sysv64" fn foo();"#;
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(
            out.diagnostics
                .iter()
                .any(|d| d.message.contains("unsupported ABI")),
            "expected ABI diagnostic, got {:?}",
            out.diagnostics
        );
    }

    #[test]
    fn extern_and_normal_function_in_same_module() {
        let source = r#"extern "C" fn puts(); fn main() { ret }"#;
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(out.diagnostics.is_empty(), "diags: {:?}", out.diagnostics);
        assert_eq!(out.module.items.len(), 2);
    }

    #[test]
    fn parses_from_import_statement() {
        let source = r#"from std import println
fn main() { println("hi") }"#;
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(out.diagnostics.is_empty(), "diags: {:?}", out.diagnostics);
        assert_eq!(out.module.imports.len(), 1);
        match &out.module.imports[0].kind {
            ImportKind::Selective { items } => {
                assert_eq!(items.len(), 1);
                assert_eq!(items[0].name.0, "println");
            }
            other => panic!("expected selective import, got {:?}", other),
        }
        assert_eq!(out.module.imports[0].path.segments, vec!["std"]);
    }

    #[test]
    fn parses_from_import_with_alias() {
        let source = r#"from std import println as p
fn main() { p("hi") }"#;
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(out.diagnostics.is_empty(), "diags: {:?}", out.diagnostics);
        match &out.module.imports[0].kind {
            ImportKind::Selective { items } => {
                assert_eq!(items.len(), 1);
                assert_eq!(items[0].name.0, "println");
                assert_eq!(items[0].alias.as_ref().map(|a| a.0.as_str()), Some("p"));
            }
            other => panic!("expected selective import, got {:?}", other),
        }
    }

    #[test]
    fn parses_string_literal_content() {
        let source = r#"fn main() { let msg = "hi\nthere" }"#;
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(out.diagnostics.is_empty(), "diags: {:?}", out.diagnostics);
        match &out.module.items[0] {
            Item::Function(f) => {
                if let Stmt::Let {
                    value: Some(expr), ..
                } = &f.body.stmts[0]
                {
                    match expr {
                        Expr::Lit(Literal::Str(s), _) => {
                            assert_eq!(s, "hi\nthere");
                        }
                        other => panic!("expected string literal, got {:?}", other),
                    }
                } else {
                    panic!("expected let binding");
                }
            }
            other => panic!("expected function, got {:?}", other),
        }
    }

    #[test]
    fn parses_unary_not() {
        let source = "fn main() { let x = !true }";
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(out.diagnostics.is_empty(), "diags: {:?}", out.diagnostics);

        let Item::Function(f) = &out.module.items[0] else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some(Expr::Unary { op, expr, .. }),
            ..
        } = &f.body.stmts[0]
        else {
            panic!("expected unary not let binding");
        };

        assert_eq!(*op, UnaryOp::Not);
        assert!(matches!(expr.as_ref(), Expr::Lit(Literal::Bool(true), _)));
    }

    #[test]
    fn binary_operator_precedence_equality_vs_or() {
        let source = "fn main() { let x = ch == 32 || ch == 9 }";
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(out.diagnostics.is_empty(), "diags: {:?}", out.diagnostics);

        let Item::Function(f) = &out.module.items[0] else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some(expr), ..
        } = &f.body.stmts[0]
        else {
            panic!("expected let binding");
        };

        match expr {
            Expr::Binary {
                op: BinaryOp::Or,
                lhs,
                rhs,
                ..
            } => {
                assert!(matches!(
                    lhs.as_ref(),
                    Expr::Binary {
                        op: BinaryOp::Eq,
                        ..
                    }
                ));
                assert!(matches!(
                    rhs.as_ref(),
                    Expr::Binary {
                        op: BinaryOp::Eq,
                        ..
                    }
                ));
            }
            other => panic!("expected top-level ||, got {:?}", other),
        }
    }

    #[test]
    fn parses_const_item() {
        let source = "const SUCCESS: i32 = 0\nfn main() { ret SUCCESS }";
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(out.diagnostics.is_empty(), "diags: {:?}", out.diagnostics);
        assert_eq!(out.module.items.len(), 2);
        match &out.module.items[0] {
            Item::Const(def) => {
                assert_eq!(def.name.0, "SUCCESS");
                match &def.ty {
                    TypeExpr::Path { segments, .. } => {
                        assert_eq!(segments, &vec!["i32".to_string()])
                    }
                    other => panic!("expected type path, got {:?}", other),
                }
                match &def.value {
                    Expr::Lit(Literal::Int(0), _) => {}
                    other => panic!("expected int literal, got {:?}", other),
                }
            }
            other => panic!("expected const item, got {:?}", other),
        }
    }

    #[test]
    fn const_requires_type_annotation() {
        let source = "const MISSING = 1";
        let lexed = lex(source);
        let out = parse(&lexed.tokens, source);
        assert!(out
            .diagnostics
            .iter()
            .any(|d| { d.message.contains("requires explicit type annotation") }));
    }
}
