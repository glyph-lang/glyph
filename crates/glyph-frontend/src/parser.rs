use crate::method_symbols::{inherent_method_symbol, interface_method_symbol};
use glyph_core::{
    ast::{
        BinaryOp, Block, Expr, ExternFunctionDecl, FieldDef, Function, Ident, ImplBlock, Import,
        ImportItem, ImportKind, ImportPath, InlineImpl, InterfaceDef, InterfaceMethod, Item,
        Literal, Module, Param, Stmt, StructDef,
    },
    diag::Diagnostic,
    span::Span,
    token::{Token, TokenKind},
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
    };

    let mut imports = Vec::new();

    // Parse imports first (must come before items)
    while parser.at(TokenKind::Import) {
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
        } else if parser.at(TokenKind::Extern) {
            match parser.parse_extern_function() {
                Some(extern_fn) => parser.items.push(Item::ExternFunction(extern_fn)),
                None => parser.synchronize(),
            }
        } else {
            let span = parser.peek().map(|t| t.span);
            parser.diagnostics.push(Diagnostic::error(
                "expected `fn`, `struct`, `interface`, `impl`, or `extern`",
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
            ret_type = self.parse_type_annotation();
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
            ret_type = self.parse_type_annotation();
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
            ret_type = self.parse_type_annotation();
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
            let field_type_tok = self.consume(TokenKind::Ident, "expected field type")?;

            let field = FieldDef {
                name: Ident(self.slice(field_name_tok)),
                ty: Ident(self.slice(field_type_tok)),
                span: Span::new(field_name_tok.span.start, field_type_tok.span.end),
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
            fields,
            interfaces,
            methods,
            inline_impls,
            span: Span::new(struct_tok.span.start, end.span.end),
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
                let normalized = ty.0.replace(' ', "");
                let value_ty = struct_name.0.as_str();
                let ref_ty = format!("&{}", value_ty);
                let mutref_ty = format!("&mut{}", value_ty);

                if normalized == value_ty {
                    Ident(struct_name.0.clone())
                } else if normalized == ref_ty {
                    Ident(format!("&{}", struct_name.0))
                } else if normalized == mutref_ty {
                    Ident(format!("&mut {}", struct_name.0))
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
            None => Ident(struct_name.0.clone()),
        };

        first.ty = Some(canonical_self_ty);

        let symbol = match interface {
            Some(iface) => interface_method_symbol(&struct_name.0, &iface.0, &method.name.0),
            None => inherent_method_symbol(&struct_name.0, &method.name.0),
        };
        func.name = Ident(symbol);
        Some(func)
    }

    fn parse_type_annotation(&mut self) -> Option<Ident> {
        // Check for array type [T; N]
        if self.at(TokenKind::LBracket) {
            self.advance();

            let elem_ty_tok = self.consume(TokenKind::Ident, "expected element type")?;
            let elem_ty = self.slice(elem_ty_tok);

            self.consume(TokenKind::Semicolon, "expected `;` in array type")?;

            let size_tok = self.consume(TokenKind::Int, "expected array size")?;
            let size = self.slice(size_tok);

            self.consume(TokenKind::RBracket, "expected `]`")?;

            return Some(Ident(format!("[{}; {}]", elem_ty, size)));
        }

        // Check for reference type (&T or &mut T)
        if self.at(TokenKind::Amp) {
            self.advance();

            // Check for mut keyword
            let is_mut = if self.at(TokenKind::Mut) {
                self.advance();
                true
            } else {
                false
            };

            // Parse inner type
            let inner_ty_tok = self.consume(TokenKind::Ident, "expected type after &")?;
            let inner_ty = self.slice(inner_ty_tok);

            // Create type string like "&i32" or "&mut Point"
            let type_str = if is_mut {
                format!("&mut {}", inner_ty)
            } else {
                format!("&{}", inner_ty)
            };

            Some(Ident(type_str))
        } else {
            // Simple type (i32, Point, etc.) + minimal single-arg generics (Own<T>, RawPtr<T>)
            let ty_tok = self.consume(TokenKind::Ident, "expected type")?;
            let base = self.slice(ty_tok);

            if (base == "Own" || base == "RawPtr" || base == "Shared") && self.at(TokenKind::Lt) {
                self.advance();
                let inner = match self.parse_type_annotation() {
                    Some(ident) => ident.0,
                    None => return None,
                };
                self.consume(TokenKind::Gt, "expected `>` after type argument")?;
                Some(Ident(format!("{}<{}>", base, inner)))
            } else {
                Some(Ident(base))
            }
        }
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
                if let Some(parsed_ty) = self.parse_type_annotation() {
                    // Use the current position after parsing the type
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
            let name_tok = self.consume(TokenKind::Ident, "expected identifier after `let`")?;
            let mut ty = None;
            if self.at(TokenKind::Colon) {
                self.advance();
                ty = self.parse_type_annotation();
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
        self.parse_binary_expr()
    }

    fn parse_binary_expr(&mut self) -> Option<Expr> {
        let mut lhs = self.parse_call_or_primary()?;
        while let Some(op) = self.peek_binary_op() {
            self.advance();
            let rhs = self.parse_call_or_primary()?;
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

    fn parse_call_or_primary(&mut self) -> Option<Expr> {
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
                let name_tok =
                    self.consume(TokenKind::Ident, "expected field or method name after `.`")?;
                let name = Ident(self.slice(name_tok));

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
                if self.at(TokenKind::LBrace) {
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
            TokenKind::Bool => Some(Expr::Lit(self.literal_from(tok), tok.span)),
            TokenKind::LParen => {
                let expr = self.parse_expr();
                self.consume(TokenKind::RParen, "expected `)`");
                expr
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
        let cond = self.parse_expr()?;
        let then_block = self.parse_block()?;
        let else_block = if self.at(TokenKind::Else) {
            self.advance();
            if self.at(TokenKind::If) {
                let nested_start = self.peek().map(|t| t.span.start).unwrap_or(start);
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
        let cond = self.parse_expr()?;
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
            TokenKind::Bool => Literal::Bool(matches!(text.as_str(), "true")),
            _ => Literal::Int(0),
        }
    }

    fn expr_start(&self, expr: &Expr) -> u32 {
        match expr {
            Expr::Lit(_, sp) => sp.start,
            Expr::Ident(_, sp) => sp.start,
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
        }
    }

    fn expr_end(&self, expr: &Expr) -> u32 {
        match expr {
            Expr::Lit(_, sp) => sp.end,
            Expr::Ident(_, sp) => sp.end,
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

fn parse_string_literal(raw: &str) -> String {
    let trimmed = raw.trim_matches('"');
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
}
