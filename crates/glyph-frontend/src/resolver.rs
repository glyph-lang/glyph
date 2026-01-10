use std::collections::{HashMap, HashSet};

use crate::method_symbols::{inherent_method_symbol, interface_method_symbol};
use glyph_core::{
    ast::{Ident, Item, Module},
    diag::Diagnostic,
    types::{StructType, Type},
};

/// Context containing resolved type information
#[derive(Debug, Clone, Default)]
pub struct ResolverContext {
    pub struct_types: HashMap<String, StructType>,
    pub inherent_methods: HashMap<String, HashMap<String, MethodInfo>>,
    pub interfaces: HashMap<String, InterfaceType>,
    pub interface_impls: HashMap<String, HashMap<String, HashMap<String, MethodInfo>>>,
    // struct_name -> interface_name -> method_name -> MethodInfo
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SelfKind {
    ByValue, // self: Point
    Ref,     // self: &Point
    MutRef,  // self: &mut Point
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub function_name: String,
    pub self_kind: SelfKind,
}

#[derive(Debug, Clone)]
pub struct InterfaceType {
    pub name: String,
    pub methods: Vec<InterfaceMethodSig>,
}

#[derive(Debug, Clone)]
pub struct InterfaceMethodSig {
    pub name: String,
    pub params: Vec<Option<Type>>,
    pub raw_param_types: Vec<Option<String>>,
    pub ret_type: Option<Type>,
    pub raw_ret_type: Option<String>,
}

/// Resolves types in a module, building a registry of struct types
/// Returns the resolver context and any diagnostics encountered
pub fn resolve_types(module: &Module) -> (ResolverContext, Vec<Diagnostic>) {
    let mut ctx = ResolverContext {
        struct_types: HashMap::new(),
        inherent_methods: HashMap::new(),
        interfaces: HashMap::new(),
        interface_impls: HashMap::new(),
    };
    let mut diagnostics = Vec::new();

    // First pass: collect all interface definitions
    for item in &module.items {
        if let Item::Interface(iface) = item {
            let iface_name = &iface.name.0;

            // Check for duplicate interface definitions
            if ctx.interfaces.contains_key(iface_name) {
                diagnostics.push(Diagnostic::error(
                    format!("interface '{}' is defined multiple times", iface_name),
                    Some(iface.span),
                ));
                continue;
            }

            let mut methods = Vec::new();
            for method in &iface.methods {
                // Resolve parameter types
                let mut param_types = Vec::new();
                let mut raw_param_types = Vec::new();
                for param in &method.params {
                    let param_ty = match &param.ty {
                        Some(ty_ident) => {
                            raw_param_types.push(Some(ty_ident.0.clone()));
                            if let Some(builtin) = Type::from_name(&ty_ident.0) {
                                Some(builtin)
                            } else if ty_ident.0.chars().all(|c| c.is_alphanumeric() || c == '_') {
                                Some(Type::Named(ty_ident.0.clone()))
                            } else {
                                None
                            }
                        }
                        None => {
                            raw_param_types.push(None);
                            None
                        }
                    };
                    param_types.push(param_ty);
                }

                // Resolve return type
                let (ret_type, raw_ret_type) = match &method.ret_type {
                    Some(ty) => {
                        if let Some(builtin) = Type::from_name(&ty.0) {
                            (Some(builtin), Some(ty.0.clone()))
                        } else if ty.0.chars().all(|c| c.is_alphanumeric() || c == '_') {
                            (Some(Type::Named(ty.0.clone())), Some(ty.0.clone()))
                        } else {
                            (None, Some(ty.0.clone()))
                        }
                    }
                    None => (None, None),
                };

                methods.push(InterfaceMethodSig {
                    name: method.name.0.clone(),
                    params: param_types,
                    raw_param_types,
                    ret_type,
                    raw_ret_type,
                });
            }

            ctx.interfaces.insert(
                iface_name.clone(),
                InterfaceType {
                    name: iface_name.clone(),
                    methods,
                },
            );
        }
    }

    // Second pass: collect all struct definitions
    for item in &module.items {
        if let Item::Struct(s) = item {
            let struct_name = s.name.0.clone();

            // Check for duplicate struct definitions
            if ctx.struct_types.contains_key(&struct_name) {
                diagnostics.push(Diagnostic::error(
                    format!("struct '{}' is defined multiple times", struct_name),
                    Some(s.span),
                ));
                continue;
            }

            let mut fields = Vec::new();
            let mut field_names = std::collections::HashSet::new();

            for field in &s.fields {
                let field_name = field.name.0.clone();
                let field_ty_name = field.ty.0.as_str();

                // Check for duplicate field names
                if !field_names.insert(field_name.clone()) {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "field '{}' is defined multiple times in struct '{}'",
                            field_name, struct_name
                        ),
                        Some(field.span),
                    ));
                    continue;
                }

                // Resolve field type
                let field_type = if let Some(ty) = Type::from_name(field_ty_name) {
                    ty
                } else {
                    // Assume it's a named type (struct) - will validate in second pass
                    Type::Named(field_ty_name.to_string())
                };

                fields.push((field_name, field_type));
            }

            ctx.struct_types.insert(
                struct_name.clone(),
                StructType {
                    name: struct_name,
                    fields,
                },
            );
        }
    }

    // Second pass: validate that all Named types reference actual structs
    let struct_names: std::collections::HashSet<_> = ctx.struct_types.keys().cloned().collect();

    for item in &module.items {
        if let Item::Struct(s) = item {
            let struct_name = &s.name.0;

            if let Some(struct_type) = ctx.struct_types.get(struct_name) {
                for (field_name, field_type) in &struct_type.fields {
                    if let Type::Named(type_name) = field_type {
                        if !struct_names.contains(type_name) {
                            diagnostics.push(Diagnostic::error(
                                format!(
                                    "undefined type '{}' used in field '{}' of struct '{}'",
                                    type_name, field_name, struct_name
                                ),
                                Some(s.span),
                            ));
                        }
                    }
                }
            }
        }
    }

    // Third pass: collect inherent methods from structs
    for item in &module.items {
        if let Item::Struct(s) = item {
            let struct_name = s.name.0.clone();

            // Collect inherent methods
            for method in &s.methods {
                // Generate mangled name using existing infrastructure
                let mangled_name = inherent_method_symbol(&struct_name, &method.name.0);

                // Detect self kind from first parameter
                let self_kind = if let Some(first_param) = method.params.first() {
                    detect_self_kind(&first_param.ty)
                } else {
                    // No parameters - should have been caught by parser, but handle it
                    SelfKind::ByValue
                };

                let method_info = MethodInfo {
                    function_name: mangled_name,
                    self_kind,
                };

                ctx.inherent_methods
                    .entry(struct_name.clone())
                    .or_insert_with(HashMap::new)
                    .insert(method.name.0.clone(), method_info);
            }
        }
    }

    validate_interface_signatures(&ctx, &struct_names, &mut diagnostics);
    collect_interface_impls(module, &mut ctx, &mut diagnostics);

    (ctx, diagnostics)
}

impl ResolverContext {
    /// Look up a struct type by name
    pub fn get_struct(&self, name: &str) -> Option<&StructType> {
        self.struct_types.get(name)
    }

    /// Get the type and index of a field in a struct
    pub fn get_field(&self, struct_name: &str, field_name: &str) -> Option<(Type, usize)> {
        let struct_type = self.get_struct(struct_name)?;

        for (i, (name, ty)) in struct_type.fields.iter().enumerate() {
            if name == field_name {
                return Some((ty.clone(), i));
            }
        }

        None
    }

    /// Look up inherent method by struct type and method name
    /// Returns mangled function name and method info
    pub fn get_inherent_method(&self, struct_name: &str, method_name: &str) -> Option<&MethodInfo> {
        self.inherent_methods.get(struct_name)?.get(method_name)
    }
}

/// Detect the self kind from a parameter type
fn detect_self_kind(param_ty: &Option<glyph_core::ast::Ident>) -> SelfKind {
    match param_ty {
        Some(ident) => {
            let ty_str = ident.0.replace(' ', "");
            if ty_str.starts_with("&mut") {
                SelfKind::MutRef
            } else if ty_str.starts_with('&') {
                SelfKind::Ref
            } else {
                SelfKind::ByValue
            }
        }
        None => SelfKind::ByValue,
    }
}

#[derive(Default)]
struct InterfaceImplState {
    methods: HashMap<String, MethodInfo>,
    has_error: bool,
}

fn normalize_type_name(raw: &str) -> String {
    raw.split_whitespace().collect()
}

fn self_param_matches_struct(param: &glyph_core::ast::Param, struct_name: &str) -> bool {
    match &param.ty {
        Some(ty) => {
            let norm = normalize_type_name(&ty.0);
            norm == struct_name
                || norm == format!("&{}", struct_name)
                || norm == format!("&mut{}", struct_name)
        }
        None => true,
    }
}

fn validate_interface_signatures(
    ctx: &ResolverContext,
    struct_names: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for iface in ctx.interfaces.values() {
        for method in &iface.methods {
            for (idx, param_ty) in method.params.iter().enumerate() {
                if let Some(Type::Named(name)) = param_ty {
                    if !struct_names.contains(name) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "interface '{}' references unknown type '{}' in parameter {}",
                                iface.name,
                                name,
                                idx + 1
                            ),
                            None,
                        ));
                    }
                }
            }

            if let Some(Type::Named(name)) = &method.ret_type {
                if !struct_names.contains(name) {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "interface '{}' references unknown return type '{}'",
                            iface.name, name
                        ),
                        None,
                    ));
                }
            }
        }
    }
}

fn record_interface_impl(
    struct_name: &Ident,
    interface: &Ident,
    methods: &[glyph_core::ast::Function],
    span: glyph_core::span::Span,
    ctx: &ResolverContext,
    impls: &mut HashMap<String, HashMap<String, InterfaceImplState>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let iface_name = &interface.0;
    let Some(iface_def) = ctx.interfaces.get(iface_name) else {
        diagnostics.push(Diagnostic::error(
            format!("impl refers to unknown interface '{}'", iface_name),
            Some(span),
        ));
        return;
    };

    let struct_entry = impls
        .entry(struct_name.0.clone())
        .or_insert_with(HashMap::new);
    let state = struct_entry
        .entry(iface_name.clone())
        .or_insert_with(InterfaceImplState::default);

    for method in methods {
        if state.methods.contains_key(&method.name.0) {
            diagnostics.push(Diagnostic::error(
                format!(
                    "method '{}' implemented multiple times for interface '{}' on struct '{}'",
                    method.name.0, iface_name, struct_name.0
                ),
                Some(method.span),
            ));
            state.has_error = true;
            continue;
        }

        if method.params.is_empty() || method.params[0].name.0 != "self" {
            diagnostics.push(Diagnostic::error(
                "interface methods must take `self` as the first parameter",
                Some(method.span),
            ));
            state.has_error = true;
            continue;
        }

        if !self_param_matches_struct(&method.params[0], &struct_name.0) {
            diagnostics.push(Diagnostic::error(
                format!(
                    "self parameter type must reference struct '{}' (by value, '&', or '&mut')",
                    struct_name.0
                ),
                Some(method.params[0].span),
            ));
            state.has_error = true;
            continue;
        }

        let Some(iface_sig) = iface_def.methods.iter().find(|m| m.name == method.name.0) else {
            diagnostics.push(Diagnostic::error(
                format!(
                    "method '{}' is not declared in interface '{}'",
                    method.name.0, iface_name
                ),
                Some(method.span),
            ));
            state.has_error = true;
            continue;
        };

        if iface_sig.params.len() != method.params.len() {
            diagnostics.push(Diagnostic::error(
                format!(
                    "method '{}' has {} parameters but interface '{}' expects {}",
                    method.name.0,
                    method.params.len(),
                    iface_name,
                    iface_sig.params.len()
                ),
                Some(method.span),
            ));
            state.has_error = true;
            continue;
        }

        for (idx, expected_raw) in iface_sig.raw_param_types.iter().enumerate() {
            if let Some(expected) = expected_raw {
                let actual = method
                    .params
                    .get(idx)
                    .and_then(|p| p.ty.as_ref())
                    .map(|t| normalize_type_name(&t.0));
                if actual.is_none() {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "parameter {} of method '{}' must have type '{}' per interface '{}'",
                            idx + 1,
                            method.name.0,
                            expected,
                            iface_name
                        ),
                        Some(method.params[idx].span),
                    ));
                    state.has_error = true;
                } else if actual.as_deref() != Some(&normalize_type_name(expected)) {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "parameter {} of method '{}' has type '{}' but interface '{}' expects '{}'",
                            idx + 1,
                            method.name.0,
                            method.params[idx]
                                .ty
                                .as_ref()
                                .map(|t| t.0.as_str())
                                .unwrap_or("<unknown>"),
                            iface_name,
                            expected
                        ),
                        Some(method.params[idx].span),
                    ));
                    state.has_error = true;
                }
            }
        }

        if let Some(expected_ret) = iface_sig.raw_ret_type.as_ref() {
            match method.ret_type.as_ref() {
                Some(actual_ret) => {
                    if normalize_type_name(&actual_ret.0) != normalize_type_name(expected_ret) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "method '{}' returns '{}' but interface '{}' requires '{}'",
                                method.name.0, actual_ret.0, iface_name, expected_ret
                            ),
                            Some(method.span),
                        ));
                        state.has_error = true;
                    }
                }
                None => {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "method '{}' must return '{}' to satisfy interface '{}'",
                            method.name.0, expected_ret, iface_name
                        ),
                        Some(method.span),
                    ));
                    state.has_error = true;
                }
            }
        }

        let self_kind = detect_self_kind(&method.params[0].ty);
        let mangled_name = interface_method_symbol(&struct_name.0, iface_name, &method.name.0);

        state.methods.insert(
            method.name.0.clone(),
            MethodInfo {
                function_name: mangled_name,
                self_kind,
            },
        );
    }
}

fn collect_interface_impls(
    module: &Module,
    ctx: &mut ResolverContext,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut impls: HashMap<String, HashMap<String, InterfaceImplState>> = HashMap::new();

    for item in &module.items {
        match item {
            Item::Struct(s) => {
                for iface in &s.interfaces {
                    if !ctx.interfaces.contains_key(&iface.0) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "unknown interface '{}' declared on struct '{}'",
                                iface.0, s.name.0
                            ),
                            Some(s.span),
                        ));
                        continue;
                    }

                    impls
                        .entry(s.name.0.clone())
                        .or_insert_with(HashMap::new)
                        .entry(iface.0.clone())
                        .or_insert_with(InterfaceImplState::default);
                }

                for inline in &s.inline_impls {
                    record_interface_impl(
                        &s.name,
                        &inline.interface,
                        &inline.methods,
                        inline.span,
                        ctx,
                        &mut impls,
                        diagnostics,
                    );
                }
            }
            Item::Impl(block) => {
                if !ctx.struct_types.contains_key(&block.target.0) {
                    diagnostics.push(Diagnostic::error(
                        format!("impl target '{}' is not a known struct", block.target.0),
                        Some(block.span),
                    ));
                    continue;
                }

                record_interface_impl(
                    &block.target,
                    &block.interface,
                    &block.methods,
                    block.span,
                    ctx,
                    &mut impls,
                    diagnostics,
                );
            }
            _ => {}
        }
    }

    for (struct_name, iface_map) in impls.into_iter() {
        for (iface_name, state) in iface_map {
            let Some(iface_def) = ctx.interfaces.get(&iface_name) else {
                continue;
            };

            let mut missing = Vec::new();
            for sig in &iface_def.methods {
                if !state.methods.contains_key(&sig.name) {
                    missing.push(sig.name.clone());
                }
            }

            if !missing.is_empty() {
                diagnostics.push(Diagnostic::error(
                    format!(
                        "struct '{}' is missing implementations for interface '{}' methods: {}",
                        struct_name,
                        iface_name,
                        missing.join(", ")
                    ),
                    None,
                ));
                continue;
            }

            if !state.has_error {
                ctx.interface_impls
                    .entry(struct_name.clone())
                    .or_insert_with(HashMap::new)
                    .insert(iface_name.clone(), state.methods);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use glyph_core::{
        ast::{
            Block, Expr, FieldDef, Function, Ident, InlineImpl, InterfaceDef, InterfaceMethod,
            Literal, Param, Stmt, StructDef,
        },
        span::Span,
    };

    fn make_span() -> Span {
        Span::new(0, 10)
    }

    #[test]
    fn resolves_empty_module() {
        let module = Module { items: vec![] };
        let (ctx, diags) = resolve_types(&module);

        assert!(ctx.struct_types.is_empty());
        assert!(diags.is_empty());
    }

    #[test]
    fn resolves_simple_struct() {
        let module = Module {
            items: vec![Item::Struct(StructDef {
                name: Ident("Point".into()),
                fields: vec![
                    FieldDef {
                        name: Ident("x".into()),
                        ty: Ident("i32".into()),
                        span: make_span(),
                    },
                    FieldDef {
                        name: Ident("y".into()),
                        ty: Ident("i32".into()),
                        span: make_span(),
                    },
                ],
                interfaces: Vec::new(),
                methods: Vec::new(),
                inline_impls: Vec::new(),
                span: make_span(),
            })],
        };

        let (ctx, diags) = resolve_types(&module);

        assert!(diags.is_empty());
        assert_eq!(ctx.struct_types.len(), 1);

        let point = ctx.get_struct("Point").unwrap();
        assert_eq!(point.name, "Point");
        assert_eq!(point.fields.len(), 2);
        assert_eq!(point.fields[0], ("x".to_string(), Type::I32));
        assert_eq!(point.fields[1], ("y".to_string(), Type::I32));
    }

    #[test]
    fn detects_duplicate_struct_definitions() {
        let module = Module {
            items: vec![
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    fields: vec![],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span: make_span(),
                }),
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    fields: vec![],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span: make_span(),
                }),
            ],
        };

        let (ctx, diags) = resolve_types(&module);

        assert_eq!(ctx.struct_types.len(), 1); // Only first one registered
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("defined multiple times"));
    }

    #[test]
    fn detects_duplicate_field_names() {
        let module = Module {
            items: vec![Item::Struct(StructDef {
                name: Ident("Point".into()),
                fields: vec![
                    FieldDef {
                        name: Ident("x".into()),
                        ty: Ident("i32".into()),
                        span: make_span(),
                    },
                    FieldDef {
                        name: Ident("x".into()),
                        ty: Ident("i32".into()),
                        span: make_span(),
                    },
                ],
                interfaces: Vec::new(),
                methods: Vec::new(),
                inline_impls: Vec::new(),
                span: make_span(),
            })],
        };

        let (ctx, diags) = resolve_types(&module);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("defined multiple times"));

        // First field should be registered
        let point = ctx.get_struct("Point").unwrap();
        assert_eq!(point.fields.len(), 1);
    }

    #[test]
    fn detects_undefined_struct_types() {
        let module = Module {
            items: vec![Item::Struct(StructDef {
                name: Ident("Rect".into()),
                fields: vec![FieldDef {
                    name: Ident("top_left".into()),
                    ty: Ident("Point".into()),
                    span: make_span(),
                }],
                interfaces: Vec::new(),
                methods: Vec::new(),
                inline_impls: Vec::new(),
                span: make_span(),
            })],
        };

        let (ctx, diags) = resolve_types(&module);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("undefined type 'Point'"));

        // Struct should still be registered with Named type
        let rect = ctx.get_struct("Rect").unwrap();
        assert_eq!(rect.fields[0].1, Type::Named("Point".to_string()));
    }

    #[test]
    fn allows_struct_with_struct_field() {
        let module = Module {
            items: vec![
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    fields: vec![FieldDef {
                        name: Ident("x".into()),
                        ty: Ident("i32".into()),
                        span: make_span(),
                    }],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span: make_span(),
                }),
                Item::Struct(StructDef {
                    name: Ident("Rect".into()),
                    fields: vec![FieldDef {
                        name: Ident("top_left".into()),
                        ty: Ident("Point".into()),
                        span: make_span(),
                    }],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span: make_span(),
                }),
            ],
        };

        let (ctx, diags) = resolve_types(&module);

        assert!(diags.is_empty());
        assert_eq!(ctx.struct_types.len(), 2);

        let rect = ctx.get_struct("Rect").unwrap();
        assert_eq!(rect.fields[0].1, Type::Named("Point".to_string()));
    }

    #[test]
    fn get_field_returns_correct_type_and_index() {
        let module = Module {
            items: vec![Item::Struct(StructDef {
                name: Ident("Point".into()),
                fields: vec![
                    FieldDef {
                        name: Ident("x".into()),
                        ty: Ident("i32".into()),
                        span: make_span(),
                    },
                    FieldDef {
                        name: Ident("y".into()),
                        ty: Ident("i64".into()),
                        span: make_span(),
                    },
                ],
                interfaces: Vec::new(),
                methods: Vec::new(),
                inline_impls: Vec::new(),
                span: make_span(),
            })],
        };

        let (ctx, _) = resolve_types(&module);

        let (ty, idx) = ctx.get_field("Point", "x").unwrap();
        assert_eq!(ty, Type::I32);
        assert_eq!(idx, 0);

        let (ty, idx) = ctx.get_field("Point", "y").unwrap();
        assert_eq!(ty, Type::I64);
        assert_eq!(idx, 1);

        assert!(ctx.get_field("Point", "z").is_none());
        assert!(ctx.get_field("NotAStruct", "x").is_none());
    }

    #[test]
    fn collects_interface_impl_methods() {
        let span = make_span();
        let module = Module {
            items: vec![
                Item::Interface(InterfaceDef {
                    name: Ident("Drawable".into()),
                    methods: vec![InterfaceMethod {
                        name: Ident("draw".into()),
                        params: vec![Param {
                            name: Ident("self".into()),
                            ty: Some(Ident("&Point".into())),
                            span,
                        }],
                        ret_type: Some(Ident("i32".into())),
                        span,
                    }],
                    span,
                }),
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    fields: vec![FieldDef {
                        name: Ident("x".into()),
                        ty: Ident("i32".into()),
                        span,
                    }],
                    interfaces: vec![Ident("Drawable".into())],
                    methods: Vec::new(),
                    inline_impls: vec![InlineImpl {
                        interface: Ident("Drawable".into()),
                        methods: vec![Function {
                            name: Ident("draw".into()),
                            params: vec![Param {
                                name: Ident("self".into()),
                                ty: Some(Ident("&Point".into())),
                                span,
                            }],
                            ret_type: Some(Ident("i32".into())),
                            body: Block {
                                span,
                                stmts: vec![Stmt::Ret(
                                    Some(Expr::Lit(Literal::Int(1), span)),
                                    span,
                                )],
                            },
                            span,
                        }],
                        span,
                    }],
                    span,
                }),
            ],
        };

        let (ctx, diags) = resolve_types(&module);
        assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

        let iface_map = ctx.interface_impls.get("Point").expect("impls for Point");
        let drawable = iface_map.get("Drawable").expect("drawable impl");
        let info = drawable.get("draw").expect("draw method info");
        assert_eq!(info.function_name, "Point::Drawable::draw");
        assert!(matches!(info.self_kind, SelfKind::Ref));
    }

    #[test]
    fn reports_missing_interface_methods() {
        let span = make_span();
        let module = Module {
            items: vec![
                Item::Interface(InterfaceDef {
                    name: Ident("Renderable".into()),
                    methods: vec![InterfaceMethod {
                        name: Ident("render".into()),
                        params: vec![Param {
                            name: Ident("self".into()),
                            ty: Some(Ident("Point".into())),
                            span,
                        }],
                        ret_type: None,
                        span,
                    }],
                    span,
                }),
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    fields: vec![],
                    interfaces: vec![Ident("Renderable".into())],
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span,
                }),
            ],
        };

        let (_ctx, diags) = resolve_types(&module);
        assert!(diags.iter().any(|d| {
            d.message
                .contains("missing implementations for interface 'Renderable'")
        }));
    }
}
