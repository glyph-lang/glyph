use std::collections::HashMap;

use glyph_core::{
    ast::{Ident, Item, Module, TypeExpr},
    diag::Diagnostic,
    types::Type,
};

use crate::method_symbols::interface_method_symbol;

use super::{type_expr_to_string, MethodInfo, ResolverContext, SelfKind, InterfaceImplState};

pub fn detect_self_kind(param_ty: &Option<TypeExpr>) -> SelfKind {
    match param_ty {
        Some(ty_expr) => {
            let ty_str = type_expr_to_string(ty_expr).replace(' ', "");
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

fn normalize_type_name(raw: &str) -> String {
    raw.split_whitespace().collect()
}

fn self_param_matches_struct(param: &glyph_core::ast::Param, struct_name: &str) -> bool {
    match &param.ty {
        Some(ty) => {
            let rendered = type_expr_to_string(ty);
            let norm = normalize_type_name(&rendered);
            norm == struct_name
                || norm == format!("&{}", struct_name)
                || norm == format!("&mut{}", struct_name)
        }
        None => true,
    }
}

pub(super) fn validate_interface_signatures(
    ctx: &ResolverContext,
    struct_names: &std::collections::HashSet<String>,
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
            // Skip self; already validated separately for struct receiver.
            if idx == 0 {
                continue;
            }
            if let Some(expected) = expected_raw {
                let actual = method
                    .params
                    .get(idx)
                    .and_then(|p| p.ty.as_ref())
                    .map(|t| normalize_type_name(&type_expr_to_string(t)));
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
                    let actual_rendered = method.params[idx]
                        .ty
                        .as_ref()
                        .map(|t| type_expr_to_string(t))
                        .unwrap_or_else(|| "<unknown>".to_string());
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "parameter {} of method '{}' has type '{}' but interface '{}' expects '{}'",
                            idx + 1,
                            method.name.0,
                            actual_rendered,
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
                    let rendered = type_expr_to_string(actual_ret);
                    if normalize_type_name(&rendered) != normalize_type_name(expected_ret) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "method '{}' returns '{}' but interface '{}' requires '{}'",
                                method.name.0, rendered, iface_name, expected_ret
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

pub(super) fn collect_interface_impls(
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
