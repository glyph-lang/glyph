use std::collections::{HashMap, HashSet, VecDeque};

use glyph_core::{
    ast::{Item, Module, TypeExpr},
    diag::Diagnostic,
    mir::{MirInst, MirModule, Rvalue},
    types::{EnumType, EnumVariant, Mutability, StructType, Type},
};

#[derive(Debug, Clone)]
enum Template {
    Struct {
        params: Vec<String>,
        fields: Vec<(String, Type)>,
    },
    Enum {
        params: Vec<String>,
        variants: Vec<EnumVariant>,
    },
}

fn collect_templates(modules: &HashMap<String, Module>) -> HashMap<String, Template> {
    let mut templates = HashMap::new();

    for (module_id, module) in modules {
        let module_prefix = module_id.replace('/', "::");

        for item in &module.items {
            match item {
                Item::Struct(def) if !def.generic_params.is_empty() => {
                    let params: Vec<String> =
                        def.generic_params.iter().map(|p| p.0.clone()).collect();
                    let param_set: HashSet<String> = params.iter().cloned().collect();
                    let fields = def
                        .fields
                        .iter()
                        .map(|f| (f.name.0.clone(), type_expr_to_type(&f.ty, &param_set)))
                        .collect();

                    let templ = Template::Struct {
                        params: params.clone(),
                        fields,
                    };
                    templates
                        .entry(def.name.0.clone())
                        .or_insert_with(|| templ.clone());
                    templates
                        .entry(format!("{}::{}", module_prefix, def.name.0))
                        .or_insert(templ);
                }
                Item::Enum(def) if !def.generic_params.is_empty() => {
                    let params: Vec<String> =
                        def.generic_params.iter().map(|p| p.0.clone()).collect();
                    let param_set: HashSet<String> = params.iter().cloned().collect();
                    let variants: Vec<EnumVariant> = def
                        .variants
                        .iter()
                        .map(|v| EnumVariant {
                            name: v.name.0.clone(),
                            payload: v.payload.as_ref().map(|p| type_expr_to_type(p, &param_set)),
                        })
                        .collect();

                    let templ = Template::Enum {
                        params: params.clone(),
                        variants,
                    };
                    templates
                        .entry(def.name.0.clone())
                        .or_insert_with(|| templ.clone());
                    templates
                        .entry(format!("{}::{}", module_prefix, def.name.0))
                        .or_insert(templ);
                }
                _ => {}
            }
        }
    }

    templates
}

fn type_expr_to_type(expr: &TypeExpr, param_set: &HashSet<String>) -> Type {
    match expr {
        TypeExpr::Path { segments, .. } => {
            let name = segments.join("::");
            if segments.len() == 1 && param_set.contains(&name) {
                return Type::Param(name);
            }
            if let Some(builtin) = Type::from_name(&name) {
                return builtin;
            }
            if name == "str" {
                return Type::Str;
            }
            Type::Named(name)
        }
        TypeExpr::Ref {
            mutability, inner, ..
        } => Type::Ref(Box::new(type_expr_to_type(inner, param_set)), *mutability),
        TypeExpr::Array { elem, size, .. } => {
            Type::Array(Box::new(type_expr_to_type(elem, param_set)), *size)
        }
        TypeExpr::App { base, args, .. } => {
            let base_name = match base.as_ref() {
                TypeExpr::Path { segments, .. } => segments.join("::"),
                _ => "<unknown>".to_string(),
            };

            let mut arg_tys: Vec<Type> = args
                .iter()
                .map(|a| type_expr_to_type(a, param_set))
                .collect();

            match base_name.as_str() {
                "Own" if arg_tys.len() == 1 => Type::Own(Box::new(arg_tys.remove(0))),
                "RawPtr" if arg_tys.len() == 1 => Type::RawPtr(Box::new(arg_tys.remove(0))),
                "Shared" if arg_tys.len() == 1 => Type::Shared(Box::new(arg_tys.remove(0))),
                _ => Type::App {
                    base: base_name,
                    args: arg_tys,
                },
            }
        }
        TypeExpr::Tuple { elements, .. } => {
            let elem_types: Vec<Type> = elements
                .iter()
                .map(|e| type_expr_to_type(e, param_set))
                .collect();
            Type::Tuple(elem_types)
        }
    }
}

fn type_key(ty: &Type) -> String {
    match ty {
        Type::I8 => "i8".into(),
        Type::I32 => "i32".into(),
        Type::I64 => "i64".into(),
        Type::U8 => "u8".into(),
        Type::U32 => "u32".into(),
        Type::U64 => "u64".into(),
        Type::Usize => "usize".into(),
        Type::F32 => "f32".into(),
        Type::F64 => "f64".into(),
        Type::Bool => "bool".into(),
        Type::Char => "char".into(),
        Type::Str => "str".into(),
        Type::String => "String".into(),
        Type::Void => "void".into(),
        Type::Named(n) => sanitize(n),
        Type::Enum(n) => format!("enum_{}", sanitize(n)),
        Type::Param(p) => format!("P_{}", sanitize(p)),
        Type::Ref(inner, mutability) => {
            let m = if matches!(mutability, Mutability::Mutable) {
                "mut"
            } else {
                "ref"
            };
            format!("{}_{}", m, type_key(inner))
        }
        Type::Array(inner, size) => format!("arr{}_{}", size, type_key(inner)),
        Type::Own(inner) => format!("own_{}", type_key(inner)),
        Type::RawPtr(inner) => format!("rawptr_{}", type_key(inner)),
        Type::Shared(inner) => format!("shared_{}", type_key(inner)),
        Type::App { base, args } => {
            let args: Vec<String> = args.iter().map(type_key).collect();
            format!("app_{}_{}", sanitize(base), args.join("__"))
        }
        Type::Tuple(elem_types) => {
            if elem_types.is_empty() {
                "unit".into()
            } else {
                let type_names: Vec<String> = elem_types.iter().map(type_key).collect();
                format!("__Tuple{}_{}", elem_types.len(), type_names.join("_"))
            }
        }
    }
}

fn sanitize(name: &str) -> String {
    name.chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect()
}

fn substitute(ty: &Type, subst: &HashMap<String, Type>) -> Type {
    match ty {
        Type::Param(p) => subst.get(p).cloned().unwrap_or(Type::Param(p.clone())),
        Type::Ref(inner, m) => Type::Ref(Box::new(substitute(inner, subst)), *m),
        Type::Array(inner, size) => Type::Array(Box::new(substitute(inner, subst)), *size),
        Type::Own(inner) => Type::Own(Box::new(substitute(inner, subst))),
        Type::RawPtr(inner) => Type::RawPtr(Box::new(substitute(inner, subst))),
        Type::Shared(inner) => Type::Shared(Box::new(substitute(inner, subst))),
        Type::App { base, args } => Type::App {
            base: base.clone(),
            args: args.iter().map(|a| substitute(a, subst)).collect(),
        },
        other => other.clone(),
    }
}

fn rewrite_type(
    ty: &Type,
    templates: &HashMap<String, Template>,
    instantiations: &mut HashMap<(String, Vec<Type>), String>,
    worklist: &mut VecDeque<Type>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Type {
    match ty {
        Type::App { base, args } => {
            let args_rewritten: Vec<Type> = args
                .iter()
                .map(|a| rewrite_type(a, templates, instantiations, worklist, diagnostics))
                .collect();

            let Some(template) = templates.get(base) else {
                diagnostics.push(Diagnostic::error(
                    format!("unknown generic template '{}'", base),
                    None,
                ));
                return Type::Named(format!(
                    "{}<{}>",
                    base,
                    args_rewritten
                        .iter()
                        .map(type_key)
                        .collect::<Vec<_>>()
                        .join(",")
                ));
            };

            let key = (base.clone(), args_rewritten.clone());
            if let Some(existing) = instantiations.get(&key) {
                return match template {
                    Template::Enum { .. } => Type::Enum(existing.clone()),
                    Template::Struct { .. } => Type::Named(existing.clone()),
                };
            }

            let inst_name = format!(
                "{}${}",
                sanitize(base),
                args_rewritten
                    .iter()
                    .map(type_key)
                    .collect::<Vec<_>>()
                    .join("__")
            );
            instantiations.insert(key, inst_name.clone());
            worklist.push_back(Type::App {
                base: base.clone(),
                args: args_rewritten,
            });

            match template {
                Template::Enum { .. } => Type::Enum(inst_name),
                Template::Struct { .. } => Type::Named(inst_name),
            }
        }
        Type::Ref(inner, m) => Type::Ref(
            Box::new(rewrite_type(
                inner,
                templates,
                instantiations,
                worklist,
                diagnostics,
            )),
            *m,
        ),
        Type::Array(inner, size) => Type::Array(
            Box::new(rewrite_type(
                inner,
                templates,
                instantiations,
                worklist,
                diagnostics,
            )),
            *size,
        ),
        Type::Own(inner) => Type::Own(Box::new(rewrite_type(
            inner,
            templates,
            instantiations,
            worklist,
            diagnostics,
        ))),
        Type::RawPtr(inner) => Type::RawPtr(Box::new(rewrite_type(
            inner,
            templates,
            instantiations,
            worklist,
            diagnostics,
        ))),
        Type::Shared(inner) => Type::Shared(Box::new(rewrite_type(
            inner,
            templates,
            instantiations,
            worklist,
            diagnostics,
        ))),
        Type::Param(p) => {
            // Fallback: treat unresolved params as i32 to keep codegen moving.
            // TODO: replace with proper contextual substitution.
            Type::I32
        }
        Type::Tuple(elem_types) => {
            let rewritten_elems: Vec<Type> = elem_types
                .iter()
                .map(|t| rewrite_type(t, templates, instantiations, worklist, diagnostics))
                .collect();
            Type::Tuple(rewritten_elems)
        }
        other => other.clone(),
    }
}

fn rewrite_type_with_instantiations(
    ty: &Type,
    templates: &HashMap<String, Template>,
    instantiations: &HashMap<(String, Vec<Type>), String>,
) -> Type {
    match ty {
        Type::App { base, args } => {
            let args_rewritten: Vec<Type> = args
                .iter()
                .map(|a| rewrite_type_with_instantiations(a, templates, instantiations))
                .collect();
            let key = (base.clone(), args_rewritten.clone());
            if let Some(inst_name) = instantiations.get(&key) {
                if let Some(template) = templates.get(base) {
                    return match template {
                        Template::Enum { .. } => Type::Enum(inst_name.clone()),
                        Template::Struct { .. } => Type::Named(inst_name.clone()),
                    };
                }
            }
            Type::App {
                base: base.clone(),
                args: args_rewritten,
            }
        }
        Type::Ref(inner, m) => Type::Ref(
            Box::new(rewrite_type_with_instantiations(
                inner,
                templates,
                instantiations,
            )),
            *m,
        ),
        Type::Array(inner, size) => Type::Array(
            Box::new(rewrite_type_with_instantiations(
                inner,
                templates,
                instantiations,
            )),
            *size,
        ),
        Type::Own(inner) => Type::Own(Box::new(rewrite_type_with_instantiations(
            inner,
            templates,
            instantiations,
        ))),
        Type::RawPtr(inner) => Type::RawPtr(Box::new(rewrite_type_with_instantiations(
            inner,
            templates,
            instantiations,
        ))),
        Type::Shared(inner) => Type::Shared(Box::new(rewrite_type_with_instantiations(
            inner,
            templates,
            instantiations,
        ))),
        other => other.clone(),
    }
}

fn rewrite_rvalue(
    rvalue: &mut Rvalue,
    assigned_ty: Option<&Type>,
    templates: &HashMap<String, Template>,
    instantiations: &mut HashMap<(String, Vec<Type>), String>,
    worklist: &mut VecDeque<Type>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match rvalue {
        Rvalue::ArrayLit { elem_type, .. } => {
            *elem_type = rewrite_type(elem_type, templates, instantiations, worklist, diagnostics);
        }
        Rvalue::VecNew { elem_type }
        | Rvalue::VecWithCapacity { elem_type, .. }
        | Rvalue::VecPush { elem_type, .. }
        | Rvalue::VecPop { elem_type, .. } => {
            *elem_type = rewrite_type(elem_type, templates, instantiations, worklist, diagnostics);
        }
        Rvalue::MapNew {
            key_type,
            value_type,
        }
        | Rvalue::MapWithCapacity {
            key_type,
            value_type,
            ..
        }
        | Rvalue::MapAdd {
            key_type,
            value_type,
            ..
        }
        | Rvalue::MapUpdate {
            key_type,
            value_type,
            ..
        }
        | Rvalue::MapDel {
            key_type,
            value_type,
            ..
        }
        | Rvalue::MapGet {
            key_type,
            value_type,
            ..
        }
        | Rvalue::MapKeys {
            key_type,
            value_type,
            ..
        }
        | Rvalue::MapVals {
            key_type,
            value_type,
            ..
        } => {
            *key_type = rewrite_type(key_type, templates, instantiations, worklist, diagnostics);
            *value_type =
                rewrite_type(value_type, templates, instantiations, worklist, diagnostics);
        }
        Rvalue::MapHas { key_type, .. } => {
            *key_type = rewrite_type(key_type, templates, instantiations, worklist, diagnostics);
        }
        Rvalue::OwnNew { elem_type, .. }
        | Rvalue::OwnIntoRaw { elem_type, .. }
        | Rvalue::OwnFromRaw { elem_type, .. }
        | Rvalue::RawPtrNull { elem_type }
        | Rvalue::SharedNew { elem_type, .. }
        | Rvalue::SharedClone { elem_type, .. } => {
            *elem_type = rewrite_type(elem_type, templates, instantiations, worklist, diagnostics);
        }
        Rvalue::EnumPayload { payload_type, .. } => {
            *payload_type = rewrite_type(
                payload_type,
                templates,
                instantiations,
                worklist,
                diagnostics,
            );
        }
        Rvalue::EnumConstruct { enum_name, .. } => {
            if let Some(ty) = assigned_ty {
                if let Type::Enum(name) | Type::Named(name) = ty {
                    *enum_name = name.clone();
                }
            }
        }
        Rvalue::StructLit { struct_name, .. } => {
            if let Some(Type::Named(name)) = assigned_ty {
                *struct_name = name.clone();
            }
        }
        _ => {}
    }
}

fn instantiate_all(
    mir: &mut MirModule,
    templates: &HashMap<String, Template>,
    instantiations: &HashMap<(String, Vec<Type>), String>,
) {
    // Reify each Type::App key into a concrete StructType/EnumType.
    for ((base, args), inst_name) in instantiations {
        let Some(template) = templates.get(base) else {
            continue;
        };

        match template {
            Template::Struct { params, fields } => {
                let subst: HashMap<String, Type> =
                    params.iter().cloned().zip(args.iter().cloned()).collect();
                let inst_fields = fields
                    .iter()
                    .map(|(n, t)| {
                        let substituted = substitute(t, &subst);
                        let rewritten = rewrite_type_with_instantiations(
                            &substituted,
                            templates,
                            instantiations,
                        );
                        (n.clone(), rewritten)
                    })
                    .collect();
                mir.struct_types.insert(
                    inst_name.clone(),
                    StructType {
                        name: inst_name.clone(),
                        fields: inst_fields,
                    },
                );
            }
            Template::Enum { params, variants } => {
                let subst: HashMap<String, Type> =
                    params.iter().cloned().zip(args.iter().cloned()).collect();

                let inst_variants = variants
                    .iter()
                    .map(|v| EnumVariant {
                        name: v.name.clone(),
                        payload: v.payload.as_ref().map(|p| substitute(p, &subst)).map(|p| {
                            rewrite_type_with_instantiations(&p, templates, instantiations)
                        }),
                    })
                    .collect();

                mir.enum_types.insert(
                    inst_name.clone(),
                    EnumType {
                        name: inst_name.clone(),
                        variants: inst_variants,
                    },
                );
            }
        }
    }
}

pub fn monomorphize_mir(mir: &mut MirModule, modules: &HashMap<String, Module>) -> Vec<Diagnostic> {
    let templates = collect_templates(modules);

    let mut diagnostics = Vec::new();
    let mut instantiations: HashMap<(String, Vec<Type>), String> = HashMap::new();
    let mut worklist = VecDeque::new();

    // Rewrite all existing types and seed instantiation worklist.
    for (name, st) in mir.struct_types.iter_mut() {
        if templates.contains_key(name) {
            continue;
        }
        for (_n, ty) in &mut st.fields {
            *ty = rewrite_type(
                ty,
                &templates,
                &mut instantiations,
                &mut worklist,
                &mut diagnostics,
            );
        }
    }

    for (name, et) in mir.enum_types.iter_mut() {
        if templates.contains_key(name) {
            continue;
        }
        for v in &mut et.variants {
            if let Some(payload) = &mut v.payload {
                *payload = rewrite_type(
                    payload,
                    &templates,
                    &mut instantiations,
                    &mut worklist,
                    &mut diagnostics,
                );
            }
        }
    }

    for func in &mut mir.functions {
        if let Some(ret) = &mut func.ret_type {
            *ret = rewrite_type(
                ret,
                &templates,
                &mut instantiations,
                &mut worklist,
                &mut diagnostics,
            );
        }
        for local in &mut func.locals {
            if let Some(ty) = &mut local.ty {
                *ty = rewrite_type(
                    ty,
                    &templates,
                    &mut instantiations,
                    &mut worklist,
                    &mut diagnostics,
                );
            }
        }

        for block in &mut func.blocks {
            for inst in &mut block.insts {
                if let MirInst::Assign { local, value } = inst {
                    let assigned_ty = func
                        .locals
                        .get(local.0 as usize)
                        .and_then(|l| l.ty.as_ref());
                    rewrite_rvalue(
                        value,
                        assigned_ty,
                        &templates,
                        &mut instantiations,
                        &mut worklist,
                        &mut diagnostics,
                    );
                }
            }
        }
    }

    for ex in &mut mir.extern_functions {
        if let Some(ret) = &mut ex.ret_type {
            *ret = rewrite_type(
                ret,
                &templates,
                &mut instantiations,
                &mut worklist,
                &mut diagnostics,
            );
        }
        for p in &mut ex.params {
            *p = rewrite_type(
                p,
                &templates,
                &mut instantiations,
                &mut worklist,
                &mut diagnostics,
            );
        }
    }

    // Generate struct types for all tuples
    let mut tuple_types_to_generate = std::collections::HashSet::new();
    for func in &mir.functions {
        // Check return type
        if let Some(Type::Tuple(elem_types)) = &func.ret_type {
            if !elem_types.is_empty() {
                tuple_types_to_generate.insert(elem_types.clone());
            }
        }
        // Check local types
        for local in &func.locals {
            if let Some(Type::Tuple(elem_types)) = &local.ty {
                if !elem_types.is_empty() {
                    tuple_types_to_generate.insert(elem_types.clone());
                }
            }
        }
    }

    // Helper function to generate tuple struct names consistently
    let tuple_struct_name_mono = |elem_types: &[Type]| -> String {
        if elem_types.is_empty() {
            "unit".to_string()
        } else {
            let type_names: Vec<String> = elem_types.iter().map(type_key).collect();
            format!("__Tuple{}_{}", elem_types.len(), type_names.join("_"))
        }
    };

    for elem_types in tuple_types_to_generate {
        let struct_name = tuple_struct_name_mono(&elem_types);
        let fields: Vec<(String, Type)> = elem_types
            .into_iter()
            .enumerate()
            .map(|(idx, ty)| (idx.to_string(), ty))
            .collect();

        mir.struct_types.insert(
            struct_name.clone(),
            StructType {
                name: struct_name,
                fields,
            },
        );
    }

    // Instantiate until queue drains. (Queue items were pushed as Type::App.)
    while let Some(Type::App { base, args }) = worklist.pop_front() {
        let Some(template) = templates.get(&base) else {
            continue;
        };

        let inst_name = instantiations
            .get(&(base.clone(), args.clone()))
            .cloned()
            .unwrap_or_else(|| {
                format!(
                    "{}${}",
                    sanitize(&base),
                    args.iter().map(type_key).collect::<Vec<_>>().join("__")
                )
            });

        // Materialize the instantiation.
        match template {
            Template::Struct { params, fields } => {
                let subst: HashMap<String, Type> =
                    params.iter().cloned().zip(args.iter().cloned()).collect();
                let mut inst_fields = Vec::new();
                for (n, t) in fields {
                    let substituted = substitute(t, &subst);
                    let rewritten = rewrite_type(
                        &substituted,
                        &templates,
                        &mut instantiations,
                        &mut worklist,
                        &mut diagnostics,
                    );
                    inst_fields.push((n.clone(), rewritten));
                }

                mir.struct_types.insert(
                    inst_name.clone(),
                    StructType {
                        name: inst_name.clone(),
                        fields: inst_fields,
                    },
                );
            }
            Template::Enum { params, variants } => {
                let subst: HashMap<String, Type> =
                    params.iter().cloned().zip(args.iter().cloned()).collect();

                let mut inst_variants = Vec::new();
                for v in variants {
                    let payload = v.payload.as_ref().map(|p| substitute(p, &subst));
                    let payload = payload.map(|p| {
                        rewrite_type(
                            &p,
                            &templates,
                            &mut instantiations,
                            &mut worklist,
                            &mut diagnostics,
                        )
                    });
                    inst_variants.push(EnumVariant {
                        name: v.name.clone(),
                        payload,
                    });
                }

                mir.enum_types.insert(
                    inst_name.clone(),
                    EnumType {
                        name: inst_name.clone(),
                        variants: inst_variants,
                    },
                );
            }
        }
    }

    // Also ensure any pre-collected instantiations exist.
    instantiate_all(mir, &templates, &instantiations);

    // Drop generic templates from the concrete MIR; keep only instantiated forms.
    for key in templates.keys() {
        mir.struct_types.remove(key);
        mir.enum_types.remove(key);
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn monomorphizes_simple_struct_template() {
        let span = glyph_core::span::Span::new(0, 0);
        let modules = HashMap::from([(
            "main".to_string(),
            Module {
                imports: vec![],
                items: vec![Item::Struct(glyph_core::ast::StructDef {
                    name: glyph_core::ast::Ident("Box".into()),
                    generic_params: vec![glyph_core::ast::Ident("T".into())],
                    fields: vec![glyph_core::ast::FieldDef {
                        name: glyph_core::ast::Ident("value".into()),
                        ty: TypeExpr::Path {
                            segments: vec!["T".into()],
                            span,
                        },
                        span,
                    }],
                    interfaces: vec![],
                    methods: vec![],
                    inline_impls: vec![],
                    span,
                })],
            },
        )]);

        let mut mir = MirModule::default();
        mir.functions.push(glyph_core::mir::MirFunction {
            name: "main".into(),
            ret_type: None,
            params: vec![],
            locals: vec![glyph_core::mir::Local {
                name: Some("x".into()),
                ty: Some(Type::App {
                    base: "Box".into(),
                    args: vec![Type::I32],
                }),
                mutable: false,
            }],
            blocks: vec![glyph_core::mir::MirBlock { insts: vec![] }],
        });

        let diags = monomorphize_mir(&mut mir, &modules);
        assert!(diags.is_empty(), "diags: {:?}", diags);

        let local_ty = mir.functions[0].locals[0].ty.clone().unwrap();
        match local_ty {
            Type::Named(name) => {
                assert!(name.starts_with("Box$"));
                assert!(mir.struct_types.contains_key(&name));
                let st = mir.struct_types.get(&name).unwrap();
                assert_eq!(st.fields[0].1, Type::I32);
            }
            _ => panic!("expected monomorphized named type"),
        }
    }

    #[test]
    fn monomorphizes_option_enum() {
        let span = glyph_core::span::Span::new(0, 0);
        let modules = HashMap::from([(
            "main".to_string(),
            Module {
                imports: vec![],
                items: vec![Item::Enum(glyph_core::ast::EnumDef {
                    name: glyph_core::ast::Ident("Option".into()),
                    generic_params: vec![glyph_core::ast::Ident("T".into())],
                    variants: vec![
                        glyph_core::ast::EnumVariantDef {
                            name: glyph_core::ast::Ident("None".into()),
                            payload: None,
                            span,
                        },
                        glyph_core::ast::EnumVariantDef {
                            name: glyph_core::ast::Ident("Some".into()),
                            payload: Some(TypeExpr::Path {
                                segments: vec!["T".into()],
                                span,
                            }),
                            span,
                        },
                    ],
                    span,
                })],
            },
        )]);

        let mut mir = MirModule::default();
        mir.functions.push(glyph_core::mir::MirFunction {
            name: "main".into(),
            ret_type: Some(Type::App {
                base: "Option".into(),
                args: vec![Type::I32],
            }),
            params: vec![],
            locals: vec![glyph_core::mir::Local {
                name: Some("v".into()),
                ty: Some(Type::App {
                    base: "Option".into(),
                    args: vec![Type::I32],
                }),
                mutable: false,
            }],
            blocks: vec![glyph_core::mir::MirBlock { insts: vec![] }],
        });

        let diags = monomorphize_mir(&mut mir, &modules);
        assert!(diags.is_empty(), "diags: {:?}", diags);

        // Ensure the enum instantiation exists and payload is i32.
        let inst_name = match mir.functions[0].locals[0].ty.as_ref().unwrap() {
            Type::Enum(name) => name.clone(),
            Type::Named(name) => name.clone(),
            other => panic!("unexpected local type: {:?}", other),
        };
        let en = mir.enum_types.get(&inst_name).expect("instantiated enum");
        assert_eq!(en.variants.len(), 2);
        assert_eq!(en.variants[1].name, "Some");
        assert_eq!(en.variants[1].payload, Some(Type::I32));
    }

    #[test]
    fn monomorphizes_two_param_enum() {
        let span = glyph_core::span::Span::new(0, 0);
        let modules = HashMap::from([(
            "main".to_string(),
            Module {
                imports: vec![],
                items: vec![Item::Enum(glyph_core::ast::EnumDef {
                    name: glyph_core::ast::Ident("Error".into()),
                    generic_params: vec![
                        glyph_core::ast::Ident("T".into()),
                        glyph_core::ast::Ident("E".into()),
                    ],
                    variants: vec![
                        glyph_core::ast::EnumVariantDef {
                            name: glyph_core::ast::Ident("Message".into()),
                            payload: Some(TypeExpr::Path {
                                segments: vec!["T".into()],
                                span,
                            }),
                            span,
                        },
                        glyph_core::ast::EnumVariantDef {
                            name: glyph_core::ast::Ident("Code".into()),
                            payload: Some(TypeExpr::Path {
                                segments: vec!["E".into()],
                                span,
                            }),
                            span,
                        },
                    ],
                    span,
                })],
            },
        )]);

        let mut mir = MirModule::default();
        mir.functions.push(glyph_core::mir::MirFunction {
            name: "main".into(),
            ret_type: Some(Type::App {
                base: "Error".into(),
                args: vec![Type::I32, Type::String],
            }),
            params: vec![],
            locals: vec![glyph_core::mir::Local {
                name: Some("e".into()),
                ty: Some(Type::App {
                    base: "Error".into(),
                    args: vec![Type::I32, Type::String],
                }),
                mutable: false,
            }],
            blocks: vec![glyph_core::mir::MirBlock { insts: vec![] }],
        });

        let diags = monomorphize_mir(&mut mir, &modules);
        assert!(diags.is_empty(), "diags: {:?}", diags);

        let inst_name = match mir.functions[0].locals[0].ty.as_ref().unwrap() {
            Type::Enum(name) => name.clone(),
            Type::Named(name) => name.clone(),
            other => panic!("unexpected local type: {:?}", other),
        };
        let en = mir.enum_types.get(&inst_name).expect("instantiated enum");
        assert_eq!(en.variants.len(), 2);
        assert_eq!(en.variants[0].payload, Some(Type::I32));
        assert_eq!(en.variants[1].payload, Some(Type::String));
    }
}
