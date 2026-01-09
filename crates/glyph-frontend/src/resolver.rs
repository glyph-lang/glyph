use std::collections::HashMap;

use glyph_core::{
    ast::{Item, Module},
    diag::Diagnostic,
    types::{StructType, Type},
};

/// Context containing resolved type information
#[derive(Debug, Clone, Default)]
pub struct ResolverContext {
    pub struct_types: HashMap<String, StructType>,
}

/// Resolves types in a module, building a registry of struct types
/// Returns the resolver context and any diagnostics encountered
pub fn resolve_types(module: &Module) -> (ResolverContext, Vec<Diagnostic>) {
    let mut ctx = ResolverContext {
        struct_types: HashMap::new(),
    };
    let mut diagnostics = Vec::new();

    // First pass: collect all struct definitions
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use glyph_core::{
        ast::{FieldDef, Ident, StructDef},
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
                    span: make_span(),
                }),
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    fields: vec![],
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
                    ty: Ident("Point".into()), // Undefined!
                    span: make_span(),
                }],
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
                    span: make_span(),
                }),
                Item::Struct(StructDef {
                    name: Ident("Rect".into()),
                    fields: vec![FieldDef {
                        name: Ident("top_left".into()),
                        ty: Ident("Point".into()),
                        span: make_span(),
                    }],
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
}
