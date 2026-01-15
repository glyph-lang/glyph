use std::collections::HashMap;

use glyph_core::ast::{
    EnumDef, EnumVariantDef, ExternFunctionDecl, Ident, Import, ImportKind, ImportPath, Module,
    Param, StructDef, TypeExpr,
};
use glyph_core::span::Span;

fn tp(name: &str, span: Span) -> TypeExpr {
    TypeExpr::Path {
        segments: vec![name.to_string()],
        span,
    }
}

/// Return the built-in std modules shipped with the compiler.
/// Currently includes:
/// - std (re-exports std::io)
/// - std/io with puts/raw_write and Write interface
/// - std/enums with generic Option<T> and monomorphic Result<i32>
pub fn std_modules() -> HashMap<String, Module> {
    let mut modules = HashMap::new();
    let span = Span::new(0, 0);

    // std::io
    let stdout_struct = StructDef {
        name: Ident("Stdout".into()),
        generic_params: Vec::new(),
        fields: vec![glyph_core::ast::FieldDef {
            name: Ident("fd".into()),
            ty: tp("i32", span),
            span,
        }],
        interfaces: vec![],
        methods: Vec::new(),
        inline_impls: Vec::new(),
        span,
    };

    let puts_extern = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("puts".into()),
        params: vec![Param {
            name: Ident("msg".into()),
            ty: Some(tp("str", span)),
            span,
        }],
        ret_type: Some(tp("i32", span)),
        link_name: None,
        span,
    };

    let raw_write_extern = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("raw_write".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(tp("i32", span)),
                span,
            },
            Param {
                name: Ident("bytes".into()),
                ty: Some(tp("RawPtr<u8>", span)),
                span,
            },
            Param {
                name: Ident("len".into()),
                ty: Some(tp("u32", span)),
                span,
            },
        ],
        ret_type: Some(tp("i32", span)),
        link_name: Some("write".into()),
        span,
    };

    let std_io_module = Module {
        imports: vec![],
        items: vec![
            glyph_core::ast::Item::Struct(stdout_struct),
            glyph_core::ast::Item::ExternFunction(puts_extern),
            glyph_core::ast::Item::ExternFunction(raw_write_extern),
        ],
    };
    modules.insert("std/io".into(), std_io_module);

    // std root re-exporting io
    let std_root = Module {
        imports: vec![
            Import {
                kind: ImportKind::Wildcard,
                path: ImportPath {
                    segments: vec!["std/io".into()],
                    span,
                },
                span,
            },
            Import {
                kind: ImportKind::Wildcard,
                path: ImportPath {
                    segments: vec!["std/vec".into()],
                    span,
                },
                span,
            },
            Import {
                kind: ImportKind::Wildcard,
                path: ImportPath {
                    segments: vec!["std/enums".into()],
                    span,
                },
                span,
            },
        ],
        items: vec![],
    };
    modules.insert("std".into(), std_root);

    // std/vec
    let vec_struct = StructDef {
        name: Ident("Vec".into()),
        generic_params: vec![Ident("T".into())],
        fields: vec![
            glyph_core::ast::FieldDef {
                name: Ident("data".into()),
                ty: TypeExpr::App {
                    base: Box::new(tp("RawPtr", span)),
                    args: vec![tp("T", span)],
                    span,
                },
                span,
            },
            glyph_core::ast::FieldDef {
                name: Ident("len".into()),
                ty: tp("usize", span),
                span,
            },
            glyph_core::ast::FieldDef {
                name: Ident("cap".into()),
                ty: tp("usize", span),
                span,
            },
        ],
        interfaces: vec![],
        methods: Vec::new(),
        inline_impls: Vec::new(),
        span,
    };

    let std_vec_module = Module {
        imports: vec![],
        items: vec![glyph_core::ast::Item::Struct(vec_struct)],
    };
    modules.insert("std/vec".into(), std_vec_module);

    // std/enums with generic Option<T> (Result remains monomorphic i32 for now)
    let option_enum = EnumDef {
        name: Ident("Option".into()),
        generic_params: vec![Ident("T".into())],
        variants: vec![
            EnumVariantDef {
                name: Ident("None".into()),
                payload: None,
                span,
            },
            EnumVariantDef {
                name: Ident("Some".into()),
                payload: Some(tp("T", span)),
                span,
            },
        ],
        span,
    };

    let result_enum = EnumDef {
        name: Ident("Result".into()),
        generic_params: Vec::new(),
        variants: vec![
            EnumVariantDef {
                name: Ident("Ok".into()),
                payload: Some(tp("i32", span)),
                span,
            },
            EnumVariantDef {
                name: Ident("Err".into()),
                payload: Some(tp("i32", span)),
                span,
            },
        ],
        span,
    };

    let std_enums = Module {
        imports: vec![],
        items: vec![
            glyph_core::ast::Item::Enum(option_enum),
            glyph_core::ast::Item::Enum(result_enum),
        ],
    };
    modules.insert("std/enums".into(), std_enums);

    modules
}
