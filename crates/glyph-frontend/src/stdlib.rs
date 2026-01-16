use std::collections::HashMap;

use glyph_core::ast::{
    EnumDef, EnumVariantDef, ExternFunctionDecl, Ident, Import, ImportKind, ImportPath,
    InterfaceDef, Module, Param, StructDef, TypeExpr,
};
use glyph_core::span::Span;
use glyph_core::types::Mutability;

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

    let fopen_extern = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("fopen".into()),
        params: vec![
            Param {
                name: Ident("path".into()),
                ty: Some(tp("str", span)),
                span,
            },
            Param {
                name: Ident("mode".into()),
                ty: Some(tp("str", span)),
                span,
            },
        ],
        ret_type: Some(tp("RawPtr<u8>", span)),
        link_name: Some("fopen".into()),
        span,
    };

    let write_extern = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("write".into()),
        params: vec![
            Param {
                name: Ident("buf".into()),
                ty: Some(tp("String", span)),
                span,
            },
            Param {
                name: Ident("size".into()),
                ty: Some(tp("u32", span)),
                span,
            },
            Param {
                name: Ident("count".into()),
                ty: Some(tp("u32", span)),
                span,
            },
            Param {
                name: Ident("file".into()),
                ty: Some(tp("RawPtr<u8>", span)),
                span,
            },
        ],
        ret_type: Some(tp("u32", span)),
        link_name: Some("fwrite".into()),
        span,
    };

    let read_extern = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("read".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(tp("i32", span)),
                span,
            },
            Param {
                name: Ident("buf".into()),
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
        link_name: Some("read".into()),
        span,
    };

    let println_extern = ExternFunctionDecl {
        name: Ident("println".into()),
        abi: Some("C".into()),
        params: vec![Param {
            name: Ident("msg".into()),
            ty: Some(tp("String", span)),
            span,
        }],
        ret_type: Some(tp("i32", span)),
        link_name: None,
        span,
    };

    let std_io_module = Module {
        imports: vec![],
        items: vec![
            glyph_core::ast::Item::Struct(stdout_struct),
            glyph_core::ast::Item::ExternFunction(puts_extern),
            glyph_core::ast::Item::ExternFunction(raw_write_extern),
            glyph_core::ast::Item::ExternFunction(fopen_extern),
            glyph_core::ast::Item::ExternFunction(write_extern),
            glyph_core::ast::Item::ExternFunction(read_extern),
            glyph_core::ast::Item::ExternFunction(println_extern),
        ],
    };
    modules.insert("std/io".into(), std_io_module);

    let std_println_extern = ExternFunctionDecl {
        name: Ident("println".into()),
        abi: Some("C".into()),
        params: vec![Param {
            name: Ident("msg".into()),
            ty: Some(tp("String", span)),
            span,
        }],
        ret_type: Some(tp("i32", span)),
        link_name: None,
        span,
    };

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
            Import {
                kind: ImportKind::Wildcard,
                path: ImportPath {
                    segments: vec!["std/string".into()],
                    span,
                },
                span,
            },
            Import {
                kind: ImportKind::Wildcard,
                path: ImportPath {
                    segments: vec!["std/sys".into()],
                    span,
                },
                span,
            },
            Import {
                kind: ImportKind::Wildcard,
                path: ImportPath {
                    segments: vec!["std/hashing".into()],
                    span,
                },
                span,
            },
            Import {
                kind: ImportKind::Wildcard,
                path: ImportPath {
                    segments: vec!["std/map".into()],
                    span,
                },
                span,
            },
        ],
        items: vec![glyph_core::ast::Item::ExternFunction(std_println_extern)],
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

    // std/enums with generic Option<T> and Result<T, E>
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

    let err_struct = StructDef {
        name: Ident("Err".into()),
        generic_params: vec![],
        fields: vec![glyph_core::ast::FieldDef {
            name: Ident("msg".into()),
            ty: tp("String", span),
            span,
        }],
        interfaces: vec![],
        methods: Vec::new(),
        inline_impls: Vec::new(),
        span,
    };

    let result_enum = EnumDef {
        name: Ident("Result".into()),
        generic_params: vec![Ident("T".into()), Ident("E".into())],
        variants: vec![
            EnumVariantDef {
                name: Ident("Ok".into()),
                payload: Some(tp("T", span)),
                span,
            },
            EnumVariantDef {
                name: Ident("Err".into()),
                payload: Some(tp("E", span)),
                span,
            },
        ],
        span,
    };

    let std_enums = Module {
        imports: vec![],
        items: vec![
            glyph_core::ast::Item::Enum(option_enum),
            glyph_core::ast::Item::Struct(err_struct),
            glyph_core::ast::Item::Enum(result_enum),
        ],
    };
    modules.insert("std/enums".into(), std_enums);

    // std/string
    let strdup_extern = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("strdup".into()),
        params: vec![Param {
            name: Ident("input".into()),
            ty: Some(tp("str", span)),
            span,
        }],
        ret_type: Some(tp("String", span)),
        link_name: Some("strdup".into()),
        span,
    };

    let std_string_module = Module {
        imports: vec![],
        items: vec![glyph_core::ast::Item::ExternFunction(strdup_extern)],
    };
    modules.insert("std/string".into(), std_string_module);

    // std/sys
    let argv_extern = ExternFunctionDecl {
        abi: None,
        name: Ident("argv".into()),
        params: vec![],
        ret_type: Some(TypeExpr::App {
            base: Box::new(tp("Vec", span)),
            args: vec![tp("String", span)],
            span,
        }),
        link_name: None,
        span,
    };

    let std_sys_module = Module {
        imports: vec![],
        items: vec![glyph_core::ast::Item::ExternFunction(argv_extern)],
    };
    modules.insert("std/sys".into(), std_sys_module);

    // std/hashing with Hash interface placeholder
    let hash_interface = InterfaceDef {
        name: Ident("Hash".into()),
        methods: vec![glyph_core::ast::InterfaceMethod {
            name: Ident("hash".into()),
            params: vec![Param {
                name: Ident("self".into()),
                ty: Some(TypeExpr::Ref {
                    mutability: Mutability::Immutable,
                    inner: Box::new(tp("Self", span)),
                    span,
                }),
                span,
            }],
            ret_type: Some(tp("u64", span)),
            span,
        }],
        span,
    };

    let std_hashing = Module {
        imports: vec![],
        items: vec![glyph_core::ast::Item::Interface(hash_interface)],
    };
    modules.insert("std/hashing".into(), std_hashing);

    // std/map
    // Map<K, V> API contract:
    // - Map::new() -> Map<K, V>
    // - Map::with_capacity(cap: usize) -> Map<K, V>
    // - add(key: K, val: V) -> Result<(), Err>
    // - update(key: K, val: V) -> Result<(), Err>
    // - del(key: K) -> Result<V, Err>
    // - get(key: K) -> Option<V>
    // - has(key: K) -> bool
    // - keys() -> Vec<K>
    // - vals() -> Vec<V>
    let map_bucket_struct = StructDef {
        name: Ident("MapBucket".into()),
        generic_params: vec![Ident("K".into()), Ident("V".into())],
        fields: vec![
            glyph_core::ast::FieldDef {
                name: Ident("key".into()),
                ty: tp("K", span),
                span,
            },
            glyph_core::ast::FieldDef {
                name: Ident("value".into()),
                ty: tp("V", span),
                span,
            },
            glyph_core::ast::FieldDef {
                name: Ident("next".into()),
                ty: TypeExpr::App {
                    base: Box::new(tp("RawPtr", span)),
                    args: vec![TypeExpr::App {
                        base: Box::new(tp("MapBucket", span)),
                        args: vec![tp("K", span), tp("V", span)],
                        span,
                    }],
                    span,
                },
                span,
            },
        ],
        interfaces: vec![],
        methods: Vec::new(),
        inline_impls: Vec::new(),
        span,
    };

    let map_struct = StructDef {
        name: Ident("Map".into()),
        generic_params: vec![Ident("K".into()), Ident("V".into())],
        fields: vec![
            glyph_core::ast::FieldDef {
                name: Ident("buckets".into()),
                ty: TypeExpr::App {
                    base: Box::new(tp("RawPtr", span)),
                    args: vec![TypeExpr::App {
                        base: Box::new(tp("RawPtr", span)),
                        args: vec![TypeExpr::App {
                            base: Box::new(tp("MapBucket", span)),
                            args: vec![tp("K", span), tp("V", span)],
                            span,
                        }],
                        span,
                    }],
                    span,
                },
                span,
            },
            glyph_core::ast::FieldDef {
                name: Ident("cap".into()),
                ty: tp("usize", span),
                span,
            },
            glyph_core::ast::FieldDef {
                name: Ident("len".into()),
                ty: tp("usize", span),
                span,
            },
        ],
        interfaces: vec![],
        methods: Vec::new(),
        inline_impls: Vec::new(),
        span,
    };

    let std_map_module = Module {
        imports: vec![],
        items: vec![
            glyph_core::ast::Item::Struct(map_bucket_struct),
            glyph_core::ast::Item::Struct(map_struct),
        ],
    };
    modules.insert("std/map".into(), std_map_module);

    modules
}
