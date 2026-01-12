use std::collections::HashMap;

use glyph_core::ast::{
    EnumDef, EnumVariantDef, ExternFunctionDecl, Ident, Import, ImportKind, ImportPath, InterfaceDef,
    InterfaceMethod, Module, Param, StructDef,
};
use glyph_core::span::Span;

/// Return the built-in std modules shipped with the compiler.
/// Currently includes:
/// - std (re-exports std::io)
/// - std/io with `extern "C" fn puts(msg: str) -> i32;`
pub fn std_modules() -> HashMap<String, Module> {
    let mut modules = HashMap::new();

    let span = Span::new(0, 0);

    // std::io with puts
    let stdout_struct = StructDef {
        name: Ident("Stdout".into()),
        fields: vec![glyph_core::ast::FieldDef {
            name: Ident("fd".into()),
            ty: Ident("i32".into()),
            span,
        }],
        interfaces: vec![Ident("Write".into())],
        methods: vec![glyph_core::ast::Function {
            name: Ident("write_all".into()),
            params: vec![
                Param {
                    name: Ident("self".into()),
                    ty: Some(Ident("&mut Stdout".into())),
                    span,
                },
                Param {
                    name: Ident("bytes".into()),
                    ty: Some(Ident("RawPtr<u8>".into())),
                    span,
                },
                Param {
                    name: Ident("len".into()),
                    ty: Some(Ident("u32".into())),
                    span,
                },
            ],
            ret_type: None,
            body: glyph_core::ast::Block {
                span,
                stmts: vec![glyph_core::ast::Stmt::Expr(
                    glyph_core::ast::Expr::Call {
                        callee: Box::new(glyph_core::ast::Expr::Ident(
                            Ident("std::io::raw_write".into()),
                            span,
                        )),
                        args: vec![
                            glyph_core::ast::Expr::FieldAccess {
                                base: Box::new(glyph_core::ast::Expr::Ident(
                                    Ident("self".into()),
                                    span,
                                )),
                                field: Ident("fd".into()),
                                span,
                            },
                            glyph_core::ast::Expr::Ident(Ident("bytes".into()), span),
                            glyph_core::ast::Expr::Ident(Ident("len".into()), span),
                        ],
                        span,
                    },
                    span,
                )],
            },
            span,
        }],
        inline_impls: vec![glyph_core::ast::InlineImpl {
            interface: Ident("Write".into()),
            methods: vec![glyph_core::ast::Function {
                name: Ident("write_all".into()),
                params: vec![
                    Param {
                        name: Ident("self".into()),
                        ty: Some(Ident("&mut Stdout".into())),
                        span,
                    },
                    Param {
                        name: Ident("bytes".into()),
                        ty: Some(Ident("RawPtr<u8>".into())),
                        span,
                    },
                    Param {
                        name: Ident("len".into()),
                        ty: Some(Ident("u32".into())),
                        span,
                    },
                ],
                ret_type: None,
                body: glyph_core::ast::Block {
                    span,
                    stmts: vec![glyph_core::ast::Stmt::Expr(
                        glyph_core::ast::Expr::Call {
                            callee: Box::new(glyph_core::ast::Expr::Ident(
                                Ident("std::io::raw_write".into()),
                                span,
                            )),
                            args: vec![
                                glyph_core::ast::Expr::FieldAccess {
                                    base: Box::new(glyph_core::ast::Expr::Ident(
                                        Ident("self".into()),
                                        span,
                                    )),
                                    field: Ident("fd".into()),
                                    span,
                                },
                                glyph_core::ast::Expr::Ident(Ident("bytes".into()), span),
                                glyph_core::ast::Expr::Ident(Ident("len".into()), span),
                            ],
                            span,
                        },
                        span,
                    )],
                },
                span,
            }],
            span,
        }],
        span,
    };

    let stderr_struct = StructDef {
        name: Ident("Stderr".into()),
        fields: vec![glyph_core::ast::FieldDef {
            name: Ident("fd".into()),
            ty: Ident("i32".into()),
            span,
        }],
        interfaces: vec![Ident("Write".into())],
        methods: vec![glyph_core::ast::Function {
            name: Ident("write_all".into()),
            params: vec![
                Param {
                    name: Ident("self".into()),
                    ty: Some(Ident("&mut Stderr".into())),
                    span,
                },
                Param {
                    name: Ident("bytes".into()),
                    ty: Some(Ident("RawPtr<u8>".into())),
                    span,
                },
                Param {
                    name: Ident("len".into()),
                    ty: Some(Ident("u32".into())),
                    span,
                },
            ],
            ret_type: None,
            body: glyph_core::ast::Block {
                span,
                stmts: vec![glyph_core::ast::Stmt::Expr(
                    glyph_core::ast::Expr::Call {
                        callee: Box::new(glyph_core::ast::Expr::Ident(
                            Ident("std::io::raw_write".into()),
                            span,
                        )),
                        args: vec![
                            glyph_core::ast::Expr::FieldAccess {
                                base: Box::new(glyph_core::ast::Expr::Ident(
                                    Ident("self".into()),
                                    span,
                                )),
                                field: Ident("fd".into()),
                                span,
                            },
                            glyph_core::ast::Expr::Ident(Ident("bytes".into()), span),
                            glyph_core::ast::Expr::Ident(Ident("len".into()), span),
                        ],
                        span,
                    },
                    span,
                )],
            },
            span,
        }],
        inline_impls: vec![glyph_core::ast::InlineImpl {
            interface: Ident("Write".into()),
            methods: vec![glyph_core::ast::Function {
                name: Ident("write_all".into()),
                params: vec![
                    Param {
                        name: Ident("self".into()),
                        ty: Some(Ident("&mut Stderr".into())),
                        span,
                    },
                    Param {
                        name: Ident("bytes".into()),
                        ty: Some(Ident("RawPtr<u8>".into())),
                        span,
                    },
                    Param {
                        name: Ident("len".into()),
                        ty: Some(Ident("u32".into())),
                        span,
                    },
                ],
                ret_type: None,
                body: glyph_core::ast::Block {
                    span,
                    stmts: vec![glyph_core::ast::Stmt::Expr(
                        glyph_core::ast::Expr::Call {
                            callee: Box::new(glyph_core::ast::Expr::Ident(
                                Ident("std::io::raw_write".into()),
                                span,
                            )),
                            args: vec![
                                glyph_core::ast::Expr::FieldAccess {
                                    base: Box::new(glyph_core::ast::Expr::Ident(
                                        Ident("self".into()),
                                        span,
                                    )),
                                    field: Ident("fd".into()),
                                    span,
                                },
                                glyph_core::ast::Expr::Ident(Ident("bytes".into()), span),
                                glyph_core::ast::Expr::Ident(Ident("len".into()), span),
                            ],
                            span,
                        },
                        span,
                    )],
                },
                span,
            }],
            span,
        }],
        span,
    };

    let puts_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("puts".into()),
        params: vec![Param {
            name: Ident("msg".into()),
            ty: Some(Ident("str".into())),
            span,
        }],
        ret_type: Some(Ident("i32".into())),
        link_name: None,
        span,
    };

    let fopen_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("fopen".into()),
        params: vec![
            Param {
                name: Ident("path".into()),
                ty: Some(Ident("str".into())),
                span,
            },
            Param {
                name: Ident("mode".into()),
                ty: Some(Ident("str".into())),
                span,
            },
        ],
        ret_type: Some(Ident("RawPtr<u8>".into())),
        link_name: None,
        span,
    };

    // read/write map to libc fread/fwrite with 1-byte element size and caller-provided count.
    let read_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("read".into()),
        params: vec![
            Param {
                name: Ident("buf".into()),
                ty: Some(Ident("RawPtr<u8>".into())),
                span,
            },
            Param {
                name: Ident("size".into()),
                ty: Some(Ident("u32".into())),
                span,
            },
            Param {
                name: Ident("nmemb".into()),
                ty: Some(Ident("u32".into())),
                span,
            },
            Param {
                name: Ident("file".into()),
                ty: Some(Ident("RawPtr<u8>".into())),
                span,
            },
        ],
        ret_type: Some(Ident("u32".into())),
        link_name: Some("fread".into()),
        span,
    };

    let raw_write_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("raw_write".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
            Param {
                name: Ident("buf".into()),
                ty: Some(Ident("RawPtr<u8>".into())),
                span,
            },
            Param {
                name: Ident("len".into()),
                ty: Some(Ident("u32".into())),
                span,
            },
        ],
        ret_type: Some(Ident("i32".into())),
        link_name: Some("write".into()),
        span,
    };

    // Legacy fwrite-style write for existing callers.
    let write_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("write".into()),
        params: vec![
            Param {
                name: Ident("buf".into()),
                ty: Some(Ident("RawPtr<u8>".into())),
                span,
            },
            Param {
                name: Ident("size".into()),
                ty: Some(Ident("u32".into())),
                span,
            },
            Param {
                name: Ident("nmemb".into()),
                ty: Some(Ident("u32".into())),
                span,
            },
            Param {
                name: Ident("file".into()),
                ty: Some(Ident("RawPtr<u8>".into())),
                span,
            },
        ],
        ret_type: Some(Ident("u32".into())),
        link_name: Some("fwrite".into()),
        span,
    };

    // Formatting helpers that convert values to bytes and write them
    let fmt_write_i32_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("fmt_write_i32".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
            Param {
                name: Ident("value".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
        ],
        ret_type: Some(Ident("i32".into())),
        link_name: Some("glyph_fmt_write_i32".into()),
        span,
    };

    let fmt_write_u32_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("fmt_write_u32".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
            Param {
                name: Ident("value".into()),
                ty: Some(Ident("u32".into())),
                span,
            },
        ],
        ret_type: Some(Ident("i32".into())),
        link_name: Some("glyph_fmt_write_u32".into()),
        span,
    };

    let fmt_write_i64_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("fmt_write_i64".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
            Param {
                name: Ident("value".into()),
                ty: Some(Ident("i64".into())),
                span,
            },
        ],
        ret_type: Some(Ident("i32".into())),
        link_name: Some("glyph_fmt_write_i64".into()),
        span,
    };

    let fmt_write_u64_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("fmt_write_u64".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
            Param {
                name: Ident("value".into()),
                ty: Some(Ident("u64".into())),
                span,
            },
        ],
        ret_type: Some(Ident("i32".into())),
        link_name: Some("glyph_fmt_write_u64".into()),
        span,
    };

    let fmt_write_bool_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("fmt_write_bool".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
            Param {
                name: Ident("value".into()),
                ty: Some(Ident("bool".into())),
                span,
            },
        ],
        ret_type: Some(Ident("i32".into())),
        link_name: Some("glyph_fmt_write_bool".into()),
        span,
    };

    let fmt_write_str_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("fmt_write_str".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
            Param {
                name: Ident("value".into()),
                ty: Some(Ident("str".into())),
                span,
            },
        ],
        ret_type: Some(Ident("i32".into())),
        link_name: Some("glyph_fmt_write_str".into()),
        span,
    };

    let fmt_write_char_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("fmt_write_char".into()),
        params: vec![
            Param {
                name: Ident("fd".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
            Param {
                name: Ident("value".into()),
                ty: Some(Ident("char".into())),
                span,
            },
        ],
        ret_type: Some(Ident("i32".into())),
        link_name: Some("glyph_fmt_write_char".into()),
        span,
    };

    let std_io = Module {
        imports: vec![],
        items: vec![
            glyph_core::ast::Item::Interface(InterfaceDef {
                name: Ident("Write".into()),
                methods: vec![InterfaceMethod {
                    name: Ident("write_all".into()),
                    params: vec![
                        Param {
                            name: Ident("self".into()),
                            ty: Some(Ident("&mut Write".into())),
                            span,
                        },
                        Param {
                            name: Ident("bytes".into()),
                            ty: Some(Ident("RawPtr<u8>".into())),
                            span,
                        },
                        Param {
                            name: Ident("len".into()),
                            ty: Some(Ident("u32".into())),
                            span,
                        },
                    ],
                    ret_type: None,
                    span,
                }],
                span,
            }),
            glyph_core::ast::Item::Struct(stdout_struct),
            glyph_core::ast::Item::Struct(stderr_struct),
            glyph_core::ast::Item::ExternFunction(puts_decl),
            glyph_core::ast::Item::ExternFunction(fopen_decl),
            glyph_core::ast::Item::ExternFunction(read_decl),
            glyph_core::ast::Item::ExternFunction(raw_write_decl),
            glyph_core::ast::Item::ExternFunction(write_decl),
            glyph_core::ast::Item::ExternFunction(fmt_write_i32_decl),
            glyph_core::ast::Item::ExternFunction(fmt_write_u32_decl),
            glyph_core::ast::Item::ExternFunction(fmt_write_i64_decl),
            glyph_core::ast::Item::ExternFunction(fmt_write_u64_decl),
            glyph_core::ast::Item::ExternFunction(fmt_write_bool_decl),
            glyph_core::ast::Item::ExternFunction(fmt_write_str_decl),
            glyph_core::ast::Item::ExternFunction(fmt_write_char_decl),
            // Helpers to construct writers
            glyph_core::ast::Item::Function(glyph_core::ast::Function {
                name: Ident("stdout".into()),
                params: vec![],
                ret_type: Some(Ident("Stdout".into())),
                body: glyph_core::ast::Block {
                    span,
                    stmts: vec![glyph_core::ast::Stmt::Ret(
                        Some(glyph_core::ast::Expr::StructLit {
                            name: Ident("Stdout".into()),
                            fields: vec![(
                                Ident("fd".into()),
                                glyph_core::ast::Expr::Lit(glyph_core::ast::Literal::Int(1), span),
                            )],
                            span,
                        }),
                        span,
                    )],
                },
                span,
            }),
            glyph_core::ast::Item::Function(glyph_core::ast::Function {
                name: Ident("stderr".into()),
                params: vec![],
                ret_type: Some(Ident("Stderr".into())),
                body: glyph_core::ast::Block {
                    span,
                    stmts: vec![glyph_core::ast::Stmt::Ret(
                        Some(glyph_core::ast::Expr::StructLit {
                            name: Ident("Stderr".into()),
                            fields: vec![(
                                Ident("fd".into()),
                                glyph_core::ast::Expr::Lit(glyph_core::ast::Literal::Int(2), span),
                            )],
                            span,
                        }),
                        span,
                    )],
                },
                span,
            }),
        ],
    };
    modules.insert("std/io".into(), std_io);

    // std::string with strdup
    let strdup_decl = ExternFunctionDecl {
        abi: Some("C".into()),
        name: Ident("strdup".into()),
        params: vec![Param {
            name: Ident("s".into()),
            ty: Some(Ident("str".into())),
            span,
        }],
        ret_type: Some(Ident("String".into())),
        link_name: None,
        span,
    };

    let std_string = Module {
        imports: vec![],
        items: vec![glyph_core::ast::Item::ExternFunction(strdup_decl)],
    };
    modules.insert("std/string".into(), std_string);

    // std that exposes std::io via wildcard import
    let import_std_io = Import {
        kind: ImportKind::Wildcard,
        path: ImportPath {
            segments: vec!["std".into(), "io".into()],
            span,
        },
        span,
    };
    let import_std_string = Import {
        kind: ImportKind::Wildcard,
        path: ImportPath {
            segments: vec!["std".into(), "string".into()],
            span,
        },
        span,
    };
    let import_std_enums = Import {
        kind: ImportKind::Wildcard,
        path: ImportPath {
            segments: vec!["std".into(), "enums".into()],
            span,
        },
        span,
    };

    // Format is modeled as free functions per type to avoid interface objects for now.
    let fmt_i32 = glyph_core::ast::Function {
        name: Ident("fmt_i32".into()),
        params: vec![
            Param {
                name: Ident("v".into()),
                ty: Some(Ident("i32".into())),
                span,
            },
            Param {
                name: Ident("w".into()),
                ty: Some(Ident("&mut Stdout".into())),
                span,
            },
        ],
        ret_type: None,
        body: glyph_core::ast::Block {
            span,
            stmts: vec![glyph_core::ast::Stmt::Expr(
                glyph_core::ast::Expr::Call {
                    callee: Box::new(glyph_core::ast::Expr::Ident(
                        Ident("std::io::fmt_write_i32".into()),
                        span,
                    )),
                    args: vec![
                        glyph_core::ast::Expr::FieldAccess {
                            base: Box::new(glyph_core::ast::Expr::Ident(
                                Ident("w".into()),
                                span,
                            )),
                            field: Ident("fd".into()),
                            span,
                        },
                        glyph_core::ast::Expr::Ident(Ident("v".into()), span),
                    ],
                    span,
                },
                span,
            )],
        },
        span,
    };

    let fmt_u32 = glyph_core::ast::Function {
        name: Ident("fmt_u32".into()),
        params: vec![
            Param {
                name: Ident("v".into()),
                ty: Some(Ident("u32".into())),
                span,
            },
            Param {
                name: Ident("w".into()),
                ty: Some(Ident("&mut Stdout".into())),
                span,
            },
        ],
        ret_type: None,
        body: glyph_core::ast::Block {
            span,
            stmts: vec![glyph_core::ast::Stmt::Expr(
                glyph_core::ast::Expr::Call {
                    callee: Box::new(glyph_core::ast::Expr::Ident(
                        Ident("std::io::fmt_write_u32".into()),
                        span,
                    )),
                    args: vec![
                        glyph_core::ast::Expr::FieldAccess {
                            base: Box::new(glyph_core::ast::Expr::Ident(
                                Ident("w".into()),
                                span,
                            )),
                            field: Ident("fd".into()),
                            span,
                        },
                        glyph_core::ast::Expr::Ident(Ident("v".into()), span),
                    ],
                    span,
                },
                span,
            )],
        },
        span,
    };

    let fmt_i64 = glyph_core::ast::Function {
        name: Ident("fmt_i64".into()),
        params: vec![
            Param {
                name: Ident("v".into()),
                ty: Some(Ident("i64".into())),
                span,
            },
            Param {
                name: Ident("w".into()),
                ty: Some(Ident("&mut Stdout".into())),
                span,
            },
        ],
        ret_type: None,
        body: glyph_core::ast::Block {
            span,
            stmts: vec![glyph_core::ast::Stmt::Expr(
                glyph_core::ast::Expr::Call {
                    callee: Box::new(glyph_core::ast::Expr::Ident(
                        Ident("std::io::fmt_write_i64".into()),
                        span,
                    )),
                    args: vec![
                        glyph_core::ast::Expr::FieldAccess {
                            base: Box::new(glyph_core::ast::Expr::Ident(
                                Ident("w".into()),
                                span,
                            )),
                            field: Ident("fd".into()),
                            span,
                        },
                        glyph_core::ast::Expr::Ident(Ident("v".into()), span),
                    ],
                    span,
                },
                span,
            )],
        },
        span,
    };

    let fmt_u64 = glyph_core::ast::Function {
        name: Ident("fmt_u64".into()),
        params: vec![
            Param {
                name: Ident("v".into()),
                ty: Some(Ident("u64".into())),
                span,
            },
            Param {
                name: Ident("w".into()),
                ty: Some(Ident("&mut Stdout".into())),
                span,
            },
        ],
        ret_type: None,
        body: glyph_core::ast::Block {
            span,
            stmts: vec![glyph_core::ast::Stmt::Expr(
                glyph_core::ast::Expr::Call {
                    callee: Box::new(glyph_core::ast::Expr::Ident(
                        Ident("std::io::fmt_write_u64".into()),
                        span,
                    )),
                    args: vec![
                        glyph_core::ast::Expr::FieldAccess {
                            base: Box::new(glyph_core::ast::Expr::Ident(
                                Ident("w".into()),
                                span,
                            )),
                            field: Ident("fd".into()),
                            span,
                        },
                        glyph_core::ast::Expr::Ident(Ident("v".into()), span),
                    ],
                    span,
                },
                span,
            )],
        },
        span,
    };

    let fmt_bool = glyph_core::ast::Function {
        name: Ident("fmt_bool".into()),
        params: vec![
            Param {
                name: Ident("v".into()),
                ty: Some(Ident("bool".into())),
                span,
            },
            Param {
                name: Ident("w".into()),
                ty: Some(Ident("&mut Stdout".into())),
                span,
            },
        ],
        ret_type: None,
        body: glyph_core::ast::Block {
            span,
            stmts: vec![glyph_core::ast::Stmt::Expr(
                glyph_core::ast::Expr::Call {
                    callee: Box::new(glyph_core::ast::Expr::Ident(
                        Ident("std::io::fmt_write_bool".into()),
                        span,
                    )),
                    args: vec![
                        glyph_core::ast::Expr::FieldAccess {
                            base: Box::new(glyph_core::ast::Expr::Ident(
                                Ident("w".into()),
                                span,
                            )),
                            field: Ident("fd".into()),
                            span,
                        },
                        glyph_core::ast::Expr::Ident(Ident("v".into()), span),
                    ],
                    span,
                },
                span,
            )],
        },
        span,
    };

    let fmt_str = glyph_core::ast::Function {
        name: Ident("fmt_str".into()),
        params: vec![
            Param {
                name: Ident("v".into()),
                ty: Some(Ident("str".into())),
                span,
            },
            Param {
                name: Ident("w".into()),
                ty: Some(Ident("&mut Stdout".into())),
                span,
            },
        ],
        ret_type: None,
        body: glyph_core::ast::Block {
            span,
            stmts: vec![glyph_core::ast::Stmt::Expr(
                glyph_core::ast::Expr::Call {
                    callee: Box::new(glyph_core::ast::Expr::Ident(
                        Ident("std::io::fmt_write_str".into()),
                        span,
                    )),
                    args: vec![
                        glyph_core::ast::Expr::FieldAccess {
                            base: Box::new(glyph_core::ast::Expr::Ident(
                                Ident("w".into()),
                                span,
                            )),
                            field: Ident("fd".into()),
                            span,
                        },
                        glyph_core::ast::Expr::Ident(Ident("v".into()), span),
                    ],
                    span,
                },
                span,
            )],
        },
        span,
    };

    let fmt_char = glyph_core::ast::Function {
        name: Ident("fmt_char".into()),
        params: vec![
            Param {
                name: Ident("v".into()),
                ty: Some(Ident("char".into())),
                span,
            },
            Param {
                name: Ident("w".into()),
                ty: Some(Ident("&mut Stdout".into())),
                span,
            },
        ],
        ret_type: None,
        body: glyph_core::ast::Block {
            span,
            stmts: vec![glyph_core::ast::Stmt::Expr(
                glyph_core::ast::Expr::Call {
                    callee: Box::new(glyph_core::ast::Expr::Ident(
                        Ident("std::io::fmt_write_char".into()),
                        span,
                    )),
                    args: vec![
                        glyph_core::ast::Expr::FieldAccess {
                            base: Box::new(glyph_core::ast::Expr::Ident(
                                Ident("w".into()),
                                span,
                            )),
                            field: Ident("fd".into()),
                            span,
                        },
                        glyph_core::ast::Expr::Ident(Ident("v".into()), span),
                    ],
                    span,
                },
                span,
            )],
        },
        span,
    };

    let import_io_for_fmt = Import {
        kind: ImportKind::Wildcard,
        path: ImportPath {
            segments: vec!["std".into(), "io".into()],
            span,
        },
        span,
    };

    let std_fmt = Module {
        imports: vec![import_io_for_fmt],
        items: vec![
            glyph_core::ast::Item::Function(fmt_i32),
            glyph_core::ast::Item::Function(fmt_u32),
            glyph_core::ast::Item::Function(fmt_i64),
            glyph_core::ast::Item::Function(fmt_u64),
            glyph_core::ast::Item::Function(fmt_bool),
            glyph_core::ast::Item::Function(fmt_str),
            glyph_core::ast::Item::Function(fmt_char),
        ],
    };
    modules.insert("std/fmt".into(), std_fmt);

    let import_std_fmt = Import {
        kind: ImportKind::Wildcard,
        path: ImportPath {
            segments: vec!["std".into(), "fmt".into()],
            span,
        },
        span,
    };

    let std_root = Module {
        imports: vec![import_std_io, import_std_string, import_std_enums, import_std_fmt],
        items: vec![
            glyph_core::ast::Item::Function(glyph_core::ast::Function {
                name: Ident("print".into()),
                params: vec![Param {
                    name: Ident("msg".into()),
                    ty: Some(Ident("str".into())),
                    span,
                }],
                ret_type: None,
                body: glyph_core::ast::Block {
                    span,
                    stmts: vec![
                        glyph_core::ast::Stmt::Expr(
                            glyph_core::ast::Expr::Call {
                                callee: Box::new(glyph_core::ast::Expr::Ident(
                                    Ident("std::io::puts".into()),
                                    span,
                                )),
                                args: vec![glyph_core::ast::Expr::Ident(
                                    Ident("msg".into()),
                                    span,
                                )],
                                span,
                            },
                            span,
                        ),
                        glyph_core::ast::Stmt::Ret(None, span),
                    ],
                },
                span,
            }),
            glyph_core::ast::Item::Function(glyph_core::ast::Function {
                name: Ident("println".into()),
                params: vec![Param {
                    name: Ident("msg".into()),
                    ty: Some(Ident("str".into())),
                    span,
                }],
                ret_type: None,
                body: glyph_core::ast::Block {
                    span,
                    stmts: vec![
                        glyph_core::ast::Stmt::Expr(
                            glyph_core::ast::Expr::Call {
                                callee: Box::new(glyph_core::ast::Expr::Ident(
                                    Ident("std::io::puts".into()),
                                    span,
                                )),
                                args: vec![glyph_core::ast::Expr::Ident(
                                    Ident("msg".into()),
                                    span,
                                )],
                                span,
                            },
                            span,
                        ),
                        glyph_core::ast::Stmt::Ret(None, span),
                    ],
                },
                span,
            }),
            glyph_core::ast::Item::Function(glyph_core::ast::Function {
                name: Ident("eprint".into()),
                params: vec![Param {
                    name: Ident("msg".into()),
                    ty: Some(Ident("str".into())),
                    span,
                }],
                ret_type: None,
                body: glyph_core::ast::Block {
                    span,
                    stmts: vec![
                        glyph_core::ast::Stmt::Expr(
                            glyph_core::ast::Expr::Call {
                                callee: Box::new(glyph_core::ast::Expr::Ident(
                                    Ident("std::io::puts".into()),
                                    span,
                                )),
                                args: vec![glyph_core::ast::Expr::Ident(
                                    Ident("msg".into()),
                                    span,
                                )],
                                span,
                            },
                            span,
                        ),
                        glyph_core::ast::Stmt::Ret(None, span),
                    ],
                },
                span,
            }),
            glyph_core::ast::Item::Function(glyph_core::ast::Function {
                name: Ident("eprintln".into()),
                params: vec![Param {
                    name: Ident("msg".into()),
                    ty: Some(Ident("str".into())),
                    span,
                }],
                ret_type: None,
                body: glyph_core::ast::Block {
                    span,
                    stmts: vec![
                        glyph_core::ast::Stmt::Expr(
                            glyph_core::ast::Expr::Call {
                                callee: Box::new(glyph_core::ast::Expr::Ident(
                                    Ident("std::io::puts".into()),
                                    span,
                                )),
                                args: vec![glyph_core::ast::Expr::Ident(
                                    Ident("msg".into()),
                                    span,
                                )],
                                span,
                            },
                            span,
                        ),
                        glyph_core::ast::Stmt::Ret(None, span),
                    ],
                },
                span,
            }),
        ],
    };
    modules.insert("std".into(), std_root);

    // std::enums module with Option/Result (monomorphic for now)
    let option_enum = EnumDef {
        name: Ident("Option".into()),
        variants: vec![
            EnumVariantDef {
                name: Ident("None".into()),
                payload: None,
                span,
            },
            EnumVariantDef {
                name: Ident("Some".into()),
                payload: Some(Ident("i32".into())),
                span,
            },
        ],
        span,
    };

    let result_enum = EnumDef {
        name: Ident("Result".into()),
        variants: vec![
            EnumVariantDef {
                name: Ident("Ok".into()),
                payload: Some(Ident("i32".into())),
                span,
            },
            EnumVariantDef {
                name: Ident("Err".into()),
                payload: Some(Ident("i32".into())),
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
