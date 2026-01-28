use super::*;
use crate::resolver::{resolve_types, ResolverContext};
use crate::{compile_source, FrontendOptions};
use glyph_core::ast::{
    BinaryOp, Block, EnumDef, EnumVariantDef, Expr, FieldDef, Function, Ident, Item, Literal,
    MatchArm, MatchPattern, Module, Param, Stmt, StructDef, TypeExpr,
};
use glyph_core::mir::{MirInst, MirValue, Rvalue};
use glyph_core::span::Span;
use glyph_core::types::{Mutability, Type};

fn path_ty(name: &str, span: Span) -> TypeExpr {
    TypeExpr::Path {
        segments: vec![name.to_string()],
        span,
    }
}

#[test]
fn lowers_reference_expression() {
    let span = Span::new(0, 5);
    let func = Function {
        name: Ident("main".into()),
        params: vec![],
        ret_type: Some(path_ty("i32", span)),

        body: Block {
            span,
            stmts: vec![
                Stmt::Let {
                    name: Ident("p".into()),
                    ty: Some(path_ty("Point", span)),
                    mutable: false,
                    value: Some(Expr::StructLit {
                        name: Ident("Point".into()),
                        fields: vec![
                            (Ident("x".into()), Expr::Lit(Literal::Int(1), span)),
                            (Ident("y".into()), Expr::Lit(Literal::Int(2), span)),
                        ],
                        span,
                    }),
                    span,
                },
                Stmt::Let {
                    name: Ident("pref".into()),
                    ty: None,
                    mutable: false,
                    value: Some(Expr::Ref {
                        expr: Box::new(Expr::Ident(Ident("p".into()), span)),
                        mutability: Mutability::Immutable,
                        span,
                    }),
                    span,
                },
                Stmt::Ret(
                    Some(Expr::FieldAccess {
                        base: Box::new(Expr::Ident(Ident("pref".into()), span)),
                        field: Ident("x".into()),
                        span,
                    }),
                    span,
                ),
            ],
        },

        span,
    };

    let point_struct = Item::Struct(StructDef {
        name: Ident("Point".into()),
        generic_params: Vec::new(),
        fields: vec![
            FieldDef {
                name: Ident("x".into()),
                ty: path_ty("i32", span),
                span,
            },
            FieldDef {
                name: Ident("y".into()),
                ty: path_ty("i32", span),
                span,
            },
        ],
        interfaces: Vec::new(),
        methods: Vec::new(),
        inline_impls: Vec::new(),
        span,
    });

    let _module = Module {
        imports: vec![],
        items: vec![point_struct.clone(), Item::Function(func.clone())],
    };

    let module = Module {
        imports: vec![],
        items: vec![point_struct, Item::Function(func)],
    };

    let (resolver, diags) = resolve_types(&module);
    assert!(diags.is_empty());

    let (_mir, lower_diags) = lower_module(&module, &resolver);
    assert!(
        lower_diags.is_empty(),
        "unexpected diagnostics: {:?}",
        lower_diags
    );
}

#[test]
fn lowers_function_to_mir_with_return() {
    let func = Function {
        name: Ident("main".into()),
        params: vec![],
        ret_type: None,
        body: Block {
            span: Span::new(0, 4),
            stmts: vec![Stmt::Ret(None, Span::new(0, 4))],
        },
        span: Span::new(0, 4),
    };
    let module = Module {
        imports: vec![],
        items: vec![Item::Function(func)],
    };
    let (mir, diags) = lower_module(&module, &ResolverContext::default());
    assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
    assert_eq!(mir.functions.len(), 1);
    assert!(mir.functions[0]
        .blocks
        .iter()
        .any(|b| matches!(b.insts.last(), Some(MirInst::Return(_)))));
}

#[test]
fn lowers_binary_expression_to_temp_and_return() {
    let func = Function {
        name: Ident("main".into()),
        params: vec![],
        ret_type: None,
        body: Block {
            span: Span::new(0, 10),
            stmts: vec![Stmt::Ret(
                Some(Expr::Binary {
                    op: BinaryOp::Add,
                    lhs: Box::new(Expr::Lit(glyph_core::ast::Literal::Int(1), Span::new(0, 1))),
                    rhs: Box::new(Expr::Lit(glyph_core::ast::Literal::Int(2), Span::new(2, 3))),
                    span: Span::new(0, 3),
                }),
                Span::new(0, 3),
            )],
        },
        span: Span::new(0, 10),
    };
    let module = Module {
        imports: vec![],
        items: vec![Item::Function(func)],
    };
    let (mir, diags) = lower_module(&module, &ResolverContext::default());
    assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
    let insts = &mir.functions[0].blocks[0].insts;
    assert!(insts.iter().any(|i| matches!(i, MirInst::Assign { .. })));
    assert!(matches!(
        insts.last(),
        Some(MirInst::Return(Some(MirValue::Local(_))))
    ));
}

#[test]
fn lowers_struct_literal_and_field_access() {
    let span = Span::new(0, 5);
    let point_struct = Item::Struct(StructDef {
        name: Ident("Point".into()),
        generic_params: Vec::new(),
        fields: vec![
            FieldDef {
                name: Ident("x".into()),
                ty: path_ty("i32", span),
                span,
            },
            FieldDef {
                name: Ident("y".into()),
                ty: path_ty("i32", span),
                span,
            },
        ],
        interfaces: Vec::new(),
        methods: Vec::new(),
        inline_impls: Vec::new(),
        span,
    });

    let func = Function {
        name: Ident("main".into()),
        params: vec![],
        ret_type: Some(path_ty("i32", span)),
        body: Block {
            span,
            stmts: vec![
                Stmt::Let {
                    name: Ident("p".into()),
                    ty: Some(path_ty("Point", span)),
                    mutable: false,
                    value: Some(Expr::StructLit {
                        name: Ident("Point".into()),
                        fields: vec![
                            (Ident("x".into()), Expr::Lit(Literal::Int(1), span)),
                            (Ident("y".into()), Expr::Lit(Literal::Int(2), span)),
                        ],
                        span,
                    }),
                    span,
                },
                Stmt::Ret(
                    Some(Expr::FieldAccess {
                        base: Box::new(Expr::Ident(Ident("p".into()), span)),
                        field: Ident("x".into()),
                        span,
                    }),
                    span,
                ),
            ],
        },
        span,
    };

    let module = Module {
        imports: vec![],
        items: vec![point_struct, Item::Function(func)],
    };

    let (resolver, diags) = resolve_types(&module);
    assert!(diags.is_empty());

    let (mir, lower_diags) = lower_module(&module, &resolver);
    assert!(
        lower_diags.is_empty(),
        "unexpected diagnostics: {:?}",
        lower_diags
    );

    assert_eq!(mir.struct_types.len(), 1);
    let insts = &mir.functions[0].blocks[0].insts;
    assert!(insts.iter().any(|inst| matches!(
        inst,
        MirInst::Assign {
            value: Rvalue::StructLit { .. },
            ..
        }
    )));
    assert!(insts.iter().any(|inst| matches!(
        inst,
        MirInst::Assign {
            value: Rvalue::FieldAccess { field_name, .. },
            ..
        } if field_name == "x"
    )));
}

#[test]
fn lowers_match_arm_block_expression_values() {
    let span = Span::new(0, 5);

    let enum_def = Item::Enum(EnumDef {
        name: Ident("Maybe".into()),
        generic_params: Vec::new(),
        variants: vec![
            EnumVariantDef {
                name: Ident("Some".into()),
                payload: Some(path_ty("i32", span)),
                span,
            },
            EnumVariantDef {
                name: Ident("None".into()),
                payload: None,
                span,
            },
        ],
        span,
    });

    let match_expr = Expr::Match {
        scrutinee: Box::new(Expr::Ident(Ident("v".into()), span)),
        arms: vec![
            MatchArm {
                pattern: MatchPattern::Variant {
                    name: Ident("Some".into()),
                    binding: Some(Ident("n".into())),
                },
                expr: Expr::Block(Block {
                    span,
                    stmts: vec![Stmt::Expr(Expr::Ident(Ident("n".into()), span), span)],
                }),
                span,
            },
            MatchArm {
                pattern: MatchPattern::Variant {
                    name: Ident("None".into()),
                    binding: None,
                },
                expr: Expr::Block(Block {
                    span,
                    stmts: vec![Stmt::Expr(Expr::Lit(Literal::Int(0), span), span)],
                }),
                span,
            },
        ],
        span,
    };

    let func = Function {
        name: Ident("main".into()),
        params: vec![],
        ret_type: Some(path_ty("i32", span)),
        body: Block {
            span,
            stmts: vec![
                Stmt::Let {
                    name: Ident("v".into()),
                    ty: None,
                    mutable: false,
                    value: Some(Expr::Call {
                        callee: Box::new(Expr::Ident(Ident("Some".into()), span)),
                        args: vec![Expr::Lit(Literal::Int(1), span)],
                        span,
                    }),
                    span,
                },
                Stmt::Ret(Some(match_expr), span),
            ],
        },
        span,
    };

    let module = Module {
        imports: vec![],
        items: vec![enum_def, Item::Function(func)],
    };

    let (resolver, diags) = resolve_types(&module);
    assert!(diags.is_empty());

    let (mir, lower_diags) = lower_module(&module, &resolver);
    assert!(
        lower_diags.is_empty(),
        "unexpected diagnostics: {:?}",
        lower_diags
    );

    let main = mir
        .functions
        .iter()
        .find(|f| f.name == "main")
        .expect("main exists");

    let ret_local = main
        .blocks
        .iter()
        .flat_map(|b| &b.insts)
        .find_map(|inst| match inst {
            MirInst::Return(Some(MirValue::Local(id))) => Some(*id),
            _ => None,
        })
        .expect("return local");

    let has_assign = main
        .blocks
        .iter()
        .flat_map(|b| &b.insts)
        .any(|inst| matches!(inst, MirInst::Assign { local, .. } if *local == ret_local));

    assert!(has_assign, "expected match arms to assign to result local");
}

#[test]
fn struct_literal_missing_field_reports_error() {
    let span = Span::new(0, 5);
    let point_struct = Item::Struct(StructDef {
        name: Ident("Point".into()),
        generic_params: Vec::new(),
        fields: vec![
            FieldDef {
                name: Ident("x".into()),
                ty: path_ty("i32", span),
                span,
            },
            FieldDef {
                name: Ident("y".into()),
                ty: path_ty("i32", span),
                span,
            },
        ],
        interfaces: Vec::new(),
        methods: Vec::new(),
        inline_impls: Vec::new(),
        span,
    });

    let func = Function {
        name: Ident("main".into()),
        params: vec![],
        ret_type: Some(path_ty("i32", span)),
        body: Block {
            span,
            stmts: vec![Stmt::Ret(
                Some(Expr::StructLit {
                    name: Ident("Point".into()),
                    fields: vec![(Ident("x".into()), Expr::Lit(Literal::Int(1), span))],
                    span,
                }),
                span,
            )],
        },
        span,
    };

    let module = Module {
        imports: vec![],
        items: vec![point_struct, Item::Function(func)],
    };

    let (resolver, diags) = resolve_types(&module);
    assert!(diags.is_empty());

    let (_mir, lower_diags) = lower_module(&module, &resolver);
    assert!(
        lower_diags
            .iter()
            .any(|d| d.message.contains("missing field")),
        "expected missing-field diagnostic, got: {:?}",
        lower_diags
    );
}

#[test]
fn lowers_assignment_statements_to_mir() {
    let span = Span::new(0, 5);
    let func = Function {
        name: Ident("main".into()),
        params: vec![],
        ret_type: None,
        body: Block {
            span,
            stmts: vec![
                Stmt::Let {
                    name: Ident("x".into()),
                    ty: Some(path_ty("i32", span)),
                    mutable: true,
                    value: Some(Expr::Lit(Literal::Int(1), span)),
                    span,
                },
                Stmt::Assign {
                    target: Expr::Ident(Ident("x".into()), span),
                    value: Expr::Lit(Literal::Int(2), span),
                    span,
                },
                Stmt::Ret(Some(Expr::Ident(Ident("x".into()), span)), span),
            ],
        },
        span,
    };

    let module = Module {
        imports: vec![],
        items: vec![Item::Function(func)],
    };

    let (mir, diags) = lower_module(&module, &ResolverContext::default());
    assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

    let assigns: Vec<_> = mir.functions[0].blocks[0]
        .insts
        .iter()
        .filter_map(|inst| match inst {
            MirInst::Assign { local, value } => Some((*local, value.clone())),
            _ => None,
        })
        .collect();

    assert!(assigns
        .iter()
        .any(|(local, value)| { local.0 == 0 && matches!(value, Rvalue::ConstInt(2)) }));
}

#[test]
fn lowers_interface_method_call_via_impl() {
    let src = r#"
interface Drawable {
  fn draw(self: &Point) -> i32;
}

struct Point {
  x: i32
  impl Drawable {
    fn draw(self: &Point) -> i32 { ret self.x }
  }
}

fn main() -> i32 {
  let p = Point { x: 5 }
  ret p.draw()
}
"#;

    let out = compile_source(
        src,
        FrontendOptions {
            emit_mir: true,
            include_std: false,
        },
    );
    assert!(
        out.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        out.diagnostics
    );

    let main_fn = out
        .mir
        .functions
        .iter()
        .find(|f| f.name == "main")
        .expect("main function present");

    let (call_name, args) = main_fn
        .blocks
        .iter()
        .flat_map(|b| &b.insts)
        .find_map(|inst| match inst {
            MirInst::Assign {
                value: Rvalue::Call { name, args },
                ..
            } => Some((name.clone(), args.clone())),
            _ => None,
        })
        .expect("call lowered");

    assert_eq!(call_name, "Point::Drawable::draw");
    assert_eq!(args.len(), 1);

    if let MirValue::Local(local) = &args[0] {
        let ty = main_fn.locals[local.0 as usize]
            .ty
            .as_ref()
            .expect("receiver type");
        assert_eq!(
            ty,
            &Type::Ref(Box::new(Type::Named("Point".into())), Mutability::Immutable)
        );
    } else {
        panic!("expected receiver argument to be a local");
    }
}

#[test]
fn lowers_extern_function_call() {
    let module = Module {
        imports: vec![],
        items: vec![
            Item::ExternFunction(glyph_core::ast::ExternFunctionDecl {
                abi: Some("C".into()),
                name: Ident("puts".into()),
                params: vec![Param {
                    name: Ident("msg".into()),
                    ty: Some(path_ty("i32", Span::new(0, 0))),
                    span: Span::new(0, 0),
                }],
                ret_type: None,
                link_name: None,
                span: Span::new(0, 0),
            }),
            Item::Function(Function {
                name: Ident("main".into()),
                params: vec![],
                ret_type: Some(path_ty("i32", Span::new(0, 0))),
                body: Block {
                    span: Span::new(0, 0),
                    stmts: vec![
                        Stmt::Expr(
                            Expr::Call {
                                callee: Box::new(Expr::Ident(
                                    Ident("puts".into()),
                                    Span::new(0, 0),
                                )),
                                args: vec![Expr::Lit(Literal::Int(1), Span::new(0, 0))],
                                span: Span::new(0, 0),
                            },
                            Span::new(0, 0),
                        ),
                        Stmt::Ret(
                            Some(Expr::Lit(Literal::Int(0), Span::new(0, 0))),
                            Span::new(0, 0),
                        ),
                    ],
                },
                span: Span::new(0, 0),
            }),
        ],
    };

    let resolver = ResolverContext::default();
    let (mir, diags) = lower_module(&module, &resolver);
    assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
    assert_eq!(mir.extern_functions.len(), 1);
    assert_eq!(mir.extern_functions[0].name, "puts");

    let main = mir
        .functions
        .iter()
        .find(|f| f.name == "main")
        .expect("main exists");
    let mut found_call = false;
    for block in &main.blocks {
        for inst in &block.insts {
            if let MirInst::Assign {
                value: Rvalue::Call { name, .. },
                ..
            } = inst
            {
                assert_eq!(name, "puts");
                found_call = true;
            }
        }
    }
    assert!(found_call, "call to extern function not lowered");
}

#[test]
fn extern_function_wrong_arity_reports_error() {
    let module = Module {
        imports: vec![],
        items: vec![
            Item::ExternFunction(glyph_core::ast::ExternFunctionDecl {
                abi: Some("C".into()),
                name: Ident("puts".into()),
                params: vec![Param {
                    name: Ident("msg".into()),
                    ty: Some(path_ty("i32", Span::new(0, 0))),
                    span: Span::new(0, 0),
                }],
                ret_type: None,
                link_name: None,
                span: Span::new(0, 0),
            }),
            Item::Function(Function {
                name: Ident("main".into()),
                params: vec![],
                ret_type: Some(path_ty("i32", Span::new(0, 0))),
                body: Block {
                    span: Span::new(0, 0),
                    stmts: vec![
                        Stmt::Expr(
                            Expr::Call {
                                callee: Box::new(Expr::Ident(
                                    Ident("puts".into()),
                                    Span::new(0, 0),
                                )),
                                args: vec![],
                                span: Span::new(0, 0),
                            },
                            Span::new(0, 0),
                        ),
                        Stmt::Ret(
                            Some(Expr::Lit(Literal::Int(0), Span::new(0, 0))),
                            Span::new(0, 0),
                        ),
                    ],
                },
                span: Span::new(0, 0),
            }),
        ],
    };

    let resolver = ResolverContext::default();
    let (_mir, diags) = lower_module(&module, &resolver);
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("expects 1 arguments but got 0")),
        "expected arity diagnostic, got {:?}",
        diags
    );
}

#[test]
fn lowers_usize_add_literal_to_usize_temp() {
    let span = Span::new(0, 0);
    let func = Function {
        name: Ident("main".into()),
        params: vec![],
        ret_type: Some(path_ty("usize", span)),
        body: Block {
            span,
            stmts: vec![
                Stmt::Let {
                    name: Ident("pos".into()),
                    ty: Some(path_ty("usize", span)),
                    mutable: false,
                    value: Some(Expr::Lit(Literal::Int(0), span)),
                    span,
                },
                Stmt::Ret(
                    Some(Expr::Binary {
                        op: BinaryOp::Add,
                        lhs: Box::new(Expr::Ident(Ident("pos".into()), span)),
                        rhs: Box::new(Expr::Lit(Literal::Int(1), span)),
                        span,
                    }),
                    span,
                ),
            ],
        },
        span,
    };

    let module = Module {
        imports: vec![],
        items: vec![Item::Function(func)],
    };

    let (mir, diags) = lower_module(&module, &ResolverContext::default());
    assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

    let main = mir
        .functions
        .iter()
        .find(|f| f.name == "main")
        .expect("main exists");

    let mut found = false;
    for block in &main.blocks {
        for inst in &block.insts {
            if let MirInst::Assign {
                local,
                value: Rvalue::Binary {
                    op: BinaryOp::Add, ..
                },
            } = inst
            {
                let ty = main.locals[local.0 as usize].ty.as_ref();
                assert_eq!(ty, Some(&Type::Usize));
                found = true;
            }
        }
    }
    assert!(found, "expected an Add binary temp assignment");
}
