use glyph_core::ast::Expr;
use glyph_core::mir::{MirInst, MirValue, Rvalue};
use glyph_core::span::Span;
use glyph_core::types::{Mutability, Type};

use super::super::context::LowerCtx;
use super::super::expr::lower_value;
use super::super::value::infer_value_type;

#[derive(Clone)]
enum PrintSegment<'a> {
    Literal(String),
    Expr(&'a Expr, Span),
}

fn resolve_builtin_target(ctx: &mut LowerCtx<'_>, key: &str, span: Span) -> Option<String> {
    if let Some(sig) = ctx.fn_sigs.get(key) {
        Some(sig.target_name.clone())
    } else {
        ctx.error(format!("unknown function '{}'", key), Some(span));
        None
    }
}

pub(crate) fn lower_print_builtin<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
    add_newline: bool,
    is_err: bool,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("print/println expects exactly one argument", Some(span));
        return None;
    }

    let fd = if is_err { 2 } else { 1 };
    let arg = &args[0];

    let mut segments: Vec<PrintSegment<'a>> = Vec::new();
    match arg {
        Expr::Lit(glyph_core::ast::Literal::Str(s), _) => {
            segments.push(PrintSegment::Literal(s.clone()));
        }
        Expr::InterpString {
            segments: segs,
            span: interp_span,
        } => {
            for seg in segs {
                match seg {
                    glyph_core::ast::InterpSegment::Literal(s) => {
                        segments.push(PrintSegment::Literal(s.clone()))
                    }
                    glyph_core::ast::InterpSegment::Expr(e) => {
                        segments.push(PrintSegment::Expr(e, *interp_span))
                    }
                }
            }
        }
        _ => {
            ctx.error(
                "print/println currently require a string or interpolated string",
                Some(span),
            );
            return None;
        }
    }

    if segments.is_empty() {
        segments.push(PrintSegment::Literal(String::new()));
    }

    if add_newline {
        segments.push(PrintSegment::Literal("\n".into()));
    }

    // Build writer: Stdout or Stderr
    let writer_struct = if is_err { "Stderr" } else { "Stdout" };
    let writer_local = ctx.fresh_local(None);
    ctx.locals[writer_local.0 as usize].ty = Some(Type::Named(writer_struct.to_string()));
    ctx.push_inst(MirInst::Assign {
        local: writer_local,
        value: Rvalue::StructLit {
            struct_name: writer_struct.to_string(),
            field_values: vec![("fd".into(), MirValue::Int(fd))],
        },
    });

    let mut last = None;
    for seg in segments {
        match seg {
            PrintSegment::Literal(s) => {
                let raw_write = resolve_builtin_target(ctx, "std::io::raw_write", span)?;
                let str_local = ctx.fresh_local(None);
                ctx.locals[str_local.0 as usize].ty = Some(Type::Str);
                let global = ctx.fresh_string_global();
                ctx.push_inst(MirInst::Assign {
                    local: str_local,
                    value: Rvalue::StringLit {
                        content: s.clone(),
                        global_name: global,
                    },
                });
                let out = ctx.fresh_local(None);
                ctx.locals[out.0 as usize].ty = Some(Type::I32);
                ctx.push_inst(MirInst::Assign {
                    local: out,
                    value: Rvalue::Call {
                        name: raw_write,
                        args: vec![
                            MirValue::Int(fd),
                            MirValue::Local(str_local),
                            MirValue::Int(s.len() as i64),
                        ],
                    },
                });
                last = Some(out);
            }
            PrintSegment::Expr(expr, seg_span) => {
                let val = lower_value(ctx, expr)?;
                let val_ty = infer_value_type(&val, ctx);

                // Prepare &mut writer for all formatting calls
                let writer_ref = ctx.fresh_local(None);
                ctx.locals[writer_ref.0 as usize].ty = Some(Type::Ref(
                    Box::new(Type::Named(writer_struct.to_string())),
                    Mutability::Mutable,
                ));
                ctx.push_inst(MirInst::Assign {
                    local: writer_ref,
                    value: Rvalue::Ref {
                        base: writer_local,
                        mutability: Mutability::Mutable,
                    },
                });

                match val_ty {
                    Some(Type::I32) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_i32", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::U32) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_u32", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::I64) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_i64", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::U64) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_u64", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::Bool) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_bool", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::Char) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_char", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::Str) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_str", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::Named(struct_name)) => {
                        // Resolve Format impl
                        // Call free function fmt_<type> if available.
                        let fmt_fn_key = format!("std::fmt::fmt_{}", struct_name);
                        let fmt_fn = resolve_builtin_target(ctx, &fmt_fn_key, seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);

                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt_fn,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    _ => {
                        ctx.error(
                            "Format not implemented for this type; supported: i32, u32, i64, u64, bool, char, str, and types with fmt_<type> in std::fmt",
                            Some(seg_span),
                        );
                        return None;
                    }
                }
            }
        }
    }

    last.map(Rvalue::Move)
}
