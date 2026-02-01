use glyph_core::ast::Expr;
use glyph_core::mir::{LocalId, MirInst, MirValue, Rvalue};
use glyph_core::span::Span;
use glyph_core::types::Type;

use super::super::context::{LocalState, LowerCtx};
use super::super::expr::lower_value;
use super::super::value::infer_value_type;

fn string_receiver_local<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    span: Span,
) -> Option<LocalId> {
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("string receiver must be a local", Some(span));
            return None;
        }
    };
    let receiver_ty = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref());
    let is_string = matches!(receiver_ty, Some(Type::String))
        || matches!(receiver_ty, Some(Type::Str))
        || matches!(receiver_ty, Some(Type::Ref(inner, _)) if matches!(inner.as_ref(), Type::String | Type::Str));
    if !is_string {
        ctx.error("string methods require a String receiver", Some(span));
        return None;
    }
    Some(receiver_local)
}

pub(crate) fn lower_string_from<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("String::from expects exactly one argument", Some(span));
        return None;
    }
    let value = lower_value(ctx, &args[0])?;
    if let Some(arg_ty) = infer_value_type(&value, ctx) {
        let is_string_like = matches!(arg_ty, Type::Str | Type::String)
            || matches!(arg_ty, Type::Ref(inner, _) if matches!(inner.as_ref(), Type::Str | Type::String));
        if !is_string_like {
            ctx.error("String::from_str expects a str or String", Some(span));
            return None;
        }
    }

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::String);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::Call {
            name: "strdup".into(),
            args: vec![value],
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_string_len<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("len() does not take arguments", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Usize);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringLen {
            base: receiver_local,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_string_concat<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("concat() expects one argument", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let value = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::String);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringConcat {
            base: receiver_local,
            value,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_string_slice<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 2 {
        ctx.error("slice() expects (start, len)", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let start = lower_value(ctx, &args[0])?;
    let len = lower_value(ctx, &args[1])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::String);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringSlice {
            base: receiver_local,
            start,
            len,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_string_trim<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("trim() does not take arguments", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::String);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringTrim {
            base: receiver_local,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_string_split<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("split() expects a separator", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let sep = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![Type::String],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringSplit {
            base: receiver_local,
            sep,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_string_starts_with<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("starts_with() expects a prefix", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let needle = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Bool);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringStartsWith {
            base: receiver_local,
            needle,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_string_clone<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("clone() does not take arguments", Some(span));
        return None;
    }

    let (receiver_local, skip_drop) = match base {
        Expr::Ident(name, _) => {
            let Some(local) = ctx.bindings.get(name.0.as_str()).copied() else {
                ctx.error(format!("unknown identifier '{}'", name.0), Some(span));
                return None;
            };
            if !ctx.consume_local(local, Some(span)) {
                return None;
            }
            if ctx.local_needs_drop(local) {
                if let Some(state) = ctx.local_states.get_mut(local.0 as usize) {
                    *state = LocalState::Initialized;
                }
            }
            (local, false)
        }
        Expr::FieldAccess {
            base: field_base, ..
        } => {
            let skip_drop = match field_base.as_ref() {
                Expr::Ident(_, _) => true,
                Expr::Ref { expr, .. } => matches!(expr.as_ref(), Expr::Ident(_, _)),
                _ => false,
            };
            let field_val = lower_value(ctx, base)?;
            let field_local = match field_val {
                MirValue::Local(id) => id,
                _ => {
                    ctx.error("string clone receiver must be a local", Some(span));
                    return None;
                }
            };
            (field_local, skip_drop)
        }
        _ => {
            let value = lower_value(ctx, base)?;
            let local = match value {
                MirValue::Local(id) => id,
                _ => {
                    ctx.error("string clone receiver must be a local", Some(span));
                    return None;
                }
            };
            (local, false)
        }
    };

    let receiver_ty = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.clone());
    let is_string = matches!(receiver_ty.as_ref(), Some(Type::String | Type::Str))
        || matches!(receiver_ty.as_ref(), Some(Type::Ref(inner, _)) if matches!(inner.as_ref(), Type::String | Type::Str));
    if !is_string {
        ctx.error("string clone requires a String receiver", Some(span));
        return None;
    }

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::String);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringClone {
            base: receiver_local,
        },
    });

    if skip_drop {
        if matches!(receiver_ty.as_ref(), Some(Type::String)) {
            if let Some(state) = ctx.local_states.get_mut(receiver_local.0 as usize) {
                *state = LocalState::Moved;
            }
        }
    }

    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_string_ends_with<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("ends_with() expects a suffix", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let needle = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Bool);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringEndsWith {
            base: receiver_local,
            needle,
        },
    });
    Some(Rvalue::Move(tmp))
}
