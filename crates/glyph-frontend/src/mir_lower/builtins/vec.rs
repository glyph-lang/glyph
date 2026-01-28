use glyph_core::ast::Expr;
use glyph_core::mir::{MirInst, MirValue, Rvalue};
use glyph_core::span::Span;
use glyph_core::types::Type;

use super::super::context::LowerCtx;
use super::super::expr::lower_value;
use super::super::types::vec_elem_type_from_type;

pub(crate) fn lower_vec_len<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    _span: Span,
) -> Option<Rvalue> {
    let base_local = match base {
        Expr::Ident(ident, _) => ctx.bindings.get(ident.0.as_str()).copied(),
        _ => None,
    }?;

    let Some(base_ty) = ctx
        .locals
        .get(base_local.0 as usize)
        .and_then(|local| local.ty.as_ref())
    else {
        return None;
    };

    if vec_elem_type_from_type(base_ty).is_none() {
        return None;
    }

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Usize);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecLen { vec: base_local },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_vec_push<'a>(
    ctx: &mut LowerCtx<'a>,
    receiver: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("Vec::push expects exactly one argument", Some(span));
        return None;
    }

    let receiver_val = lower_value(ctx, receiver)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("Vec::push receiver must be a local", Some(span));
            return None;
        }
    };

    let elem_type = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(vec_elem_type_from_type)
        .unwrap_or_else(|| {
            ctx.error("could not infer Vec element type", Some(span));
            Type::I32
        });

    let value = lower_value(ctx, &args[0])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![elem_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecPush {
            vec: receiver_local,
            elem_type,
            value,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_vec_pop<'a>(
    ctx: &mut LowerCtx<'a>,
    receiver: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("Vec::pop does not take arguments", Some(span));
        return None;
    }

    let receiver_val = lower_value(ctx, receiver)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("Vec::pop receiver must be a local", Some(span));
            return None;
        }
    };

    let elem_type = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(vec_elem_type_from_type)
        .unwrap_or_else(|| {
            ctx.error("could not infer Vec element type", Some(span));
            Type::I32
        });

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Option".into(),
        args: vec![elem_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecPop {
            vec: receiver_local,
            elem_type,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_vec_static_new<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
    expected_ret: Option<&Type>,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("Vec::new does not take arguments", Some(span));
        return None;
    }

    let elem_type = match expected_ret {
        Some(Type::App { base, args })
            if (base == "Vec" || base.ends_with("::Vec")) && args.len() == 1 =>
        {
            args[0].clone()
        }
        _ => Type::I32,
    };
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![elem_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecNew { elem_type },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_vec_static_with_capacity<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
    expected_ret: Option<&Type>,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("Vec::with_capacity expects one argument", Some(span));
        return None;
    }

    let capacity = lower_value(ctx, &args[0])?;
    let elem_type = match expected_ret {
        Some(Type::App { base, args })
            if (base == "Vec" || base.ends_with("::Vec")) && args.len() == 1 =>
        {
            args[0].clone()
        }
        _ => Type::I32,
    };
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![elem_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecWithCapacity {
            elem_type,
            capacity,
        },
    });
    Some(Rvalue::Move(tmp))
}
