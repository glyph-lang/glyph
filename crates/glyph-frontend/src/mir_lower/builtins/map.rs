use glyph_core::ast::Expr;
use glyph_core::mir::{MirInst, MirValue, Rvalue};
use glyph_core::span::Span;
use glyph_core::types::Type;

use super::super::context::LowerCtx;
use super::super::expr::lower_value;

fn map_key_value_types(ty: &Type) -> Option<(Type, Type)> {
    match ty {
        Type::App { base, args } if base == "Map" || base.ends_with("::Map") => {
            let key = args.get(0)?.clone();
            let val = args.get(1)?.clone();
            Some((key, val))
        }
        Type::Ref(inner, _) | Type::Own(inner) | Type::RawPtr(inner) | Type::Shared(inner) => {
            map_key_value_types(inner)
        }
        _ => None,
    }
}

pub(crate) fn lower_map_static_new<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
    expected_ret: Option<&Type>,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("Map::new does not take arguments", Some(span));
        return None;
    }
    let (key_type, value_type) = match expected_ret {
        Some(Type::App { base, args })
            if (base == "Map" || base.ends_with("::Map")) && args.len() == 2 =>
        {
            (args[0].clone(), args[1].clone())
        }
        _ => (Type::I32, Type::I32),
    };
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Map".into(),
        args: vec![key_type.clone(), value_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapNew {
            key_type,
            value_type,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_map_static_with_capacity<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
    expected_ret: Option<&Type>,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("Map::with_capacity expects one argument", Some(span));
        return None;
    }
    let capacity = lower_value(ctx, &args[0])?;
    let (key_type, value_type) = match expected_ret {
        Some(Type::App { base, args })
            if (base == "Map" || base.ends_with("::Map")) && args.len() == 2 =>
        {
            (args[0].clone(), args[1].clone())
        }
        _ => (Type::I32, Type::I32),
    };
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Map".into(),
        args: vec![key_type.clone(), value_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapWithCapacity {
            key_type,
            value_type,
            capacity,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_map_add<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 2 {
        ctx.error("add expects (key, value)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("add receiver must be a local", Some(span));
            return None;
        }
    };
    let Some((key_type, value_type)) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
    else {
        ctx.error("get is only supported on Map", Some(span));
        return None;
    };

    let key = lower_value(ctx, &args[0])?;
    let value = lower_value(ctx, &args[1])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Result".into(),
        args: vec![Type::Void, Type::Named("Err".into())],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapAdd {
            map: receiver_local,
            key_type,
            key,
            value_type,
            value,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_map_update<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 2 {
        ctx.error("update expects (key, value)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("update receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));

    let key = lower_value(ctx, &args[0])?;
    let value = lower_value(ctx, &args[1])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Result".into(),
        args: vec![Type::Void, Type::Named("Err".into())],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapUpdate {
            map: receiver_local,
            key_type,
            key,
            value_type,
            value,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_map_del<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("del expects (key)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("del receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));

    let key = lower_value(ctx, &args[0])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Result".into(),
        args: vec![value_type.clone(), Type::Named("Err".into())],
    });

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapDel {
            map: receiver_local,
            key_type,
            value_type,
            key,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_map_get<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("get expects (key)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("get receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));

    let key = lower_value(ctx, &args[0])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Option".into(),
        args: vec![value_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapGet {
            map: receiver_local,
            key_type,
            value_type,
            key,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_map_has<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("has expects (key)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("has receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));
    let _ = value_type; // unused but ensures inference

    let key = lower_value(ctx, &args[0])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Bool);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapHas {
            map: receiver_local,
            key_type,
            key,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_map_keys<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("keys does not take arguments", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("keys receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));
    let _ = value_type;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![key_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapKeys {
            map: receiver_local,
            key_type,
            value_type,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_map_vals<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("vals does not take arguments", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("vals receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));
    let _ = key_type;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![value_type.clone()],
    });

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapVals {
            map: receiver_local,
            key_type,
            value_type,
        },
    });
    Some(Rvalue::Move(tmp))
}
