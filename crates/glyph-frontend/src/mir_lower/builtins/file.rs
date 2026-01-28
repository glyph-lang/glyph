use glyph_core::ast::Expr;
use glyph_core::mir::{LocalId, MirInst, MirValue, Rvalue};
use glyph_core::span::Span;
use glyph_core::types::Type;

use super::super::context::LowerCtx;
use super::super::expr::lower_value;

fn file_result_type(ok_type: Type) -> Type {
    Type::App {
        base: "Result".into(),
        args: vec![ok_type, Type::Named("Err".into())],
    }
}

fn file_receiver_local<'a>(ctx: &mut LowerCtx<'a>, base: &'a Expr, span: Span) -> Option<LocalId> {
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("file receiver must be a local", Some(span));
            return None;
        }
    };
    let receiver_ty = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref());
    let is_file = matches!(receiver_ty, Some(Type::Named(name)) if name == "File")
        || matches!(receiver_ty, Some(Type::Ref(inner, _)) if matches!(inner.as_ref(), Type::Named(name) if name == "File"));
    if !is_file {
        ctx.error("file methods require a File receiver", Some(span));
        return None;
    }
    Some(receiver_local)
}

pub(crate) fn lower_file_open<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
    create: bool,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("File::open expects a path", Some(span));
        return None;
    }
    let path = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(file_result_type(Type::Named("File".into())));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::FileOpen { path, create },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_file_read_to_string<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("read_to_string() takes no arguments", Some(span));
        return None;
    }
    let receiver_local = file_receiver_local(ctx, base, span)?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(file_result_type(Type::String));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::FileReadToString {
            file: receiver_local,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_file_write_string<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("write_string() expects one argument", Some(span));
        return None;
    }
    let receiver_local = file_receiver_local(ctx, base, span)?;
    let contents = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(file_result_type(Type::U32));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::FileWriteString {
            file: receiver_local,
            contents,
        },
    });
    Some(Rvalue::Move(tmp))
}

pub(crate) fn lower_file_close<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("close() takes no arguments", Some(span));
        return None;
    }
    let receiver_local = file_receiver_local(ctx, base, span)?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(file_result_type(Type::I32));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::FileClose {
            file: receiver_local,
        },
    });
    Some(Rvalue::Move(tmp))
}
