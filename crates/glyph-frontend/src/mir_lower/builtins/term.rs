use glyph_core::ast::Expr;
use glyph_core::mir::{MirInst, MirValue, Rvalue};
use glyph_core::span::Span;
use glyph_core::types::Type;

use super::super::context::LowerCtx;

pub(crate) fn lower_term_stdout<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("Terminal::stdout() does not take arguments", Some(span));
        return None;
    }

    let id_local = ctx.fresh_local(None);
    ctx.locals[id_local.0 as usize].ty = Some(Type::I32);
    ctx.push_inst(MirInst::Assign {
        local: id_local,
        value: Rvalue::Call {
            name: "glyph_term_stdout".into(),
            args: Vec::new(),
        },
    });

    let terminal_local = ctx.fresh_local(None);
    ctx.locals[terminal_local.0 as usize].ty = Some(Type::Named("Terminal".into()));
    ctx.push_inst(MirInst::Assign {
        local: terminal_local,
        value: Rvalue::StructLit {
            struct_name: "Terminal".into(),
            field_values: vec![("id".into(), MirValue::Local(id_local))],
        },
    });

    Some(Rvalue::Move(terminal_local))
}
