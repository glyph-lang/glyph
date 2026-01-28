use glyph_core::ast::{Block, Expr, Function, Ident, Stmt};
use glyph_core::mir::{BlockId, LocalId, MirFunction, MirInst, MirValue, Rvalue};
use glyph_core::span::Span;
use glyph_core::types::Type;

use crate::resolver::ResolverContext;

use super::context::{LocalState, LowerCtx};
use super::expr::{lower_expr, lower_expr_with_expected, lower_value, lower_value_with_expected};
use super::types::{resolve_type_name, type_expr_to_string};
use super::value::{
    coerce_to_bool, expr_span, infer_value_type, rvalue_from_value, update_local_type_from_rvalue,
};

fn imports_sys_argv(resolver: &ResolverContext) -> bool {
    resolver
        .import_scope
        .as_ref()
        .and_then(|scope| scope.direct_symbols.get("argv"))
        .map(|(module, name)| module == "std/sys" && name == "argv")
        .unwrap_or(false)
}

pub(crate) fn lower_function(
    func: &Function,
    module: &glyph_core::ast::Module,
    resolver: &ResolverContext,
    fn_sigs: &std::collections::HashMap<String, super::signatures::FnSig>,
) -> (MirFunction, Vec<glyph_core::diag::Diagnostic>) {
    // Resolve return type
    let ret_type = func
        .ret_type
        .as_ref()
        .and_then(|t| crate::resolver::resolve_type_expr_to_type(t, resolver));

    let mut ctx = LowerCtx::new(resolver, module, fn_sigs, func.name.0.clone());
    ctx.fn_ret_type = ret_type.clone();

    // Create locals for parameters and bind them
    let mut param_locals = Vec::new();
    for param in &func.params {
        let local = ctx.fresh_local(Some(&param.name.0));
        // Try to infer parameter type from annotation
        if let Some(ty) = param
            .ty
            .as_ref()
            .and_then(|t| crate::resolver::resolve_type_expr_to_type(t, resolver))
        {
            ctx.locals[local.0 as usize].ty = Some(ty);
        }
        if let Some(state) = ctx.local_states.get_mut(local.0 as usize) {
            *state = LocalState::Initialized;
        }
        ctx.bindings.insert(&param.name.0, local);
        param_locals.push(local);
    }

    if imports_sys_argv(resolver) && !ctx.bindings.contains_key("argv") {
        let argv_local = ctx.fresh_local(Some("argv"));
        ctx.locals[argv_local.0 as usize].ty = Some(Type::App {
            base: "Vec".into(),
            args: vec![Type::String],
        });
        if let Some(state) = ctx.local_states.get_mut(argv_local.0 as usize) {
            *state = LocalState::Initialized;
        }
        ctx.bindings.insert("argv", argv_local);
        ctx.push_inst(MirInst::Assign {
            local: argv_local,
            value: Rvalue::Call {
                name: "argv".into(),
                args: Vec::new(),
            },
        });
    }

    let implicit_return =
        lower_block_with_expected(&mut ctx, &func.body, ret_type.as_ref(), false, true);
    if !ctx.terminated() {
        // If block returned a value, use it; otherwise return void
        ctx.drop_all_active_locals();
        ctx.push_inst(MirInst::Return(implicit_return));
    }

    let lowered = MirFunction {
        name: func.name.0.clone(),
        ret_type,
        params: param_locals,
        locals: ctx.locals,
        blocks: ctx.blocks,
    };

    (lowered, ctx.diagnostics)
}

pub(crate) fn lower_block<'a>(ctx: &mut LowerCtx<'a>, block: &'a Block) -> Option<MirValue> {
    lower_block_with_expected(ctx, block, None, false, false)
}

pub(crate) fn lower_block_with_expected<'a>(
    ctx: &mut LowerCtx<'a>,
    block: &'a Block,
    expected: Option<&Type>,
    control_value_context: bool,
    move_returned_local: bool,
) -> Option<MirValue> {
    ctx.enter_scope();
    let mut last_value = None;

    for (i, stmt) in block.stmts.iter().enumerate() {
        let is_last = i == block.stmts.len() - 1;

        match stmt {
            Stmt::Let {
                name,
                mutable,
                ty,
                value,
                ..
            } => {
                let local = ctx.fresh_local(Some(&name.0));
                ctx.bindings.insert(&name.0, local);

                // Set mutability
                ctx.locals[local.0 as usize].mutable = *mutable;

                if let Some(annot_ty) = ty
                    .as_ref()
                    .and_then(|t| resolve_type_name(&type_expr_to_string(t), ctx.resolver))
                {
                    ctx.locals[local.0 as usize].ty = Some(annot_ty);
                }

                let expected = ctx.locals[local.0 as usize].ty.clone();
                if let Some(mut rv) = value
                    .as_ref()
                    .and_then(|e| lower_expr_with_expected(ctx, e, expected.as_ref()))
                {
                    update_local_type_from_rvalue(ctx, local, &mut rv);
                    ctx.push_inst(MirInst::Assign { local, value: rv });
                } else {
                    ctx.push_inst(MirInst::Nop);
                }
            }
            Stmt::Ret(expr, _) => {
                let expected = ctx.fn_ret_type.clone();
                let value = expr
                    .as_ref()
                    .and_then(|e| lower_value_with_expected(ctx, e, expected.as_ref()));
                ctx.drop_all_active_locals();
                ctx.push_inst(MirInst::Return(value));
            }
            Stmt::Break(span) => {
                if let Some(loop_ctx) = ctx.current_loop() {
                    let exit = loop_ctx.exit;
                    ctx.push_inst(MirInst::Goto(exit));
                } else {
                    ctx.error("break statement outside of loop", Some(*span));
                    ctx.push_inst(MirInst::Nop);
                }
            }
            Stmt::Continue(span) => {
                if let Some(loop_ctx) = ctx.current_loop() {
                    let header = loop_ctx.header;
                    ctx.push_inst(MirInst::Goto(header));
                } else {
                    ctx.error("continue statement outside of loop", Some(*span));
                    ctx.push_inst(MirInst::Nop);
                }
            }
            Stmt::Expr(expr, _) => {
                if is_last {
                    // Last expression is the implicit return value
                    match expr {
                        Expr::If {
                            cond,
                            then_block,
                            else_block,
                            ..
                        } => {
                            if control_value_context {
                                last_value = lower_if_value(
                                    ctx,
                                    cond,
                                    then_block,
                                    else_block.as_ref(),
                                    expected,
                                );
                            } else {
                                lower_if(ctx, cond, then_block, else_block.as_ref());
                            }
                        }
                        Expr::While { cond, body, span } => {
                            lower_while(ctx, cond, body);
                            if control_value_context {
                                if !matches!(expected, Some(Type::Void)) {
                                    ctx.error("while expression produces unit", Some(*span));
                                }
                                last_value = Some(MirValue::Unit);
                            }
                        }
                        Expr::For {
                            var,
                            start,
                            end,
                            body,
                            span,
                        } => {
                            lower_for(ctx, var, start, end, body);
                            if control_value_context {
                                if !matches!(expected, Some(Type::Void)) {
                                    ctx.error("for expression produces unit", Some(*span));
                                }
                                last_value = Some(MirValue::Unit);
                            }
                        }
                        _ => {
                            last_value = lower_value_with_expected(ctx, expr, expected);
                        }
                    }
                } else {
                    match expr {
                        Expr::If {
                            cond,
                            then_block,
                            else_block,
                            ..
                        } => lower_if(ctx, cond, then_block, else_block.as_ref()),
                        Expr::While { cond, body, .. } => lower_while(ctx, cond, body),
                        Expr::For {
                            var,
                            start,
                            end,
                            body,
                            ..
                        } => lower_for(ctx, var, start, end, body),
                        Expr::Block(block) => {
                            let _ = lower_block_with_expected(ctx, block, None, false, false);
                            ctx.push_inst(MirInst::Nop);
                        }
                        _ => {
                            let _ = lower_expr(ctx, expr);
                            ctx.push_inst(MirInst::Nop);
                        }
                    }
                }
            }
            Stmt::Assign { target, value, .. } => {
                if let Some(local) = lower_assignment_target(ctx, target) {
                    let expected = ctx.locals[local.0 as usize].ty.clone();
                    if let Some(mut rv) = lower_expr_with_expected(ctx, value, expected.as_ref()) {
                        update_local_type_from_rvalue(ctx, local, &mut rv);
                        ctx.push_inst(MirInst::Assign { local, value: rv });
                    } else {
                        ctx.push_inst(MirInst::Nop);
                    }
                } else {
                    ctx.push_inst(MirInst::Nop);
                }
            }
        }
    }

    if move_returned_local {
        if let Some(MirValue::Local(local)) = last_value {
            if let Some(state) = ctx.local_states.get_mut(local.0 as usize) {
                *state = LocalState::Moved;
            }
        }
    }

    let result = last_value;
    ctx.exit_scope();
    result
}

pub(crate) fn lower_if<'a>(
    ctx: &mut LowerCtx<'a>,
    cond: &'a Expr,
    then_blk: &'a Block,
    else_blk: Option<&'a Block>,
) {
    let header = ctx.current;
    let then_id = ctx.new_block();
    let else_id = else_blk.map(|_| ctx.new_block());
    let join_id = ctx.new_block();

    // Lower short-circuiting conditions directly into control flow.
    // Using `lower_value()` here is incorrect because `&&`/`||` lowerings emit
    // their own `If`/`Goto` terminators.
    ctx.switch_to(header);
    lower_cond_branch(ctx, cond, then_id, else_id.unwrap_or(join_id));

    ctx.switch_to(then_id);
    let _ = lower_block(ctx, then_blk);
    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(join_id));
    }

    if let Some(else_block) = else_blk {
        let else_id = else_id.unwrap();
        ctx.switch_to(else_id);
        let _ = lower_block(ctx, else_block);
        if !ctx.terminated() {
            ctx.push_inst(MirInst::Goto(join_id));
        }
    }

    ctx.switch_to(join_id);
}

pub(crate) fn lower_if_value<'a>(
    ctx: &mut LowerCtx<'a>,
    cond: &'a Expr,
    then_blk: &'a Block,
    else_blk: Option<&'a Block>,
    expected: Option<&Type>,
) -> Option<MirValue> {
    let Some(else_block) = else_blk else {
        if !matches!(expected, Some(Type::Void)) {
            ctx.error("if expression requires an else branch", Some(then_blk.span));
        }
        lower_if(ctx, cond, then_blk, None);
        return Some(MirValue::Unit);
    };

    let header = ctx.current;
    let then_id = ctx.new_block();
    let else_id = ctx.new_block();
    let join_id = ctx.new_block();

    ctx.switch_to(header);
    lower_cond_branch(ctx, cond, then_id, else_id);

    let result_local = ctx.fresh_local(None);
    if let Some(ty) = expected {
        ctx.locals[result_local.0 as usize].ty = Some(ty.clone());
    }

    let assign_result = |ctx: &mut LowerCtx<'a>, value: Option<MirValue>, span: Span| {
        let Some(value) = value else {
            if !matches!(expected, Some(Type::Void)) {
                ctx.error("if expression branch missing value", Some(span));
            }
            return;
        };

        let inferred = infer_value_type(&value, ctx);
        if ctx.locals[result_local.0 as usize].ty.is_none() {
            ctx.locals[result_local.0 as usize].ty = inferred.clone();
        }
        if matches!(value, MirValue::Unit) && !matches!(expected, Some(Type::Void)) {
            ctx.error("if expression branch produced unit", Some(span));
            return;
        }
        if let Some(rv) = rvalue_from_value(value) {
            ctx.push_inst(MirInst::Assign {
                local: result_local,
                value: rv,
            });
        }
    };

    ctx.switch_to(then_id);
    let then_val = lower_block_with_expected(ctx, then_blk, expected, true, true);
    assign_result(ctx, then_val, then_blk.span);
    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(join_id));
    }

    ctx.switch_to(else_id);
    let else_val = lower_block_with_expected(ctx, else_block, expected, true, true);
    assign_result(ctx, else_val, else_block.span);
    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(join_id));
    }

    ctx.switch_to(join_id);
    if matches!(expected, Some(Type::Void)) {
        Some(MirValue::Unit)
    } else {
        Some(MirValue::Local(result_local))
    }
}

pub(crate) fn lower_cond_branch<'a>(
    ctx: &mut LowerCtx<'a>,
    cond: &'a Expr,
    then_bb: BlockId,
    else_bb: BlockId,
) {
    match cond {
        Expr::Binary {
            op: glyph_core::ast::BinaryOp::And,
            lhs,
            rhs,
            ..
        } => {
            // (lhs && rhs): if lhs then eval rhs else false
            let mid = ctx.new_block();
            lower_cond_branch(ctx, lhs, mid, else_bb);
            ctx.switch_to(mid);
            lower_cond_branch(ctx, rhs, then_bb, else_bb);
        }
        Expr::Binary {
            op: glyph_core::ast::BinaryOp::Or,
            lhs,
            rhs,
            ..
        } => {
            // (lhs || rhs): if lhs then true else eval rhs
            let mid = ctx.new_block();
            lower_cond_branch(ctx, lhs, then_bb, mid);
            ctx.switch_to(mid);
            lower_cond_branch(ctx, rhs, then_bb, else_bb);
        }
        _ => {
            let cond_val = lower_value(ctx, cond).unwrap_or(MirValue::Bool(false));
            let cond_bool = coerce_to_bool(cond_val);
            ctx.push_inst(MirInst::If {
                cond: cond_bool,
                then_bb,
                else_bb,
            });
        }
    }
}

pub(crate) fn lower_while<'a>(ctx: &mut LowerCtx<'a>, cond: &'a Expr, body_block: &'a Block) {
    let header_block = ctx.new_block();
    let body_bb = ctx.new_block();
    let exit_bb = ctx.new_block();

    // Jump from current block to header
    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(header_block));
    }

    // Header: evaluate condition and branch
    ctx.switch_to(header_block);
    lower_cond_branch(ctx, cond, body_bb, exit_bb);

    // Body: lower statements, add back edge
    ctx.switch_to(body_bb);
    ctx.enter_loop(header_block, exit_bb);
    let _ = lower_block(ctx, body_block);
    ctx.exit_loop();

    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(header_block)); // Back edge
    }

    // Exit: continue after loop
    ctx.switch_to(exit_bb);
}

pub(crate) fn lower_for<'a>(
    ctx: &mut LowerCtx<'a>,
    var: &'a Ident,
    start: &'a Expr,
    end: &'a Expr,
    body_block: &'a Block,
) {
    // Initialize loop variable: let var = start
    let var_local = ctx.fresh_local(Some(&var.0));
    ctx.bindings.insert(&var.0, var_local);

    if let Some(mut rv) = lower_expr(ctx, start) {
        update_local_type_from_rvalue(ctx, var_local, &mut rv);
        ctx.push_inst(MirInst::Assign {
            local: var_local,
            value: rv,
        });
    } else {
        ctx.push_inst(MirInst::Nop);
    }

    // Create blocks for while loop structure
    let header_block = ctx.new_block();
    let body_bb = ctx.new_block();
    let exit_bb = ctx.new_block();

    // Jump to header
    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(header_block));
    }

    // Header: check condition (var < end)
    ctx.switch_to(header_block);
    let var_val = MirValue::Local(var_local);
    let end_val = lower_value(ctx, end).unwrap_or(MirValue::Int(0));
    let cond_temp = ctx.fresh_local(None);
    ctx.locals[cond_temp.0 as usize].ty = Some(Type::Bool);
    ctx.push_inst(MirInst::Assign {
        local: cond_temp,
        value: Rvalue::Binary {
            op: glyph_core::ast::BinaryOp::Lt,
            lhs: var_val.clone(),
            rhs: end_val,
        },
    });
    ctx.push_inst(MirInst::If {
        cond: MirValue::Local(cond_temp),
        then_bb: body_bb,
        else_bb: exit_bb,
    });

    // Body: execute loop body and increment
    ctx.switch_to(body_bb);
    ctx.enter_loop(header_block, exit_bb);
    let _ = lower_block(ctx, body_block);
    ctx.exit_loop();

    // Increment: var = var + 1
    if !ctx.terminated() {
        let inc_temp = ctx.fresh_local(None);
        ctx.locals[inc_temp.0 as usize].ty = ctx.locals[var_local.0 as usize].ty.clone();
        ctx.push_inst(MirInst::Assign {
            local: inc_temp,
            value: Rvalue::Binary {
                op: glyph_core::ast::BinaryOp::Add,
                lhs: var_val.clone(),
                rhs: MirValue::Int(1),
            },
        });
        ctx.push_inst(MirInst::Assign {
            local: var_local,
            value: Rvalue::Move(inc_temp),
        });
        ctx.push_inst(MirInst::Goto(header_block));
    }

    // Exit: continue after loop
    ctx.switch_to(exit_bb);
}

pub(crate) fn lower_assignment_target<'a>(
    ctx: &mut LowerCtx<'a>,
    target: &'a Expr,
) -> Option<LocalId> {
    match target {
        Expr::Ident(ident, span) => {
            let local_id = ctx.bindings.get(ident.0.as_str()).copied().or_else(|| {
                ctx.error(format!("unknown identifier '{}'", ident.0), Some(*span));
                None
            })?;

            // Check mutability
            if let Some(local) = ctx.locals.get(local_id.0 as usize) {
                if !local.mutable {
                    ctx.error(
                        format!("cannot assign to immutable variable '{}'", ident.0),
                        Some(*span),
                    );
                    return None;
                }
            }

            Some(local_id)
        }
        Expr::FieldAccess { span, .. } => {
            ctx.error("assignment to fields is not supported yet", Some(*span));
            None
        }
        _ => {
            ctx.error("assignment target must be an identifier", expr_span(target));
            None
        }
    }
}
