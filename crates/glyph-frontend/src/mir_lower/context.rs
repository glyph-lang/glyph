use std::collections::HashMap;

use glyph_core::ast::Module;
use glyph_core::diag::Diagnostic;
use glyph_core::mir::{BlockId, Local, LocalId, MirBlock, MirInst, Rvalue};
use glyph_core::span::Span;
use glyph_core::types::Type;

use crate::resolver::ResolverContext;

use super::signatures::FnSig;

#[derive(Debug, Clone)]
pub(crate) struct LoopContext {
    pub(crate) header: BlockId, // for continue
    pub(crate) exit: BlockId,   // for break
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum LocalState {
    Uninitialized,
    Initialized,
    Moved,
}

pub(crate) struct LowerCtx<'a> {
    pub(crate) resolver: &'a ResolverContext,
    pub(crate) module: &'a Module,
    pub(crate) fn_sigs: &'a HashMap<String, FnSig>,
    pub(crate) diagnostics: Vec<Diagnostic>,
    pub(crate) locals: Vec<Local>,
    pub(crate) bindings: HashMap<&'a str, LocalId>,
    pub(crate) next_local: u32,
    pub(crate) function_name: String,
    pub(crate) fn_ret_type: Option<Type>,
    pub(crate) blocks: Vec<MirBlock>,
    pub(crate) current: BlockId,
    pub(crate) loop_stack: Vec<LoopContext>,
    pub(crate) scope_stack: Vec<Vec<LocalId>>,
    pub(crate) local_states: Vec<LocalState>,
    pub(crate) string_counter: u32,
}

impl<'a> LowerCtx<'a> {
    pub(crate) fn new(
        resolver: &'a ResolverContext,
        module: &'a Module,
        fn_sigs: &'a HashMap<String, FnSig>,
        function_name: String,
    ) -> Self {
        let mut blocks = Vec::new();
        blocks.push(MirBlock::default());
        Self {
            resolver,
            module,
            fn_sigs,
            diagnostics: Vec::new(),
            locals: Vec::new(),
            bindings: HashMap::new(),
            next_local: 0,
            function_name,
            fn_ret_type: None,
            blocks,
            current: BlockId(0),
            loop_stack: Vec::new(),
            scope_stack: vec![Vec::new()],
            local_states: Vec::new(),
            string_counter: 0,
        }
    }

    pub(crate) fn error(&mut self, message: impl Into<String>, span: Option<Span>) {
        self.diagnostics.push(Diagnostic::error(message, span));
    }

    pub(crate) fn current_block_mut(&mut self) -> &mut MirBlock {
        &mut self.blocks[self.current.0 as usize]
    }

    pub(crate) fn push_inst(&mut self, inst: MirInst) {
        match &inst {
            MirInst::Assign { local, value } => {
                self.handle_reassign(*local);
                if let Some(state) = self.local_states.get_mut(local.0 as usize) {
                    *state = LocalState::Initialized;
                }
                if let Rvalue::Move(src) = value {
                    if let Some(state) = self.local_states.get_mut(src.0 as usize) {
                        *state = LocalState::Moved;
                    }
                }
            }
            MirInst::AssignField { value, .. } => {
                if let Rvalue::Move(src) = value {
                    if let Some(state) = self.local_states.get_mut(src.0 as usize) {
                        *state = LocalState::Moved;
                    }
                }
            }
            _ => {}
        }
        self.current_block_mut().insts.push(inst);
    }

    pub(crate) fn terminated(&self) -> bool {
        self.blocks[self.current.0 as usize]
            .insts
            .last()
            .map(|inst| {
                matches!(
                    inst,
                    MirInst::Return(_) | MirInst::Goto(_) | MirInst::If { .. }
                )
            })
            .unwrap_or(false)
    }

    pub(crate) fn fresh_local(&mut self, name: Option<&'a str>) -> LocalId {
        let id = LocalId(self.next_local);
        self.next_local += 1;
        self.locals.push(Local {
            name: name.map(|s| s.to_string()),
            ty: None,
            mutable: false,
        });
        self.local_states.push(LocalState::Uninitialized);
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.push(id);
        }
        id
    }

    pub(crate) fn fresh_string_global(&mut self) -> String {
        let fn_part = self.function_name.replace("::", "_");
        let name = format!(".str.{}.{}", fn_part, self.string_counter);
        self.string_counter += 1;
        name
    }

    pub(crate) fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(MirBlock::default());
        id
    }

    pub(crate) fn switch_to(&mut self, id: BlockId) {
        self.current = id;
    }

    pub(crate) fn enter_loop(&mut self, header: BlockId, exit: BlockId) {
        self.loop_stack.push(LoopContext { header, exit });
    }

    pub(crate) fn exit_loop(&mut self) {
        self.loop_stack.pop();
    }

    pub(crate) fn current_loop(&self) -> Option<&LoopContext> {
        self.loop_stack.last()
    }

    pub(crate) fn handle_reassign(&mut self, local: LocalId) {
        if !self.local_needs_drop(local) {
            return;
        }
        if let Some(LocalState::Initialized) = self.local_states.get(local.0 as usize) {
            self.emit_drop(local);
        }
    }

    pub(crate) fn enter_scope(&mut self) {
        self.scope_stack.push(Vec::new());
    }

    pub(crate) fn exit_scope(&mut self) {
        if self.scope_stack.len() <= 1 {
            return;
        }
        if let Some(locals) = self.scope_stack.pop() {
            for local in locals.into_iter().rev() {
                self.drop_local_if_needed(local);
            }
        }
    }

    pub(crate) fn drop_local_if_needed(&mut self, local: LocalId) {
        if !self.local_needs_drop(local) {
            return;
        }
        let idx = local.0 as usize;
        if matches!(self.local_states.get(idx), Some(LocalState::Initialized)) {
            self.emit_drop(local);
        }
    }

    pub(crate) fn drop_all_active_locals(&mut self) {
        let scopes: Vec<Vec<LocalId>> = self.scope_stack.clone();
        for scope in scopes.iter().rev() {
            for &local in scope.iter().rev() {
                self.drop_local_if_needed(local);
            }
        }
    }

    pub(crate) fn emit_drop(&mut self, local: LocalId) {
        let insert_before_terminator = self
            .current_block_mut()
            .insts
            .last()
            .map(|inst| {
                matches!(
                    inst,
                    MirInst::Return(_) | MirInst::Goto(_) | MirInst::If { .. }
                )
            })
            .unwrap_or(false);
        if insert_before_terminator {
            let idx = self.current_block_mut().insts.len() - 1;
            self.current_block_mut()
                .insts
                .insert(idx, MirInst::Drop(local));
        } else {
            self.current_block_mut().insts.push(MirInst::Drop(local));
        }
        if let Some(state) = self.local_states.get_mut(local.0 as usize) {
            *state = LocalState::Moved;
        }
    }

    pub(crate) fn local_ty(&self, local: LocalId) -> Option<&Type> {
        self.locals
            .get(local.0 as usize)
            .and_then(|l| l.ty.as_ref())
    }

    pub(crate) fn local_needs_drop(&self, local: LocalId) -> bool {
        self.local_ty(local)
            .map(|ty| matches!(ty, Type::Own(_) | Type::Shared(_) | Type::String))
            .unwrap_or(false)
    }

    pub(crate) fn consume_local(&mut self, local: LocalId, span: Option<Span>) -> bool {
        // Shared pointers are copyable - they don't move
        let ty = self.local_ty(local);
        if matches!(ty, Some(Type::Shared(_))) {
            return true; // Always allow reuse
        }

        if !self.local_needs_drop(local) {
            return true;
        }
        let idx = local.0 as usize;
        match self.local_states.get(idx) {
            Some(LocalState::Moved) => {
                self.error("use of moved value", span);
                false
            }
            Some(LocalState::Initialized) => {
                if let Some(state) = self.local_states.get_mut(idx) {
                    *state = LocalState::Moved;
                }
                true
            }
            _ => {
                self.error("use of uninitialized ownership pointer", span);
                if let Some(state) = self.local_states.get_mut(idx) {
                    *state = LocalState::Moved;
                }
                false
            }
        }
    }
}
