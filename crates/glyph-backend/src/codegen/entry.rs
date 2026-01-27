use super::*;

impl CodegenContext {
    pub(super) fn mir_uses_sys_argv(&self, mir_module: &MirModule) -> bool {
        for func in &mir_module.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let MirInst::Assign { value, .. } = inst {
                        if let Rvalue::Call { name, .. } = value {
                            if name == "argv" || name == "std::sys::argv" {
                                return true;
                            }
                        }
                    }
                }
            }
        }
        false
    }

    pub(super) fn argv_vec_llvm_type(&self) -> Result<LLVMTypeRef> {
        let vec_name = format!("Vec${}", self.type_key(&Type::String));
        self.get_struct_type(&vec_name)
    }

    pub(super) fn ensure_sys_argv_globals(
        &mut self,
        vec_ty: LLVMTypeRef,
    ) -> Result<(LLVMValueRef, LLVMValueRef, LLVMValueRef)> {
        unsafe {
            if self.argc_global.is_none() {
                let usize_ty = LLVMInt64TypeInContext(self.context);
                let name = CString::new("glyph.argc")?;
                let global = LLVMAddGlobal(self.module, usize_ty, name.as_ptr());
                LLVMSetInitializer(global, LLVMConstInt(usize_ty, 0, 0));
                self.argc_global = Some(global);
            }
            if self.argv_global.is_none() {
                let i8_ty = LLVMInt8TypeInContext(self.context);
                let i8_ptr = LLVMPointerType(i8_ty, 0);
                let i8_ptr_ptr = LLVMPointerType(i8_ptr, 0);
                let name = CString::new("glyph.argv")?;
                let global = LLVMAddGlobal(self.module, i8_ptr_ptr, name.as_ptr());
                LLVMSetInitializer(global, LLVMConstPointerNull(i8_ptr_ptr));
                self.argv_global = Some(global);
            }
            if self.argv_vec_global.is_none() {
                let name = CString::new("glyph.argv.vec")?;
                let global = LLVMAddGlobal(self.module, vec_ty, name.as_ptr());
                LLVMSetInitializer(global, LLVMConstNull(vec_ty));
                self.argv_vec_global = Some(global);
            }
        }

        Ok((
            self.argc_global.unwrap(),
            self.argv_global.unwrap(),
            self.argv_vec_global.unwrap(),
        ))
    }

    pub(super) fn codegen_sys_argv_value(&mut self) -> Result<LLVMValueRef> {
        let vec_ty = self.argv_vec_llvm_type()?;
        let (_, _, argv_vec_global) = self.ensure_sys_argv_globals(vec_ty)?;
        let load_name = CString::new("argv.load")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, vec_ty, argv_vec_global, load_name.as_ptr()) })
    }

    pub(super) fn codegen_sys_argv_build(
        &mut self,
        argc_val: LLVMValueRef,
        argv_val: LLVMValueRef,
    ) -> Result<LLVMValueRef> {
        let vec_ty = self.argv_vec_llvm_type()?;
        let (argc_global, argv_global, argv_vec_global) = self.ensure_sys_argv_globals(vec_ty)?;
        let usize_ty = unsafe { LLVMInt64TypeInContext(self.context) };
        let argc_val = self.ensure_usize(argc_val, usize_ty)?;

        unsafe {
            LLVMBuildStore(self.builder, argc_val, argc_global);
            LLVMBuildStore(self.builder, argv_val, argv_global);
        }

        let vec_val = self.codegen_vec_init_with_capacity_value(&Type::String, argc_val)?;
        let vec_ptr = unsafe {
            let alloca = LLVMBuildAlloca(self.builder, vec_ty, CString::new("argv.vec")?.as_ptr());
            LLVMBuildStore(self.builder, vec_val, alloca);
            alloca
        };

        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let len_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                argc_val,
                zero,
                CString::new("argv.len.zero")?.as_ptr(),
            )
        };
        let i8_ty = unsafe { LLVMInt8TypeInContext(self.context) };
        let i8_ptr = unsafe { LLVMPointerType(i8_ty, 0) };
        let i8_ptr_ptr = unsafe { LLVMPointerType(i8_ptr, 0) };
        let argv_is_null = unsafe {
            let null_ptr = LLVMConstPointerNull(i8_ptr_ptr);
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                argv_val,
                null_ptr,
                CString::new("argv.ptr.null")?.as_ptr(),
            )
        };
        let skip_loop = unsafe {
            LLVMBuildOr(
                self.builder,
                len_is_zero,
                argv_is_null,
                CString::new("argv.loop.skip")?.as_ptr(),
            )
        };

        unsafe {
            let parent_bb = LLVMGetInsertBlock(self.builder);
            let parent_fn = LLVMGetBasicBlockParent(parent_bb);
            let loop_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.loop")?.as_ptr(),
            );
            let done_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.done")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, skip_loop, done_bb, loop_bb);

            LLVMPositionBuilderAtEnd(self.builder, loop_bb);
            let idx_slot =
                LLVMBuildAlloca(self.builder, usize_ty, CString::new("argv.idx")?.as_ptr());
            LLVMBuildStore(self.builder, zero, idx_slot);

            let check_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.check")?.as_ptr(),
            );
            let body_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.body")?.as_ptr(),
            );
            let loop_done_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.loop.done")?.as_ptr(),
            );
            LLVMBuildBr(self.builder, check_bb);

            LLVMPositionBuilderAtEnd(self.builder, check_bb);
            let idx_val = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                idx_slot,
                CString::new("argv.idx.load")?.as_ptr(),
            );
            let idx_ok = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntULT,
                idx_val,
                argc_val,
                CString::new("argv.idx.lt")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, idx_ok, body_bb, loop_done_bb);

            LLVMPositionBuilderAtEnd(self.builder, body_bb);
            let arg_ptr_ptr = LLVMBuildGEP2(
                self.builder,
                i8_ptr,
                argv_val,
                vec![idx_val].as_mut_ptr(),
                1,
                CString::new("argv.gep")?.as_ptr(),
            );
            let arg_ptr = LLVMBuildLoad2(
                self.builder,
                i8_ptr,
                arg_ptr_ptr,
                CString::new("argv.arg")?.as_ptr(),
            );
            let name = CString::new("strdup")?;
            let strdup_fn = LLVMGetNamedFunction(self.module, name.as_ptr());
            if strdup_fn.is_null() {
                bail!("missing strdup declaration for argv initialization");
            }
            let mut args = vec![arg_ptr];
            let dup = LLVMBuildCall2(
                self.builder,
                self.strdup_function_type(),
                strdup_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                CString::new("argv.dup")?.as_ptr(),
            );
            self.codegen_vec_push_no_grow(vec_ptr, &Type::String, dup)?;
            let next_idx = LLVMBuildAdd(
                self.builder,
                idx_val,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("argv.idx.next")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, next_idx, idx_slot);
            LLVMBuildBr(self.builder, check_bb);

            LLVMPositionBuilderAtEnd(self.builder, loop_done_bb);
            LLVMBuildBr(self.builder, done_bb);

            LLVMPositionBuilderAtEnd(self.builder, done_bb);
            let vec_val = LLVMBuildLoad2(
                self.builder,
                vec_ty,
                vec_ptr,
                CString::new("argv.vec.load")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, vec_val, argv_vec_global);
            Ok(vec_val)
        }
    }

    pub(super) fn codegen_main_wrapper(
        &mut self,
        mir_module: &MirModule,
        user_main: LLVMValueRef,
    ) -> Result<()> {
        let Some(main_mir) = mir_module.functions.iter().find(|f| f.name == "main") else {
            return Ok(());
        };
        if !main_mir.params.is_empty() {
            return Ok(());
        }

        unsafe {
            let i32_ty = LLVMInt32TypeInContext(self.context);
            let i8_ty = LLVMInt8TypeInContext(self.context);
            let i8_ptr = LLVMPointerType(i8_ty, 0);
            let i8_ptr_ptr = LLVMPointerType(i8_ptr, 0);
            let mut params = vec![i32_ty, i8_ptr_ptr];
            let fn_ty = LLVMFunctionType(i32_ty, params.as_mut_ptr(), params.len() as u32, 0);
            let name = CString::new("main")?;
            let wrapper = LLVMAddFunction(self.module, name.as_ptr(), fn_ty);
            let entry = LLVMAppendBasicBlockInContext(
                self.context,
                wrapper,
                CString::new("entry")?.as_ptr(),
            );
            LLVMPositionBuilderAtEnd(self.builder, entry);

            let argc_param = LLVMGetParam(wrapper, 0);
            let argv_param = LLVMGetParam(wrapper, 1);
            let argc_val = LLVMBuildZExt(
                self.builder,
                argc_param,
                LLVMInt64TypeInContext(self.context),
                CString::new("argc.ext")?.as_ptr(),
            );
            let _ = self.codegen_sys_argv_build(argc_val, argv_param)?;

            let fn_ty = self.llvm_function_type(main_mir)?;
            let call_name = CString::new("main.call")?;
            let call_val = LLVMBuildCall2(
                self.builder,
                fn_ty,
                user_main,
                std::ptr::null_mut(),
                0,
                call_name.as_ptr(),
            );

            match main_mir.ret_type.as_ref() {
                None | Some(Type::Void) => {
                    LLVMBuildRet(self.builder, LLVMConstInt(i32_ty, 0, 0));
                }
                Some(Type::I32) => {
                    LLVMBuildRet(self.builder, call_val);
                }
                Some(_) => {
                    let cast_val = LLVMBuildTrunc(
                        self.builder,
                        call_val,
                        i32_ty,
                        CString::new("main.ret.cast")?.as_ptr(),
                    );
                    LLVMBuildRet(self.builder, cast_val);
                }
            };
        }

        Ok(())
    }
}
