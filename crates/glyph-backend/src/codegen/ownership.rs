use super::*;

impl CodegenContext {
    pub(super) fn codegen_drop_local(
        &mut self,
        local: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<()> {
        let ty = match func
            .locals
            .get(local.0 as usize)
            .and_then(|l| l.ty.as_ref())
        {
            Some(ty) => ty,
            None => return Ok(()),
        };
        match ty {
            Type::Own(inner) => {
                let slot = local_map
                    .get(&local)
                    .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                self.codegen_drop_own_slot(*slot, inner)
            }
            Type::Shared(inner) => {
                let slot = local_map
                    .get(&local)
                    .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                self.codegen_drop_shared_slot(*slot, inner)
            }
            Type::String => {
                let slot = local_map
                    .get(&local)
                    .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                self.codegen_drop_string_slot(*slot)
            }
            Type::App { base, args } if base == "Vec" => {
                let elem = args.get(0).cloned().unwrap_or(Type::I32);
                self.codegen_drop_vec(local, &elem, func, local_map)
            }
            Type::App { base, args } if base == "Map" => {
                let key_ty = args.get(0).cloned().unwrap_or(Type::I32);
                let val_ty = args.get(1).cloned().unwrap_or(Type::I32);
                self.codegen_drop_map(local, &key_ty, &val_ty, func, local_map)
            }
            _ => Ok(()),
        }
    }

    pub(super) fn codegen_drop_own_slot(&mut self, slot: LLVMValueRef, inner: &Type) -> Result<()> {
        unsafe {
            let ptr_ty = self.get_llvm_type(&Type::Own(Box::new(inner.clone())))?;
            let load_name = CString::new("own.ptr")?;
            let ptr_val = LLVMBuildLoad2(self.builder, ptr_ty, slot, load_name.as_ptr());
            let null_ptr = LLVMConstPointerNull(ptr_ty);
            let cmp_name = CString::new("own.isnull")?;
            let is_null = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                ptr_val,
                null_ptr,
                cmp_name.as_ptr(),
            );

            let current_bb = LLVMGetInsertBlock(self.builder);
            if current_bb.is_null() {
                bail!("builder not positioned in a block for drop");
            }
            let parent_func = LLVMGetBasicBlockParent(current_bb);
            if parent_func.is_null() {
                bail!("drop parent function missing");
            }

            let drop_name = CString::new("own.drop")?;
            let cont_name = CString::new("own.drop.cont")?;
            let drop_bb =
                LLVMAppendBasicBlockInContext(self.context, parent_func, drop_name.as_ptr());
            let cont_bb =
                LLVMAppendBasicBlockInContext(self.context, parent_func, cont_name.as_ptr());

            LLVMBuildCondBr(self.builder, is_null, cont_bb, drop_bb);

            LLVMPositionBuilderAtEnd(self.builder, drop_bb);
            self.codegen_free(ptr_val)?;
            LLVMBuildStore(self.builder, null_ptr, slot);
            LLVMBuildBr(self.builder, cont_bb);

            LLVMPositionBuilderAtEnd(self.builder, cont_bb);
        }
        Ok(())
    }

    pub(super) fn codegen_drop_shared_slot(
        &mut self,
        slot: LLVMValueRef,
        elem_type: &Type,
    ) -> Result<()> {
        unsafe {
            // 1. Load pointer
            let ptr_ty = self.get_llvm_type(&Type::Shared(Box::new(elem_type.clone())))?;
            let load_name = CString::new("shared.drop.load")?;
            let ptr_val = LLVMBuildLoad2(self.builder, ptr_ty, slot, load_name.as_ptr());

            // 2. Null check (pointer might already be consumed)
            let null_ptr = LLVMConstPointerNull(ptr_ty);
            let is_null_name = CString::new("shared.drop.is_null")?;
            let is_null = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                ptr_val,
                null_ptr,
                is_null_name.as_ptr(),
            );

            // 3. Create basic blocks
            let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
            let dec_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("shared.dec")?.as_ptr(),
            );
            let free_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("shared.free")?.as_ptr(),
            );
            let cont_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("shared.cont")?.as_ptr(),
            );

            // If null, skip to continue
            LLVMBuildCondBr(self.builder, is_null, cont_bb, dec_bb);

            // 4. Decrement block
            LLVMPositionBuilderAtEnd(self.builder, dec_bb);

            // Get struct type
            let usize_ty = LLVMInt64TypeInContext(self.context);
            let elem_llvm_ty = self.get_llvm_type(elem_type)?;
            let mut field_tys = vec![usize_ty, elem_llvm_ty];
            let rc_struct = LLVMStructTypeInContext(self.context, field_tys.as_mut_ptr(), 2, 0);

            // Get refcount pointer
            let refcount_ptr = LLVMBuildStructGEP2(
                self.builder,
                rc_struct,
                ptr_val,
                0,
                CString::new("shared.rc.ptr")?.as_ptr(),
            );

            // Load old refcount
            let old_count = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                refcount_ptr,
                CString::new("shared.rc.old")?.as_ptr(),
            );

            // Decrement refcount
            let one = LLVMConstInt(usize_ty, 1, 0);
            let new_count = LLVMBuildSub(
                self.builder,
                old_count,
                one,
                CString::new("shared.rc.new")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, new_count, refcount_ptr);

            // Check if refcount reached zero
            let zero = LLVMConstInt(usize_ty, 0, 0);
            let is_zero = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                new_count,
                zero,
                CString::new("shared.rc.is_zero")?.as_ptr(),
            );

            // If zero, free; else continue
            LLVMBuildCondBr(self.builder, is_zero, free_bb, cont_bb);

            // 5. Free block (refcount is zero)
            LLVMPositionBuilderAtEnd(self.builder, free_bb);
            self.codegen_free(ptr_val)?;

            // Set slot to null
            LLVMBuildStore(self.builder, null_ptr, slot);
            LLVMBuildBr(self.builder, cont_bb);

            // 6. Continue block
            LLVMPositionBuilderAtEnd(self.builder, cont_bb);

            Ok(())
        }
    }

    pub(super) fn codegen_drop_string_slot(&mut self, slot: LLVMValueRef) -> Result<()> {
        unsafe {
            let ptr_ty = self.get_llvm_type(&Type::String)?;
            let load_name = CString::new("string.ptr")?;
            let ptr_val = LLVMBuildLoad2(self.builder, ptr_ty, slot, load_name.as_ptr());
            let null_ptr = LLVMConstPointerNull(ptr_ty);
            let cmp_name = CString::new("string.isnull")?;
            let is_null = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                ptr_val,
                null_ptr,
                cmp_name.as_ptr(),
            );

            let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
            let drop_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("string.drop")?.as_ptr(),
            );
            let cont_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("string.drop.cont")?.as_ptr(),
            );

            LLVMBuildCondBr(self.builder, is_null, cont_bb, drop_bb);

            LLVMPositionBuilderAtEnd(self.builder, drop_bb);
            self.codegen_free(ptr_val)?;
            LLVMBuildStore(self.builder, null_ptr, slot);
            LLVMBuildBr(self.builder, cont_bb);

            LLVMPositionBuilderAtEnd(self.builder, cont_bb);
        }
        Ok(())
    }

    pub(super) fn codegen_drop_file_slot(&mut self, slot: LLVMValueRef) -> Result<()> {
        unsafe {
            let file_ty = self.get_struct_type("File")?;
            let layout = self
                .struct_layouts
                .get("File")
                .ok_or_else(|| anyhow!("missing layout for struct File"))?;
            let handle_field = layout
                .fields
                .first()
                .ok_or_else(|| anyhow!("missing File handle field"))?;
            let handle_ty = self.get_llvm_type(&handle_field.1)?;
            let handle_ptr = LLVMBuildStructGEP2(
                self.builder,
                file_ty,
                slot,
                0,
                CString::new("file.handle")?.as_ptr(),
            );
            let handle_val = LLVMBuildLoad2(
                self.builder,
                handle_ty,
                handle_ptr,
                CString::new("file.handle.load")?.as_ptr(),
            );
            let null_ptr = LLVMConstNull(handle_ty);
            let is_null = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                handle_val,
                null_ptr,
                CString::new("file.handle.null")?.as_ptr(),
            );

            let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
            let drop_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("file.drop")?.as_ptr(),
            );
            let cont_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("file.drop.cont")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, is_null, cont_bb, drop_bb);

            LLVMPositionBuilderAtEnd(self.builder, drop_bb);
            let fclose_fn = LLVMGetNamedFunction(self.module, CString::new("fclose")?.as_ptr());
            if !fclose_fn.is_null() {
                let mut args = vec![handle_val];
                if let Ok(fn_ty) = self.function_type_for("fclose") {
                    LLVMBuildCall2(
                        self.builder,
                        fn_ty,
                        fclose_fn,
                        args.as_mut_ptr(),
                        args.len() as u32,
                        CString::new("file.drop.close")?.as_ptr(),
                    );
                }
            }
            LLVMBuildStore(self.builder, null_ptr, handle_ptr);
            LLVMBuildBr(self.builder, cont_bb);

            LLVMPositionBuilderAtEnd(self.builder, cont_bb);
        }
        Ok(())
    }

    pub(super) fn codegen_drop_elem_slot(
        &mut self,
        slot: LLVMValueRef,
        elem_type: &Type,
    ) -> Result<()> {
        match elem_type {
            Type::Own(inner) => self.codegen_drop_own_slot(slot, inner),
            Type::Shared(inner) => self.codegen_drop_shared_slot(slot, inner),
            Type::String => self.codegen_drop_string_slot(slot),
            Type::Named(name) if name == "File" => self.codegen_drop_file_slot(slot),
            _ => Ok(()),
        }
    }

    pub(super) fn codegen_own_new(
        &mut self,
        elem_type: &Type,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            let llvm_elem_ty = self.get_llvm_type(elem_type)?;
            let size_val = LLVMSizeOf(llvm_elem_ty);
            let malloc_fn = self.ensure_malloc_fn()?;
            let malloc_ty = self.malloc_function_type();
            let mut args = vec![size_val];
            let call_name = CString::new("own.alloc")?;
            let raw_ptr = LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                call_name.as_ptr(),
            );
            let typed_ptr_ty = LLVMPointerType(llvm_elem_ty, 0);
            let cast_name = CString::new("own.ptr")?;
            let typed_ptr =
                LLVMBuildBitCast(self.builder, raw_ptr, typed_ptr_ty, cast_name.as_ptr());
            let value_val = self.codegen_value(value, func, local_map)?;
            LLVMBuildStore(self.builder, value_val, typed_ptr);
            Ok(typed_ptr)
        }
    }

    pub(super) fn codegen_own_into_raw(
        &mut self,
        base: LocalId,
        elem_type: &Type,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let slot = local_map
            .get(&base)
            .ok_or_else(|| anyhow!("undefined local {:?}", base))?;
        let llvm_ty = self.get_llvm_type(&Type::Own(Box::new(elem_type.clone())))?;
        unsafe {
            let load_name = CString::new("own.into")?;
            let ptr_val = LLVMBuildLoad2(self.builder, llvm_ty, *slot, load_name.as_ptr());
            let null_ptr = LLVMConstPointerNull(llvm_ty);
            LLVMBuildStore(self.builder, null_ptr, *slot);
            Ok(ptr_val)
        }
    }

    pub(super) fn codegen_own_from_raw(
        &mut self,
        ptr: &MirValue,
        elem_type: &Type,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let value = self.codegen_value(ptr, func, local_map)?;
        let desired_ty = self.get_llvm_type(&Type::Own(Box::new(elem_type.clone())))?;
        unsafe {
            if LLVMTypeOf(value) == desired_ty {
                Ok(value)
            } else {
                let cast_name = CString::new("own.from.raw")?;
                Ok(LLVMBuildBitCast(
                    self.builder,
                    value,
                    desired_ty,
                    cast_name.as_ptr(),
                ))
            }
        }
    }

    pub(super) fn codegen_raw_ptr_null(&mut self, elem_type: &Type) -> Result<LLVMValueRef> {
        let ty = self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?;
        Ok(unsafe { LLVMConstPointerNull(ty) })
    }

    pub(super) fn codegen_shared_new(
        &mut self,
        elem_type: &Type,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            // 1. Create struct type [refcount: usize, data: T]
            let usize_ty = LLVMInt64TypeInContext(self.context);
            let elem_llvm_ty = self.get_llvm_type(elem_type)?;
            let mut field_tys = vec![usize_ty, elem_llvm_ty];
            let rc_struct = LLVMStructTypeInContext(
                self.context,
                field_tys.as_mut_ptr(),
                2,
                0, // not packed
            );

            // 2. Allocate memory
            let size_val = LLVMSizeOf(rc_struct);
            let malloc_fn = self.ensure_malloc_fn()?;
            let malloc_ty = self.malloc_function_type();
            let mut args = vec![size_val];
            let call_name = CString::new("shared.alloc")?;
            let raw_ptr = LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                call_name.as_ptr(),
            );

            // 3. Cast to typed pointer
            let typed_ptr_ty = LLVMPointerType(rc_struct, 0);
            let cast_name = CString::new("shared.ptr")?;
            let typed_ptr =
                LLVMBuildBitCast(self.builder, raw_ptr, typed_ptr_ty, cast_name.as_ptr());

            // 4. Initialize refcount to 1
            let refcount_ptr = LLVMBuildStructGEP2(
                self.builder,
                rc_struct,
                typed_ptr,
                0,
                CString::new("shared.rc")?.as_ptr(),
            );
            let one = LLVMConstInt(usize_ty, 1, 0);
            LLVMBuildStore(self.builder, one, refcount_ptr);

            // 5. Store the data value
            let data_ptr = LLVMBuildStructGEP2(
                self.builder,
                rc_struct,
                typed_ptr,
                1,
                CString::new("shared.data")?.as_ptr(),
            );
            let value_val = self.codegen_value(value, func, local_map)?;
            LLVMBuildStore(self.builder, value_val, data_ptr);

            // 6. Return pointer (API returns pointer to whole struct)
            Ok(typed_ptr)
        }
    }

    pub(super) fn codegen_shared_clone(
        &mut self,
        base: LocalId,
        elem_type: &Type,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            // 1. Load the Shared<T> pointer
            let slot = local_map
                .get(&base)
                .ok_or_else(|| anyhow!("undefined local {:?}", base))?;
            let shared_ty = self.get_llvm_type(&Type::Shared(Box::new(elem_type.clone())))?;
            let load_name = CString::new("shared.load")?;
            let ptr_val = LLVMBuildLoad2(self.builder, shared_ty, *slot, load_name.as_ptr());

            // 2. Get struct type for GEP
            let usize_ty = LLVMInt64TypeInContext(self.context);
            let elem_llvm_ty = self.get_llvm_type(elem_type)?;
            let mut field_tys = vec![usize_ty, elem_llvm_ty];
            let rc_struct = LLVMStructTypeInContext(self.context, field_tys.as_mut_ptr(), 2, 0);

            // 3. Get pointer to refcount field
            let refcount_ptr = LLVMBuildStructGEP2(
                self.builder,
                rc_struct,
                ptr_val,
                0,
                CString::new("shared.rc.ptr")?.as_ptr(),
            );

            // 4. Load current refcount
            let rc_name = CString::new("shared.rc.old")?;
            let old_count = LLVMBuildLoad2(self.builder, usize_ty, refcount_ptr, rc_name.as_ptr());

            // 5. Increment refcount
            let one = LLVMConstInt(usize_ty, 1, 0);
            let inc_name = CString::new("shared.rc.new")?;
            let new_count = LLVMBuildAdd(self.builder, old_count, one, inc_name.as_ptr());
            LLVMBuildStore(self.builder, new_count, refcount_ptr);

            // 6. Return the same pointer (refcount incremented)
            Ok(ptr_val)
        }
    }
}
