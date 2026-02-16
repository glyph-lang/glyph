use super::*;

impl CodegenContext {
    pub(super) fn codegen_string_literal(
        &mut self,
        content: &str,
        global_name: &str,
    ) -> Result<LLVMValueRef> {
        if let Some(&ptr) = self.string_globals.get(global_name) {
            return Ok(ptr);
        }

        unsafe {
            // Null-terminated bytes for C interop
            let mut bytes: Vec<u8> = content.bytes().collect();
            bytes.push(0);

            let i8_ty = LLVMInt8TypeInContext(self.context);
            let array_ty = LLVMArrayType2(i8_ty, bytes.len() as u64);

            let name_c = CString::new(global_name)?;
            let global = LLVMAddGlobal(self.module, array_ty, name_c.as_ptr());
            LLVMSetGlobalConstant(global, 1);
            LLVMSetLinkage(global, LLVMLinkage::LLVMPrivateLinkage);

            let mut const_bytes: Vec<LLVMValueRef> = bytes
                .iter()
                .map(|b| LLVMConstInt(i8_ty, *b as u64, 0))
                .collect();
            let init = LLVMConstArray2(i8_ty, const_bytes.as_mut_ptr(), const_bytes.len() as u64);
            LLVMSetInitializer(global, init);

            // Pointer to first character
            let i32_ty = LLVMInt32TypeInContext(self.context);
            let zero = LLVMConstInt(i32_ty, 0, 0);
            let mut indices = vec![zero, zero];
            let ptr = LLVMConstGEP2(array_ty, global, indices.as_mut_ptr(), indices.len() as u32);

            self.string_globals.insert(global_name.to_string(), ptr);
            Ok(ptr)
        }
    }

    pub(super) fn codegen_string_len_value(
        &mut self,
        value: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let strlen_fn = self.get_extern_function(functions, "strlen")?;
        let mut args = vec![value];
        let fn_ty = self.function_type_for("strlen")?;
        Ok(unsafe {
            LLVMBuildCall2(
                self.builder,
                fn_ty,
                strlen_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                CString::new("str.len")?.as_ptr(),
            )
        })
    }

    pub(super) fn codegen_string_base_value(
        &mut self,
        base: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_val = self.codegen_value(&MirValue::Local(base), func, local_map)?;
        let base_ty = func
            .locals
            .get(base.0 as usize)
            .and_then(|local| local.ty.as_ref());
        if let Some(Type::Ref(inner, _)) = base_ty {
            if matches!(inner.as_ref(), Type::Str | Type::String) {
                let inner_llvm = self.get_llvm_type(inner)?;
                return Ok(unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        inner_llvm,
                        base_val,
                        CString::new("str.deref")?.as_ptr(),
                    )
                });
            }
        }
        Ok(base_val)
    }

    pub(super) fn codegen_string_copy_from_ptr_len(
        &mut self,
        ptr: LLVMValueRef,
        len: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let malloc_fn = self.ensure_malloc_fn()?;
        let malloc_ty = self.malloc_function_type();
        let size_plus = unsafe {
            LLVMBuildAdd(
                self.builder,
                len,
                LLVMConstInt(LLVMTypeOf(len), 1, 0),
                CString::new("str.len.plus")?.as_ptr(),
            )
        };
        let mut malloc_args = vec![size_plus];
        let buf = unsafe {
            LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                malloc_args.as_mut_ptr(),
                malloc_args.len() as u32,
                CString::new("str.alloc")?.as_ptr(),
            )
        };
        let memcpy_fn = self.get_extern_function(functions, "memcpy")?;
        let mut memcpy_args = vec![buf, ptr, len];
        let memcpy_ty = self.function_type_for("memcpy")?;
        unsafe {
            LLVMBuildCall2(
                self.builder,
                memcpy_ty,
                memcpy_fn,
                memcpy_args.as_mut_ptr(),
                memcpy_args.len() as u32,
                CString::new("str.memcpy")?.as_ptr(),
            );
        }
        let i8_ty = unsafe { LLVMInt8TypeInContext(self.context) };
        let buf_i8 = unsafe {
            LLVMBuildBitCast(
                self.builder,
                buf,
                LLVMPointerType(i8_ty, 0),
                CString::new("str.buf.cast")?.as_ptr(),
            )
        };
        let term_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                i8_ty,
                buf_i8,
                vec![len].as_mut_ptr(),
                1,
                CString::new("str.term")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, LLVMConstInt(i8_ty, 0, 0), term_ptr) };
        Ok(buf)
    }

    pub(super) fn codegen_string_empty(
        &mut self,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let zero = unsafe { LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0) };
        let empty_ptr = self.codegen_string_literal("", ".str.empty")?;
        self.codegen_string_copy_from_ptr_len(empty_ptr, zero, functions)
    }

    pub(super) fn codegen_string_len(
        &mut self,
        base: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_val = self.codegen_string_base_value(base, func, local_map)?;
        self.codegen_string_len_value(base_val, functions)
    }

    pub(super) fn codegen_string_concat(
        &mut self,
        base: LocalId,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let left_val = self.codegen_string_base_value(base, func, local_map)?;
        let right_val = match value {
            MirValue::Local(id) => self.codegen_string_base_value(*id, func, local_map)?,
            _ => self.codegen_value(value, func, local_map)?,
        };
        let left_len = self.codegen_string_len_value(left_val, functions)?;
        let right_len = self.codegen_string_len_value(right_val, functions)?;
        let total_len = unsafe {
            LLVMBuildAdd(
                self.builder,
                left_len,
                right_len,
                CString::new("str.concat.len")?.as_ptr(),
            )
        };
        let malloc_fn = self.ensure_malloc_fn()?;
        let malloc_ty = self.malloc_function_type();
        let size_plus = unsafe {
            LLVMBuildAdd(
                self.builder,
                total_len,
                LLVMConstInt(LLVMTypeOf(total_len), 1, 0),
                CString::new("str.concat.size")?.as_ptr(),
            )
        };
        let mut malloc_args = vec![size_plus];
        let buf = unsafe {
            LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                malloc_args.as_mut_ptr(),
                malloc_args.len() as u32,
                CString::new("str.concat.alloc")?.as_ptr(),
            )
        };
        let memcpy_fn = self.get_extern_function(functions, "memcpy")?;
        let memcpy_ty = self.function_type_for("memcpy")?;
        let mut memcpy_args = vec![buf, left_val, left_len];
        unsafe {
            LLVMBuildCall2(
                self.builder,
                memcpy_ty,
                memcpy_fn,
                memcpy_args.as_mut_ptr(),
                memcpy_args.len() as u32,
                CString::new("str.concat.copy1")?.as_ptr(),
            );
        }
        let i8_ty = unsafe { LLVMInt8TypeInContext(self.context) };
        let buf_i8 = unsafe {
            LLVMBuildBitCast(
                self.builder,
                buf,
                LLVMPointerType(i8_ty, 0),
                CString::new("str.concat.cast")?.as_ptr(),
            )
        };
        let right_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                i8_ty,
                buf_i8,
                vec![left_len].as_mut_ptr(),
                1,
                CString::new("str.concat.ptr")?.as_ptr(),
            )
        };
        let mut memcpy_args2 = vec![right_ptr, right_val, right_len];
        unsafe {
            LLVMBuildCall2(
                self.builder,
                memcpy_ty,
                memcpy_fn,
                memcpy_args2.as_mut_ptr(),
                memcpy_args2.len() as u32,
                CString::new("str.concat.copy2")?.as_ptr(),
            );
        }
        let term_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                i8_ty,
                buf_i8,
                vec![total_len].as_mut_ptr(),
                1,
                CString::new("str.concat.term")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, LLVMConstInt(i8_ty, 0, 0), term_ptr) };
        Ok(buf)
    }

    pub(super) fn codegen_string_compare(
        &mut self,
        left: LLVMValueRef,
        right: LLVMValueRef,
        len: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let memcmp_fn = self.get_extern_function(functions, "memcmp")?;
        let mut args = vec![left, right, len];
        let memcmp_ty = self.function_type_for("memcmp")?;
        let cmp_val = unsafe {
            LLVMBuildCall2(
                self.builder,
                memcmp_ty,
                memcmp_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                CString::new("str.memcmp")?.as_ptr(),
            )
        };
        Ok(cmp_val)
    }

    pub(super) fn codegen_string_eq(
        &mut self,
        left: LLVMValueRef,
        right: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let left_len = self.codegen_string_len_value(left, functions)?;
        let right_len = self.codegen_string_len_value(right, functions)?;
        let len_eq = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                left_len,
                right_len,
                CString::new("str.len.eq")?.as_ptr(),
            )
        };
        let len_lt = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntULT,
                left_len,
                right_len,
                CString::new("str.len.lt")?.as_ptr(),
            )
        };
        let min_len = unsafe {
            LLVMBuildSelect(
                self.builder,
                len_lt,
                left_len,
                right_len,
                CString::new("str.len.min")?.as_ptr(),
            )
        };
        let cmp_val = self.codegen_string_compare(left, right, min_len, functions)?;
        let i32_ty = unsafe { LLVMInt32TypeInContext(self.context) };
        let cmp_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cmp_val,
                LLVMConstInt(i32_ty, 0, 0),
                CString::new("str.eq")?.as_ptr(),
            )
        };
        Ok(unsafe {
            LLVMBuildAnd(
                self.builder,
                len_eq,
                cmp_zero,
                CString::new("str.eq.and")?.as_ptr(),
            )
        })
    }

    pub(super) fn codegen_string_clone(
        &mut self,
        base: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_val = self.codegen_string_base_value(base, func, local_map)?;
        let len_val = self.codegen_string_len_value(base_val, functions)?;
        self.codegen_string_copy_from_ptr_len(base_val, len_val, functions)
    }
}
