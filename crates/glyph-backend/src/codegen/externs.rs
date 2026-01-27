use super::*;

impl CodegenContext {
    pub(super) fn function_type_for(&self, name: &str) -> Result<LLVMTypeRef> {
        self.function_types
            .get(name)
            .copied()
            .or_else(|| self.builtin_extern_function_type(name))
            .ok_or_else(|| anyhow!("missing function type for {}", name))
    }

    pub(super) fn builtin_extern_function_type(&self, name: &str) -> Option<LLVMTypeRef> {
        unsafe {
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let usize_ty = LLVMInt64TypeInContext(self.context);
            Some(match name {
                // size_t strlen(const char*)
                "strlen" => {
                    let mut params = vec![i8_ptr];
                    LLVMFunctionType(usize_ty, params.as_mut_ptr(), params.len() as u32, 0)
                }
                // void* memcpy(void*, const void*, size_t)
                "memcpy" => {
                    let mut params = vec![i8_ptr, i8_ptr, usize_ty];
                    LLVMFunctionType(i8_ptr, params.as_mut_ptr(), params.len() as u32, 0)
                }
                // int memcmp(const void*, const void*, size_t)
                "memcmp" => {
                    let mut params = vec![i8_ptr, i8_ptr, usize_ty];
                    LLVMFunctionType(
                        LLVMInt32TypeInContext(self.context),
                        params.as_mut_ptr(),
                        params.len() as u32,
                        0,
                    )
                }
                // char* strstr(const char*, const char*)
                "strstr" => {
                    let mut params = vec![i8_ptr, i8_ptr];
                    LLVMFunctionType(i8_ptr, params.as_mut_ptr(), params.len() as u32, 0)
                }
                // int isspace(int)
                "isspace" => {
                    let mut params = vec![LLVMInt32TypeInContext(self.context)];
                    LLVMFunctionType(
                        LLVMInt32TypeInContext(self.context),
                        params.as_mut_ptr(),
                        params.len() as u32,
                        0,
                    )
                }
                _ => return None,
            })
        }
    }

    pub(super) fn get_or_declare_extern_function(&self, name: &str) -> Result<LLVMValueRef> {
        unsafe {
            let name_c = CString::new(name)?;
            let existing = LLVMGetNamedFunction(self.module, name_c.as_ptr());
            if !existing.is_null() {
                return Ok(existing);
            }
            if let Some(fn_ty) = self.builtin_extern_function_type(name) {
                return Ok(LLVMAddFunction(self.module, name_c.as_ptr(), fn_ty));
            }
            Err(anyhow!("missing extern function {}", name))
        }
    }

    pub(super) fn get_extern_function(
        &self,
        functions: &HashMap<String, LLVMValueRef>,
        name: &str,
    ) -> Result<LLVMValueRef> {
        if let Some(func) = functions.get(name).copied() {
            return Ok(func);
        }
        self.get_or_declare_extern_function(name)
    }

    pub(super) fn ensure_malloc_fn(&mut self) -> Result<LLVMValueRef> {
        if let Some(func) = self.malloc_fn {
            return Ok(func);
        }
        unsafe {
            let fn_ty = self.malloc_function_type();
            let name = CString::new("malloc")?;
            let func = LLVMAddFunction(self.module, name.as_ptr(), fn_ty);
            self.malloc_fn = Some(func);
            Ok(func)
        }
    }

    pub(super) fn ensure_free_fn(&mut self) -> Result<LLVMValueRef> {
        if let Some(func) = self.free_fn {
            return Ok(func);
        }
        unsafe {
            let fn_ty = self.free_function_type();
            let name = CString::new("free")?;
            let func = LLVMAddFunction(self.module, name.as_ptr(), fn_ty);
            self.free_fn = Some(func);
            Ok(func)
        }
    }

    pub(super) fn ensure_strdup_fn(&mut self) -> Result<LLVMValueRef> {
        if let Some(func) = self.strdup_fn {
            return Ok(func);
        }
        unsafe {
            let name = CString::new("strdup")?;
            let existing = LLVMGetNamedFunction(self.module, name.as_ptr());
            if !existing.is_null() {
                self.strdup_fn = Some(existing);
                return Ok(existing);
            }
            let fn_ty = self.strdup_function_type();
            let func = LLVMAddFunction(self.module, name.as_ptr(), fn_ty);
            self.strdup_fn = Some(func);
            Ok(func)
        }
    }

    pub(super) fn ensure_printf_fn(&mut self) -> Result<LLVMValueRef> {
        let name = CString::new("printf")?;
        unsafe {
            let existing = LLVMGetNamedFunction(self.module, name.as_ptr());
            if !existing.is_null() {
                return Ok(existing);
            }
            let fn_ty = self.printf_function_type();
            Ok(LLVMAddFunction(self.module, name.as_ptr(), fn_ty))
        }
    }

    pub(super) fn strdup_function_type(&self) -> LLVMTypeRef {
        unsafe {
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let mut params = vec![i8_ptr];
            LLVMFunctionType(i8_ptr, params.as_mut_ptr(), params.len() as u32, 0)
        }
    }

    pub(super) fn printf_function_type(&self) -> LLVMTypeRef {
        unsafe {
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let mut params = vec![i8_ptr];
            LLVMFunctionType(
                LLVMInt32TypeInContext(self.context),
                params.as_mut_ptr(),
                params.len() as u32,
                1,
            )
        }
    }

    pub(super) fn malloc_function_type(&self) -> LLVMTypeRef {
        unsafe {
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let size_ty = LLVMInt64TypeInContext(self.context);
            let mut params = vec![size_ty];
            LLVMFunctionType(i8_ptr, params.as_mut_ptr(), params.len() as u32, 0)
        }
    }

    pub(super) fn free_function_type(&self) -> LLVMTypeRef {
        unsafe {
            let void_ty = LLVMVoidTypeInContext(self.context);
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let mut params = vec![i8_ptr];
            LLVMFunctionType(void_ty, params.as_mut_ptr(), params.len() as u32, 0)
        }
    }

    pub(super) fn codegen_free(&mut self, ptr: LLVMValueRef) -> Result<()> {
        unsafe {
            let free_fn = self.ensure_free_fn()?;
            let fn_ty = self.free_function_type();
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let cast_name = CString::new("own.free.cast")?;
            let cast_ptr = LLVMBuildBitCast(self.builder, ptr, i8_ptr, cast_name.as_ptr());
            let mut args = vec![cast_ptr];
            let call_name = CString::new("")?;
            LLVMBuildCall2(
                self.builder,
                fn_ty,
                free_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                call_name.as_ptr(),
            );
        }
        Ok(())
    }
}
