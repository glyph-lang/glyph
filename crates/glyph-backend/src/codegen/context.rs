use super::*;

impl CodegenContext {
    pub fn new(module_name: &str) -> Result<Self> {
        unsafe {
            let context = LLVMContextCreate();
            let module_name_c = CString::new(module_name)?;
            let module = LLVMModuleCreateWithNameInContext(module_name_c.as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);

            Ok(Self {
                context,
                module,
                builder,
                struct_types: HashMap::new(),
                struct_layouts: HashMap::new(),
                enum_types: HashMap::new(),
                enum_layouts: HashMap::new(),
                malloc_fn: None,
                free_fn: None,
                strdup_fn: None,
                string_globals: HashMap::new(),
                function_types: HashMap::new(),
                argv_global: None,
                argc_global: None,
                argv_vec_global: None,
            })
        }
    }

    pub fn get_module(&self) -> *mut LLVMModule {
        self.module
    }

    pub fn dump_ir(&self) -> String {
        unsafe {
            let ir_ptr = LLVMPrintModuleToString(self.module);
            let ir_cstr = CStr::from_ptr(ir_ptr);
            let ir_string = ir_cstr.to_string_lossy().into_owned();
            LLVMDisposeMessage(ir_ptr);
            ir_string
        }
    }

    pub(super) fn build_call2(
        &mut self,
        fn_ty: LLVMTypeRef,
        callee: LLVMValueRef,
        args: &mut [LLVMValueRef],
        name: &str,
    ) -> Result<LLVMValueRef> {
        unsafe {
            let ret_ty = LLVMGetReturnType(fn_ty);
            let name = if LLVMGetTypeKind(ret_ty) == llvm_sys::LLVMTypeKind::LLVMVoidTypeKind {
                ""
            } else {
                name
            };
            let name_c = CString::new(name)?;
            let args_ptr = if args.is_empty() {
                std::ptr::null_mut()
            } else {
                args.as_mut_ptr()
            };
            Ok(LLVMBuildCall2(
                self.builder,
                fn_ty,
                callee,
                args_ptr,
                args.len() as u32,
                name_c.as_ptr(),
            ))
        }
    }

    pub(super) fn debug_log(&self, msg: &str) {
        if std::env::var("GLYPH_DEBUG_CODEGEN").is_ok() {
            eprintln!("[codegen] {}", msg);
        }
    }
}
