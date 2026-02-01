use super::*;

impl CodegenContext {
    pub fn codegen_module(&mut self, mir_module: &MirModule) -> Result<()> {
        self.debug_log("create_named_types start");
        self.create_named_types(mir_module)?;
        self.debug_log("register_struct_types start");
        self.register_struct_types(mir_module)?;
        self.debug_log("register_enum_types start");
        self.register_enum_types(mir_module)?;

        let needs_sys_argv = self.mir_uses_sys_argv(mir_module);

        // Declare all functions up-front so calls can reference any order.
        self.debug_log("declare_functions start");
        let functions = self.declare_functions(mir_module, needs_sys_argv)?;

        for func in &mir_module.functions {
            self.debug_log(&format!("codegen_function_body start: {}", func.name));
            let llvm_func = *functions
                .get(&func.name)
                .ok_or_else(|| anyhow!("missing declared function {}", func.name))?;
            self.codegen_function_body(func, llvm_func, &functions, mir_module)?;
            self.debug_log(&format!("codegen_function_body done: {}", func.name));
        }

        if needs_sys_argv {
            if let Some(main_func) = functions.get("main").copied() {
                self.codegen_main_wrapper(mir_module, main_func)?;
            }
        }

        Ok(())
    }

    pub(super) fn declare_functions(
        &mut self,
        mir_module: &MirModule,
        needs_sys_argv: bool,
    ) -> Result<HashMap<String, LLVMValueRef>> {
        let mut functions = HashMap::new();

        for func in &mir_module.functions {
            let func_type = self.llvm_function_type(func)?;
            let llvm_name = if needs_sys_argv && func.name == "main" && func.params.is_empty() {
                "__glyph_main"
            } else {
                func.name.as_str()
            };
            let func_name = CString::new(llvm_name)?;
            let llvm_func = unsafe { LLVMAddFunction(self.module, func_name.as_ptr(), func_type) };
            self.function_types.insert(func.name.clone(), func_type);
            functions.insert(func.name.clone(), llvm_func);
        }

        for func in &mir_module.extern_functions {
            if functions.contains_key(&func.name) {
                continue;
            }
            let func_type = self.llvm_extern_function_type(func)?;
            let symbol_name = func.link_name.as_ref().unwrap_or(&func.name);
            let func_name = CString::new(symbol_name.as_str())?;
            let llvm_func = unsafe { LLVMAddFunction(self.module, func_name.as_ptr(), func_type) };
            self.function_types.insert(func.name.clone(), func_type);
            unsafe { LLVMSetLinkage(llvm_func, LLVMLinkage::LLVMExternalLinkage) };
            if symbol_name == "strdup" && self.strdup_fn.is_none() {
                self.strdup_fn = Some(llvm_func);
            }
            // ABI mapping (v0: only "C" supported; default LLVM CC is C).
            if let Some(abi) = &func.abi {
                if abi != "C" {
                    bail!("unsupported ABI '{}': only \"C\" is supported", abi);
                }
                // If additional ABIs are added, map them via LLVMSetFunctionCallConv here.
            }
            functions.insert(func.name.clone(), llvm_func);
        }

        Ok(functions)
    }

    pub(super) fn llvm_function_type(&self, func: &MirFunction) -> Result<LLVMTypeRef> {
        // Determine return type
        let ret_type = func
            .ret_type
            .as_ref()
            .map(|t| self.get_llvm_type(t))
            .transpose()?
            .unwrap_or_else(|| unsafe { LLVMVoidTypeInContext(self.context) });

        // Build parameter types
        let mut param_types: Vec<LLVMTypeRef> = Vec::new();
        for &param_id in &func.params {
            let local = &func.locals[param_id.0 as usize];
            let param_ty = local
                .ty
                .as_ref()
                .map(|t| self.get_llvm_type(t))
                .transpose()?
                .unwrap_or_else(|| unsafe { LLVMInt32TypeInContext(self.context) });
            param_types.push(param_ty);
        }

        Ok(unsafe {
            LLVMFunctionType(
                ret_type,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                0, // not variadic
            )
        })
    }

    pub(super) fn llvm_extern_function_type(
        &self,
        func: &MirExternFunction,
    ) -> Result<LLVMTypeRef> {
        let ret_type = func
            .ret_type
            .as_ref()
            .map(|t| self.get_llvm_type(t))
            .transpose()?
            .unwrap_or_else(|| unsafe { LLVMVoidTypeInContext(self.context) });

        let mut param_types: Vec<LLVMTypeRef> = Vec::new();
        for param_ty in &func.params {
            param_types.push(self.get_llvm_type(param_ty)?);
        }

        Ok(unsafe {
            LLVMFunctionType(
                ret_type,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                0, // not variadic in v0
            )
        })
    }

    pub(super) fn codegen_function_body(
        &mut self,
        func: &MirFunction,
        llvm_func: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<()> {
        // Create basic blocks
        let mut bb_map: HashMap<BlockId, LLVMBasicBlockRef> = HashMap::new();
        for (i, _) in func.blocks.iter().enumerate() {
            let bb_name = CString::new(format!("bb{}", i))?;
            let bb =
                unsafe { LLVMAppendBasicBlockInContext(self.context, llvm_func, bb_name.as_ptr()) };
            bb_map.insert(BlockId(i as u32), bb);
        }

        self.debug_log("codegen_function_body allocas start");

        // Create local allocas
        let mut local_map: HashMap<LocalId, LLVMValueRef> = HashMap::new();

        // Set up entry block for allocas
        if let Some(&entry_bb) = bb_map.get(&BlockId(0)) {
            unsafe { LLVMPositionBuilderAtEnd(self.builder, entry_bb) };

            for (i, local) in func.locals.iter().enumerate() {
                let local_id = LocalId(i as u32);

                self.debug_log(&format!("alloca local {} ty={:?}", i, local.ty));

                let local_ty = local
                    .ty
                    .as_ref()
                    .map(|t| match t {
                        Type::Void => Ok(unsafe { LLVMInt8TypeInContext(self.context) }),
                        _ => self.get_llvm_type(t),
                    })
                    .transpose()?
                    .unwrap_or_else(|| unsafe { LLVMInt32TypeInContext(self.context) });

                let local_name = local.name.clone().unwrap_or_else(|| format!("tmp{}", i));
                let local_name = CString::new(local_name)?;

                let alloca =
                    unsafe { LLVMBuildAlloca(self.builder, local_ty, local_name.as_ptr()) };
                local_map.insert(local_id, alloca);
            }

            for (i, &param_id) in func.params.iter().enumerate() {
                let param_val = unsafe { LLVMGetParam(llvm_func, i as u32) };
                let slot = local_map
                    .get(&param_id)
                    .ok_or_else(|| anyhow!("missing storage for param {:?}", param_id))?;
                unsafe {
                    LLVMBuildStore(self.builder, param_val, *slot);
                }
            }
        }

        self.debug_log("codegen_function_body allocas done");

        // Codegen each basic block
        for (i, block) in func.blocks.iter().enumerate() {
            self.debug_log(&format!("codegen_block start bb{}", i));
            let bb = bb_map.get(&BlockId(i as u32)).unwrap();
            unsafe { LLVMPositionBuilderAtEnd(self.builder, *bb) };
            self.codegen_block(func, block, &local_map, &bb_map, functions, mir_module)?;
            self.debug_log(&format!("codegen_block done bb{}", i));
        }

        Ok(())
    }

    pub(super) fn codegen_block(
        &mut self,
        func: &MirFunction,
        block: &MirBlock,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        bb_map: &HashMap<BlockId, LLVMBasicBlockRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<()> {
        for inst in &block.insts {
            self.codegen_inst(func, inst, local_map, bb_map, functions, mir_module)?;
        }
        Ok(())
    }

    pub(super) fn codegen_inst(
        &mut self,
        func: &MirFunction,
        inst: &MirInst,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        bb_map: &HashMap<BlockId, LLVMBasicBlockRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<()> {
        unsafe {
            match inst {
                MirInst::Assign { local, value } => {
                    self.debug_log(&format!("codegen_inst assign {}", self.rvalue_tag(value)));
                    let val = self.codegen_rvalue(value, func, local_map, functions, mir_module)?;
                    let local_ty = func
                        .locals
                        .get(local.0 as usize)
                        .and_then(|l| l.ty.as_ref());
                    let is_void = matches!(local_ty, Some(Type::Void))
                        || matches!(local_ty, Some(Type::Tuple(elem_types)) if elem_types.is_empty());
                    if !is_void {
                        let local_ptr = local_map
                            .get(local)
                            .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                        let target_ty = self.local_llvm_type(func, *local)?;
                        let signed = matches!(local_ty, Some(Type::I8 | Type::I32 | Type::I64));
                        let val = self.coerce_int_value(val, target_ty, signed);
                        LLVMBuildStore(self.builder, val, *local_ptr);
                    }
                }
                MirInst::Return(val) => {
                    if let Some(v) = val {
                        let ret_val = self.codegen_value(v, func, local_map)?;
                        LLVMBuildRet(self.builder, ret_val);
                    } else {
                        match func.ret_type.as_ref() {
                            None | Some(Type::Void) => {
                                LLVMBuildRetVoid(self.builder);
                            }
                            Some(Type::Tuple(elem_types)) if elem_types.is_empty() => {
                                LLVMBuildRetVoid(self.builder);
                            }
                            Some(ret_ty) => {
                                let llvm_ret_ty = self.get_llvm_type(ret_ty)?;
                                let zero = LLVMConstNull(llvm_ret_ty);
                                LLVMBuildRet(self.builder, zero);
                            }
                        }
                    }
                }
                MirInst::Goto(target) => {
                    let target_bb = bb_map
                        .get(target)
                        .ok_or_else(|| anyhow!("undefined block {:?}", target))?;
                    LLVMBuildBr(self.builder, *target_bb);
                }
                MirInst::If {
                    cond,
                    then_bb,
                    else_bb,
                } => {
                    let cond_val = self.codegen_value(cond, func, local_map)?;
                    let then_block = bb_map
                        .get(then_bb)
                        .ok_or_else(|| anyhow!("undefined block {:?}", then_bb))?;
                    let else_block = bb_map
                        .get(else_bb)
                        .ok_or_else(|| anyhow!("undefined block {:?}", else_bb))?;
                    LLVMBuildCondBr(self.builder, cond_val, *then_block, *else_block);
                }
                MirInst::Drop(local) => {
                    self.codegen_drop_local(*local, func, local_map)?;
                }
                MirInst::Nop => {}
            }
        }
        Ok(())
    }
}
