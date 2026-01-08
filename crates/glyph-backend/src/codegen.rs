use std::collections::HashMap;
use std::ffi::{CStr, CString};

use anyhow::{Result, anyhow};
use glyph_core::mir::{
    BlockId, LocalId, MirBlock, MirFunction, MirInst, MirModule, MirValue, Rvalue,
};
use glyph_core::types::Type;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMBuilder, LLVMContext, LLVMModule};

pub struct CodegenContext {
    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,
}

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

    fn get_llvm_type(&self, ty: &Type) -> LLVMTypeRef {
        unsafe {
            match ty {
                Type::I32 => LLVMInt32TypeInContext(self.context),
                Type::I64 => LLVMInt64TypeInContext(self.context),
                Type::U32 => LLVMInt32TypeInContext(self.context),
                Type::U64 => LLVMInt64TypeInContext(self.context),
                Type::F32 => LLVMFloatTypeInContext(self.context),
                Type::F64 => LLVMDoubleTypeInContext(self.context),
                Type::Bool => LLVMInt1TypeInContext(self.context),
                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Named(_) => {
                    // Placeholder for struct types - will implement later
                    LLVMInt32TypeInContext(self.context)
                }
            }
        }
    }

    pub fn codegen_module(&mut self, mir_module: &MirModule) -> Result<()> {
        for func in &mir_module.functions {
            self.codegen_function(func)?;
        }
        Ok(())
    }

    fn codegen_function(&mut self, func: &MirFunction) -> Result<()> {
        // Determine return type
        let ret_type = func
            .ret_type
            .as_ref()
            .map(|t| self.get_llvm_type(t))
            .unwrap_or_else(|| unsafe { LLVMVoidTypeInContext(self.context) });

        // Build parameter types
        let mut param_types: Vec<LLVMTypeRef> = Vec::new();
        for &param_id in &func.params {
            let local = &func.locals[param_id.0 as usize];
            let param_ty = local
                .ty
                .as_ref()
                .map(|t| self.get_llvm_type(t))
                .unwrap_or_else(|| unsafe { LLVMInt32TypeInContext(self.context) });
            param_types.push(param_ty);
        }

        // Create function type
        let func_type = unsafe {
            LLVMFunctionType(
                ret_type,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                0, // not variadic
            )
        };

        // Create function
        let func_name = CString::new(func.name.as_str())?;
        let llvm_func = unsafe { LLVMAddFunction(self.module, func_name.as_ptr(), func_type) };

        // Create basic blocks
        let mut bb_map: HashMap<BlockId, LLVMBasicBlockRef> = HashMap::new();
        for (i, _) in func.blocks.iter().enumerate() {
            let bb_name = CString::new(format!("bb{}", i))?;
            let bb =
                unsafe { LLVMAppendBasicBlockInContext(self.context, llvm_func, bb_name.as_ptr()) };
            bb_map.insert(BlockId(i as u32), bb);
        }

        // Create local allocas
        let mut local_map: HashMap<LocalId, LLVMValueRef> = HashMap::new();

        // Set up entry block for allocas
        if let Some(&entry_bb) = bb_map.get(&BlockId(0)) {
            unsafe { LLVMPositionBuilderAtEnd(self.builder, entry_bb) };

            // Map parameters to function arguments
            for (i, &param_id) in func.params.iter().enumerate() {
                let param_val = unsafe { LLVMGetParam(llvm_func, i as u32) };
                local_map.insert(param_id, param_val);
            }

            // Allocate space for other locals
            for (i, local) in func.locals.iter().enumerate() {
                let local_id = LocalId(i as u32);
                if func.params.contains(&local_id) {
                    continue; // Already handled as parameter
                }

                let local_ty = local
                    .ty
                    .as_ref()
                    .map(|t| self.get_llvm_type(t))
                    .unwrap_or_else(|| unsafe { LLVMInt32TypeInContext(self.context) });

                let local_name = CString::new(
                    local
                        .name
                        .as_ref()
                        .map(|s| s.as_str())
                        .unwrap_or(&format!("tmp{}", i)),
                )?;

                let alloca =
                    unsafe { LLVMBuildAlloca(self.builder, local_ty, local_name.as_ptr()) };
                local_map.insert(local_id, alloca);
            }
        }

        // Codegen each basic block
        for (i, block) in func.blocks.iter().enumerate() {
            let bb = bb_map.get(&BlockId(i as u32)).unwrap();
            unsafe { LLVMPositionBuilderAtEnd(self.builder, *bb) };
            self.codegen_block(block, &local_map, &bb_map)?;
        }

        Ok(())
    }

    fn codegen_block(
        &mut self,
        block: &MirBlock,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        bb_map: &HashMap<BlockId, LLVMBasicBlockRef>,
    ) -> Result<()> {
        for inst in &block.insts {
            self.codegen_inst(inst, local_map, bb_map)?;
        }
        Ok(())
    }

    fn codegen_inst(
        &mut self,
        inst: &MirInst,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        bb_map: &HashMap<BlockId, LLVMBasicBlockRef>,
    ) -> Result<()> {
        unsafe {
            match inst {
                MirInst::Assign { local, value } => {
                    let val = self.codegen_rvalue(value, local_map)?;
                    let local_ptr = local_map
                        .get(local)
                        .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                    LLVMBuildStore(self.builder, val, *local_ptr);
                }
                MirInst::Return(val) => {
                    if let Some(v) = val {
                        let ret_val = self.codegen_value(v, local_map)?;
                        LLVMBuildRet(self.builder, ret_val);
                    } else {
                        LLVMBuildRetVoid(self.builder);
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
                    let cond_val = self.codegen_value(cond, local_map)?;
                    let then_block = bb_map
                        .get(then_bb)
                        .ok_or_else(|| anyhow!("undefined block {:?}", then_bb))?;
                    let else_block = bb_map
                        .get(else_bb)
                        .ok_or_else(|| anyhow!("undefined block {:?}", else_bb))?;
                    LLVMBuildCondBr(self.builder, cond_val, *then_block, *else_block);
                }
                MirInst::Nop => {}
            }
        }
        Ok(())
    }

    fn codegen_rvalue(
        &mut self,
        rvalue: &Rvalue,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            match rvalue {
                Rvalue::ConstInt(i) => {
                    let ty = LLVMInt32TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, *i as u64, 1))
                }
                Rvalue::ConstBool(b) => {
                    let ty = LLVMInt1TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, if *b { 1 } else { 0 }, 0))
                }
                Rvalue::Move(local_id) => {
                    let local_ptr = local_map
                        .get(local_id)
                        .ok_or_else(|| anyhow!("undefined local {:?}", local_id))?;

                    // Parameters are values, locals are allocas
                    if LLVMGetTypeKind(LLVMTypeOf(*local_ptr))
                        == llvm_sys::LLVMTypeKind::LLVMPointerTypeKind
                    {
                        let load_name = CString::new("load")?;
                        Ok(LLVMBuildLoad2(
                            self.builder,
                            LLVMInt32TypeInContext(self.context), // TODO: use actual type
                            *local_ptr,
                            load_name.as_ptr(),
                        ))
                    } else {
                        Ok(*local_ptr)
                    }
                }
                Rvalue::Binary { op, lhs, rhs } => {
                    let lhs_val = self.codegen_value(lhs, local_map)?;
                    let rhs_val = self.codegen_value(rhs, local_map)?;
                    let name = CString::new("binop")?;

                    use glyph_core::ast::BinaryOp;
                    let result = match op {
                        BinaryOp::Add => {
                            LLVMBuildAdd(self.builder, lhs_val, rhs_val, name.as_ptr())
                        }
                        BinaryOp::Sub => {
                            LLVMBuildSub(self.builder, lhs_val, rhs_val, name.as_ptr())
                        }
                        BinaryOp::Mul => {
                            LLVMBuildMul(self.builder, lhs_val, rhs_val, name.as_ptr())
                        }
                        BinaryOp::Div => {
                            LLVMBuildSDiv(self.builder, lhs_val, rhs_val, name.as_ptr())
                        }
                        BinaryOp::Eq => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Ne => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntNE,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Lt => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Le => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Gt => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Ge => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::And | BinaryOp::Or => {
                            return Err(anyhow!("logical ops should be lowered in MIR"));
                        }
                    };
                    Ok(result)
                }
                Rvalue::Call { name, args } => {
                    // Placeholder - will implement function calls later
                    let _ = (name, args);
                    Err(anyhow!("function calls not yet implemented in codegen"))
                }
            }
        }
    }

    fn codegen_value(
        &mut self,
        value: &MirValue,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            match value {
                MirValue::Unit => {
                    // Return a placeholder zero value
                    let ty = LLVMInt32TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, 0, 0))
                }
                MirValue::Int(i) => {
                    let ty = LLVMInt32TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, *i as u64, 1))
                }
                MirValue::Bool(b) => {
                    let ty = LLVMInt1TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, if *b { 1 } else { 0 }, 0))
                }
                MirValue::Local(local_id) => {
                    let local_ptr = local_map
                        .get(local_id)
                        .ok_or_else(|| anyhow!("undefined local {:?}", local_id))?;

                    // Check if this is a parameter (no need to load)
                    if LLVMGetTypeKind(LLVMTypeOf(*local_ptr))
                        == llvm_sys::LLVMTypeKind::LLVMPointerTypeKind
                    {
                        let load_name = CString::new("load")?;
                        Ok(LLVMBuildLoad2(
                            self.builder,
                            LLVMInt32TypeInContext(self.context), // TODO: use actual type
                            *local_ptr,
                            load_name.as_ptr(),
                        ))
                    } else {
                        Ok(*local_ptr)
                    }
                }
            }
        }
    }

    /// Execute a function via JIT for testing purposes
    /// Note: This creates a clone of the module for JIT execution
    pub fn jit_execute_i32(&self, fn_name: &str) -> Result<i32> {
        use llvm_sys::execution_engine::*;
        use llvm_sys::target::*;

        unsafe {
            // Initialize native target for JIT
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();

            // Clone the module so the execution engine can take ownership
            let module_clone = LLVMCloneModule(self.module);

            // Create MCJIT execution engine
            let mut ee = std::ptr::null_mut();
            let mut error = std::ptr::null_mut();

            LLVMLinkInMCJIT();

            if LLVMCreateMCJITCompilerForModule(
                &mut ee,
                module_clone,
                std::ptr::null_mut(),
                0,
                &mut error,
            ) != 0
            {
                let err_msg = if error.is_null() {
                    "unknown error".to_string()
                } else {
                    let msg = CStr::from_ptr(error).to_string_lossy().into_owned();
                    LLVMDisposeMessage(error);
                    msg
                };
                return Err(anyhow!("Failed to create execution engine: {}", err_msg));
            }

            // Find the function
            let fn_name_c = CString::new(fn_name)?;
            let func_addr = LLVMGetFunctionAddress(ee, fn_name_c.as_ptr());

            if func_addr == 0 {
                LLVMDisposeExecutionEngine(ee);
                return Err(anyhow!("Function {} not found", fn_name));
            }

            // Cast to function pointer and call it
            let func: extern "C" fn() -> i32 = std::mem::transmute(func_addr);
            let result = func();

            // Clean up execution engine (which also disposes the cloned module)
            LLVMDisposeExecutionEngine(ee);

            Ok(result)
        }
    }
}

impl Drop for CodegenContext {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use glyph_core::mir::{MirBlock, MirFunction, MirModule};

    #[test]
    fn creates_empty_module() {
        let ctx = CodegenContext::new("test").unwrap();
        let ir = ctx.dump_ir();
        assert!(ir.contains("test"));
    }

    #[test]
    fn codegens_simple_function() {
        let mut ctx = CodegenContext::new("test").unwrap();
        let mir = MirModule {
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![],
                blocks: vec![MirBlock {
                    insts: vec![MirInst::Return(Some(MirValue::Int(42)))],
                }],
            }],
        };
        ctx.codegen_module(&mir).unwrap();
        let ir = ctx.dump_ir();
        assert!(ir.contains("define i32 @main"));
        assert!(ir.contains("ret i32 42"));
    }

    #[test]
    fn jit_executes_simple_function() {
        let mut ctx = CodegenContext::new("test").unwrap();
        let mir = MirModule {
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![],
                blocks: vec![MirBlock {
                    insts: vec![MirInst::Return(Some(MirValue::Int(42)))],
                }],
            }],
        };
        ctx.codegen_module(&mir).unwrap();
        let result = ctx.jit_execute_i32("main").unwrap();
        assert_eq!(result, 42);
    }
}
