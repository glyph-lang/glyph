use std::collections::HashMap;
use std::ffi::{CStr, CString};

use anyhow::{Result, anyhow, bail};
use glyph_core::mir::{
    BlockId, LocalId, MirBlock, MirFunction, MirInst, MirModule, MirValue, Rvalue,
};
use glyph_core::types::{Mutability, StructType, Type};
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMBuilder, LLVMContext, LLVMModule};

pub struct CodegenContext {
    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,
    struct_types: HashMap<String, LLVMTypeRef>,
    struct_layouts: HashMap<String, StructType>,
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
                struct_types: HashMap::new(),
                struct_layouts: HashMap::new(),
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

    fn register_struct_types(&mut self, mir_module: &MirModule) -> Result<()> {
        // First pass: create named struct types
        for (name, layout) in &mir_module.struct_types {
            let name_c = CString::new(name.as_str())?;
            let llvm_ty = unsafe { LLVMStructCreateNamed(self.context, name_c.as_ptr()) };
            self.struct_types.insert(name.clone(), llvm_ty);
            self.struct_layouts.insert(name.clone(), layout.clone());
        }

        // Second pass: set struct bodies
        for (name, layout) in &mir_module.struct_types {
            let llvm_ty = *self
                .struct_types
                .get(name)
                .ok_or_else(|| anyhow!("missing llvm type for struct {}", name))?;

            let mut field_tys: Vec<LLVMTypeRef> = Vec::new();
            for (_, field_ty) in &layout.fields {
                field_tys.push(self.get_llvm_type(field_ty)?);
            }

            unsafe {
                LLVMStructSetBody(llvm_ty, field_tys.as_mut_ptr(), field_tys.len() as u32, 0);
            }
        }

        Ok(())
    }

    fn get_struct_type(&self, name: &str) -> Result<LLVMTypeRef> {
        self.struct_types
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("unknown struct type {}", name))
    }

    fn get_llvm_type(&self, ty: &Type) -> Result<LLVMTypeRef> {
        unsafe {
            Ok(match ty {
                Type::I32 => LLVMInt32TypeInContext(self.context),
                Type::I64 => LLVMInt64TypeInContext(self.context),
                Type::U32 => LLVMInt32TypeInContext(self.context),
                Type::U64 => LLVMInt64TypeInContext(self.context),
                Type::F32 => LLVMFloatTypeInContext(self.context),
                Type::F64 => LLVMDoubleTypeInContext(self.context),
                Type::Bool => LLVMInt1TypeInContext(self.context),
                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Named(name) => self.get_struct_type(name)?,
                Type::Ref(inner, _) => {
                    // TODO(Phase 6): Implement proper reference type support
                    // References should lower to LLVM pointer types
                    let inner_ty = self.get_llvm_type(inner)?;
                    LLVMPointerType(inner_ty, 0)
                }
            })
        }
    }

    pub fn codegen_module(&mut self, mir_module: &MirModule) -> Result<()> {
        self.register_struct_types(mir_module)?;

        // Declare all functions up-front so calls can reference any order.
        let functions = self.declare_functions(mir_module)?;

        for func in &mir_module.functions {
            let llvm_func = *functions
                .get(&func.name)
                .ok_or_else(|| anyhow!("missing declared function {}", func.name))?;
            self.codegen_function_body(func, llvm_func, &functions, mir_module)?;
        }

        Ok(())
    }

    fn declare_functions(
        &mut self,
        mir_module: &MirModule,
    ) -> Result<HashMap<String, LLVMValueRef>> {
        let mut functions = HashMap::new();

        for func in &mir_module.functions {
            let func_type = self.llvm_function_type(func)?;
            let func_name = CString::new(func.name.as_str())?;
            let llvm_func = unsafe { LLVMAddFunction(self.module, func_name.as_ptr(), func_type) };
            functions.insert(func.name.clone(), llvm_func);
        }

        Ok(functions)
    }

    fn llvm_function_type(&self, func: &MirFunction) -> Result<LLVMTypeRef> {
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

    fn codegen_function_body(
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

        // Create local allocas
        let mut local_map: HashMap<LocalId, LLVMValueRef> = HashMap::new();

        // Set up entry block for allocas
        if let Some(&entry_bb) = bb_map.get(&BlockId(0)) {
            unsafe { LLVMPositionBuilderAtEnd(self.builder, entry_bb) };

            for (i, local) in func.locals.iter().enumerate() {
                let local_id = LocalId(i as u32);

                let local_ty = local
                    .ty
                    .as_ref()
                    .map(|t| self.get_llvm_type(t))
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

        // Codegen each basic block
        for (i, block) in func.blocks.iter().enumerate() {
            let bb = bb_map.get(&BlockId(i as u32)).unwrap();
            unsafe { LLVMPositionBuilderAtEnd(self.builder, *bb) };
            self.codegen_block(func, block, &local_map, &bb_map, functions, mir_module)?;
        }

        Ok(())
    }

    fn codegen_block(
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

    fn codegen_inst(
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
                    let val = self.codegen_rvalue(value, func, local_map, functions, mir_module)?;
                    let local_ptr = local_map
                        .get(local)
                        .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                    LLVMBuildStore(self.builder, val, *local_ptr);
                }
                MirInst::Return(val) => {
                    if let Some(v) = val {
                        let ret_val = self.codegen_value(v, func, local_map)?;
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
                    let cond_val = self.codegen_value(cond, func, local_map)?;
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
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
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

                    if LLVMGetTypeKind(LLVMTypeOf(*local_ptr))
                        == llvm_sys::LLVMTypeKind::LLVMPointerTypeKind
                    {
                        let load_name = CString::new("load")?;
                        let llvm_ty = self.local_llvm_type(func, *local_id)?;
                        Ok(LLVMBuildLoad2(
                            self.builder,
                            llvm_ty,
                            *local_ptr,
                            load_name.as_ptr(),
                        ))
                    } else {
                        Ok(*local_ptr)
                    }
                }
                Rvalue::Binary { op, lhs, rhs } => {
                    let lhs_val = self.codegen_value(lhs, func, local_map)?;
                    let rhs_val = self.codegen_value(rhs, func, local_map)?;
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
                Rvalue::StructLit {
                    struct_name,
                    field_values,
                } => self.codegen_struct_literal(struct_name, field_values, func, local_map),
                Rvalue::FieldAccess {
                    base,
                    field_name: _,
                    field_index,
                } => self.codegen_field_access(*base, *field_index as usize, func, local_map),
                Rvalue::Call { name, args } => {
                    let callee = functions
                        .get(name)
                        .copied()
                        .ok_or_else(|| anyhow!("unknown function {}", name))?;

                    // Find the MIR function to get its type signature
                    let target_func = mir_module
                        .functions
                        .iter()
                        .find(|f| f.name == *name)
                        .ok_or_else(|| anyhow!("function {} not found in MIR module", name))?;

                    // Build the LLVM function type from the MIR function signature
                    let fn_ty = self.llvm_function_type(target_func)?;

                    // Codegen arguments
                    let mut llvm_args: Vec<LLVMValueRef> = Vec::new();
                    for arg in args {
                        llvm_args.push(self.codegen_value(arg, func, local_map)?);
                    }

                    let call_name = CString::new("call")?;
                    Ok(LLVMBuildCall2(
                        self.builder,
                        fn_ty,
                        callee,
                        llvm_args.as_mut_ptr(),
                        llvm_args.len() as u32,
                        call_name.as_ptr(),
                    ))
                }
                Rvalue::Ref { base, .. } => {
                    let base_ptr = local_map
                        .get(base)
                        .ok_or_else(|| anyhow!("undefined local {:?}", base))?;
                    Ok(*base_ptr)
                }
            }
        }
    }

    fn codegen_value(
        &mut self,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            match value {
                MirValue::Unit => {
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

                    if LLVMGetTypeKind(LLVMTypeOf(*local_ptr))
                        == llvm_sys::LLVMTypeKind::LLVMPointerTypeKind
                    {
                        let load_name = CString::new("load")?;
                        let llvm_ty = self.local_llvm_type(func, *local_id)?;
                        Ok(LLVMBuildLoad2(
                            self.builder,
                            llvm_ty,
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

    fn local_llvm_type(&self, func: &MirFunction, local_id: LocalId) -> Result<LLVMTypeRef> {
        let local = func
            .locals
            .get(local_id.0 as usize)
            .ok_or_else(|| anyhow!("missing local {:?}", local_id))?;
        if let Some(ty) = local.ty.as_ref() {
            self.get_llvm_type(ty)
        } else {
            Ok(unsafe { LLVMInt32TypeInContext(self.context) })
        }
    }

    fn codegen_struct_literal(
        &mut self,
        struct_name: &str,
        field_values: &[(String, MirValue)],
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let llvm_struct = self.get_struct_type(struct_name)?;
        let layout = self
            .struct_layouts
            .get(struct_name)
            .ok_or_else(|| anyhow!("missing layout for struct {}", struct_name))?;
        let fields = layout.fields.clone();

        let alloca_name = CString::new(format!("{}_tmp", struct_name))?;
        let alloca = unsafe { LLVMBuildAlloca(self.builder, llvm_struct, alloca_name.as_ptr()) };

        let mut value_map = HashMap::new();
        for (name, value) in field_values {
            value_map.insert(name.as_str(), value);
        }

        for (idx, (field_name, _)) in fields.iter().enumerate() {
            let value = value_map.get(field_name.as_str()).ok_or_else(|| {
                anyhow!("missing field {} for struct {}", field_name, struct_name)
            })?;
            let llvm_field_val = self.codegen_value(value, func, local_map)?;
            let gep_name = CString::new(format!("{}.{}", struct_name, field_name))?;
            let field_ptr = unsafe {
                LLVMBuildStructGEP2(
                    self.builder,
                    llvm_struct,
                    alloca,
                    idx as u32,
                    gep_name.as_ptr(),
                )
            };
            unsafe {
                LLVMBuildStore(self.builder, llvm_field_val, field_ptr);
            }
        }

        let load_name = CString::new(format!("{}_val", struct_name))?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, llvm_struct, alloca, load_name.as_ptr()) })
    }

    fn codegen_field_access(
        &mut self,
        base: LocalId,
        field_index: usize,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let (struct_name, struct_ptr) = self.struct_pointer_for_local(base, func, local_map)?;
        let llvm_struct = self.get_struct_type(struct_name.as_str())?;
        let field_ty = {
            let layout = self
                .struct_layouts
                .get(&struct_name)
                .ok_or_else(|| anyhow!("missing layout for struct {}", struct_name))?;
            layout
                .fields
                .get(field_index)
                .map(|(_, ty)| ty.clone())
                .ok_or_else(|| anyhow!("invalid field index {} for {}", field_index, struct_name))?
        };

        let gep_name = CString::new(format!("{}.field{}", struct_name, field_index))?;
        let field_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_struct,
                struct_ptr,
                field_index as u32,
                gep_name.as_ptr(),
            )
        };
        let llvm_field_ty = self.get_llvm_type(&field_ty)?;
        let load_name = CString::new("field.load")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, llvm_field_ty, field_ptr, load_name.as_ptr()) })
    }

    fn struct_pointer_for_local(
        &mut self,
        local: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<(String, LLVMValueRef)> {
        let mut ty = func
            .locals
            .get(local.0 as usize)
            .and_then(|l| l.ty.clone())
            .ok_or_else(|| anyhow!("field access base has unknown type"))?;
        let mut ptr = *local_map
            .get(&local)
            .ok_or_else(|| anyhow!("undefined local {:?}", local))?;

        loop {
            match ty.clone() {
                Type::Named(name) => return Ok((name, ptr)),
                Type::Ref(inner, mutability) => {
                    let ref_ty = Type::Ref(inner.clone(), mutability);
                    let load_ty = self.get_llvm_type(&ref_ty)?;
                    let load_name = CString::new("deref.struct")?;
                    ptr = unsafe { LLVMBuildLoad2(self.builder, load_ty, ptr, load_name.as_ptr()) };
                    ty = *inner;
                }
                _ => bail!("field access base is not a struct"),
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
    use glyph_core::mir::{
        Local, LocalId, MirBlock, MirFunction, MirInst, MirModule, MirValue, Rvalue,
    };
    use std::collections::HashMap;

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
            struct_types: HashMap::new(),
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
            struct_types: HashMap::new(),
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

    #[test]
    fn codegens_struct_literal() {
        let mut ctx = CodegenContext::new("test").unwrap();
        let mut struct_types = HashMap::new();
        struct_types.insert(
            "Point".into(),
            StructType {
                name: "Point".into(),
                fields: vec![("x".into(), Type::I32), ("y".into(), Type::I32)],
            },
        );

        let mir = MirModule {
            struct_types,
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::Named("Point".into())),
                params: vec![],
                locals: vec![
                    Local {
                        name: Some("p".into()),
                        ty: Some(Type::Named("Point".into())),
                    },
                    Local {
                        name: None,
                        ty: Some(Type::Named("Point".into())),
                    },
                ],
                blocks: vec![MirBlock {
                    insts: vec![
                        MirInst::Assign {
                            local: LocalId(1),
                            value: Rvalue::StructLit {
                                struct_name: "Point".into(),
                                field_values: vec![
                                    ("x".into(), MirValue::Int(1)),
                                    ("y".into(), MirValue::Int(2)),
                                ],
                            },
                        },
                        MirInst::Assign {
                            local: LocalId(0),
                            value: Rvalue::Move(LocalId(1)),
                        },
                        MirInst::Return(Some(MirValue::Local(LocalId(0)))),
                    ],
                }],
            }],
        };

        ctx.codegen_module(&mir).unwrap();
        let ir = ctx.dump_ir();
        assert!(ir.contains("%Point = type { i32, i32 }"));
        assert!(ir.contains("getelementptr inbounds"));
    }

    #[test]
    fn codegens_field_access() {
        let mut ctx = CodegenContext::new("test").unwrap();
        let mut struct_types = HashMap::new();
        struct_types.insert(
            "Point".into(),
            StructType {
                name: "Point".into(),
                fields: vec![("x".into(), Type::I32), ("y".into(), Type::I32)],
            },
        );

        let mir = MirModule {
            struct_types,
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![
                    Local {
                        name: Some("p".into()),
                        ty: Some(Type::Named("Point".into())),
                    },
                    Local {
                        name: None,
                        ty: Some(Type::Named("Point".into())),
                    },
                    Local {
                        name: None,
                        ty: Some(Type::I32),
                    },
                ],
                blocks: vec![MirBlock {
                    insts: vec![
                        MirInst::Assign {
                            local: LocalId(1),
                            value: Rvalue::StructLit {
                                struct_name: "Point".into(),
                                field_values: vec![
                                    ("x".into(), MirValue::Int(10)),
                                    ("y".into(), MirValue::Int(20)),
                                ],
                            },
                        },
                        MirInst::Assign {
                            local: LocalId(0),
                            value: Rvalue::Move(LocalId(1)),
                        },
                        MirInst::Assign {
                            local: LocalId(2),
                            value: Rvalue::FieldAccess {
                                base: LocalId(0),
                                field_name: "y".into(),
                                field_index: 1,
                            },
                        },
                        MirInst::Return(Some(MirValue::Local(LocalId(2)))),
                    ],
                }],
            }],
        };

        ctx.codegen_module(&mir).unwrap();
        let ir = ctx.dump_ir();
        assert!(ir.contains("getelementptr inbounds"));
        assert!(ir.contains("ret i32"));
    }
}
