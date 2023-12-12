use std::ffi::*;
use std::fmt;
use std::marker::PhantomData;
use std::mem;
use std::ptr::null_mut;

use crate::c_str;
use crate::frontend::ast::CallingConvention;
use llvm_sys::analysis::*;
use llvm_sys::core::*;

use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::*;


macro_rules! opt_c_str {
    ($e:expr) => {
        ($e.map(|n| c_str!(n)).unwrap_or(c_str!("")))
    };
}

macro_rules! slice_llvm_ref {
    ($s:expr) => {
        $s.iter()
            .map(|v| v.as_llvm_ref())
            .collect::<Vec<_>>()
            .as_mut_ptr()
    };
}

macro_rules! new_type {
    ($e:expr) => {
        unsafe { Type::from_llvm_ref($e) }
    };
}

macro_rules! new_value {
    ($e:expr) => {
        {
            unsafe { Value::from_llvm_ref($e) }
        }
    };
}

pub trait AsLLVMRef {
    type Target;

    unsafe fn as_llvm_ref(&self) -> Self::Target;
}

pub trait FromLLVMRef {
    type From;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self;
}

pub struct Context(LLVMContextRef);

impl AsLLVMRef for Context {
    type Target = LLVMContextRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

pub fn get_enum_attribute_kind_for_name(name: &str) -> usize {
    (unsafe { LLVMGetEnumAttributeKindForName(c_str!(name), name.len()) }) as usize
}

impl Context {
    pub fn create() -> Context {
        Self(unsafe { LLVMContextCreate() })
    }

    pub fn new() -> Context {
        Self::create()
    }

    pub fn get_diagnostic_context(&self) -> DiagnosticContext {
        unsafe { LLVMContextGetDiagnosticContext(self.as_llvm_ref()) }
    }

    pub fn get_diagnostic_handler(&self) -> DiagnosticHandler {
        unsafe {
            DiagnosticHandler::from_llvm_ref(LLVMContextGetDiagnosticHandler(self.as_llvm_ref()))
        }
    }

    pub fn create_named_struct(&self, name: &str, struct_fields: &[Type], packed: bool) -> Type {
        unsafe {
            let struct_ty = LLVMStructCreateNamed(self.as_llvm_ref(), c_str!(name));
            LLVMStructSetBody(struct_ty, struct_fields.iter().map(|t| t.as_llvm_ref()).collect::<Vec<_>>().as_mut_ptr(), struct_fields.len() as u32, packed as i32);
            Type::from_llvm_ref(struct_ty)
        }
    }

    pub fn set_diagnostic_handler(
        &self,
        handler: DiagnosticHandler,
        diagnostic_context: DiagnosticContext,
    ) {
        unsafe {
            LLVMContextSetDiagnosticHandler(
                self.as_llvm_ref(),
                handler.as_llvm_ref(),
                diagnostic_context,
            )
        }
    }

    pub fn set_discard_value_names(&self, discard: bool) {
        unsafe { LLVMContextSetDiscardValueNames(self.as_llvm_ref(), discard as i32) }
    }

    pub fn should_discard_value_names(&self) -> bool {
        (unsafe { LLVMContextShouldDiscardValueNames(self.as_llvm_ref()) } == 0)
    }

    pub fn create_enum_attribute(&self, kind_id: u32, value: u64) -> Attribute {
        unsafe {
            Attribute::from_llvm_ref(LLVMCreateEnumAttribute(self.as_llvm_ref(), kind_id, value))
        }
    }

    pub fn create_string_attribute(&self, key: &str, value: &str) -> Attribute {
        unsafe {
            Attribute::from_llvm_ref(LLVMCreateStringAttribute(
                self.as_llvm_ref(),
                key.as_ptr().cast(),
                key.len() as u32,
                value.as_ptr().cast(),
                value.len() as u32,
            ))
        }
    }

    pub fn create_type_attribute(&self, kind_id: u32, target_type: Type) -> Attribute {
        unsafe {
            Attribute::from_llvm_ref(LLVMCreateTypeAttribute(
                self.as_llvm_ref(),
                kind_id,
                target_type.as_llvm_ref(),
            ))
        }
    }

    pub fn struct_type(&self, element_types: &[Type], is_packed: bool) -> Type {
        unsafe {
            let struct_type_ptr = LLVMStructTypeInContext(
                self.as_llvm_ref(),
                slice_llvm_ref!(element_types),
                element_types.len() as u32,
                is_packed as i32,
            );
            Type::from_llvm_ref(struct_type_ptr)
        }
    }
}

pub type DiagnosticContext = *mut c_void;

pub struct DiagnosticInfo(LLVMDiagnosticInfoRef);

impl AsLLVMRef for DiagnosticInfo {
    type Target = LLVMDiagnosticInfoRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

impl FromLLVMRef for DiagnosticInfo {
    type From = LLVMDiagnosticInfoRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating DiagnosticInfo from null");
        Self(llvm_ref)
    }
}

pub struct DiagnosticHandler(LLVMDiagnosticHandler);

impl AsLLVMRef for DiagnosticHandler {
    type Target = LLVMDiagnosticHandler;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

impl FromLLVMRef for DiagnosticHandler {
    type From = LLVMDiagnosticHandler;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        Self(llvm_ref)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VerifyError(String);

impl VerifyError {
    pub fn new(message: String) -> Self {
        Self(message)
    }

    pub fn new_from_raw(ptr: *mut i8) -> Self {
        Self(
            unsafe { CString::from_raw(ptr) }
                .to_string_lossy()
                .to_string(),
        )
    }
}

impl fmt::Display for VerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for VerifyError {}

pub struct Module<'ctx>(LLVMModuleRef, PhantomData<&'ctx ()>);

impl<'ctx> Module<'ctx> {
    pub fn create(name: &str) -> Module<'ctx> {
        Module(
            unsafe { LLVMModuleCreateWithName(c_str!(name)) },
            PhantomData,
        )
    }

    pub fn data_layout(&self) -> LLVMTargetDataRef {
        unsafe { LLVMGetModuleDataLayout(self.as_llvm_ref()) }
    }

    pub fn new(name: &str) -> Module<'ctx> {
        Self::create(name)
    }

    pub fn create_in_context(name: &str, context: &'ctx Context) -> Module<'ctx> {
        Module(
            unsafe { LLVMModuleCreateWithNameInContext(c_str!(name), context.as_llvm_ref()) },
            PhantomData,
        )
    }

    pub fn new_in_context(name: &str, context: &'ctx Context) -> Module<'ctx> {
        Self::create_in_context(name, context)
    }

    pub fn set_target_triple(&self, target: &str) {
        unsafe { LLVMSetTarget(self.as_llvm_ref(), c_str!(target)) };
    }

    pub fn get_named_function(&self, function_name: &str) -> Option<Function> {
        let function = unsafe { LLVMGetNamedFunction(self.as_llvm_ref(), c_str!(function_name)) };
        if function.is_null() {
            None
        } else {
            Some(Function(new_value!(function)))
        }
    }

    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.as_llvm_ref()) }
    }

    /// Verifies this module.
    pub fn verify(&self) -> Result<(), VerifyError> {
        let message = null_mut();
        if unsafe {
            LLVMVerifyModule(
                self.as_llvm_ref(),
                LLVMVerifierFailureAction::LLVMPrintMessageAction,
                message,
            )
        } != 0
        {
            if message.is_null() {
                return Err(VerifyError::new("Error verifying the module".to_string()));
            } else {
                return Err(VerifyError::new_from_raw(unsafe { *message }));
            }
        }
        Ok(())
    }

    pub fn add_alias(
        &self,
        value_type: Type,
        address_space: u32,
        aliasee: Value,
        name: &str,
    ) -> Value {
        unsafe {
            Value::from_llvm_ref(LLVMAddAlias2(
                self.as_llvm_ref(),
                value_type.as_llvm_ref(),
                address_space,
                aliasee.as_llvm_ref(),
                c_str!(name),
            ))
        }
    }

    pub fn add_function(&self, function_name: &str, function_type: Type) -> Function {
        unsafe {
            Function(Value::from_llvm_ref(LLVMAddFunction(
                self.as_llvm_ref(),
                c_str!(function_name),
                function_type.as_llvm_ref(),
            )))
        }
    }

    pub fn add_global(&self, name: &str, ty: Type) -> Value {
        unsafe {
            Value::from_llvm_ref(LLVMAddGlobal(
                self.as_llvm_ref(),
                ty.as_llvm_ref(),
                c_str!(name),
            ))
        }
    }

    pub fn add_global_in_address_space(&self, name: &str, ty: Type, address_space: u32) -> Value {
        unsafe {
            Value::from_llvm_ref(LLVMAddGlobalInAddressSpace(
                self.as_llvm_ref(),
                ty.as_llvm_ref(),
                c_str!(name),
                address_space,
            ))
        }
    }

    pub fn add_global_ifunc(
        &self,
        name: &str,
        function_type: Type,
        address_space: u32,
        resolver: Value,
    ) -> Value {
        unsafe {
            Value::from_llvm_ref(LLVMAddGlobalIFunc(
                self.as_llvm_ref(),
                c_str!(name),
                name.len(),
                function_type.as_llvm_ref(),
                address_space,
                resolver.as_llvm_ref(),
            ))
        }
    }

    /// Add a module-level flag to the module-level flags metadata if it doesn’t already exist.
    pub fn add_flag(
        &self,
        behaviour: FlagBehaviour,
        key: &str,
        key_length: usize,
        value: Metadata,
    ) {
        unsafe {
            LLVMAddModuleFlag(
                self.as_llvm_ref(),
                behaviour.as_llvm_ref(),
                c_str!(key),
                key_length,
                value.as_llvm_ref(),
            )
        }
    }

    pub fn add_named_metadata_operand(&self, name: &str, value: Value) {
        unsafe {
            LLVMAddNamedMetadataOperand(self.as_llvm_ref(), c_str!(name), value.as_llvm_ref())
        }
    }

    /// Append to the module-scope inline assembly blocks.
    pub fn append_inline_assembly(&self, assembly: &str) {
        unsafe { LLVMAppendModuleInlineAsm(self.as_llvm_ref(), c_str!(assembly), assembly.len()) }
    }
}

impl<'ctx> Clone for Module<'ctx> {
    fn clone(&self) -> Self {
        Self(unsafe { LLVMCloneModule(self.0) }, PhantomData)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FlagBehaviour {
    Error,
    Warning,
    Require,
    Override,
    Append,
    AppendUnique,
}

impl AsLLVMRef for FlagBehaviour {
    type Target = LLVMModuleFlagBehavior;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        mem::transmute(*self as u8 as u32)
    }
}

#[derive(Clone, Copy)]
pub struct Metadata(LLVMMetadataRef);

impl FromLLVMRef for Metadata {
    type From = LLVMMetadataRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating Metadata from null");
        Self(llvm_ref)
    }
}

impl AsLLVMRef for Metadata {
    type Target = LLVMMetadataRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

pub struct Builder<'ctx>(LLVMBuilderRef, PhantomData<&'ctx ()>);

impl<'ctx> Builder<'ctx> {
    pub fn create() -> Builder<'ctx> {
        Self(unsafe { LLVMCreateBuilder() }, PhantomData)
    }

    pub fn create_in_context(context: &'ctx Context) -> Builder<'ctx> {
        Self(
            unsafe { LLVMCreateBuilderInContext(context.as_llvm_ref()) },
            PhantomData,
        )
    }

    pub fn new() -> Builder<'ctx> {
        Self::create()
    }

    pub fn new_in_context(context: &'ctx Context) -> Builder<'ctx> {
        Self::create_in_context(context)
    }

    /// Adds the metadata registered with the given builder to the given instruction.
    pub fn add_metadata_to_instruction(&self, instruction: Value) {
        unsafe { LLVMAddMetadataToInst(self.as_llvm_ref(), instruction.as_llvm_ref()) }
    }

    pub fn position_at_end(&self, block: &BasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.as_llvm_ref(), block.as_llvm_ref()) }
    }

    pub fn insert_position(&self) -> Option<BasicBlock> {
        let block = unsafe { LLVMGetInsertBlock(self.as_llvm_ref()) };
        if block.is_null() {
            None
        } else {
            Some(unsafe { BasicBlock::from_llvm_ref(block) })
        }
    }

    pub fn build_ashr(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildAShr(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_add(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildAdd(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_address_space_cast(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildAddrSpaceCast(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_aggregate_return(&self, return_values: &[Value]) -> Value {
        new_value!(LLVMBuildAggregateRet(
            self.as_llvm_ref(),
            slice_llvm_ref!(return_values),
            return_values.len() as u32
        ))
    }

    pub fn build_alloca(&self, target_type: Type, name: Option<&str>) -> Value {
        new_value!(LLVMBuildAlloca(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_and(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildAnd(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_array_alloca(&self, target_type: Type, value: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildArrayAlloca(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            value.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_array_malloc(&self, target_type: Type, value: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildArrayMalloc(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            value.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_atomic_cmp_xchg(
        &self,
        pointer: Value,
        cmp: Value,
        new: Value,
        success_ordering: AtomicOrdering,
        failure_ordering: AtomicOrdering,
        single_thread: bool,
    ) -> Value {
        new_value!(LLVMBuildAtomicCmpXchg(
            self.as_llvm_ref(),
            pointer.as_llvm_ref(),
            cmp.as_llvm_ref(),
            new.as_llvm_ref(),
            success_ordering.as_llvm_ref(),
            failure_ordering.as_llvm_ref(),
            single_thread as i32
        ))
    }

    pub fn build_atomic_rmw(
        &self,
        operator: RMWBinaryOperator,
        pointer: Value,
        value: Value,
        ordering: AtomicOrdering,
        single_thread: bool,
    ) -> Value {
        new_value!(LLVMBuildAtomicRMW(
            self.as_llvm_ref(),
            operator.as_llvm_ref(),
            pointer.as_llvm_ref(),
            value.as_llvm_ref(),
            ordering.as_llvm_ref(),
            single_thread as i32
        ))
    }

    pub fn build_binary_operator(
        &self,
        operator: OperatorCode,
        lhs: Value,
        rhs: Value,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildBinOp(
            self.as_llvm_ref(),
            operator.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_bitcast(&self, value: Value, destine_type: Type, name: Option<&str>) -> Value {
        new_value!(LLVMBuildBitCast(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a branch
    pub fn build_br(&self, destine_block: BasicBlock) -> Value {
        new_value!(LLVMBuildBr(self.as_llvm_ref(), destine_block.as_llvm_ref()))
    }

    /// Builds a call instruction
    pub fn build_call(
        &self,
        function_type: Type,
        function: Value,
        arguments: &[Value],
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildCall2(
            self.as_llvm_ref(),
            function_type.as_llvm_ref(),
            function.as_llvm_ref(),
            slice_llvm_ref!(arguments),
            arguments.len() as u32,
            opt_c_str!(name)
        ))
    }

    pub fn build_cast(
        &self,
        operator: OperatorCode,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildCast(
            self.as_llvm_ref(),
            operator.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_catch_pad(
        &self,
        parent_pad: Value,
        arguments: &[Value],
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildCatchPad(
            self.as_llvm_ref(),
            parent_pad.as_llvm_ref(),
            slice_llvm_ref!(arguments),
            arguments.len() as u32,
            opt_c_str!(name)
        ))
    }

    pub fn build_catch_return(&self, catch_pad: Value, basic_block: BasicBlock) -> Value {
        new_value!(LLVMBuildCatchRet(
            self.as_llvm_ref(),
            catch_pad.as_llvm_ref(),
            basic_block.as_llvm_ref()
        ))
    }

    pub fn build_catch_switch(
        &self,
        parent_pad: Value,
        unwind_basic_block: BasicBlock,
        number_handler: u32,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildCatchSwitch(
            self.as_llvm_ref(),
            parent_pad.as_llvm_ref(),
            unwind_basic_block.as_llvm_ref(),
            number_handler,
            opt_c_str!(name)
        ))
    }

    pub fn build_cleanup_pad(
        &self,
        parent_pad: Value,
        arguments: &[Value],
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildCleanupPad(
            self.as_llvm_ref(),
            parent_pad.as_llvm_ref(),
            slice_llvm_ref!(arguments),
            arguments.len() as u32,
            opt_c_str!(name)
        ))
    }

    pub fn build_cleanup_return(&self, catch_pad: Value, basic_block: BasicBlock) -> Value {
        new_value!(LLVMBuildCleanupRet(
            self.as_llvm_ref(),
            catch_pad.as_llvm_ref(),
            basic_block.as_llvm_ref()
        ))
    }

    /// Builds a conditional branch
    pub fn build_conditional_br(
        &self,
        condition: Value,
        then_jump_to: BasicBlock,
        else_jump_to: BasicBlock,
    ) -> Value {
        new_value!(LLVMBuildCondBr(
            self.as_llvm_ref(),
            condition.as_llvm_ref(),
            then_jump_to.as_llvm_ref(),
            else_jump_to.as_llvm_ref()
        ))
    }

    pub fn build_exact_signed_div(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildExactSDiv(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_exact_unsigned_div(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildExactUDiv(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_extract_element(
        &self,
        vector_value: Value,
        index: Value,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildExtractElement(
            self.as_llvm_ref(),
            vector_value.as_llvm_ref(),
            index.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// This instruction extracts a struct member or array element value from an aggregate value.
    pub fn build_extract_value(&self, agg_val: Value, index: u32, name: Option<&str>) -> Value {
        new_value!(LLVMBuildExtractValue(
            self.as_llvm_ref(),
            agg_val.as_llvm_ref(),
            index,
            opt_c_str!(name)
        ))
    }

    /// Builts a floating point addition
    pub fn build_floating_point_add(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildFAdd(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a floating point `cmp` instruction
    pub fn build_floating_point_cmp(
        &self,
        operator: RealPredicate,
        lhs: Value,
        rhs: Value,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildFCmp(
            self.as_llvm_ref(),
            operator.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a floating point division
    pub fn build_floating_point_div(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildFDiv(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a floating point multiplication
    pub fn build_floating_point_mul(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildFMul(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a floating point negation
    pub fn build_floating_point_neg(&self, v: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildFNeg(
            self.as_llvm_ref(),
            v.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a floating point cast
    pub fn build_floating_point_cast(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildFPCast(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_floating_point_extend(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildFPExt(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a cast from floating point to signed integer
    pub fn build_floating_point_to_signed_integer(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildFPToSI(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a cast from floating point to unsigned integer
    pub fn build_floating_point_to_unsigned_integer(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildFPToUI(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_floating_point_truncate(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildFPTrunc(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a floating point remainder
    pub fn build_floating_point_rem(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildFRem(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a floating point subtraction
    pub fn build_floating_point_sub(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildFSub(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_fence(
        &self,
        ordering: AtomicOrdering,
        single_thread: bool,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildFence(
            self.as_llvm_ref(),
            ordering.as_llvm_ref(),
            single_thread as i32,
            opt_c_str!(name)
        ))
    }

    /// Builds a new call to libc's `free`
    pub fn build_free(&self, pointer: Value) -> Value {
        new_value!(LLVMBuildFree(self.as_llvm_ref(), pointer.as_llvm_ref()))
    }

    pub fn build_freeze(&self, value: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildFreeze(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a `getelementptr` instruction
    pub fn build_get_element_ptr(
        &self,
        target_type: Type,
        pointer: Value,
        indices: &[Value],
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildGEP2(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            pointer.as_llvm_ref(),
            slice_llvm_ref!(indices),
            indices.len() as u32,
            opt_c_str!(name)
        ))
    }

    /// Builds a new global string
    pub fn build_global_string(&self, string: &str, name: Option<&str>) -> Value {
        new_value!(LLVMBuildGlobalString(
            self.as_llvm_ref(),
            c_str!(string),
            opt_c_str!(name)
        ))
    }

    /// Builds a new global string pointer
    pub fn build_global_string_pointer(&self, string: &str, name: Option<&str>) -> Value {
        new_value!(LLVMBuildGlobalStringPtr(
            self.as_llvm_ref(),
            c_str!(string),
            opt_c_str!(name)
        ))
    }

    /// Builds an integer `cmp`instruction
    pub fn build_integer_cmp(
        &self,
        operator: IntPredicate,
        lhs: Value,
        rhs: Value,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildICmp(
            self.as_llvm_ref(),
            operator.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a `getelementptr inbounds` instruction
    pub fn build_get_element_ptr_in_bounds(
        &self,
        target_type: Type,
        pointer: Value,
        indices: &[Value],
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildInBoundsGEP2(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            pointer.as_llvm_ref(),
            slice_llvm_ref!(indices),
            indices.len() as u32,
            opt_c_str!(name)
        ))
    }

    pub fn build_indirect_br(&self, address: Value, number_of_destinations: u32) -> Value {
        new_value!(LLVMBuildIndirectBr(
            self.as_llvm_ref(),
            address.as_llvm_ref(),
            number_of_destinations
        ))
    }

    pub fn build_insert_element(
        &self,
        vector_value: Value,
        element: Value,
        index: Value,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildInsertElement(
            self.as_llvm_ref(),
            vector_value.as_llvm_ref(),
            element.as_llvm_ref(),
            index.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_insert_value(
        &self,
        vector_value: Value,
        element: Value,
        index: u32,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildInsertValue(
            self.as_llvm_ref(),
            vector_value.as_llvm_ref(),
            element.as_llvm_ref(),
            index,
            opt_c_str!(name)
        ))
    }

    pub fn build_integer_cast(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildIntCast(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_integer_cast2(
        &self,
        value: Value,
        destine_type: Type,
        is_signed: bool,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildIntCast2(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            is_signed as i32,
            opt_c_str!(name)
        ))
    }

    /// Builds a cast from integer to a pointer
    pub fn build_int_to_pointer(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildIntToPtr(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_invoke(
        &self,
        target_type: Type,
        function: Value,
        arguments: &[Value],
        then: BasicBlock,
        catch: BasicBlock,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildInvoke2(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            function.as_llvm_ref(),
            slice_llvm_ref!(arguments),
            arguments.len() as u32,
            then.as_llvm_ref(),
            catch.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_is_not_null(&self, value: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildIsNotNull(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_is_null(&self, value: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildIsNull(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_lshr(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildLShr(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_landing_pad(
        &self,
        target_type: Type,
        pers_fn: Value,
        number_of_clauses: u32,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildLandingPad(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            pers_fn.as_llvm_ref(),
            number_of_clauses,
            opt_c_str!(name)
        ))
    }

    /// Builds a `load` instruction
    pub fn build_load(&self, target_type: Type, pointer: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildLoad2(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            pointer.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a call to the `malloc` function
    pub fn build_malloc(&self, target_type: Type, name: Option<&str>) -> Value {
        new_value!(LLVMBuildMalloc(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a call to the `memcpy` function
    pub fn build_memcpy(
        &self,
        destine: Value,
        destine_align: u32,
        source: Value,
        source_align: u32,
        size: Value,
    ) -> Value {
        new_value!(LLVMBuildMemCpy(
            self.as_llvm_ref(),
            destine.as_llvm_ref(),
            destine_align,
            source.as_llvm_ref(),
            source_align,
            size.as_llvm_ref()
        ))
    }

    /// Builds a call to the `memmove` function
    pub fn build_memmove(
        &self,
        destine: Value,
        destine_align: u32,
        source: Value,
        source_align: u32,
        size: Value,
    ) -> Value {
        new_value!(LLVMBuildMemMove(
            self.as_llvm_ref(),
            destine.as_llvm_ref(),
            destine_align,
            source.as_llvm_ref(),
            source_align,
            size.as_llvm_ref()
        ))
    }

    /// Builds a call to the `memset` function
    pub fn build_memset(&self, pointer: Value, value: Value, length: Value, align: u32) -> Value {
        new_value!(LLVMBuildMemSet(
            self.as_llvm_ref(),
            pointer.as_llvm_ref(),
            value.as_llvm_ref(),
            length.as_llvm_ref(),
            align
        ))
    }

    /// Builds a `mul` instruction
    pub fn build_mul(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildMul(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_nsw_add(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNSWAdd(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_nsw_mul(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNSWMul(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_nsw_neg(&self, value: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNSWNeg(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_nsw_sub(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNSWSub(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_nuw_add(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNUWAdd(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_nuw_mul(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNUWMul(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_nuw_neg(&self, value: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNUWNeg(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_nuw_sub(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNUWSub(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_neg(&self, value: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNeg(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_not(&self, value: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildNot(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    /// Builds a bitwise or instruction
    pub fn build_or(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildOr(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_phi(&self, target_type: Type, name: Option<&str>) -> Value {
        new_value!(LLVMBuildPhi(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_pointer_cast(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildPointerCast(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_pointer_difference(
        &self,
        element_type: Type,
        lhs: Value,
        rhs: Value,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildPtrDiff2(
            self.as_llvm_ref(),
            element_type.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_pointer_to_integer(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildPtrToInt(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_resume(&self, exn: Value) -> Value {
        new_value!(LLVMBuildResume(self.as_llvm_ref(), exn.as_llvm_ref()))
    }

    pub fn build_return(&self, value: Value) -> Value {
        new_value!(LLVMBuildRet(self.as_llvm_ref(), value.as_llvm_ref()))
    }

    pub fn build_return_void(&self) -> Value {
        new_value!(LLVMBuildRetVoid(self.as_llvm_ref()))
    }

    pub fn build_signed_div(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildSDiv(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_signed_extend(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildSExt(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_signed_extend_or_bitcast(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildSExtOrBitCast(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_signed_integer_to_floating_point(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildSIToFP(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_signed_rem(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildSRem(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_select(
        &self,
        r#if: Value,
        then: Value,
        r#else: Value,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildSelect(
            self.as_llvm_ref(),
            r#if.as_llvm_ref(),
            then.as_llvm_ref(),
            r#else.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_shl(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildShl(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_shuffle_vector(
        &self,
        value_1: Value,
        value_2: Value,
        mask: Value,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildShuffleVector(
            self.as_llvm_ref(),
            value_1.as_llvm_ref(),
            value_2.as_llvm_ref(),
            mask.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_store(&self, value: Value, pointer: Value) -> Value {
        new_value!(LLVMBuildStore(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            pointer.as_llvm_ref()
        ))
    }

    pub fn build_struct_get_element_ptr(
        &self,
        target_type: Type,
        pointer: Value,
        index: u32,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildStructGEP2(
            self.as_llvm_ref(),
            target_type.as_llvm_ref(),
            pointer.as_llvm_ref(),
            index,
            opt_c_str!(name)
        ))
    }

    pub fn build_sub(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildSub(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_switch(&self, value: Value, default: BasicBlock, number_of_cases: u32) -> Value {
        new_value!(LLVMBuildSwitch(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            r#default.as_llvm_ref(),
            number_of_cases
        ))
    }

    pub fn build_truncate(&self, value: Value, destine_type: Type, name: Option<&str>) -> Value {
        new_value!(LLVMBuildTrunc(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_trunc_or_bitcast(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildTruncOrBitCast(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_unsigned_div(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildUDiv(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_unsigned_integer_to_floating_point(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildUIToFP(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_unsigned_rem(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildURem(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_unreachable(&self) -> Value {
        new_value!(LLVMBuildUnreachable(self.as_llvm_ref()))
    }

    pub fn build_variant_args(&self, list: Value, target_type: Type, name: Option<&str>) -> Value {
        new_value!(LLVMBuildVAArg(
            self.as_llvm_ref(),
            list.as_llvm_ref(),
            target_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_xor(&self, lhs: Value, rhs: Value, name: Option<&str>) -> Value {
        new_value!(LLVMBuildXor(
            self.as_llvm_ref(),
            lhs.as_llvm_ref(),
            rhs.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_zext(&self, value: Value, destine_type: Type, name: Option<&str>) -> Value {
        new_value!(LLVMBuildZExt(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn build_zext_or_bitcast(
        &self,
        value: Value,
        destine_type: Type,
        name: Option<&str>,
    ) -> Value {
        new_value!(LLVMBuildZExtOrBitCast(
            self.as_llvm_ref(),
            value.as_llvm_ref(),
            destine_type.as_llvm_ref(),
            opt_c_str!(name)
        ))
    }

    pub fn get_default_floating_point_math_tag(&self) -> Metadata {
        unsafe { Metadata::from_llvm_ref(LLVMBuilderGetDefaultFPMathTag(self.as_llvm_ref())) }
    }

    pub fn set_default_flaoting_point_math_tag(&self, floating_point_math_tag: Metadata) {
        unsafe {
            LLVMBuilderSetDefaultFPMathTag(
                self.as_llvm_ref(),
                floating_point_math_tag.as_llvm_ref(),
            )
        }
    }

    pub fn clear_insertion_position(&self) {
        unsafe { LLVMClearInsertionPosition(self.as_llvm_ref()) }
    }
}

impl<'ctx> FromLLVMRef for Builder<'ctx> {
    type From = LLVMBuilderRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating Builder from null");
        Self(llvm_ref, PhantomData)
    }
}

impl<'ctx> AsLLVMRef for Builder<'ctx> {
    type Target = LLVMBuilderRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

impl<'ctx> FromLLVMRef for Module<'ctx> {
    type From = LLVMModuleRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating Module from null");
        Self(llvm_ref, PhantomData)
    }
}

impl<'ctx> AsLLVMRef for Module<'ctx> {
    type Target = LLVMModuleRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

#[derive(Clone, Copy)]
pub struct Type(LLVMTypeRef);

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord, Hash)]
pub enum TypeKind {
    Void,
    Half,
    Float,
    Double,
    X86Amx,
    FP128,
    PpcFp128,
    Label,
    Integer,
    Function,
    Struct,
    Array,
    Pointer,
    Vector,
    Metadata,
    X86Mmx,
    Token,
    ScalableVector,
    BFloat,
    X86AMx,
    TargetExt,
}

impl Type {
    pub fn size_of_in_module(&self, module: &Module) -> u64 {
        unsafe { LLVMStoreSizeOfType(module.data_layout(), self.as_llvm_ref()) }
    }

    pub fn null() -> Self {
        Self(null_mut())
    }

    pub fn kind(&self) -> TypeKind {
        unsafe { std::mem::transmute(LLVMGetTypeKind(self.0) as c_char) }
    }

    pub fn align_of(&self) -> Value {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "using align_of in null Type");
        unsafe { Value::from_llvm_ref(LLVMAlignOf(self.as_llvm_ref())) }
    }

    pub fn count_parameter_types(&self) -> u32 {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "using count_parameter_types in null Type");
        unsafe { LLVMCountParamTypes(self.as_llvm_ref()) }
    }

    pub fn count_struct_element_types(&self) -> u32 {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "using count_struct_element_types in null Type");
        unsafe { LLVMCountStructElementTypes(self.as_llvm_ref()) }
    }

    pub fn dump(&self) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "using dump in null Type");
        unsafe { LLVMDumpType(self.as_llvm_ref()) }
    }

    pub fn get_array_length(&self) -> u64 {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "using get_array_length in null Type");
        unsafe { LLVMGetArrayLength2(self.as_llvm_ref()) }
    }
}

impl FromLLVMRef for Type {
    type From = LLVMTypeRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating Type from null");
        Self(llvm_ref)
    }
}

impl AsLLVMRef for Type {
    type Target = LLVMTypeRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

#[derive(Clone, Copy)]
pub struct Value(LLVMValueRef);

impl Value {
    pub fn get_parameters(&self) -> Vec<Value> {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use get_parameters in null Value");
        let num_args = unsafe { LLVMCountParams(self.as_llvm_ref()) };
        let mut params = Vec::with_capacity(num_args as usize);
        unsafe {
            LLVMGetParams(self.as_llvm_ref(), params.as_mut_ptr());
            params.set_len(num_args as usize);
        }
        params
            .into_iter()
            .map(|i| unsafe { Value::from_llvm_ref(i) })
            .collect()
    }

    pub fn null() -> Self {
        Self(null_mut())
    }

    pub fn type_of(&self) -> Type {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use type_of in null Value");
        new_type!(LLVMTypeOf(self.as_llvm_ref()))
    }

    pub fn get_as_string(&self, mut length: usize) -> String {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use get_as_string in null Value");
        unsafe {
            CString::from_raw(
                LLVMGetAsString(self.as_llvm_ref(), &mut length as *mut usize) as *mut i8,
            )
        }
        .to_string_lossy()
        .to_string()
    }

    pub fn aggregate_element(&self, index: u32) -> Value {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use aggregate_element in null Value");
        new_value!(LLVMGetAggregateElement(self.as_llvm_ref(), index))
    }

    /// Get the number of funcletpad arguments.
    pub fn get_argument_operand(&self, idx: u32) -> Value {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use get_argument_operand in null Value");
        new_value!(LLVMGetArgOperand(self.as_llvm_ref(), idx))
    }

    pub fn alignment(&self) -> u32 {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use alignment in null Value");
        unsafe { LLVMGetAlignment(self.as_llvm_ref()) }
    }

    pub fn allocated_type(&self) -> Type {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use allocated_type in null Value");
        new_type!(LLVMGetAllocatedType(self.as_llvm_ref()))
    }

    /// Add an attribute to a function.
    pub fn add_attribute_at_index(&self, idx: u32, attribute: Attribute) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use add_attribute_at_index in null Value");
        unsafe { LLVMAddAttributeAtIndex(self.as_llvm_ref(), idx, attribute.as_llvm_ref()) }
    }

    pub fn add_call_site_attribute(&self, idx: u32, attribute: Attribute) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use add_call_site_attribute in null Value");
        unsafe { LLVMAddCallSiteAttribute(self.as_llvm_ref(), idx, attribute.as_llvm_ref()) }
    }

    /// Add a case to a `switch` instruction
    pub fn add_case(&self, on_value: Value, destine: BasicBlock) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use add_case in null Value");
        unsafe {
            LLVMAddCase(
                self.as_llvm_ref(),
                on_value.as_llvm_ref(),
                destine.as_llvm_ref(),
            )
        }
    }

    /// Add a catch or filter to a `landingpad` instruction
    pub fn add_clause(&self, clause_value: Value) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use add_clause in null Value");
        unsafe { LLVMAddClause(self.as_llvm_ref(), clause_value.as_llvm_ref()) }
    }

    /// Add a destination to an `indirectbr` instruction
    pub fn add_destination(&self, destination: BasicBlock) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use add_destination in null Value");
        unsafe { LLVMAddDestination(self.as_llvm_ref(), destination.as_llvm_ref()) }
    }

    /// Add a destination to the catchswitch instruction
    pub fn add_handler(&self, destination: BasicBlock) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use add_handler in null Value");
        unsafe { LLVMAddHandler(self.as_llvm_ref(), destination.as_llvm_ref()) }
    }

    /// Add an incoming value to the end of the PHI list.
    pub fn add_incoming(
        &self,
        incoming_values: &[Value],
        incoming_blocks: &[BasicBlock],
        count: u32,
    ) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use add_incoming in null Value");
        unsafe {
            LLVMAddIncoming(
                self.as_llvm_ref(),
                slice_llvm_ref!(incoming_values),
                slice_llvm_ref!(incoming_blocks),
                count,
            )
        }
    }

    pub fn add_target_dependent_function_attribute(&self, a: &str, v: &str) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use add_target_dependent_function_attribute in null Value");
        unsafe { LLVMAddTargetDependentFunctionAttr(self.as_llvm_ref(), c_str!(a), c_str!(v)) }
    }

    /// Retrieve the target value of an alias.
    pub fn get_aliasee(&self) -> Value {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use get_aliasee in null Value");
        unsafe { Value::from_llvm_ref(LLVMAliasGetAliasee(self.as_llvm_ref())) }
    }

    /// Set the target value of an alias.
    pub fn set_aliasee(&self, aliasee: Value) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use set_aliasee in null Value");
        unsafe { LLVMAliasSetAliasee(self.as_llvm_ref(), aliasee.as_llvm_ref()) }
    }

    pub fn append_basic_block(&self, name: &str) -> BasicBlock {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use append_basic_block in null Value");
        unsafe { BasicBlock::from_llvm_ref(LLVMAppendBasicBlock(self.as_llvm_ref(), c_str!(name))) }
    }

    pub fn append_basic_block_in_context(&self, name: &str, context: &Context) -> BasicBlock {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use append_basic_block_in_context in null Value");
        unsafe {
            BasicBlock::from_llvm_ref(LLVMAppendBasicBlockInContext(
                context.as_llvm_ref(),
                self.as_llvm_ref(),
                c_str!(name),
            ))
        }
    }

    /// Append the given basic block to the basic block list of the given function.
    pub fn append_existing_basic_block(&self, basic_block: BasicBlock) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use append_existing_basic_block in null Value");
        unsafe { LLVMAppendExistingBasicBlock(self.as_llvm_ref(), basic_block.as_llvm_ref()) }
    }

    pub fn block_address(&self, block: &BasicBlock) -> Value {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use block_address in null Value");
        new_value!(LLVMBlockAddress(self.as_llvm_ref(), block.as_llvm_ref()))
    }

    pub fn count_basic_blocks(&self) -> u32 {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use count_basic_blocks in null Value");
        unsafe { LLVMCountBasicBlocks(self.as_llvm_ref()) }
    }

    pub fn count_incoming(&self) -> u32 {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use count_incoming in null Value");
        unsafe { LLVMCountIncoming(self.as_llvm_ref()) }
    }

    pub fn count_parameters(&self) -> u32 {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use count_parameters in null Value");
        unsafe { LLVMCountParams(self.as_llvm_ref()) }
    }

    pub fn delete_function(self) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use delete_function in null Value");
        unsafe { LLVMDeleteFunction(self.as_llvm_ref()) }
    }

    pub fn delete_global(self) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use delete_global in null Value");
        unsafe { LLVMDeleteGlobal(self.as_llvm_ref()) }
    }

    pub fn delete_instruction(self) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use delete_instruction in null Value");
        unsafe { LLVMDeleteInstruction(self.as_llvm_ref()) }
    }

    pub fn dump(&self) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use dump in null Value");
        unsafe { LLVMDumpValue(self.as_llvm_ref()) }
    }

    pub fn erase_global_ifunc(self) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use erase_global_ifunc in null Value");
        unsafe { LLVMEraseGlobalIFunc(self.as_llvm_ref()) }
    }

    /// Sets the cleanup property of a landingpad
    pub fn set_cleanup(&self, cleanup: bool) {
        assert!(!unsafe { self.as_llvm_ref().is_null() }, "Cannot use set_cleanup in null Value");
        unsafe { LLVMSetCleanup(self.as_llvm_ref(), cleanup as i32) }
    }
}

impl FromLLVMRef for Value {
    type From = LLVMValueRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating Value from null");
        Self(llvm_ref)
    }
}

impl AsLLVMRef for Value {
    type Target = LLVMValueRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

#[derive(Clone, Copy)]
pub struct Attribute(LLVMAttributeRef);

impl FromLLVMRef for Attribute {
    type From = LLVMAttributeRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating Attribute from null");
        Self(llvm_ref)
    }
}

impl AsLLVMRef for Attribute {
    type Target = LLVMAttributeRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

#[derive(Clone, Copy)]
pub struct BasicBlock(LLVMBasicBlockRef);

impl BasicBlock {
    pub fn get_name(&self) -> String {
        unsafe { CString::from_raw(LLVMGetBasicBlockName(self.as_llvm_ref()) as *mut i8) }
            .to_string_lossy()
            .to_string()
    }

    pub fn get_parent(&self) -> Option<Value> {
        let parent = unsafe { LLVMGetBasicBlockParent(self.as_llvm_ref()) };
        if parent.is_null() {
            None
        } else {
            Some(new_value!(parent))
        }
    }

    pub fn get_terminator(&self) -> Option<Value> {
        let parent = unsafe { LLVMGetBasicBlockTerminator(self.as_llvm_ref()) };
        if parent.is_null() {
            None
        } else {
            Some(new_value!(parent))
        }
    }

    pub fn as_value(&self) -> Value {
        new_value!(LLVMBasicBlockAsValue(self.as_llvm_ref()))
    }

    pub fn create_in_context(context: &Context, name: &str) -> BasicBlock {
        Self(unsafe { LLVMCreateBasicBlockInContext(context.as_llvm_ref(), c_str!(name)) })
    }

    pub fn new_in_context(context: &Context, name: &str) -> BasicBlock {
        Self::create_in_context(context, name)
    }

    pub fn delete(self) {
        unsafe { LLVMDeleteBasicBlock(self.as_llvm_ref()) }
    }
}

impl FromLLVMRef for BasicBlock {
    type From = LLVMBasicBlockRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating BasicBlock from null");
        Self(llvm_ref)
    }
}

impl AsLLVMRef for BasicBlock {
    type Target = LLVMBasicBlockRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Function(pub Value);

impl Function {
    pub fn verify(&self) -> Result<(), VerifyError> {
        if unsafe {
            LLVMVerifyFunction(
                self.as_llvm_ref(),
                LLVMVerifierFailureAction::LLVMPrintMessageAction,
            )
        } != 0
        {
            return Err(VerifyError::new("Function verification failed".to_string()));
        }
        Ok(())
    }

    pub fn set_calling_convention(&self, convention: CallingConvention) {
        unsafe { LLVMSetFunctionCallConv(self.as_llvm_ref(), mem::transmute(convention)) }
    }
}

impl std::ops::Deref for Function {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Function {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Create a fixed size array type that refers to a specific type.
///
/// The created type will exist in the context that its element type exists in.
pub fn array_type(element_type: Type, element_count: usize) -> Type {
    unsafe {
        Type::from_llvm_ref(LLVMArrayType2(
            element_type.as_llvm_ref(),
            element_count as u64,
        ))
    }
}

pub fn bfloating_point_type() -> Type {
    new_type!(LLVMBFloatType())
}

pub fn bfloating_point_type_in_context(context: &Context) -> Type {
    new_type!(LLVMBFloatTypeInContext(context.as_llvm_ref()))
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AtomicOrdering {
    NotAtomic,
    Unordered,
    Monotonic,
    Acquire,
    Release,
    AcquireRelease,
    SequentiallyConsistent,
}

impl AsLLVMRef for AtomicOrdering {
    type Target = LLVMAtomicOrdering;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        mem::transmute(*self as u8 as u32)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RMWBinaryOperator {
    Xchg,
    Add,
    Sub,
    And,
    Nand,
    Or,
    Xor,
    Max,
    Min,
    UMax,
    FAdd,
    FSub,
    FMax,
    FMin,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum OperatorCode {
    Ret,
    Br,
    Switch,
    IndirectBr,
    Invoke,
    Unreachable,
    CallBr,
    FNeg,
    Add,
    FAdd,
    Sub,
    FSub,
    Mul,
    FMul,
    UDiv,
    SDiv,
    FDiv,
    URem,
    SRem,
    FRem,
    Shl,
    LShr,
    AShr,
    And,
    Or,
    Xor,
    Alloca,
    Load,
    Store,
    GetElementPtr,
    Trunc,
    ZExt,
    SExt,
    FPToUI,
    FPToSI,
    UIToFP,
    SIToFP,
    FPTrunc,
    FPExt,
    PtrToInt,
    IntToPtr,
    BitCast,
    AddrSpaceCast,
    ICmp,
    FCmp,
    Phi,
    Call,
    Select,
    UserOp1,
    UserOp2,
    VAArg,
    ExtractElement,
    InsertElement,
    ShuffleVector,
    ExtractValue,
    InsertValue,
    Freeze,
    Fence,
    AtomicCmpXchg,
    AtomicRMW,
    Resume,
    LandingPad,
    CleanupRet,
    CatchRet,
    CatchPad,
    CleanupPad,
    CatchSwitch,
}

impl AsLLVMRef for OperatorCode {
    type Target = LLVMOpcode;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        mem::transmute(*self as u8 as u32)
    }
}

impl AsLLVMRef for RMWBinaryOperator {
    type Target = LLVMAtomicRMWBinOp;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        mem::transmute(*self as u8 as u32)
    }
}

impl FromLLVMRef for RMWBinaryOperator {
    type From = LLVMAtomicRMWBinOp;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        mem::transmute(llvm_ref as u8)
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { LLVMContextDispose(self.as_llvm_ref()) }
    }
}

impl<'ctx> Drop for Module<'ctx> {
    fn drop(&mut self) {
        unsafe { LLVMDisposeModule(self.as_llvm_ref()) }
    }
}

impl<'ctx> Drop for Builder<'ctx> {
    fn drop(&mut self) {
        unsafe { LLVMDisposeBuilder(self.as_llvm_ref()) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RealPredicate {
    False,
    Oeq,
    Ogt,
    Oge,
    Olt,
    Ole,
    One,
    Ord,
    Uno,
    Ueq,
    Ugt,
    Uge,
    Ult,
    Ule,
    Une,
    True,
}

impl AsLLVMRef for RealPredicate {
    type Target = LLVMRealPredicate;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        mem::transmute(*self as u8 as u32)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntPredicate {
    Eq = 32,
    Ne = 33,
    Ugt = 34,
    Uge = 35,
    Ult = 36,
    Ule = 37,
    Sgt = 38,
    Sge = 39,
    Slt = 40,
    Sle = 41,
}

impl AsLLVMRef for IntPredicate {
    type Target = LLVMIntPredicate;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        mem::transmute(*self as u8 as u32)
    }
}

pub fn const_ashr(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstAShr(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_add(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstAdd(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_address_space_cast(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstAddrSpaceCast(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_all_ones(target_type: Type) -> Value {
    new_value!(LLVMConstAllOnes(target_type.as_llvm_ref()))
}

pub fn const_and(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstAnd(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_array(element_type: Type, constant_values: &[Value], length: u64) -> Value {
    new_value!(LLVMConstArray2(
        element_type.as_llvm_ref(),
        slice_llvm_ref!(constant_values),
        length
    ))
}

pub fn const_bitcast(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstBitCast(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_extract_element(vector_constant: Value, index_constant: Value) -> Value {
    new_value!(LLVMConstExtractElement(
        vector_constant.as_llvm_ref(),
        index_constant.as_llvm_ref()
    ))
}

pub fn const_floating_point_cmp(predicate: RealPredicate, lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstFCmp(
        predicate.as_llvm_ref(),
        lhs.as_llvm_ref(),
        rhs.as_llvm_ref()
    ))
}

pub fn const_floating_point_cast(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstFPCast(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_floating_point_extend(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstFPExt(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_floating_point_to_signed_integer(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstFPToSI(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_floating_point_to_unsigned_integer(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstFPToUI(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_floating_point_truncate(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstFPTrunc(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_get_element_ptr(
    target_type: Type,
    constant_value: Value,
    constant_indices: &[Value],
) -> Value {
    new_value!(LLVMConstGEP2(
        target_type.as_llvm_ref(),
        constant_value.as_llvm_ref(),
        slice_llvm_ref!(constant_indices),
        constant_indices.len() as u32
    ))
}

pub fn const_integer_cmp(predicate: IntPredicate, lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstICmp(
        predicate.as_llvm_ref(),
        lhs.as_llvm_ref(),
        rhs.as_llvm_ref()
    ))
}

pub fn const_get_element_ptr_in_bounds(
    target_type: Type,
    constant_value: Value,
    constant_indices: &[Value],
) -> Value {
    new_value!(LLVMConstInBoundsGEP2(
        target_type.as_llvm_ref(),
        constant_value.as_llvm_ref(),
        slice_llvm_ref!(constant_indices),
        constant_indices.len() as u32
    ))
}

pub fn const_insert_element(
    vector_constant: Value,
    element_constant: Value,
    index_constant: Value,
) -> Value {
    new_value!(LLVMConstInsertElement(
        vector_constant.as_llvm_ref(),
        element_constant.as_llvm_ref(),
        index_constant.as_llvm_ref()
    ))
}

pub fn const_integer(int_ty: Type, bits: u64, sign_extend: bool) -> Value {
    Value(unsafe { LLVMConstInt(int_ty.as_llvm_ref(), bits, sign_extend as i32) })
}

pub fn const_integer_cast(constant_value: Value, target_type: Type, is_signed: bool) -> Value {
    new_value!(LLVMConstIntCast(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref(),
        is_signed as i32
    ))
}

pub fn const_integer_get_signed_extended_value(constant_value: Value) -> i64 {
    unsafe { LLVMConstIntGetSExtValue(constant_value.as_llvm_ref()) }
}

pub fn const_integer_get_zero_extended_value(constant_value: Value) -> u64 {
    unsafe { LLVMConstIntGetZExtValue(constant_value.as_llvm_ref()) }
}

pub fn const_integer_of_arbitrary_precision(target_type: Type, words: &[u64]) -> Value {
    new_value!(LLVMConstIntOfArbitraryPrecision(
        target_type.as_llvm_ref(),
        words.len() as u32,
        words.as_ptr()
    ))
}

pub fn const_integer_of_string(target_type: Type, text: &str, radix: u8) -> Value {
    new_value!(LLVMConstIntOfString(
        target_type.as_llvm_ref(),
        c_str!(text),
        radix
    ))
}

pub fn const_integer_to_pointer(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstIntToPtr(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_lshr(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstLShr(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_mul(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstMul(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_nsw_add(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstNSWAdd(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_nsw_mul(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstNSWMul(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_nsw_neg(value: Value) -> Value {
    new_value!(LLVMConstNSWNeg(value.as_llvm_ref()))
}

pub fn const_nsw_sub(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstNUWSub(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_nuw_add(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstNUWAdd(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_nuw_mul(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstNUWMul(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_nuw_neg(value: Value) -> Value {
    new_value!(LLVMConstNUWNeg(value.as_llvm_ref()))
}

pub fn const_nuw_sub(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstNUWSub(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_named_struct(struct_type: Type, constant_values: &[Value]) -> Value {
    new_value!(LLVMConstNamedStruct(
        struct_type.as_llvm_ref(),
        slice_llvm_ref!(constant_values),
        constant_values.len() as u32
    ))
}

pub fn const_neg(value: Value) -> Value {
    new_value!(LLVMConstNeg(value.as_llvm_ref()))
}

pub fn const_not(value: Value) -> Value {
    new_value!(LLVMConstNot(value.as_llvm_ref()))
}

pub fn const_null(target_type: Type) -> Value {
    new_value!(LLVMConstNull(target_type.as_llvm_ref()))
}

pub fn const_or(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstOr(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_pointer_cast(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstPointerCast(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_pointer_null(target_type: Type) -> Value {
    new_value!(LLVMConstPointerNull(target_type.as_llvm_ref()))
}

pub fn const_pointer_to_integer(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstPtrToInt(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_real(real_type: Type, number: f64) -> Value {
    new_value!(LLVMConstReal(real_type.as_llvm_ref(), number))
}

pub fn const_real_get_double(constant_value: Value, loses_info: bool) -> f64 {
    unsafe {
        LLVMConstRealGetDouble(
            constant_value.as_llvm_ref(),
            &(loses_info as i32) as *const i32 as *mut i32,
        )
    }
}

pub fn const_real_of_string(real_type: Type, text: &str) -> Value {
    new_value!(LLVMConstRealOfString(real_type.as_llvm_ref(), c_str!(text)))
}

pub fn const_signed_extend(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstSExt(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_signed_extend_or_bitcast(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstSExtOrBitCast(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_signed_integer_to_floating_point(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstSIToFP(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_shl(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstShl(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_shuffle_vector(value_1: Value, value_2: Value, mask: Value) -> Value {
    new_value!(LLVMConstShuffleVector(
        value_1.as_llvm_ref(),
        value_2.as_llvm_ref(),
        mask.as_llvm_ref()
    ))
}

pub fn const_string(string: &str, null_terminated: bool) -> Value {
    new_value!(LLVMConstString(
        string.as_ptr().cast(),
        string.len() as u32,
        !null_terminated as i32
    ))
}

pub fn const_string_in_context(context: &Context, string: &str, null_terminated: bool) -> Value {
    new_value!(LLVMConstStringInContext(
        context.as_llvm_ref(),
        string.as_ptr().cast(),
        string.len() as u32,
        !null_terminated as i32
    ))
}

pub fn const_sub(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstSub(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_truncate(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstTrunc(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_truncate_or_bitcase(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstTruncOrBitCast(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_unsigned_integer_to_floating_point(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstUIToFP(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_vector(scalar_constants: &[Value]) -> Value {
    new_value!(LLVMConstVector(
        slice_llvm_ref!(scalar_constants),
        scalar_constants.len() as u32
    ))
}

pub fn const_xor(lhs: Value, rhs: Value) -> Value {
    new_value!(LLVMConstXor(lhs.as_llvm_ref(), rhs.as_llvm_ref()))
}

pub fn const_zero_extend(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstZExt(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub fn const_zero_extend_or_bitcast(constant_value: Value, target_type: Type) -> Value {
    new_value!(LLVMConstZExtOrBitCast(
        constant_value.as_llvm_ref(),
        target_type.as_llvm_ref()
    ))
}

pub struct ModuleProvider<'module>(LLVMModuleProviderRef, PhantomData<&'module ()>);

impl<'module> ModuleProvider<'module> {
    pub fn create_function_pass_manager(&self) -> PassManager {
        unsafe { PassManager::from_llvm_ref(LLVMCreateFunctionPassManager(self.as_llvm_ref())) }
    }

    pub fn create_function_pass_manager_for_module<'m>(
        &self,
        module: &'m Module,
    ) -> PassManager<'m> {
        unsafe {
            PassManager::from_llvm_ref(LLVMCreateFunctionPassManagerForModule(module.as_llvm_ref()))
        }
    }

    pub fn create_for_existing_module(module: &'module Module) -> Self {
        unsafe {
            Self::from_llvm_ref(LLVMCreateModuleProviderForExistingModule(
                module.as_llvm_ref(),
            ))
        }
    }
}

impl<'module> AsLLVMRef for ModuleProvider<'module> {
    type Target = LLVMModuleProviderRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

impl<'module> FromLLVMRef for ModuleProvider<'module> {
    type From = LLVMModuleProviderRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating ModuleProvider from null");
        Self(llvm_ref, PhantomData)
    }
}

pub struct PassManager<'m>(LLVMPassManagerRef, PhantomData<&'m ()>);

impl<'m> PassManager<'m> {
    pub fn create() -> PassManager<'m> {
        Self(unsafe { LLVMCreatePassManager() }, PhantomData)
    }

    pub fn finalize_function_pass_manager(&self) -> bool {
        (unsafe { LLVMFinalizeFunctionPassManager(self.as_llvm_ref()) } != 0)
    }

    pub fn run(&self, module: &'m Module<'m>) {
        unsafe {
            LLVMRunPassManager(self.0, module.as_llvm_ref());
        }
    }
}

impl<'m> AsLLVMRef for PassManager<'m> {
    type Target = LLVMPassManagerRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

impl<'m> FromLLVMRef for PassManager<'m> {
    type From = LLVMPassManagerRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating PassManager from null");
        Self(llvm_ref, PhantomData)
    }
}

impl<'m> Drop for PassManager<'m> {
    fn drop(&mut self) {
        unsafe { LLVMDisposePassManager(self.as_llvm_ref()) }
    }
}

pub struct MemoryBuffer(LLVMMemoryBufferRef);

impl AsLLVMRef for MemoryBuffer {
    type Target = LLVMMemoryBufferRef;

    unsafe fn as_llvm_ref(&self) -> Self::Target {
        self.0
    }
}

impl FromLLVMRef for MemoryBuffer {
    type From = LLVMMemoryBufferRef;

    unsafe fn from_llvm_ref(llvm_ref: Self::From) -> Self {
        assert!(!llvm_ref.is_null(), "Creating MemoryBuffer from null");
        Self(llvm_ref)
    }
}

impl Drop for MemoryBuffer {
    fn drop(&mut self) {
        unsafe { LLVMDisposeMemoryBuffer(self.as_llvm_ref()) }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemoryBufferCreationError(String);

impl MemoryBufferCreationError {
    pub fn new(message: String) -> Self {
        Self(message)
    }

    pub fn new_from_raw(ptr: *mut i8) -> Self {
        Self(
            unsafe { CString::from_raw(ptr) }
                .to_string_lossy()
                .to_string(),
        )
    }
}

impl fmt::Display for MemoryBufferCreationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for MemoryBufferCreationError {}

pub fn create_memory_buffer_with_contents_of_file(
    file_path: &str,
) -> Result<MemoryBuffer, MemoryBufferCreationError> {
    let message = null_mut();
    let mem_buf = null_mut();

    if unsafe { LLVMCreateMemoryBufferWithContentsOfFile(c_str!(file_path), mem_buf, message) } != 0
    {
        if message.is_null() {
            Err(MemoryBufferCreationError::new(
                "No message was returned".to_string(),
            ))
        } else {
            Err(MemoryBufferCreationError::new_from_raw(unsafe { *message }))
        }
    } else {
        Ok(unsafe { MemoryBuffer::from_llvm_ref(*mem_buf) })
    }
}

pub fn double_type() -> Type {
    new_type!(LLVMDoubleType())
}

pub fn float64_type_in_context(context: &Context) -> Type {
    new_type!(LLVMDoubleTypeInContext(context.as_llvm_ref()))
}

pub fn f128_type() -> Type {
    new_type!(LLVMFP128Type())
}

pub fn f128_type_in_context(context: &Context) -> Type {
    new_type!(LLVMFP128TypeInContext(context.as_llvm_ref()))
}

pub fn floating_point_type() -> Type {
    new_type!(LLVMFloatType())
}

pub fn float32_type_in_context(context: &Context) -> Type {
    new_type!(LLVMFloatTypeInContext(context.as_llvm_ref()))
}

pub fn function_type(return_type: Type, parameter_types: &[Type], is_var_args: bool) -> Type {
    unsafe {
        Type::from_llvm_ref(LLVMFunctionType(
            return_type.as_llvm_ref(),
            slice_llvm_ref!(parameter_types),
            parameter_types.len() as u32,
            is_var_args as i32,
        ))
    }
}

pub fn int1_type() -> Type {
    new_type!(LLVMInt1Type())
}

pub fn int1_type_in_context(context: &Context) -> Type {
    new_type!(LLVMInt1TypeInContext(context.as_llvm_ref()))
}

pub fn int8_type() -> Type {
    new_type!(LLVMInt8Type())
}

pub fn int8_type_in_context(context: &Context) -> Type {
    new_type!(LLVMInt8TypeInContext(context.as_llvm_ref()))
}

pub fn int16_type() -> Type {
    new_type!(LLVMInt16Type())
}

pub fn int16_type_in_context(context: &Context) -> Type {
    new_type!(LLVMInt16TypeInContext(context.as_llvm_ref()))
}

pub fn int32_type() -> Type {
    new_type!(LLVMInt32Type())
}

pub fn int32_type_in_context(context: &Context) -> Type {
    new_type!(LLVMInt32TypeInContext(context.as_llvm_ref()))
}

pub fn int64_type() -> Type {
    new_type!(LLVMInt64Type())
}

pub fn int64_type_in_context(context: &Context) -> Type {
    new_type!(LLVMInt64TypeInContext(context.as_llvm_ref()))
}

pub fn int128_type() -> Type {
    new_type!(LLVMInt128Type())
}

pub fn int128_type_in_context(context: &Context) -> Type {
    new_type!(LLVMInt128TypeInContext(context.as_llvm_ref()))
}

pub fn int_type(number_of_bits: u32) -> Type {
    new_type!(LLVMIntType(number_of_bits))
}

pub fn int_type_in_context(number_of_bits: u32, context: &Context) -> Type {
    new_type!(LLVMIntTypeInContext(context.as_llvm_ref(), number_of_bits))
}

pub fn pointer_type(pointee: Type, address_space: u32) -> Type {
    new_type!(LLVMPointerType(pointee.as_llvm_ref(), address_space))
}

pub fn void_type() -> Type {
    new_type!(LLVMVoidType())
}

pub fn void_type_in_context(context: &Context) -> Type {
    new_type!(LLVMVoidTypeInContext(context.as_llvm_ref()))
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeTargetInitializationError(String);

impl NativeTargetInitializationError {
    pub fn new(message: String) -> Self {
        Self(message)
    }

    pub fn new_from_raw(ptr: *mut i8) -> Self {
        Self(
            unsafe { CString::from_raw(ptr) }
                .to_string_lossy()
                .to_string(),
        )
    }
}

impl fmt::Display for NativeTargetInitializationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for NativeTargetInitializationError {}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeAsmPrinterInitializationError(String);

impl NativeAsmPrinterInitializationError {
    pub fn new(message: String) -> Self {
        Self(message)
    }

    pub fn new_from_raw(ptr: *mut i8) -> Self {
        Self(
            unsafe { CString::from_raw(ptr) }
                .to_string_lossy()
                .to_string(),
        )
    }
}

impl fmt::Display for NativeAsmPrinterInitializationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for NativeAsmPrinterInitializationError {}

pub fn initialize_native_target() -> Result<(), NativeTargetInitializationError> {
    let initialized_native_target = unsafe { LLVM_InitializeNativeTarget() };

    if initialized_native_target != 0 {
        Err(NativeTargetInitializationError::new(
            "Initializing the native target failed".to_string(),
        ))
    } else {
        Ok(())
    }
}

pub fn initialize_native_asm_printer() -> Result<(), NativeAsmPrinterInitializationError> {
    let initialized_native_asm_printer = unsafe { LLVM_InitializeNativeAsmPrinter() };

    if initialized_native_asm_printer != 0 {
        Err(NativeAsmPrinterInitializationError::new(
            "Initializing the native assembly printer failed".to_string(),
        ))
    } else {
        Ok(())
    }
}

// typedef enum
//   {
//     _URC_NO_REASON = 0,
//     _URC_FOREIGN_EXCEPTION_CAUGHT = 1,
//     _URC_FATAL_PHASE2_ERROR = 2,
//     _URC_FATAL_PHASE1_ERROR = 3,
//     _URC_NORMAL_STOP = 4,
//     _URC_END_OF_STACK = 5,
//     _URC_HANDLER_FOUND = 6,
//     _URC_INSTALL_CONTEXT = 7,
//     _URC_CONTINUE_UNWIND = 8
//   }
// _Unwind_Reason_Code;

