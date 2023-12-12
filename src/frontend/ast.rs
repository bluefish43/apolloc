use std::{collections::HashMap, str::FromStr};



use super::tokens::{Token, Type};

pub type BExpression = Box<Expression>;

                            // return type - arguments - throws - is var args
pub type FunctionSignature = (Type, Vec<(String, Type)>, bool, bool);

#[derive(Debug, Clone, PartialEq)]
pub struct Namespace {
    pub static_variables: HashMap<String, Expression>,
    pub static_functions: HashMap<String, (FunctionSignature, Vec<StatementD>)>,
    pub static_namespaces: HashMap<String, Namespace>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessModifier {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub fields: Vec<(String, Type)>,
    pub functions: HashMap<String, (FunctionSignature, Vec<StatementD>)>,
    pub virtual_functions: Vec<(String, FunctionSignature, Vec<StatementD>)>,
    pub extends: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    NamespaceDeclaration(String, Namespace),
    ClassDeclaration(String, Class),
    VariableDeclaration(String, Option<Type>, Expression),
    Assignment(String, Expression),
    FunctionDeclaration(String, FunctionSignature, CallingConvention),
    FunctionDefinition(String, FunctionSignature, Vec<StatementD>, CallingConvention),
    IfElse(Expression, Box<StatementD>, Option<Box<StatementD>>),
    While(Expression, Box<StatementD>),
    DoWhile(Box<StatementD>, Expression),
    Call(FunctionSignature, String, Vec<Expression>, usize),
    Return(Expression),
    ReturnVoid,
    Block(Vec<StatementD>),
    DropGlue(String),
    DefineStruct(String, Vec<(String, Type)>),
    TryCatch(Box<StatementD>, String, Box<StatementD>),
    Throw(Expression),
    SysDefReturn(Expression),
    StructFieldAssignment(String, String, Expression, usize, Type),
    StructFieldIndirectAssignment(String, String, Expression, usize, Type),
    StructMethodDefinition(String, String, FunctionSignature, Vec<StatementD>, Type),
    StructMethodCall(BExpression, FunctionSignature, String, String, Vec<Expression>, usize),
}

pub type StatementD = (Token, Statement);

#[derive(Debug, Clone, PartialEq)]
pub enum DivType {
    SignedInt,
    UnsignedInt,
    Float,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    List(Vec<Expression>, Type),
    UncheckedCast(BExpression, Type),
    Reference(BExpression, String),
    StructFieldAccess(BExpression, String, usize, Type, Type),
    IndirectStructAccess(BExpression, String, usize, Type, Type),
    New(String, Vec<Expression>),
    Deref(Box<Expression>, Type),
    Variable(String),
    String(String),
    Call(FunctionSignature, String, Vec<Expression>, usize),
    Boolean(bool),
    NumberI64(i64),
    NumberI32(i32),
    NumberI16(i16),
    NumberI8(i8),
    NumberU64(u64),
    NumberU32(u32),
    NumberU16(u16),
    NumberU8(u8),
    Addition(BExpression, BExpression),
    Subtraction(BExpression, BExpression),
    Multiplication(BExpression, BExpression),
    Division(BExpression, BExpression, DivType, Type),
    Remainder(BExpression, BExpression, DivType, Type),
    Eq(BExpression, BExpression, Type, Type),
    Ne(BExpression, BExpression, Type, Type),
    Lt(BExpression, BExpression, Type, Type),
    Le(BExpression, BExpression, Type, Type),
    Gt(BExpression, BExpression, Type, Type),
    Ge(BExpression, BExpression, Type, Type),
    And(BExpression, BExpression),
    Or(BExpression, BExpression),
    Xor(BExpression, BExpression),
    TypeCast(BExpression, Type, Type),
    StructInstantiate(String, Vec<(String, Expression)>),
    NullPointer,
    StructMethodCall(BExpression, FunctionSignature, String, String, Vec<Expression>, usize),
}

#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(C)]
pub enum CallingConvention {
    C,
    FastCall,
    Cold,
    GHC,
    HiPE,
    WebKitJS,
    AnyReg,
    PreserveMost,
    PreserveAll,
    Swift,
    CXXFastTLS,
    Tail,
    CFGuardCheck,
    SwiftTail,
    X86StdCall,
    X86FastCall,
    ARMAPCS,
    ARMAAPCS,
    ARMAAPCSVFP,
    MSP430Intr,
    X86ThisCall,
    PTXKernel,
    PTXDevice,
    SPIRFunc,
    SPIRKernel,
    IntelOCLBI,
    X8664SystemV,
    Windows,
    X86VectorCall,
    DummyHHVM,
    DummyHHVMC,
    X86Intr,
    AVRIntr,
    AVRSignal,
    AVRBuiltin,
    AMDGPUVS,
    AMDGPUGS,
    AMDGPUPS,
    AMDGPUCS,
    AMDGPUKernel,
    X86RegCall,
    AMDGPUHS,
    MSP430Builtin,
    AMDGPULS,
    AMDGPUES,
    AArch64VectorCall,
    AArch64SVEVectorCall,
    WASMEmscriptenInvoke,
    AMDGPUGfx,
    M68kIntr,
    AArch64SMEABI_Support_RoutinesPreserveMostFromX0,
    AArch64SMEABI_Support_RoutinesPreserveMostFromX2,
    AMDGPUCSChain,
    AMDGPUCSChainPreserve,
    MaxID,
}

impl FromStr for CallingConvention {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.to_lowercase(); // Convert to lowercase to handle case-insensitivity

        match s.as_str() {
            "c" => Ok(CallingConvention::C),
            "fastcall" => Ok(CallingConvention::FastCall),
            "cold" => Ok(CallingConvention::Cold),
            "ghc" => Ok(CallingConvention::GHC),
            "hipe" => Ok(CallingConvention::HiPE),
            "webkitjs" => Ok(CallingConvention::WebKitJS),
            "anyreg" => Ok(CallingConvention::AnyReg),
            "preservemost" => Ok(CallingConvention::PreserveMost),
            "preserveall" => Ok(CallingConvention::PreserveAll),
            "swift" => Ok(CallingConvention::Swift),
            "cxxfasttls" => Ok(CallingConvention::CXXFastTLS),
            "tail" => Ok(CallingConvention::Tail),
            "cfguardcheck" => Ok(CallingConvention::CFGuardCheck),
            "swifttail" => Ok(CallingConvention::SwiftTail),
            "x86_stdcall" => Ok(CallingConvention::X86StdCall),
            "x86_fastcall" => Ok(CallingConvention::X86FastCall),
            "armapcs" => Ok(CallingConvention::ARMAPCS),
            "armaapcs" => Ok(CallingConvention::ARMAAPCS),
            "armaapcsvfp" => Ok(CallingConvention::ARMAAPCSVFP),
            "msp430intr" => Ok(CallingConvention::MSP430Intr),
            "x86thiscall" => Ok(CallingConvention::X86ThisCall),
            "ptxkernel" => Ok(CallingConvention::PTXKernel),
            "ptxdevice" => Ok(CallingConvention::PTXDevice),
            "spirfunc" => Ok(CallingConvention::SPIRFunc),
            "spirkernel" => Ok(CallingConvention::SPIRKernel),
            "inteloclbi" => Ok(CallingConvention::IntelOCLBI),
            "systemv" => Ok(CallingConvention::X8664SystemV),
            "windows" => Ok(CallingConvention::Windows),
            "x86vectorcall" => Ok(CallingConvention::X86VectorCall),
            "dummyhhvm" => Ok(CallingConvention::DummyHHVM),
            "dummyhhvmc" => Ok(CallingConvention::DummyHHVMC),
            "x86intr" => Ok(CallingConvention::X86Intr),
            "avrintr" => Ok(CallingConvention::AVRIntr),
            "avrsignal" => Ok(CallingConvention::AVRSignal),
            "avrbuiltin" => Ok(CallingConvention::AVRBuiltin),
            "amdgpuvs" => Ok(CallingConvention::AMDGPUVS),
            "amdgpugs" => Ok(CallingConvention::AMDGPUGS),
            "amdgpups" => Ok(CallingConvention::AMDGPUPS),
            "amdgpucs" => Ok(CallingConvention::AMDGPUCS),
            "amdgptkernel" => Ok(CallingConvention::AMDGPUKernel),
            "x86regcall" => Ok(CallingConvention::X86RegCall),
            "amdgpuhs" => Ok(CallingConvention::AMDGPUHS),
            "msp430builtin" => Ok(CallingConvention::MSP430Builtin),
            "amdgpuls" => Ok(CallingConvention::AMDGPULS),
            "amdgpues" => Ok(CallingConvention::AMDGPUES),
            "aarch64vectorcall" => Ok(CallingConvention::AArch64VectorCall),
            "aarch64svevectorcall" => Ok(CallingConvention::AArch64SVEVectorCall),
            "wasmemscripteninvoke" => Ok(CallingConvention::WASMEmscriptenInvoke),
            "amdgpugfx" => Ok(CallingConvention::AMDGPUGfx),
            "m68kintr" => Ok(CallingConvention::M68kIntr),
            "aarch64smeabi_support_routines_preservemost_from_x0" => {
                Ok(CallingConvention::AArch64SMEABI_Support_RoutinesPreserveMostFromX0)
            }
            "aarch64smeabi_support_routines_preservemost_from_x2" => {
                Ok(CallingConvention::AArch64SMEABI_Support_RoutinesPreserveMostFromX2)
            }
            "amdgpucschain" => Ok(CallingConvention::AMDGPUCSChain),
            "amdgpucschainpreserve" => Ok(CallingConvention::AMDGPUCSChainPreserve),
            "maxid" => Ok(CallingConvention::MaxID),
            _ => Err(()), // Return an error for unknown conventions
        }
    }
}
