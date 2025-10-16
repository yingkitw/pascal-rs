use crate::traits::*;
use minipas_ast::enhanced_ast::*;
use std::collections::HashMap;
use std::fmt;

/// Enhanced code generator based on Free Pascal Compiler
/// Provides comprehensive code generation for multiple target architectures

#[derive(Debug, Clone)]
pub struct EnhancedCodeGenerator {
    pub target_architecture: TargetArchitecture,
    pub calling_convention: CallingConvention,
    pub instructions: Vec<Instruction>,
    pub labels: HashMap<String, Label>,
    pub functions: HashMap<String, FunctionInfo>,
    pub variables: HashMap<String, VariableInfo>,
    pub types: HashMap<String, TypeInfo>,
    pub registers: HashMap<String, Register>,
    pub current_function: Option<String>,
    pub stack_offset: usize,
    pub label_counter: u32,
    pub optimizer_settings: OptimizerSettings,
    pub debug_info: Vec<DebugInfo>,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TargetArchitecture {
    X86_64,
    X86_32,
    ARM64,
    ARM32,
    RiscV64,
    RiscV32,
    Mips64,
    Mips32,
    PowerPC64,
    PowerPC32,
    Sparc64,
    Sparc32,
    Wasm32,
    Z80,
    AVR,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallingConvention {
    Default,
    Cdecl,
    Pascal,
    Register,
    Safecall,
    Stdcall,
    Varargs,
    Cppdecl,
    Mwpascal,
    Syscall,
    Hardfloat,
    Softfloat,
    Vectorcall,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegisterClass {
    General,
    Float,
    Vector,
    Special,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Register {
    pub name: String,
    pub class: RegisterClass,
    pub size: usize,
    pub is_callee_saved: bool,
    pub is_caller_saved: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub opcode: String,
    pub operands: Vec<Operand>,
    pub comment: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Register(String),
    Immediate(i64),
    Memory(MemoryOperand),
    Label(String),
    Relocation(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemoryOperand {
    pub base: Option<String>,
    pub index: Option<String>,
    pub scale: u8,
    pub displacement: i64,
    pub size: Option<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub name: String,
    pub is_global: bool,
    pub is_exported: bool,
    pub is_imported: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OptimizerSettings {
    pub enable_constant_folding: bool,
    pub enable_dead_code_elimination: bool,
    pub enable_common_subexpression_elimination: bool,
    pub enable_loop_optimization: bool,
    pub enable_inlining: bool,
    pub enable_vectorization: bool,
    pub optimization_level: u8,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DebugInfo {
    pub file_name: String,
    pub line_number: u32,
    pub column_number: u32,
    pub symbol_name: Option<String>,
    pub symbol_type: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableInfo {
    pub name: String,
    pub variable_type: TypeInfo,
    pub location: VariableLocation,
    pub is_global: bool,
    pub is_static: bool,
    pub is_thread_local: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableLocation {
    Stack(usize),
    Register(String),
    Global(String),
    Parameter(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    pub name: String,
    pub size: usize,
    pub alignment: usize,
    pub is_signed: bool,
    pub is_float: bool,
    pub is_pointer: bool,
    pub is_array: bool,
    pub is_record: bool,
    pub is_enum: bool,
    pub is_set: bool,
    pub is_file: bool,
    pub is_procedure: bool,
    pub is_function: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterInfo {
    pub name: String,
    pub parameter_type: TypeInfo,
    pub location: ParameterLocation,
    pub is_var: bool,
    pub is_const: bool,
    pub is_out: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParameterLocation {
    Register(String),
    Stack(usize),
    Memory(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub calling_convention: CallingConvention,
    pub parameters: Vec<ParameterInfo>,
    pub return_type: Option<TypeInfo>,
    pub local_variables: Vec<VariableInfo>,
    pub stack_size: usize,
    pub is_exported: bool,
    pub is_imported: bool,
}
