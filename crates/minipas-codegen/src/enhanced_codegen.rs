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
    Memory(MemoryOperand),
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
    Register(String),
    Stack(usize),
    Global(String),
    ThreadLocal(String),
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
    pub source_file: String,
    pub line_number: usize,
    pub column_number: usize,
    pub function_name: Option<String>,
    pub variable_name: Option<String>,
}

/// Enhanced code generator
pub struct EnhancedCodeGenerator {
    target_architecture: TargetArchitecture,
    calling_convention: CallingConvention,
    instructions: Vec<Instruction>,
    labels: HashMap<String, Label>,
    functions: HashMap<String, FunctionInfo>,
    variables: HashMap<String, VariableInfo>,
    types: HashMap<String, TypeInfo>,
    registers: HashMap<String, Register>,
    current_function: Option<String>,
    stack_offset: usize,
    label_counter: usize,
    optimizer_settings: OptimizerSettings,
    debug_info: Vec<DebugInfo>,
    errors: Vec<String>,
    warnings: Vec<String>,
}

impl EnhancedCodeGenerator {
    /// Creates a new enhanced code generator
    pub fn new(target_architecture: TargetArchitecture) -> Self {
        let mut generator = Self {
            target_architecture,
            calling_convention: CallingConvention::Default,
            instructions: Vec::new(),
            labels: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
            types: HashMap::new(),
            registers: HashMap::new(),
            current_function: None,
            stack_offset: 0,
            label_counter: 0,
            optimizer_settings: OptimizerSettings {
                enable_constant_folding: true,
                enable_dead_code_elimination: true,
                enable_common_subexpression_elimination: true,
                enable_loop_optimization: true,
                enable_inlining: true,
                enable_vectorization: false,
                optimization_level: 2,
            },
            debug_info: Vec::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        };
        
        generator.initialize_target();
        generator
    }
    
    /// Initializes target-specific settings
    fn initialize_target(&mut self) {
        match self.target_architecture {
            TargetArchitecture::X86_64 => {
                self.initialize_x86_64();
            }
            TargetArchitecture::X86_32 => {
                self.initialize_x86_32();
            }
            TargetArchitecture::ARM64 => {
                self.initialize_arm64();
            }
            TargetArchitecture::ARM32 => {
                self.initialize_arm32();
            }
            TargetArchitecture::RiscV64 => {
                self.initialize_riscv64();
            }
            TargetArchitecture::RiscV32 => {
                self.initialize_riscv32();
            }
            _ => {
                // Initialize with default settings
                self.initialize_default();
            }
        }
    }
    
    /// Initializes x86-64 target
    fn initialize_x86_64(&mut self) {
        // General purpose registers
        self.registers.insert("rax".to_string(), Register {
            name: "rax".to_string(),
            class: RegisterClass::General,
            size: 8,
            is_callee_saved: false,
            is_caller_saved: true,
        });
        
        self.registers.insert("rbx".to_string(), Register {
            name: "rbx".to_string(),
            class: RegisterClass::General,
            size: 8,
            is_callee_saved: true,
            is_caller_saved: false,
        });
        
        self.registers.insert("rcx".to_string(), Register {
            name: "rcx".to_string(),
            class: RegisterClass::General,
            size: 8,
            is_callee_saved: false,
            is_caller_saved: true,
        });
        
        self.registers.insert("rdx".to_string(), Register {
            name: "rdx".to_string(),
            class: RegisterClass::General,
            size: 8,
            is_callee_saved: false,
            is_caller_saved: true,
        });
        
        self.registers.insert("rsi".to_string(), Register {
            name: "rsi".to_string(),
            class: RegisterClass::General,
            size: 8,
            is_callee_saved: false,
            is_caller_saved: true,
        });
        
        self.registers.insert("rdi".to_string(), Register {
            name: "rdi".to_string(),
            class: RegisterClass::General,
            size: 8,
            is_callee_saved: false,
            is_caller_saved: true,
        });
        
        self.registers.insert("rsp".to_string(), Register {
            name: "rsp".to_string(),
            class: RegisterClass::Special,
            size: 8,
            is_callee_saved: true,
            is_caller_saved: false,
        });
        
        self.registers.insert("rbp".to_string(), Register {
            name: "rbp".to_string(),
            class: RegisterClass::General,
            size: 8,
            is_callee_saved: true,
            is_caller_saved: false,
        });
        
        // Floating point registers
        for i in 0..16 {
            let name = format!("xmm{}", i);
            self.registers.insert(name.clone(), Register {
                name,
                class: RegisterClass::Float,
                size: 16,
                is_callee_saved: i < 6,
                is_caller_saved: i >= 6,
            });
        }
    }
    
    /// Initializes x86-32 target
    fn initialize_x86_32(&mut self) {
        // General purpose registers
        self.registers.insert("eax".to_string(), Register {
            name: "eax".to_string(),
            class: RegisterClass::General,
            size: 4,
            is_callee_saved: false,
            is_caller_saved: true,
        });
        
        self.registers.insert("ebx".to_string(), Register {
            name: "ebx".to_string(),
            class: RegisterClass::General,
            size: 4,
            is_callee_saved: true,
            is_caller_saved: false,
        });
        
        self.registers.insert("ecx".to_string(), Register {
            name: "ecx".to_string(),
            class: RegisterClass::General,
            size: 4,
            is_callee_saved: false,
            is_caller_saved: true,
        });
        
        self.registers.insert("edx".to_string(), Register {
            name: "edx".to_string(),
            class: RegisterClass::General,
            size: 4,
            is_callee_saved: false,
            is_caller_saved: true,
        });
        
        self.registers.insert("esi".to_string(), Register {
            name: "esi".to_string(),
            class: RegisterClass::General,
            size: 4,
            is_callee_saved: true,
            is_caller_saved: false,
        });
        
        self.registers.insert("edi".to_string(), Register {
            name: "edi".to_string(),
            class: RegisterClass::General,
            size: 4,
            is_callee_saved: true,
            is_caller_saved: false,
        });
        
        self.registers.insert("esp".to_string(), Register {
            name: "esp".to_string(),
            class: RegisterClass::Special,
            size: 4,
            is_callee_saved: true,
            is_caller_saved: false,
        });
        
        self.registers.insert("ebp".to_string(), Register {
            name: "ebp".to_string(),
            class: RegisterClass::General,
            size: 4,
            is_callee_saved: true,
            is_caller_saved: false,
        });
    }
    
    /// Initializes ARM64 target
    fn initialize_arm64(&mut self) {
        // General purpose registers
        for i in 0..31 {
            let name = format!("x{}", i);
            self.registers.insert(name.clone(), Register {
                name,
                class: RegisterClass::General,
                size: 8,
                is_callee_saved: i >= 19,
                is_caller_saved: i < 19,
            });
        }
        
        // Floating point registers
        for i in 0..32 {
            let name = format!("v{}", i);
            self.registers.insert(name.clone(), Register {
                name,
                class: RegisterClass::Float,
                size: 16,
                is_callee_saved: i >= 8,
                is_caller_saved: i < 8,
            });
        }
    }
    
    /// Initializes ARM32 target
    fn initialize_arm32(&mut self) {
        // General purpose registers
        for i in 0..16 {
            let name = format!("r{}", i);
            self.registers.insert(name.clone(), Register {
                name,
                class: RegisterClass::General,
                size: 4,
                is_callee_saved: i >= 4,
                is_caller_saved: i < 4,
            });
        }
        
        // Floating point registers
        for i in 0..32 {
            let name = format!("s{}", i);
            self.registers.insert(name.clone(), Register {
                name,
                class: RegisterClass::Float,
                size: 4,
                is_callee_saved: i >= 16,
                is_caller_saved: i < 16,
            });
        }
    }
    
    /// Initializes RISC-V 64-bit target
    fn initialize_riscv64(&mut self) {
        // General purpose registers
        for i in 0..32 {
            let name = format!("x{}", i);
            self.registers.insert(name.clone(), Register {
                name,
                class: RegisterClass::General,
                size: 8,
                is_callee_saved: i >= 8,
                is_caller_saved: i < 8,
            });
        }
        
        // Floating point registers
        for i in 0..32 {
            let name = format!("f{}", i);
            self.registers.insert(name.clone(), Register {
                name,
                class: RegisterClass::Float,
                size: 8,
                is_callee_saved: i >= 8,
                is_caller_saved: i < 8,
            });
        }
    }
    
    /// Initializes RISC-V 32-bit target
    fn initialize_riscv32(&mut self) {
        // General purpose registers
        for i in 0..32 {
            let name = format!("x{}", i);
            self.registers.insert(name.clone(), Register {
                name,
                class: RegisterClass::General,
                size: 4,
                is_callee_saved: i >= 8,
                is_caller_saved: i < 8,
            });
        }
        
        // Floating point registers
        for i in 0..32 {
            let name = format!("f{}", i);
            self.registers.insert(name.clone(), Register {
                name,
                class: RegisterClass::Float,
                size: 4,
                is_callee_saved: i >= 8,
                is_caller_saved: i < 8,
            });
        }
    }
    
    /// Initializes default target
    fn initialize_default(&mut self) {
        // Basic register set
        self.registers.insert("r0".to_string(), Register {
            name: "r0".to_string(),
            class: RegisterClass::General,
            size: 8,
            is_callee_saved: false,
            is_caller_saved: true,
        });
    }
    
    /// Generates code for a program
    pub fn generate_program(&mut self, program: &Program) -> Result<String, String> {
        self.emit_header()?;
        
        // Generate code for each function
        for function in &program.block.functions {
            self.generate_function(function)?;
        }
        
        for procedure in &program.block.procedures {
            self.generate_procedure(procedure)?;
        }
        
        // Generate main program code
        self.generate_main_program(program)?;
        
        self.emit_footer()?;
        
        Ok(self.get_assembly_code())
    }
    
    /// Generates code for a function
    fn generate_function(&mut self, function: &Function) -> Result<(), String> {
        self.current_function = Some(function.name.clone());
        
        // Create function info
        let function_info = FunctionInfo {
            name: function.name.clone(),
            calling_convention: function.calling_convention.clone(),
            parameters: function.parameters.iter().map(|p| ParameterInfo {
                name: p.name.clone(),
                parameter_type: self.convert_type(&p.parameter_type),
                location: ParameterLocation::Stack(0), // TODO: Calculate actual location
                is_var: p.is_var,
                is_const: p.is_const,
                is_out: p.is_out,
            }).collect(),
            return_type: Some(self.convert_type(&function.return_type)),
            local_variables: Vec::new(),
            stack_size: 0,
            is_exported: function.is_exported,
            is_imported: function.is_external,
        };
        
        self.functions.insert(function.name.clone(), function_info);
        
        // Generate function prologue
        self.emit_function_prologue(&function.name)?;
        
        // Generate function body
        if let Some(body) = &function.body {
            self.generate_block(body)?;
        }
        
        // Generate function epilogue
        self.emit_function_epilogue(&function.name)?;
        
        self.current_function = None;
        Ok(())
    }
    
    /// Generates code for a procedure
    fn generate_procedure(&mut self, procedure: &Procedure) -> Result<(), String> {
        self.current_function = Some(procedure.name.clone());
        
        // Create function info
        let function_info = FunctionInfo {
            name: procedure.name.clone(),
            calling_convention: procedure.calling_convention.clone(),
            parameters: procedure.parameters.iter().map(|p| ParameterInfo {
                name: p.name.clone(),
                parameter_type: self.convert_type(&p.parameter_type),
                location: ParameterLocation::Stack(0), // TODO: Calculate actual location
                is_var: p.is_var,
                is_const: p.is_const,
                is_out: p.is_out,
            }).collect(),
            return_type: None,
            local_variables: Vec::new(),
            stack_size: 0,
            is_exported: procedure.is_exported,
            is_imported: procedure.is_external,
        };
        
        self.functions.insert(procedure.name.clone(), function_info);
        
        // Generate procedure prologue
        self.emit_function_prologue(&procedure.name)?;
        
        // Generate procedure body
        if let Some(body) = &procedure.body {
            self.generate_block(body)?;
        }
        
        // Generate procedure epilogue
        self.emit_function_epilogue(&procedure.name)?;
        
        self.current_function = None;
        Ok(())
    }
    
    /// Generates code for a block
    fn generate_block(&mut self, block: &Block) -> Result<(), String> {
        // Generate code for variable declarations
        for variable in &block.vars {
            self.generate_variable_declaration(variable)?;
        }
        
        // Generate code for statements
        for statement in &block.statements {
            self.generate_statement(statement)?;
        }
        
        Ok(())
    }
    
    /// Generates code for a variable declaration
    fn generate_variable_declaration(&mut self, variable: &Variable) -> Result<(), String> {
        let type_info = self.convert_type(&variable.variable_type);
        
        let variable_info = VariableInfo {
            name: variable.name.clone(),
            variable_type: type_info.clone(),
            location: VariableLocation::Stack(self.stack_offset),
            is_global: false,
            is_static: false,
            is_thread_local: false,
        };
        
        self.variables.insert(variable.name.clone(), variable_info);
        self.stack_offset += type_info.size;
        
        // Generate initialization code if needed
        if let Some(initial_value) = &variable.initial_value {
            self.generate_expression(initial_value)?;
            self.emit_store_variable(&variable.name)?;
        }
        
        Ok(())
    }
    
    /// Generates code for a statement
    fn generate_statement(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Assignment { target, value } => {
                self.generate_expression(value)?;
                self.generate_assignment_target(target)?;
            }
            Statement::If { condition, then_branch, else_branch } => {
                self.generate_if_statement(condition, then_branch, else_branch)?;
            }
            Statement::While { condition, body } => {
                self.generate_while_statement(condition, body)?;
            }
            Statement::Repeat { body, condition } => {
                self.generate_repeat_statement(body, condition)?;
            }
            Statement::For { variable, start_value, end_value, step, body, direction } => {
                self.generate_for_statement(variable, start_value, end_value, step, body, direction)?;
            }
            Statement::Case { expression, cases, else_branch } => {
                self.generate_case_statement(expression, cases, else_branch)?;
            }
            Statement::Try { body, except_clauses, finally_clause } => {
                self.generate_try_statement(body, except_clauses, finally_clause)?;
            }
            Statement::With { expressions, body } => {
                self.generate_with_statement(expressions, body)?;
            }
            Statement::Goto { label } => {
                self.emit_goto(label)?;
            }
            Statement::Exit { return_value } => {
                self.generate_exit_statement(return_value)?;
            }
            Statement::Break => {
                self.emit_break()?;
            }
            Statement::Continue => {
                self.emit_continue()?;
            }
            Statement::Halt { exit_code } => {
                self.generate_halt_statement(exit_code)?;
            }
            Statement::Block(block) => {
                self.generate_block(block)?;
            }
            Statement::Empty => {
                // Do nothing
            }
            _ => {
                return Err("Unsupported statement type".to_string());
            }
        }
        
        Ok(())
    }
    
    /// Generates code for an expression
    fn generate_expression(&mut self, expression: &Expression) -> Result<(), String> {
        match expression {
            Expression::Literal(literal) => {
                self.generate_literal(literal)?;
            }
            Expression::Identifier(name) => {
                self.emit_load_variable(name)?;
            }
            Expression::BinaryOp { left, operator, right } => {
                self.generate_binary_operation(left, operator, right)?;
            }
            Expression::UnaryOp { operator, operand } => {
                self.generate_unary_operation(operator, operand)?;
            }
            Expression::FunctionCall { name, arguments } => {
                self.generate_function_call(name, arguments)?;
            }
            Expression::MethodCall { object, method, arguments } => {
                self.generate_method_call(object, method, arguments)?;
            }
            Expression::ArrayAccess { array, index } => {
                self.generate_array_access(array, index)?;
            }
            Expression::RecordAccess { record, field } => {
                self.generate_record_access(record, field)?;
            }
            Expression::PointerDeref { pointer } => {
                self.generate_pointer_dereference(pointer)?;
            }
            Expression::AddressOf { variable } => {
                self.generate_address_of(variable)?;
            }
            Expression::TypeCast { target_type, expression } => {
                self.generate_type_cast(target_type, expression)?;
            }
            Expression::SetLiteral { elements } => {
                self.generate_set_literal(elements)?;
            }
            Expression::Range { start, end } => {
                self.generate_range(start, end)?;
            }
        }
        
        Ok(())
    }
    
    /// Generates code for a literal
    fn generate_literal(&mut self, literal: &Literal) -> Result<(), String> {
        match literal {
            Literal::Integer(value) => {
                self.emit_load_immediate(*value)?;
            }
            Literal::Real(value) => {
                self.emit_load_float(*value)?;
            }
            Literal::Boolean(value) => {
                self.emit_load_immediate(if *value { 1 } else { 0 })?;
            }
            Literal::Char(value) => {
                self.emit_load_immediate(*value as i64)?;
            }
            Literal::String(value) => {
                self.emit_load_string(value)?;
            }
            Literal::WideString(value) => {
                self.emit_load_wide_string(value)?;
            }
            Literal::Nil => {
                self.emit_load_immediate(0)?;
            }
        }
        
        Ok(())
    }
    
    /// Generates code for a binary operation
    fn generate_binary_operation(&mut self, left: &Expression, operator: &BinaryOperator, right: &Expression) -> Result<(), String> {
        self.generate_expression(left)?;
        self.generate_expression(right)?;
        
        match operator {
            BinaryOperator::Add => {
                self.emit_add()?;
            }
            BinaryOperator::Subtract => {
                self.emit_subtract()?;
            }
            BinaryOperator::Multiply => {
                self.emit_multiply()?;
            }
            BinaryOperator::Divide => {
                self.emit_divide()?;
            }
            BinaryOperator::IntDivide => {
                self.emit_int_divide()?;
            }
            BinaryOperator::Modulus => {
                self.emit_modulus()?;
            }
            BinaryOperator::Equal => {
                self.emit_equal()?;
            }
            BinaryOperator::NotEqual => {
                self.emit_not_equal()?;
            }
            BinaryOperator::LessThan => {
                self.emit_less_than()?;
            }
            BinaryOperator::GreaterThan => {
                self.emit_greater_than()?;
            }
            BinaryOperator::LessEqual => {
                self.emit_less_equal()?;
            }
            BinaryOperator::GreaterEqual => {
                self.emit_greater_equal()?;
            }
            BinaryOperator::And => {
                self.emit_and()?;
            }
            BinaryOperator::Or => {
                self.emit_or()?;
            }
            BinaryOperator::Xor => {
                self.emit_xor()?;
            }
            BinaryOperator::Shl => {
                self.emit_shift_left()?;
            }
            BinaryOperator::Shr => {
                self.emit_shift_right()?;
            }
            BinaryOperator::In => {
                self.emit_in()?;
            }
            BinaryOperator::Is => {
                self.emit_is()?;
            }
            BinaryOperator::As => {
                self.emit_as()?;
            }
        }
        
        Ok(())
    }
    
    /// Generates code for a unary operation
    fn generate_unary_operation(&mut self, operator: &UnaryOperator, operand: &Expression) -> Result<(), String> {
        self.generate_expression(operand)?;
        
        match operator {
            UnaryOperator::Plus => {
                // Do nothing, value is already on stack
            }
            UnaryOperator::Minus => {
                self.emit_negate()?;
            }
            UnaryOperator::Not => {
                self.emit_not()?;
            }
            UnaryOperator::AddressOf => {
                self.emit_address_of()?;
            }
            UnaryOperator::PointerDeref => {
                self.emit_pointer_deref()?;
            }
        }
        
        Ok(())
    }
    
    /// Converts enhanced type to type info
    fn convert_type(&mut self, enhanced_type: &EnhancedType) -> TypeInfo {
        match enhanced_type {
            EnhancedType::Integer => TypeInfo {
                name: "integer".to_string(),
                size: 8,
                alignment: 8,
                is_signed: true,
                is_float: false,
                is_pointer: false,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: false,
                is_procedure: false,
                is_function: false,
            },
            EnhancedType::Real => TypeInfo {
                name: "real".to_string(),
                size: 8,
                alignment: 8,
                is_signed: true,
                is_float: true,
                is_pointer: false,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: false,
                is_procedure: false,
                is_function: false,
            },
            EnhancedType::Boolean => TypeInfo {
                name: "boolean".to_string(),
                size: 1,
                alignment: 1,
                is_signed: false,
                is_float: false,
                is_pointer: false,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: false,
                is_procedure: false,
                is_function: false,
            },
            EnhancedType::Char => TypeInfo {
                name: "char".to_string(),
                size: 1,
                alignment: 1,
                is_signed: false,
                is_float: false,
                is_pointer: false,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: false,
                is_procedure: false,
                is_function: false,
            },
            EnhancedType::String => TypeInfo {
                name: "string".to_string(),
                size: 8, // Pointer size
                alignment: 8,
                is_signed: false,
                is_float: false,
                is_pointer: true,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: false,
                is_procedure: false,
                is_function: false,
            },
            EnhancedType::WideString => TypeInfo {
                name: "widestring".to_string(),
                size: 8, // Pointer size
                alignment: 8,
                is_signed: false,
                is_float: false,
                is_pointer: true,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: false,
                is_procedure: false,
                is_function: false,
            },
            EnhancedType::Array { element_type, dimensions } => {
                let element_info = self.convert_type(element_type);
                TypeInfo {
                    name: "array".to_string(),
                    size: element_info.size * dimensions.len(),
                    alignment: element_info.alignment,
                    is_signed: element_info.is_signed,
                    is_float: element_info.is_float,
                    is_pointer: false,
                    is_array: true,
                    is_record: false,
                    is_enum: false,
                    is_set: false,
                    is_file: false,
                    is_procedure: false,
                    is_function: false,
                }
            }
            EnhancedType::Record { fields, .. } => {
                let mut size = 0;
                let mut alignment = 1;
                
                for field in fields {
                    let field_info = self.convert_type(&field.field_type);
                    size += field_info.size;
                    alignment = alignment.max(field_info.alignment);
                }
                
                TypeInfo {
                    name: "record".to_string(),
                    size,
                    alignment,
                    is_signed: false,
                    is_float: false,
                    is_pointer: false,
                    is_array: false,
                    is_record: true,
                    is_enum: false,
                    is_set: false,
                    is_file: false,
                    is_procedure: false,
                    is_function: false,
                }
            }
            EnhancedType::Set { base_type } => {
                let base_info = self.convert_type(base_type);
                TypeInfo {
                    name: "set".to_string(),
                    size: 8, // Set size
                    alignment: 8,
                    is_signed: false,
                    is_float: false,
                    is_pointer: false,
                    is_array: false,
                    is_record: false,
                    is_enum: false,
                    is_set: true,
                    is_file: false,
                    is_procedure: false,
                    is_function: false,
                }
            }
            EnhancedType::File { .. } => TypeInfo {
                name: "file".to_string(),
                size: 8,
                alignment: 8,
                is_signed: false,
                is_float: false,
                is_pointer: true,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: true,
                is_procedure: false,
                is_function: false,
            },
            EnhancedType::Pointer { target_type } => {
                let target_info = self.convert_type(target_type);
                TypeInfo {
                    name: "pointer".to_string(),
                    size: 8,
                    alignment: 8,
                    is_signed: false,
                    is_float: false,
                    is_pointer: true,
                    is_array: false,
                    is_record: false,
                    is_enum: false,
                    is_set: false,
                    is_file: false,
                    is_procedure: false,
                    is_function: false,
                }
            }
            EnhancedType::Procedure { .. } => TypeInfo {
                name: "procedure".to_string(),
                size: 8,
                alignment: 8,
                is_signed: false,
                is_float: false,
                is_pointer: true,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: false,
                is_procedure: true,
                is_function: false,
            }
            EnhancedType::Function { .. } => TypeInfo {
                name: "function".to_string(),
                size: 8,
                alignment: 8,
                is_signed: false,
                is_float: false,
                is_pointer: true,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: false,
                is_procedure: false,
                is_function: true,
            }
            _ => TypeInfo {
                name: "unknown".to_string(),
                size: 8,
                alignment: 8,
                is_signed: false,
                is_float: false,
                is_pointer: false,
                is_array: false,
                is_record: false,
                is_enum: false,
                is_set: false,
                is_file: false,
                is_procedure: false,
                is_function: false,
            }
        }
    }
    
    /// Emits assembly header
    fn emit_header(&mut self) -> Result<(), String> {
        match self.target_architecture {
            TargetArchitecture::X86_64 => {
                self.instructions.push(Instruction {
                    opcode: ".text".to_string(),
                    operands: Vec::new(),
                    comment: Some("Code section".to_string()),
                });
                self.instructions.push(Instruction {
                    opcode: ".global".to_string(),
                    operands: vec![Operand::Label("_start".to_string())],
                    comment: Some("Entry point".to_string()),
                });
            }
            TargetArchitecture::X86_32 => {
                self.instructions.push(Instruction {
                    opcode: ".text".to_string(),
                    operands: Vec::new(),
                    comment: Some("Code section".to_string()),
                });
                self.instructions.push(Instruction {
                    opcode: ".global".to_string(),
                    operands: vec![Operand::Label("_start".to_string())],
                    comment: Some("Entry point".to_string()),
                });
            }
            _ => {
                // Default header
                self.instructions.push(Instruction {
                    opcode: ".text".to_string(),
                    operands: Vec::new(),
                    comment: Some("Code section".to_string()),
                });
            }
        }
        
        Ok(())
    }
    
    /// Emits assembly footer
    fn emit_footer(&mut self) -> Result<(), String> {
        // Add any necessary footer code
        Ok(())
    }
    
    /// Gets the generated assembly code
    fn get_assembly_code(&self) -> String {
        let mut code = String::new();
        
        for instruction in &self.instructions {
            if let Some(comment) = &instruction.comment {
                code.push_str(&format!("    # {}\n", comment));
            }
            
            code.push_str(&format!("    {}", instruction.opcode));
            
            for (i, operand) in instruction.operands.iter().enumerate() {
                if i == 0 {
                    code.push_str(" ");
                } else {
                    code.push_str(", ");
                }
                
                match operand {
                    Operand::Register(name) => {
                        code.push_str(name);
                    }
                    Operand::Immediate(value) => {
                        code.push_str(&format!("${}", value));
                    }
                    Operand::Memory(mem) => {
                        code.push_str(&format!("{}", mem));
                    }
                    Operand::Label(name) => {
                        code.push_str(name);
                    }
                    Operand::Relocation(name) => {
                        code.push_str(&format!("{}@PLT", name));
                    }
                }
            }
            
            code.push_str("\n");
        }
        
        code
    }
    
    // Placeholder methods for code generation
    // These would be implemented with actual assembly generation logic
    
    fn emit_function_prologue(&mut self, _name: &str) -> Result<(), String> { Ok(()) }
    fn emit_function_epilogue(&mut self, _name: &str) -> Result<(), String> { Ok(()) }
    fn emit_main_program(&mut self, _program: &Program) -> Result<(), String> { Ok(()) }
    fn emit_load_immediate(&mut self, _value: i64) -> Result<(), String> { Ok(()) }
    fn emit_load_float(&mut self, _value: f64) -> Result<(), String> { Ok(()) }
    fn emit_load_string(&mut self, _value: &str) -> Result<(), String> { Ok(()) }
    fn emit_load_wide_string(&mut self, _value: &str) -> Result<(), String> { Ok(()) }
    fn emit_load_variable(&mut self, _name: &str) -> Result<(), String> { Ok(()) }
    fn emit_store_variable(&mut self, _name: &str) -> Result<(), String> { Ok(()) }
    fn emit_add(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_subtract(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_multiply(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_divide(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_int_divide(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_modulus(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_equal(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_not_equal(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_less_than(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_greater_than(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_less_equal(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_greater_equal(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_and(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_or(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_xor(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_shift_left(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_shift_right(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_in(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_is(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_as(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_negate(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_not(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_address_of(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_pointer_deref(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_goto(&mut self, _label: &str) -> Result<(), String> { Ok(()) }
    fn emit_break(&mut self) -> Result<(), String> { Ok(()) }
    fn emit_continue(&mut self) -> Result<(), String> { Ok(()) }
    
    // Additional placeholder methods
    fn generate_main_program(&mut self, _program: &Program) -> Result<(), String> { Ok(()) }
    fn generate_assignment_target(&mut self, _target: &Expression) -> Result<(), String> { Ok(()) }
    fn generate_if_statement(&mut self, _condition: &Expression, _then_branch: &Block, _else_branch: &Option<Block>) -> Result<(), String> { Ok(()) }
    fn generate_while_statement(&mut self, _condition: &Expression, _body: &Block) -> Result<(), String> { Ok(()) }
    fn generate_repeat_statement(&mut self, _body: &Block, _condition: &Expression) -> Result<(), String> { Ok(()) }
    fn generate_for_statement(&mut self, _variable: &str, _start_value: &Expression, _end_value: &Expression, _step: &Option<Expression>, _body: &Block, _direction: &ForDirection) -> Result<(), String> { Ok(()) }
    fn generate_case_statement(&mut self, _expression: &Expression, _cases: &[CaseBranch], _else_branch: &Option<Block>) -> Result<(), String> { Ok(()) }
    fn generate_try_statement(&mut self, _body: &Block, _except_clauses: &[ExceptClause], _finally_clause: &Option<Block>) -> Result<(), String> { Ok(()) }
    fn generate_with_statement(&mut self, _expressions: &[Expression], _body: &Block) -> Result<(), String> { Ok(()) }
    fn generate_exit_statement(&mut self, _return_value: &Option<Expression>) -> Result<(), String> { Ok(()) }
    fn generate_halt_statement(&mut self, _exit_code: &Option<Expression>) -> Result<(), String> { Ok(()) }
    fn generate_function_call(&mut self, _name: &str, _arguments: &[Expression]) -> Result<(), String> { Ok(()) }
    fn generate_method_call(&mut self, _object: &Expression, _method: &str, _arguments: &[Expression]) -> Result<(), String> { Ok(()) }
    fn generate_array_access(&mut self, _array: &Expression, _index: &[Expression]) -> Result<(), String> { Ok(()) }
    fn generate_record_access(&mut self, _record: &Expression, _field: &str) -> Result<(), String> { Ok(()) }
    fn generate_pointer_dereference(&mut self, _pointer: &Expression) -> Result<(), String> { Ok(()) }
    fn generate_address_of(&mut self, _variable: &Expression) -> Result<(), String> { Ok(()) }
    fn generate_type_cast(&mut self, _target_type: &EnhancedType, _expression: &Expression) -> Result<(), String> { Ok(()) }
    fn generate_set_literal(&mut self, _elements: &[Expression]) -> Result<(), String> { Ok(()) }
    fn generate_range(&mut self, _start: &Expression, _end: &Expression) -> Result<(), String> { Ok(()) }
}

impl fmt::Display for MemoryOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(base) = &self.base {
            write!(f, "({})", base)?;
        }
        if let Some(index) = &self.index {
            write!(f, ",{},{}", index, self.scale)?;
        }
        if self.displacement != 0 {
            write!(f, ",{}", self.displacement)?;
        }
        Ok(())
    }
}

// Implement traits for enhanced code generator
impl CodeGeneratorCapability for EnhancedCodeGenerator {
    fn generate_code(&mut self, ast: &minipas_ast::Program) -> Result<String, String> {
        // Convert old Program to new Program format
        let enhanced_program = Program {
            name: ast.name.clone(),
            uses: ast.uses.iter().map(|u| UseClause {
                unit_name: u.clone(),
                alias: None,
                is_in_interface: false,
            }).collect(),
            block: Block {
                consts: ast.block.consts.iter().map(|c| Constant {
                    name: c.name.clone(),
                    constant_type: None,
                    value: Expression::Literal(Literal::Integer(0)), // TODO: Convert properly
                }).collect(),
                types: ast.block.types.iter().map(|t| TypeDefinition {
                    name: t.name.clone(),
                    type_definition: EnhancedType::Custom(t.name.clone()),
                }).collect(),
                vars: ast.block.vars.iter().map(|v| Variable {
                    name: v.name.clone(),
                    variable_type: EnhancedType::Custom("".to_string()), // TODO: Convert properly
                    initial_value: None,
                    is_absolute: false,
                    absolute_address: None,
                    is_external: false,
                    external_name: None,
                    is_public: false,
                    is_exported: false,
                }).collect(),
                procedures: ast.block.procedures.iter().map(|p| Procedure {
                    name: p.name.clone(),
                    parameters: p.params.iter().map(|param| Parameter {
                        name: param.name.clone(),
                        parameter_type: EnhancedType::Custom("".to_string()), // TODO: Convert properly
                        is_var: param.is_var,
                        is_const: param.is_const,
                        is_out: param.is_out,
                        default_value: None,
                        is_array_of_const: false,
                    }).collect(),
                    calling_convention: CallingConvention::Default,
                    is_forward: p.is_forward,
                    is_external: p.is_external,
                    external_name: p.external_name.clone(),
                    is_public: false,
                    is_exported: false,
                    is_inline: p.is_inline,
                    is_assembler: p.is_assembler,
                    body: None, // TODO: Convert properly
                }).collect(),
                functions: ast.block.functions.iter().map(|f| Function {
                    name: f.name.clone(),
                    parameters: f.params.iter().map(|param| Parameter {
                        name: param.name.clone(),
                        parameter_type: EnhancedType::Custom("".to_string()), // TODO: Convert properly
                        is_var: param.is_var,
                        is_const: param.is_const,
                        is_out: param.is_out,
                        default_value: None,
                        is_array_of_const: false,
                    }).collect(),
                    return_type: EnhancedType::Custom("".to_string()), // TODO: Convert properly
                    calling_convention: CallingConvention::Default,
                    is_forward: f.is_forward,
                    is_external: f.is_external,
                    external_name: f.external_name.clone(),
                    is_public: false,
                    is_exported: false,
                    is_inline: f.is_inline,
                    is_assembler: f.is_assembler,
                    body: None, // TODO: Convert properly
                }).collect(),
                statements: Vec::new(), // TODO: Convert properly
                labels: Vec::new(),
            },
        };
        
        self.generate_program(&enhanced_program)
    }
}

impl AssemblyOutput for EnhancedCodeGenerator {
    fn get_assembly(&self) -> String {
        self.get_assembly_code()
    }
    
    fn add_instruction(&mut self, instruction: String) {
        self.instructions.push(Instruction {
            opcode: instruction,
            operands: Vec::new(),
            comment: None,
        });
    }
    
    fn add_label(&mut self, name: String) {
        self.labels.insert(name.clone(), Label {
            name,
            is_global: false,
            is_exported: false,
            is_imported: false,
        });
    }
}

impl VariableManager for EnhancedCodeGenerator {
    fn allocate_variable(&mut self, name: &str, var_type: &str) -> Result<usize, String> {
        let type_info = TypeInfo {
            name: var_type.to_string(),
            size: 8, // Default size
            alignment: 8,
            is_signed: true,
            is_float: false,
            is_pointer: false,
            is_array: false,
            is_record: false,
            is_enum: false,
            is_set: false,
            is_file: false,
            is_procedure: false,
            is_function: false,
        };
        
        let variable_info = VariableInfo {
            name: name.to_string(),
            variable_type: type_info.clone(),
            location: VariableLocation::Stack(self.stack_offset),
            is_global: false,
            is_static: false,
            is_thread_local: false,
        };
        
        self.variables.insert(name.to_string(), variable_info);
        let offset = self.stack_offset;
        self.stack_offset += type_info.size;
        
        Ok(offset)
    }
    
    fn get_variable_offset(&mut self, name: &str) -> i32 {
        if let Some(variable) = self.variables.get(name) {
            match &variable.location {
                VariableLocation::Stack(offset) => *offset as i32,
                _ => 0,
            }
        } else {
            0
        }
    }
}

impl TargetArchitecture for EnhancedCodeGenerator {
    fn get_target(&self) -> String {
        format!("{:?}", self.target_architecture)
    }
    
    fn get_register_size(&self) -> usize {
        match self.target_architecture {
            TargetArchitecture::X86_64 => 8,
            TargetArchitecture::X86_32 => 4,
            TargetArchitecture::ARM64 => 8,
            TargetArchitecture::ARM32 => 4,
            TargetArchitecture::RiscV64 => 8,
            TargetArchitecture::RiscV32 => 4,
            _ => 8,
        }
    }
    
    fn get_pointer_size(&self) -> usize {
        self.get_register_size()
    }
}
