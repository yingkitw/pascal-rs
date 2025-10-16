//! SIMD instruction support
//! 
//! Vectorization and SIMD code generation

use anyhow::Result;
use std::fmt::Write as FmtWrite;

/// SIMD register types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimdRegister {
    // SSE registers (128-bit)
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
    
    // AVX registers (256-bit)
    YMM0, YMM1, YMM2, YMM3, YMM4, YMM5, YMM6, YMM7,
    YMM8, YMM9, YMM10, YMM11, YMM12, YMM13, YMM14, YMM15,
    
    // AVX-512 registers (512-bit)
    ZMM0, ZMM1, ZMM2, ZMM3, ZMM4, ZMM5, ZMM6, ZMM7,
}

impl SimdRegister {
    /// Get register name
    pub fn name(&self) -> &str {
        match self {
            SimdRegister::XMM0 => "xmm0",
            SimdRegister::XMM1 => "xmm1",
            SimdRegister::XMM2 => "xmm2",
            SimdRegister::XMM3 => "xmm3",
            SimdRegister::XMM4 => "xmm4",
            SimdRegister::XMM5 => "xmm5",
            SimdRegister::XMM6 => "xmm6",
            SimdRegister::XMM7 => "xmm7",
            SimdRegister::YMM0 => "ymm0",
            SimdRegister::YMM1 => "ymm1",
            SimdRegister::ZMM0 => "zmm0",
            _ => "xmm0",
        }
    }
    
    /// Get register width in bits
    pub fn width(&self) -> usize {
        match self {
            SimdRegister::XMM0 | SimdRegister::XMM1 | SimdRegister::XMM2 | SimdRegister::XMM3 |
            SimdRegister::XMM4 | SimdRegister::XMM5 | SimdRegister::XMM6 | SimdRegister::XMM7 |
            SimdRegister::XMM8 | SimdRegister::XMM9 | SimdRegister::XMM10 | SimdRegister::XMM11 |
            SimdRegister::XMM12 | SimdRegister::XMM13 | SimdRegister::XMM14 | SimdRegister::XMM15 => 128,
            
            SimdRegister::YMM0 | SimdRegister::YMM1 | SimdRegister::YMM2 | SimdRegister::YMM3 |
            SimdRegister::YMM4 | SimdRegister::YMM5 | SimdRegister::YMM6 | SimdRegister::YMM7 |
            SimdRegister::YMM8 | SimdRegister::YMM9 | SimdRegister::YMM10 | SimdRegister::YMM11 |
            SimdRegister::YMM12 | SimdRegister::YMM13 | SimdRegister::YMM14 | SimdRegister::YMM15 => 256,
            
            SimdRegister::ZMM0 | SimdRegister::ZMM1 | SimdRegister::ZMM2 | SimdRegister::ZMM3 |
            SimdRegister::ZMM4 | SimdRegister::ZMM5 | SimdRegister::ZMM6 | SimdRegister::ZMM7 => 512,
        }
    }
}

/// SIMD operation type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimdOp {
    // Arithmetic
    AddPacked,
    SubPacked,
    MulPacked,
    DivPacked,
    
    // Logical
    AndPacked,
    OrPacked,
    XorPacked,
    
    // Comparison
    CmpEqPacked,
    CmpLtPacked,
    CmpGtPacked,
    
    // Data movement
    MovePacked,
    LoadPacked,
    StorePacked,
    
    // Shuffle
    Shuffle,
    Permute,
    Broadcast,
}

/// SIMD code generator
pub struct SimdCodeGen {
    output: String,
    target_features: SimdFeatures,
}

#[derive(Debug, Clone)]
pub struct SimdFeatures {
    pub sse: bool,
    pub sse2: bool,
    pub sse3: bool,
    pub sse4_1: bool,
    pub avx: bool,
    pub avx2: bool,
    pub avx512: bool,
}

impl SimdFeatures {
    /// Create with SSE2 support (baseline)
    pub fn baseline() -> Self {
        Self {
            sse: true,
            sse2: true,
            sse3: false,
            sse4_1: false,
            avx: false,
            avx2: false,
            avx512: false,
        }
    }
    
    /// Create with AVX2 support
    pub fn avx2() -> Self {
        Self {
            sse: true,
            sse2: true,
            sse3: true,
            sse4_1: true,
            avx: true,
            avx2: true,
            avx512: false,
        }
    }
}

impl SimdCodeGen {
    /// Create a new SIMD code generator
    pub fn new(features: SimdFeatures) -> Self {
        Self {
            output: String::new(),
            target_features: features,
        }
    }
    
    /// Generate SIMD instruction
    pub fn emit_simd(&mut self, op: SimdOp, dest: SimdRegister, src1: SimdRegister, src2: Option<SimdRegister>) -> Result<()> {
        let instr = match op {
            SimdOp::AddPacked => {
                if self.target_features.avx {
                    "vaddps"
                } else {
                    "addps"
                }
            }
            SimdOp::SubPacked => {
                if self.target_features.avx {
                    "vsubps"
                } else {
                    "subps"
                }
            }
            SimdOp::MulPacked => {
                if self.target_features.avx {
                    "vmulps"
                } else {
                    "mulps"
                }
            }
            SimdOp::DivPacked => {
                if self.target_features.avx {
                    "vdivps"
                } else {
                    "divps"
                }
            }
            SimdOp::MovePacked => {
                if self.target_features.avx {
                    "vmovaps"
                } else {
                    "movaps"
                }
            }
            SimdOp::LoadPacked => "movups",
            SimdOp::StorePacked => "movups",
            _ => "movaps",
        };
        
        if let Some(src2) = src2 {
            writeln!(&mut self.output, "    {} {}, {}, {}", 
                instr, dest.name(), src1.name(), src2.name())?;
        } else {
            writeln!(&mut self.output, "    {} {}, {}", 
                instr, dest.name(), src1.name())?;
        }
        
        Ok(())
    }
    
    /// Vectorize a loop
    pub fn vectorize_loop(&mut self, iterations: usize, element_size: usize) -> Result<String> {
        let vector_width = if self.target_features.avx2 { 256 } else { 128 };
        let elements_per_vector = vector_width / (element_size * 8);
        
        let mut code = String::new();
        writeln!(&mut code, "    # Vectorized loop ({} elements per iteration)", elements_per_vector)?;
        writeln!(&mut code, "    mov rcx, {}", iterations / elements_per_vector)?;
        writeln!(&mut code, ".Lvector_loop:")?;
        
        // Load vectors
        writeln!(&mut code, "    movups xmm0, [rsi]")?;
        writeln!(&mut code, "    movups xmm1, [rdi]")?;
        
        // Perform operation
        writeln!(&mut code, "    addps xmm0, xmm1")?;
        
        // Store result
        writeln!(&mut code, "    movups [rdx], xmm0")?;
        
        // Advance pointers
        writeln!(&mut code, "    add rsi, {}", elements_per_vector * element_size)?;
        writeln!(&mut code, "    add rdi, {}", elements_per_vector * element_size)?;
        writeln!(&mut code, "    add rdx, {}", elements_per_vector * element_size)?;
        
        writeln!(&mut code, "    dec rcx")?;
        writeln!(&mut code, "    jnz .Lvector_loop")?;
        
        // Handle remainder
        let remainder = iterations % elements_per_vector;
        if remainder > 0 {
            writeln!(&mut code, "    # Handle {} remaining elements", remainder)?;
        }
        
        Ok(code)
    }
    
    /// Get generated code
    pub fn get_code(&self) -> &str {
        &self.output
    }
}

/// Calling convention
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallingConvention {
    /// System V AMD64 ABI (Linux, macOS)
    SystemV,
    /// Microsoft x64 calling convention (Windows)
    Win64,
    /// Custom calling convention
    Custom,
}

impl CallingConvention {
    /// Get parameter registers
    pub fn param_registers(&self) -> Vec<&'static str> {
        match self {
            CallingConvention::SystemV => {
                vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
            }
            CallingConvention::Win64 => {
                vec!["rcx", "rdx", "r8", "r9"]
            }
            CallingConvention::Custom => {
                vec!["rdi", "rsi", "rdx", "rcx"]
            }
        }
    }
    
    /// Get return register
    pub fn return_register(&self) -> &'static str {
        "rax"
    }
    
    /// Get callee-saved registers
    pub fn callee_saved_registers(&self) -> Vec<&'static str> {
        match self {
            CallingConvention::SystemV => {
                vec!["rbx", "r12", "r13", "r14", "r15", "rbp"]
            }
            CallingConvention::Win64 => {
                vec!["rbx", "rbp", "rdi", "rsi", "r12", "r13", "r14", "r15"]
            }
            CallingConvention::Custom => {
                vec!["rbx", "r12", "r13", "r14", "r15"]
            }
        }
    }
    
    /// Check if stack alignment is required
    pub fn requires_stack_alignment(&self) -> bool {
        true // Both conventions require 16-byte alignment
    }
    
    /// Get stack alignment
    pub fn stack_alignment(&self) -> usize {
        16
    }
}

/// Function call generator with calling convention support
pub struct CallGenerator {
    convention: CallingConvention,
}

impl CallGenerator {
    /// Create a new call generator
    pub fn new(convention: CallingConvention) -> Self {
        Self { convention }
    }
    
    /// Generate function prologue
    pub fn generate_prologue(&self, stack_size: usize) -> String {
        let mut code = String::new();
        
        // Save frame pointer
        code.push_str("    push rbp\n");
        code.push_str("    mov rbp, rsp\n");
        
        // Align stack if needed
        let aligned_size = if self.convention.requires_stack_alignment() {
            (stack_size + self.convention.stack_alignment() - 1) 
                & !(self.convention.stack_alignment() - 1)
        } else {
            stack_size
        };
        
        if aligned_size > 0 {
            code.push_str(&format!("    sub rsp, {}\n", aligned_size));
        }
        
        // Save callee-saved registers
        for reg in self.convention.callee_saved_registers() {
            code.push_str(&format!("    push {}\n", reg));
        }
        
        code
    }
    
    /// Generate function epilogue
    pub fn generate_epilogue(&self) -> String {
        let mut code = String::new();
        
        // Restore callee-saved registers (in reverse order)
        for reg in self.convention.callee_saved_registers().iter().rev() {
            code.push_str(&format!("    pop {}\n", reg));
        }
        
        // Restore stack
        code.push_str("    mov rsp, rbp\n");
        code.push_str("    pop rbp\n");
        code.push_str("    ret\n");
        
        code
    }
    
    /// Generate function call
    pub fn generate_call(&self, func_name: &str, args: &[String]) -> String {
        let mut code = String::new();
        let param_regs = self.convention.param_registers();
        
        // Move arguments to registers
        for (i, arg) in args.iter().enumerate() {
            if i < param_regs.len() {
                code.push_str(&format!("    mov {}, {}\n", param_regs[i], arg));
            } else {
                // Push to stack for extra arguments
                code.push_str(&format!("    push {}\n", arg));
            }
        }
        
        // Align stack before call if needed
        if self.convention.requires_stack_alignment() {
            code.push_str("    and rsp, -16\n");
        }
        
        // Call function
        code.push_str(&format!("    call {}\n", func_name));
        
        // Clean up stack for extra arguments
        let stack_args = args.len().saturating_sub(param_regs.len());
        if stack_args > 0 {
            code.push_str(&format!("    add rsp, {}\n", stack_args * 8));
        }
        
        code
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_simd_codegen() {
        let mut codegen = SimdCodeGen::new(SimdFeatures::baseline());
        
        codegen.emit_simd(
            SimdOp::AddPacked,
            SimdRegister::XMM0,
            SimdRegister::XMM1,
            Some(SimdRegister::XMM2)
        ).unwrap();
        
        let code = codegen.get_code();
        assert!(code.contains("addps"));
    }
    
    #[test]
    fn test_calling_convention() {
        let sysv = CallingConvention::SystemV;
        let params = sysv.param_registers();
        
        assert_eq!(params[0], "rdi");
        assert_eq!(params[1], "rsi");
    }
    
    #[test]
    fn test_call_generator() {
        let generator = CallGenerator::new(CallingConvention::SystemV);
        let prologue = generator.generate_prologue(32);
        
        assert!(prologue.contains("push rbp"));
        assert!(prologue.contains("mov rbp, rsp"));
    }
}
