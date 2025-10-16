//! Main compiler implementation

use std::path::{Path, PathBuf};
use std::collections::{HashMap, HashSet};
use std::fs;
use minipas_ast::{Unit, Program};
use minipas_parser::Parser;
use minipas_module::{ModuleLoader, ModuleManager, Module};
use minipas_codegen::UnitCodeGenerator;
use crate::{CompileOptions, CompileResult, CompilerError, CompilerResult};

/// Main compiler struct
pub struct Compiler {
    /// Compilation options
    options: CompileOptions,
    
    /// Module loader
    loader: ModuleLoader,
    
    /// Module manager
    manager: ModuleManager,
    
    /// Compiled modules cache
    compiled: HashMap<String, Module>,
}

impl Compiler {
    /// Create a new compiler with the given options
    pub fn new(options: CompileOptions) -> Self {
        let mut loader = ModuleLoader::new();
        
        // Add search paths
        for path in &options.search_paths {
            loader.add_search_path(path);
        }
        
        Self {
            options,
            loader,
            manager: ModuleManager::new(),
            compiled: HashMap::new(),
        }
    }
    
    /// Compile a Pascal source file
    pub fn compile_file<P: AsRef<Path>>(&mut self, path: P) -> CompilerResult<CompileResult> {
        let path = path.as_ref();
        
        // Check if file exists
        if !path.exists() {
            return Err(CompilerError::FileNotFound(path.to_path_buf()));
        }
        
        // Read source file
        let source = fs::read_to_string(path)
            .map_err(|e| CompilerError::IoError {
                path: path.to_path_buf(),
                error: e,
            })?;
        
        // Parse the file
        let mut parser = Parser::new(&source);
        
        // Determine if it's a unit or program
        let is_unit = source.trim_start().to_lowercase().starts_with("unit");
        
        if is_unit {
            self.compile_unit(path, &source)
        } else {
            self.compile_program(path, &source)
        }
    }
    
    /// Compile a Pascal unit
    fn compile_unit(&mut self, path: &Path, source: &str) -> CompilerResult<CompileResult> {
        let mut parser = Parser::new(source);
        let unit = parser.parse_unit()
            .map_err(|e| CompilerError::ParseError {
                file: path.to_path_buf(),
                message: format!("{:?}", e),
            })?;
        
        let unit_name = unit.name.clone();
        let mut warnings = Vec::new();
        
        // Compile dependencies first
        for dep in &unit.uses {
            if !self.compiled.contains_key(dep) && !self.is_system_unit(dep) {
                self.compile_dependency(dep)?;
            }
        }
        
        // Create module using the constructor
        let module = Module::new(unit_name.clone(), path.to_path_buf(), unit.clone());
        
        // Add to manager
        self.manager.register_module(module.clone())?;
        self.compiled.insert(unit_name.clone(), module.clone());
        
        // Generate PPU if requested
        let ppu_path = if self.options.generate_ppu {
            let ppu_path = self.loader.save_to_ppu(&unit, Some(&self.options.output_dir))?;
            Some(ppu_path)
        } else {
            None
        };
        
        // Generate assembly code if requested
        let (assembly, asm_path) = if self.options.generate_asm {
            let mut codegen = UnitCodeGenerator::new();
            let asm_code = codegen.generate_unit(&unit)
                .map_err(|e| CompilerError::CodeGenError {
                    file: path.to_path_buf(),
                    message: format!("{}", e),
                })?;
            
            // Save assembly to file
            let asm_filename = format!("{}.s", unit.name.to_lowercase());
            let asm_path = self.options.output_dir.join(&asm_filename);
            fs::write(&asm_path, &asm_code)
                .map_err(|e| CompilerError::IoError {
                    path: asm_path.clone(),
                    error: e,
                })?;
            
            (Some(asm_code), Some(asm_path))
        } else {
            (None, None)
        };
        
        Ok(CompileResult {
            module,
            ppu_path,
            assembly,
            asm_path,
            warnings,
        })
    }
    
    /// Compile a Pascal program
    fn compile_program(&mut self, path: &Path, source: &str) -> CompilerResult<CompileResult> {
        let mut parser = Parser::new(source);
        let program = parser.parse_program()
            .map_err(|e| CompilerError::ParseError {
                file: path.to_path_buf(),
                message: format!("{:?}", e),
            })?;
        
        let program_name = program.name.clone();
        let mut warnings = Vec::new();
        
        // Compile dependencies first
        for dep in &program.uses {
            if !self.compiled.contains_key(dep) && !self.is_system_unit(dep) {
                self.compile_dependency(dep)?;
            }
        }
        
        // Convert program to a unit-like structure for the module system
        // Programs are treated as units with an empty interface
        let unit = Unit {
            name: program_name.clone(),
            uses: program.uses.clone(),
            interface: minipas_ast::UnitInterface {
                types: Vec::new(),
                constants: Vec::new(),
                variables: Vec::new(),
                procedures: Vec::new(),
                functions: Vec::new(),
                classes: Vec::new(),
                interfaces: Vec::new(),
            },
            implementation: minipas_ast::UnitImplementation {
                uses: Vec::new(),
                types: program.block.types.clone(),
                constants: program.block.consts.clone(),
                variables: program.block.vars.clone(),
                procedures: program.block.procedures.clone(),
                functions: program.block.functions.clone(),
                classes: Vec::new(),
                interfaces: Vec::new(),
                initialization: Some(program.block.statements.clone()),
                finalization: None,
            },
        };
        
        // Create module (clone unit for later use in codegen)
        let module = Module::new(program_name.clone(), path.to_path_buf(), unit.clone());
        
        // Add to manager
        self.manager.register_module(module.clone())?;
        self.compiled.insert(program_name.clone(), module.clone());
        
        // Generate assembly code if requested (for programs)
        let (assembly, asm_path) = if self.options.generate_asm {
            let mut codegen = UnitCodeGenerator::new();
            let asm_code = codegen.generate_unit(&unit)
                .map_err(|e| CompilerError::CodeGenError {
                    file: path.to_path_buf(),
                    message: format!("{}", e),
                })?;
            
            // Save assembly to file
            let asm_filename = format!("{}.s", program_name.to_lowercase());
            let asm_path = self.options.output_dir.join(&asm_filename);
            fs::write(&asm_path, &asm_code)
                .map_err(|e| CompilerError::IoError {
                    path: asm_path.clone(),
                    error: e,
                })?;
            
            (Some(asm_code), Some(asm_path))
        } else {
            (None, None)
        };
        
        Ok(CompileResult {
            module,
            ppu_path: None,
            assembly,
            asm_path,
            warnings,
        })
    }
    
    /// Compile a dependency unit
    fn compile_dependency(&mut self, unit_name: &str) -> CompilerResult<()> {
        // Try to load from PPU first if enabled
        if self.options.use_ppu && self.loader.is_ppu_up_to_date(unit_name) {
            match self.loader.load_from_ppu(unit_name) {
                Ok(unit) => {
                    let ppu_path = PathBuf::from(format!("{}.ppu", unit_name));
                    let module = Module::new(unit.name.clone(), ppu_path, unit);
                    self.manager.register_module(module.clone())?;
                    self.compiled.insert(unit_name.to_string(), module);
                    return Ok(());
                }
                Err(_) => {
                    // Fall through to compile from source
                }
            }
        }
        
        // Load and compile from source
        let (path, source) = self.loader.load_unit_source(unit_name)
            .map_err(|_| CompilerError::UnitNotFound(unit_name.to_string()))?;
        
        self.compile_unit(&path, &source)?;
        Ok(())
    }
    
    /// Check if a unit is a system unit (built-in)
    fn is_system_unit(&self, unit_name: &str) -> bool {
        matches!(unit_name.to_lowercase().as_str(), 
            "system" | "sysutils" | "classes" | "math" | "strings")
    }
    
    /// Get compilation order for all modules
    pub fn get_compilation_order(&mut self) -> CompilerResult<Vec<String>> {
        self.manager.compute_compile_order()
            .map_err(|e| CompilerError::from(e))
    }
    
    /// Get a compiled module by name
    pub fn get_module(&self, name: &str) -> Option<&Module> {
        self.compiled.get(name)
    }
    
    /// Get all compiled modules
    pub fn get_all_modules(&self) -> &HashMap<String, Module> {
        &self.compiled
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;
    
    #[test]
    fn test_compiler_creation() {
        let opts = CompileOptions::default();
        let compiler = Compiler::new(opts);
        assert_eq!(compiler.compiled.len(), 0);
    }
    
    #[test]
    fn test_is_system_unit() {
        let opts = CompileOptions::default();
        let compiler = Compiler::new(opts);
        
        assert!(compiler.is_system_unit("System"));
        assert!(compiler.is_system_unit("SysUtils"));
        assert!(compiler.is_system_unit("system"));
        assert!(!compiler.is_system_unit("MyUnit"));
    }
}
