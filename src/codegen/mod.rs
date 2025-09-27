use crate::ast::*;
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::fmt::Write;

pub struct CodeGenerator {
    output: String,
    label_counter: u32,
    variables: HashMap<Vec<String>, VariableInfo>, // Maps scoped variable names to their info
    current_scope: Vec<String>, // Current scope path
    string_literals: HashMap<String, String>, // String literals and their labels
    current_function: Option<FunctionContext>,
}

#[derive(Debug)]
struct VariableInfo {
    offset: i32,    // Stack offset
    typ: Type,      // Variable type
    is_param: bool, // Is this a function parameter?
}

#[derive(Debug)]
struct FunctionContext {
    name: String,
    return_type: Option<Type>,
    param_size: i32,
    local_size: i32,
    label: String,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            label_counter: 0,
            variables: HashMap::new(),
            current_scope: Vec::new(),
            string_literals: HashMap::new(),
            current_function: None,
        }
    }
    fn enter_scope(&mut self, name: &str) {
        self.current_scope.push(name.to_string());
    }
    
    fn exit_scope(&mut self) -> Vec<String> {
        let mut result = Vec::new();
        if let Some(name) = self.current_scope.pop() {
            result.push(name);
            
            // Remove all variables in the current scope
            let current_scope_path = self.current_scope.join("::");
            let prefix = if current_scope_path.is_empty() {
                String::new()
            } else {
                format!("{}::", current_scope_path)
            };
            
            // Convert the prefix to a vector of strings for comparison
            let prefix_parts: Vec<String> = prefix
                .split("::")
                .filter(|s| !s.is_empty())
                .map(|s| s.to_string())
                .collect();
            
            self.variables.retain(|k, _| !k.starts_with(&prefix_parts[..]));
        }
        result
    }
    fn get_var_info(&self, name: &[String]) -> Option<&VariableInfo> {
        // Try to find the variable in the current scope or any parent scope
        for i in (0..=self.current_scope.len()).rev() {
            let mut full_path = self.current_scope[..i].to_vec();
            full_path.extend_from_slice(name);
            if let Some(info) = self.variables.get(&full_path) {
                return Some(info);
            }
        }
        None
    }
    
    fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        format!("_{}_{}", prefix, self.label_counter)
    }
    
    fn emit(&mut self, code: &str) {
        writeln!(&mut self.output, "    {}", code).unwrap();
    }
    
    fn emit_label(&mut self, label: &str) {
        writeln!(&mut self.output, "{}:", label).unwrap();
    }
    
    fn emit_comment(&mut self, comment: &str) {
        writeln!(&mut self.output, "    ; {}", comment).unwrap();
    }

    pub fn generate(&mut self, program: &Program) -> Result<String> {
        self.output.clear();
        
        // Emit header
        self.emit(".intel_syntax noprefix");
        self.emit(".section .text");
        
        // Generate code for the program
        self.generate_block(&program.block, Some("main"))?;
        
        // Generate string literals section if needed
        if !self.string_literals.is_empty() {
            self.emit("\n.section .rodata");
            // Collect string literals to avoid borrowing self while iterating
            let literals: Vec<(String, String)> = self.string_literals
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            for (label, value) in literals {
                self.emit(&format!("{}:", label));
                self.emit(&format!("    .asciz \"{}\"", value));
            }
        }
        
        Ok(self.output.clone())
    }
    
    fn generate_block(&mut self, block: &Block, entry_point: Option<&str>) -> Result<()> {
        // Emit entry point label if provided
        if let Some(label) = entry_point {
            self.emit(&format!("{}:", label));
        }
        
        // Enter a new scope
        self.enter_scope("");
        
        // Process const declarations
        for const_decl in &block.consts {
            // For now, we'll just store the value in the variables map
            // In a real implementation, we'd want to evaluate the constant expression
            self.variables.insert(
                vec![const_decl.name.clone()],
                VariableInfo {
                    offset: 0, // Constants don't have stack offsets
                    typ: Type::Integer, // Default type, should be inferred
                    is_param: false,
                },
            );
        }
        
        // Process type declarations
        for _type_decl in &block.types {
            // Store the type in the current scope
            // In a real implementation, we'd want to process the type definition
        }
        
        // Process variable declarations
        let mut local_size: i32 = 0;
        for var_decl in &block.vars {
            // Calculate size based on type
            let size = self.type_size(&var_decl.var_type) as i32;
            local_size += size;
            
            // Align to 8 bytes
            if size % 8 != 0 {
                local_size = ((local_size + 7) / 8) * 8;
            }
            
            // Store variable info
            self.variables.insert(
                vec![var_decl.name.clone()],
                VariableInfo {
                    offset: -local_size,
                    typ: var_decl.var_type.clone(),
                    is_param: false,
                },
            );
            
            // Generate initialization code if needed
            if let Some(init_expr) = &var_decl.initializer {
                self.generate_expression(init_expr)?;
                self.emit(&format!("    mov [rbp - {}], eax", local_size));
            }
        }
        
        // Generate code for procedures and functions
        for proc in &block.procedures {
            self.generate_procedure(proc)?;
        }
        
        // Generate function declarations (they will be defined later)
        for func in &block.functions {
            // Just declare the function, the definition will be handled by generate_function
            let label = format!("_{}:", func.name);
            self.emit(&label);
        }
        
        // Generate code for statements
        self.generate_statement(&block.statements)?;
        
        // If this is the main block, add the entry point
        if self.current_scope.is_empty() {
            // Add the entry point for the program
            self.emit("global _start");
            self.emit("section .text");
            self.emit("_start:");
            
            // Call the main function if it exists
            if block.functions.iter().any(|f| f.name == "main") {
                self.emit("    call _main");
            }
            
            // Generate code for statements
            self.generate_statement(&block.statements)?;
            
            // Set return value (0 by default for main)
            self.emit("    mov eax, 0");
            
            // Clean up and return
            self.emit("    leave");
            self.emit("    ret");
        }
        
        Ok(())
    }
    
    fn generate_procedure(&mut self, proc: &ProcedureDecl) -> Result<()> {
        self.enter_scope(&proc.name);
        
        // Generate procedure prologue
        let label = format!("_{}", proc.name);
        self.emit(&format!("{}:", label));
        self.emit("    push rbp");
        self.emit("    mov rbp, rsp");
        
        // Process parameters
        let mut param_offset = 16; // Skip saved rbp (8) and return address (8)
        for param in &proc.params {
            let size = self.type_size(&param.param_type);
            self.variables.insert(
                vec![param.name.clone()],
                VariableInfo {
                    offset: param_offset,
                    typ: param.param_type.clone(),
                    is_param: true,
                },
            );
            param_offset += 8; // All parameters are 8 bytes on the stack (64-bit ABI)
        }
        
        // Generate procedure body
        self.generate_block(&proc.block, None)?;
        
        // Generate epilogue
        self.emit("    leave");
        self.emit("    ret");
        
        // Exit scope
        self.exit_scope();
        
        Ok(())
    }
    
    fn generate_procedure_call(&mut self, name: &str, args: &[Expr]) -> Result<()> {
        // Push arguments in reverse order (right-to-left)
        for arg in args.iter().rev() {
            self.generate_expression(arg)?;
            self.emit("    push rax");
        }
        
        // Align the stack to 16 bytes before the call (System V ABI requirement)
        let stack_adjustment = ((args.len() % 2) * 8) as i32;
        if stack_adjustment != 0 {
            self.emit(&format!("    sub rsp, {}", stack_adjustment));
        }
        
        // Call the procedure
        self.emit(&format!("    call {}", name));
        
        // Clean up the stack
        if stack_adjustment != 0 {
            self.emit(&format!("    add rsp, {}", stack_adjustment));
        }
        
        // Remove arguments from the stack
        if !args.is_empty() {
            self.emit(&format!("    add rsp, {}", 8 * args.len()));
        }
        
        Ok(())
    }
    
    fn generate_function_call(&mut self, name: &str, args: &[Expr]) -> Result<()> {
        // Push arguments in reverse order (right-to-left)
        for arg in args.iter().rev() {
            self.generate_expression(arg)?;
            self.emit("    push rax");
        }
        
        // Align the stack to 16 bytes before the call (System V ABI requirement)
        let stack_adjustment = ((args.len() % 2) * 8) as i32;
        if stack_adjustment != 0 {
            self.emit(&format!("    sub rsp, {}", stack_adjustment));
        }
        
        // Call the function
        self.emit(&format!("    call {}", name));
        
        // Clean up the stack
        if stack_adjustment != 0 {
            self.emit(&format!("    add rsp, {}", stack_adjustment));
        }
        
        // Remove arguments from the stack
        if !args.is_empty() {
            self.emit(&format!("    add rsp, {}", 8 * args.len()));
        }
        
        // The return value is in rax (for integers) or xmm0 (for floats)
        Ok(())
    }
}

impl CodeGenerator {
    fn generate_statement(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            match stmt {
                Stmt::Block(block) => {
                    // Only enter a new scope if the block has its own declarations
                    if !block.consts.is_empty() || !block.types.is_empty() || !block.vars.is_empty() {
                        self.enter_scope("");
                        self.generate_block(block, None)?;
                        self.exit_scope();
                    } else {
                        // For begin blocks without declarations, just generate the statements
                        self.generate_statement(&block.statements)?;
                    }
                }
                _ => {
                    self.generate_single_statement(stmt)?;
                }
            }
        }
        Ok(())
    }
    
    fn generate_single_statement(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Assignment { target, value } => {
                self.generate_expression(value)?;
                
                match target {
                    Expr::Identifier(name_parts) => {
                        if let Some(var_info) = self.get_var_info(name_parts) {
                            match &var_info.typ {
                                Type::Integer | Type::Boolean | Type::Char => {
                                    self.emit(&format!("    mov [rbp - {}], eax", -var_info.offset));
                                }
                                Type::Real => {
                                    self.emit(&format!("    movq [rbp - {}], xmm0", -var_info.offset));
                                }
                                _ => {
                                    return Err(anyhow!("Unsupported type for assignment"));
                                }
                            }
                        } else {
                            return Err(anyhow!("Undefined variable: {:?}", name_parts));
                        }
                    }
                    Expr::Variable(name) => {
                        if let Some(var_info) = self.get_var_info(&[name.clone()]) {
                            match &var_info.typ {
                                Type::Integer | Type::Boolean | Type::Char => {
                                    self.emit(&format!("    mov [rbp - {}], eax", -var_info.offset));
                                }
                                Type::Real => {
                                    self.emit(&format!("    movq [rbp - {}], xmm0", -var_info.offset));
                                }
                                _ => {
                                    return Err(anyhow!("Unsupported type for assignment"));
                                }
                            }
                        } else {
                            return Err(anyhow!("Undefined variable: {}", name));
                        }
                    }
                    Expr::ArrayAccess { array, index } => {
                        // Generate code to compute array address and store the value
                        self.generate_expression(array)?;
                        self.emit("    push rax"); // Save array base
                        self.generate_expression(index)?;
                        self.emit("    pop rcx"); // Restore array base to rcx
                        
                        // TODO: Handle different element types and sizes
                        self.emit("    mov [rcx + rax*8], eax");
                    }
                    Expr::RecordAccess { record, field } => {
                        // Generate code to compute record field address and store the value
                        self.generate_expression(record)?;
                        // TODO: Calculate field offset based on record type
                        self.emit(&format!("    mov [rax + {}], eax", field));
                    }
                    _ => {
                        return Err(anyhow!("Invalid assignment target"));
                    }
                }
            }
            
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.generate_expression(condition)?;
                let else_label = self.new_label("else");
                let end_label = self.new_label("endif");
                
                self.emit("    test eax, eax");
                self.emit(&format!("    je {}", else_label));
                
                // Then branch
                for stmt in then_branch {
                    self.generate_statement(std::slice::from_ref(stmt))?;
                }
                
                if else_branch.is_some() {
                    self.emit(&format!("    jmp {}", end_label));
                }
                
                // Else branch (if exists)
                self.emit(&format!("{}:", else_label));
                if let Some(else_branch) = else_branch {
                    for stmt in else_branch {
                        self.generate_statement(std::slice::from_ref(stmt))?;
                    }
                }
                
                self.emit(&format!("{}:", end_label));
            }
            
            Stmt::While { condition, body } => self.generate_while_loop(condition, body)?,
            
            Stmt::Repeat { body, condition } => {
                let start_label = self.new_label("repeat");
                
                // Start of the loop
                self.emit(&format!("{}:", start_label));
                
                // Generate loop body
                for stmt in body {
                    self.generate_statement(std::slice::from_ref(stmt))?;
                }
                
                // Check condition (repeat until condition is true)
                self.generate_expression(condition)?;
                self.emit("    test eax, eax");
                self.emit(&format!("    jz {}", start_label));
            },
            
            Stmt::For {
                var_name,
                start,
                direction,
                end,
                body,
            } => self.generate_for_loop(var_name, start, direction, end, body)?,
            
            Stmt::Case {
                expr,
                arms,
                else_arm,
            } => self.generate_case_statement(expr, arms, else_arm.as_deref())?,
            
            Stmt::With { record, body } => self.generate_with_statement(record, body)?,
            
            Stmt::ProcedureCall { name, args } => self.generate_procedure_call(name, args)?,
            
            Stmt::Exit(expr) => {
                if let Some(expr) = expr {
                    self.generate_expression(expr)?;
                } else if let Some(func_ctx) = &self.current_function {
                    // Default return value based on function return type
                    if let Some(return_type) = &func_ctx.return_type {
                        match return_type {
                            Type::Integer => self.emit("    xor eax, eax"),
                            Type::Real => self.emit("    pxor xmm0, xmm0"),
                            Type::Boolean => self.emit("    xor al, al"),
                            _ => self.emit("    xor eax, eax"),
                        }
                    }
                }
                self.emit("    leave");
                self.emit("    ret");
            }
            
            Stmt::Break => self.emit("    jmp .break"),
            
            Stmt::Continue => self.emit("    jmp .continue"),
            
            Stmt::Goto(label) => self.emit(&format!("    jmp {}", label)),
            
            Stmt::LabeledStmt { label, stmt } => {
                self.emit(&format!("{}:", label));
                self.generate_statement(&[*(*stmt).clone()])?;
            }
            
            Stmt::RepeatUntil { body, condition } => {
                let loop_start = self.new_label("loop_start");
                let loop_end = self.new_label("loop_end");
                
                self.emit(&format!("{}:", loop_start));
                self.generate_statement(body)?;
                self.generate_expression(condition)?;
                self.emit(&format!("    cmp eax, 0"));
                self.emit(&format!("    je {}", loop_end));
                self.emit(&format!("    jmp {}", loop_start));
                self.emit(&format!("{}:", loop_end));
            }
            
            Stmt::Empty => {
                // Empty statement - do nothing
            }
            
            Stmt::Block(block) => {
                self.generate_block(block, None)?;
            }
        }
        
        Ok(())
    }

    /// Returns the size in bytes of a given type
    fn type_size(&self, typ: &Type) -> usize {
        match typ {
            Type::Integer => 4,
            Type::Real => 8,
            Type::Boolean => 1,
            Type::Char => 1,
            Type::String(_) => 8, // String is a pointer (size is ignored for now)
            Type::Array { element_type, range, .. } => {
                // Calculate the size based on the range if available, otherwise use a default
                let element_size = self.type_size(element_type);
                if let Some((start, end)) = range {
                    let count = (end - start + 1) as usize;
                    count * element_size
                } else {
                    // Default to 10 elements if range is not specified
                    10 * element_size
                }
            }
            Type::Record(fields) => {
                fields.values().map(|field_type| self.type_size(field_type)).sum()
            }
            Type::Pointer(_) => 8, // 64-bit pointer
            Type::Procedure => 0,   // Procedures don't have a size
            Type::Function(return_type) => self.type_size(return_type),
            Type::Custom(_) => 4,   // Default size for custom types
        }
    }
    
    fn generate_literal(&mut self, lit: &Literal) -> Result<()> {
        match lit {
            Literal::Integer(n) => {
                self.emit(&format!("    mov eax, {}", n));
            }
            Literal::Real(f) => {
                // In a real implementation, we'd need to handle floating-point constants properly
                // This is a simplified version that assumes the value is already in memory
                let label = format!("float_const_{}", *f as i64);
                self.emit(&format!("    movq xmm0, [{}]", label));
                
                // Add the constant to the data section
                self.string_literals.insert(label, format!("dq {}", f));
            }
            Literal::Boolean(b) => {
                let val = if *b { 1 } else { 0 };
                self.emit(&format!("    mov eax, {}", val));
            }
            Literal::Char(c) => {
                self.emit(&format!("    mov eax, {}", *c as u32));
            }
            Literal::String(s) => {
                let label = format!("str_lit_{}", s.as_ptr() as usize);
                self.emit(&format!("    lea rax, [{}]", label));
                
                // Add the string to the data section
                self.string_literals.insert(label, format!("db \"{}\", 0", s));
            }
            Literal::Array(_) => {
                return Err(anyhow!("Array literals not implemented yet"));
            }
            Literal::Record(_) => {
                return Err(anyhow!("Record literals not implemented yet"));
            }
            Literal::Nil => {
                self.emit("    xor eax, eax");
            }
        }
        Ok(())
    }
    
    fn generate_while_loop(&mut self, condition: &Expr, body: &[Stmt]) -> Result<()> {
        let start_label = self.new_label("while_start");
        let end_label = self.new_label("while_end");
        
        // Start of loop
        self.emit(&format!("{}:", start_label));
        
        // Check condition
        self.generate_expression(condition)?;
        self.emit("    test eax, eax");
        self.emit(&format!("    jz {}", end_label));
        
        // Generate loop body
        for stmt in body {
            self.generate_statement(std::slice::from_ref(stmt))?;
        }
        
        // Jump back to condition check
        self.emit(&format!("    jmp {}", start_label));
        
        // End of loop
        self.emit(&format!("{}:", end_label));
        
        Ok(())
    }
    
    fn generate_for_loop(&mut self, var: &str, start: &Expr, direction: &ForDirection, end: &Expr, body: &[Stmt]) -> Result<()> {
        let start_label = self.new_label("for_start");
        let end_label = self.new_label("for_end");
        
        // Get variable info first to avoid borrowing issues
        let var_offset = if let Some(var_info) = self.get_var_info(&[var.to_string()]) {
            -var_info.offset
        } else {
            return Err(anyhow!("Undefined loop variable: {}", var));
        };
        
        // Generate start value
        self.generate_expression(start)?;
        
        // Store in loop variable
        self.emit(&format!("    mov [rbp - {}], eax", var_offset));
        
        // Start of loop
        self.emit(&format!("{}:", start_label));
        
        // Generate end value
        self.generate_expression(end)?;
        
        // Move loop variable to edx for comparison
        self.emit(&format!("    mov edx, [rbp - {}]", var_offset));
        
        // Compare and conditionally jump based on direction
        match direction {
            ForDirection::To => {
                self.emit("    cmp edx, eax");
                self.emit(&format!("    jg {}", end_label));
            },
            ForDirection::DownTo => {
                self.emit("    cmp edx, eax");
                self.emit(&format!("    jl {}", end_label));
            },
        }
        
        // Generate loop body
        for stmt in body {
            self.generate_statement(std::slice::from_ref(stmt))?;
        }
        
        // Increment or decrement loop variable based on direction
        match direction {
            ForDirection::To => {
                self.emit(&format!("    inc dword [rbp - {}]", var_offset));
            },
            ForDirection::DownTo => {
                self.emit(&format!("    dec dword [rbp - {}]", var_offset));
            },
        }
        
        // Jump back to condition check
        self.emit(&format!("    jmp {}", start_label));
        
        // End of loop
        self.emit(&format!("{}:", end_label));
        
        Ok(())
    }
    
    fn generate_case_statement(
        &mut self,
        expr: &Expr,
        arms: &[CaseArm],
        else_arm: Option<&[Stmt]>,
    ) -> Result<()> {
        // Generate the expression to match against
        self.generate_expression(expr)?;
        
        // Save the expression value for comparison
        self.emit("    push rax");
        
        let end_label = self.new_label("case_end");
        let mut next_label = self.new_label("case_next");
        
        // Generate each case arm
        for arm in arms {
            // Compare against each constant in the arm
            for constant in &arm.constants {
                // Load the value to compare against
                self.emit("    pop rax");
                self.emit("    push rax");
                
                // Generate the constant value
                self.generate_expression(&Expr::Literal(constant.clone()))?;
                
                // Compare and jump if not equal
                self.emit("    cmp rax, [rsp]");
                self.emit(&format!("    jne {}", next_label));
                
                // If we get here, we have a match
                // Generate the statements for this case
                for stmt in &arm.stmts {
                    self.generate_statement(std::slice::from_ref(stmt))?;
                }
                
                // Jump to the end of the case statement
                self.emit(&format!("    jmp {}", end_label));
                
                // Next case
                self.emit(&format!("{}:", next_label));
                
                // Reset next_label for the next iteration
                next_label = self.new_label("case_next");
            }
        }
        
        // Handle else arm if it exists
        if let Some(else_stmts) = else_arm {
            for stmt in else_stmts {
                self.generate_statement(std::slice::from_ref(stmt))?;
            }
        }
        
        // Clean up the stack
        self.emit("    add rsp, 8");
        
        // End of case statement
        self.emit(&format!("{}:", end_label));
        
        Ok(())
    }
    
    fn generate_with_statement(&mut self, record: &Expr, body: &[Stmt]) -> Result<()> {
        // Generate code to evaluate the record expression
        self.generate_expression(record)?;
        
        // Save the record address
        self.emit("    push rax");
        
        // Generate the body of the with statement
        for stmt in body {
            self.generate_statement(std::slice::from_ref(stmt))?;
        }
        
        // Clean up the stack
        self.emit("    add rsp, 8");
        
        Ok(())
    }
    
    fn generate_expression(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Literal(lit) => self.generate_literal(lit)?,
            
            Expr::Identifier(name_parts) => {
                if let Some(var_info) = self.get_var_info(name_parts) {
                    match &var_info.typ {
                        Type::Integer | Type::Boolean | Type::Char => {
                            self.emit(&format!("    mov eax, [rbp - {}]", -var_info.offset));
                        }
                        Type::Real => {
                            self.emit(&format!("    movq xmm0, [rbp - {}]", -var_info.offset));
                        }
                        _ => {
                            // For other types, load the address
                            self.emit(&format!("    lea rax, [rbp - {}]", -var_info.offset));
                        }
                    }
                } else {
                    return Err(anyhow!("Undefined variable: {:?}", name_parts));
                }
            }
            
            Expr::BinaryOp { left, op, right } => {
                // First, evaluate the left operand
                self.generate_expression(left)?;
                
                // Save the result of the left operand
                self.emit("    push rax");
                
                // Evaluate the right operand
                self.generate_expression(right)?;
                
                // Restore the left operand into rdx
                self.emit("    pop rdx");
                
                match op {
                    BinaryOp::Add => {
                        self.emit("    add eax, edx");
                    }
                    BinaryOp::Subtract => {
                        self.emit("    sub edx, eax");
                        self.emit("    mov eax, edx");
                    }
                    BinaryOp::Multiply => {
                        self.emit("    imul eax, edx");
                    }
                    BinaryOp::Divide => {
                        self.emit("    xchg eax, edx");
                        self.emit("    cdq");
                        self.emit("    idiv edx");
                    }
                    BinaryOp::IntDivide => {
                        self.emit("    xchg eax, edx");
                        self.emit("    cdq");
                        self.emit("    idiv edx");
                    }
                    BinaryOp::Modulo => {
                        self.emit("    xchg eax, edx");
                        self.emit("    cdq");
                        self.emit("    idiv edx");
                        self.emit("    mov eax, edx");
                    }
                    BinaryOp::Equal => {
                        self.emit("    cmp eax, edx");
                        self.emit("    sete al");
                        self.emit("    movzx eax, al");
                    }
                    BinaryOp::NotEqual => {
                        self.emit("    cmp eax, edx");
                        self.emit("    setne al");
                        self.emit("    movzx eax, al");
                    }
                    BinaryOp::Less => {
                        self.emit("    cmp edx, eax");
                        self.emit("    setl al");
                        self.emit("    movzx eax, al");
                    }
                    BinaryOp::LessOrEqual => {
                        self.emit("    cmp edx, eax");
                        self.emit("    setle al");
                        self.emit("    movzx eax, al");
                    }
                    BinaryOp::Greater => {
                        self.emit("    cmp edx, eax");
                        self.emit("    setg al");
                        self.emit("    movzx eax, al");
                    }
                    BinaryOp::GreaterOrEqual => {
                        self.emit("    cmp edx, eax");
                        self.emit("    setge al");
                        self.emit("    movzx eax, al");
                    }
                    BinaryOp::And => {
                        self.emit("    and eax, edx");
                    }
                    BinaryOp::Or => {
                        self.emit("    or eax, edx");
                    }
                    BinaryOp::Xor => {
                        self.emit("    xor eax, edx");
                    }
                    BinaryOp::BitwiseAnd => {
                        self.emit("    and eax, edx");
                    }
                    BinaryOp::BitwiseOr => {
                        self.emit("    or eax, edx");
                    }
                    BinaryOp::BitwiseXor => {
                        self.emit("    xor eax, edx");
                    }
                    _ => {
                        return Err(anyhow!("Unsupported binary operator: {:?}", op));
                    }
                }
            }
            
            Expr::UnaryOp { op, expr } => {
                // First, evaluate the operand
                self.generate_expression(expr)?;
                
                match op {
                    UnaryOp::Minus => {
                        // For integers, use neg instruction
                        self.emit("    neg eax");
                    }
                    UnaryOp::Not => {
                        // For boolean NOT, just flip the lowest bit
                        self.emit("    xor eax, 1");
                    }
                    UnaryOp::Plus => {
                        // Unary plus is a no-op
                    }
                    _ => {
                        return Err(anyhow!("Unsupported unary operator: {:?}", op));
                    }
                }
            }
            
            Expr::FunctionCall { name, args } => {
                // Push arguments in reverse order (right-to-left)
                for arg in args.iter().rev() {
                    self.generate_expression(arg)?;
                    self.emit("    push rax");
                }
                
                // Call the function
                self.emit(&format!("    call {}", name));
                
                // Clean up the stack (remove arguments)
                if !args.is_empty() {
                    self.emit(&format!("    add rsp, {}", 8 * args.len()));
                }
                
                // The return value is in rax (for integers) or xmm0 (for floats)
            }
            
            Expr::ArrayAccess { array, index } => {
                // First, get the array address
                self.generate_expression(array)?;
                
                // Save the array address
                self.emit("    push rax");
                
                // Calculate the index
                self.generate_expression(index)?;
                
                // Restore array address into rdi
                self.emit("    pop rdi");
                
                // Calculate the element address (assuming 4-byte elements for now)
                self.emit("    shl eax, 2");  // Multiply index by 4 (size of int)
                self.emit("    add rax, rdi"); // Add to array base address
                
                // Load the element value
                self.emit("    mov eax, [rax]");
            }
            
            Expr::RecordAccess { record, field } => {
                // First, get the record address
                self.generate_expression(record)?;
                
                // For now, we'll assume all fields are 4 bytes and in order
                // In a real implementation, we'd need to look up the field offset in the type info
                self.emit(&format!("    add rax, {}", 0)); // Placeholder for field offset
                
                // Load the field value
                self.emit("    mov eax, [rax]");
            }
            
            Expr::AddressOf(expr) => {
                if let Expr::Identifier(name_parts) = expr.as_ref() {
                    if let Some(var_info) = self.get_var_info(name_parts) {
                        self.emit(&format!("    lea rax, [rbp - {}]", -var_info.offset));
                    } else {
                        return Err(anyhow!("Cannot take address of undefined variable: {:?}", name_parts));
                    }
                } else {
                    return Err(anyhow!("Can only take address of a variable"));
                }
            }
            
            Expr::Dereference(expr) => {
                self.generate_expression(expr)?;
                self.emit("    mov rax, [rax]");
            }
            
            Expr::TypeCast { target_type, expr } => {
                self.generate_expression(expr)?;
                // TODO: Implement proper type casting
                match target_type {
                    Type::Integer => { /* Already in integer format */ }
                    Type::Real => {
                        self.emit("    cvtsi2sd xmm0, eax");
                    }
                    _ => {
                        return Err(anyhow!("Unsupported type cast to {:?}", target_type));
                    }
                }
            }
            
            Expr::Variable(name) => {
                // Simple variable reference
                if let Some(var_info) = self.get_var_info(&[name.clone()]) {
                    match &var_info.typ {
                        Type::Integer | Type::Boolean | Type::Char => {
                            self.emit(&format!("    mov eax, [rbp - {}]", -var_info.offset));
                        }
                        Type::Real => {
                            self.emit(&format!("    movq xmm0, [rbp - {}]", -var_info.offset));
                        }
                        _ => {
                            return Err(anyhow!("Unsupported variable type for code generation"));
                        }
                    }
                } else {
                    return Err(anyhow!("Undefined variable: {}", name));
                }
            }
            
            Expr::Call { name, args } => {
                // Function call - simplified implementation
                for arg in args {
                    self.generate_expression(arg)?;
                    self.emit("    push rax");
                }
                self.emit(&format!("    call {}", name));
                // Clean up arguments from stack
                if !args.is_empty() {
                    self.emit(&format!("    add rsp, {}", args.len() * 8));
                }
            }
            
            Expr::Set(elements) => {
                // Set literal - simplified implementation
                // For now, just generate the first element
                if let Some(first) = elements.first() {
                    self.generate_expression(first)?;
                } else {
                    // Empty set
                    self.emit("    mov eax, 0");
                }
            }
        }
        
        Ok(())
    }

    fn get_variable_offset(&mut self, name: &str) -> i32 {
        if !self.variables.contains_key(&vec![name.to_string()]) {
            // This is a simplified version - in a real implementation, we'd track the stack offset
            // and manage the stack frame properly
            let offset = 8 * (self.variables.len() as i32 + 1);
            self.variables.insert(
                vec![name.to_string()], 
                VariableInfo {
                    offset,
                    typ: Type::Integer, // Default type
                    is_param: false,
                }
            );
            offset
        } else {
            self.variables[&vec![name.to_string()]].offset
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    #[test]
    fn test_codegen_simple_program() {
        // A simple program that assigns 42 to x and then adds 1 to it
        let program = Program {
            name: "Test".to_string(),
            uses: Vec::new(),
            block: Block {
                consts: Vec::new(),
                types: Vec::new(),
                vars: vec![VariableDecl {
                    name: "x".to_string(),
                    var_type: Type::Integer,
                    initializer: None,
                }],
                procedures: Vec::new(),
                functions: Vec::new(),
                statements: vec![
                    Stmt::Assignment {
                        target: Expr::Identifier(vec!["x".to_string()]),
                        value: Expr::Literal(Literal::Integer(42)),
                    },
                    Stmt::Assignment {
                        target: Expr::Identifier(vec!["x".to_string()]),
                        value: Expr::BinaryOp {
                            left: Box::new(Expr::Identifier(vec!["x".to_string()])),
                            op: BinaryOp::Add,
                            right: Box::new(Expr::Literal(Literal::Integer(1))),
                        },
                    },
                ],
            },
        };

        let mut codegen = CodeGenerator::new();
        let asm = codegen.generate(&program).unwrap();
        
        // Basic checks to ensure the assembly looks reasonable
        assert!(asm.contains("mov eax, 42"));
        assert!(asm.contains("add eax, edx")); // The actual generated instruction
        assert!(asm.contains("mov [rbp - 8], eax")); // Variable assignment
    }
}
