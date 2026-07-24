//! Tree-walking interpreter for Pascal programs
//!
//! Executes Pascal AST directly without compilation.
//! Uses modular structure with separate runtime, scoping, function, and builtin modules.

mod value;
pub use value::*;

mod runtime;
pub use runtime::*;

mod scoping;
pub use scoping::*;

mod functions;
pub use functions::*;

mod builtins;
pub use builtins::*;

use crate::ast::{Block, Expr, ForDirection, Literal, Program, Stmt, ClassDecl, Type, SimpleType};
use crate::advanced_types::{GenericTypeDefinition, OperatorRegistry, OperatorOverload, OverloadableOperator};
use anyhow::Result;
use std::collections::HashMap;

/// Callback invoked before a user procedure/function when debugging (name -> should break)
pub type DebugBreakpointCheck = Box<dyn FnMut(&str) -> bool>;

/// Callback invoked when a breakpoint is hit (can show REPL, inspect state)
pub type DebugBreakpointHandler = Box<dyn FnMut(&mut Interpreter)>;

/// Pascal interpreter using modular architecture
pub struct Interpreter {
    runtime: RuntimeEnvironment,
    scope_manager: ScopeManager,
    functions: FunctionRegistry,
    classes: HashMap<String, ClassDecl>,
    generic_types: HashMap<String, GenericTypeDefinition>,
    interface_registry: crate::interfaces::InterfaceRegistry,
    string_pool: crate::memory_pool::StringPool,
    builtins: BuiltinRegistry,
    ffi: crate::ffi::FfiRegistry,
    /// Heap-allocated slots referenced by `Value::Pointer(id)`.
    heap: HashMap<usize, Value>,
    /// Next free heap slot id.
    next_heap_id: usize,
    /// Registered operator overloads (keyed by operator + operand types).
    operator_registry: OperatorRegistry,
    pub debug_breakpoint_check: Option<DebugBreakpointCheck>,
    pub debug_breakpoint_handler: Option<DebugBreakpointHandler>,
}

impl Interpreter {
    /// Create a new interpreter
    pub fn new(verbose: bool) -> Self {
        Self {
            runtime: RuntimeEnvironment::new(verbose),
            scope_manager: ScopeManager::new(verbose),
            functions: FunctionRegistry::new(),
            classes: HashMap::new(),
            generic_types: HashMap::new(),
            interface_registry: crate::interfaces::InterfaceRegistry::new(),
            string_pool: crate::memory_pool::StringPool::new(),
            builtins: create_default_registry(),
            ffi: crate::ffi::create_default_ffi_registry(),
            heap: HashMap::new(),
            next_heap_id: 0,
            operator_registry: OperatorRegistry::new(),
            debug_breakpoint_check: None,
            debug_breakpoint_handler: None,
        }
    }

    /// Allocate a new heap slot initialized to `value`, returning its pointer id.
    fn heap_alloc(&mut self, value: Value) -> usize {
        let id = self.next_heap_id;
        self.next_heap_id += 1;
        self.heap.insert(id, value);
        id
    }

    /// Read a variable, supporting dotted record/object field access (e.g. `a.b.c`).
    fn eval_variable(&mut self, name: &str) -> Result<Value> {
        if let Some((base, rest)) = name.split_once('.') {
            let base_val = self.runtime.get_variable_value(base).ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", base))?;
            return Ok(Self::read_field_path(&base_val, rest));
        }
        if let Some(val) = self.runtime.get_variable_value(name) {
            Ok(val)
        } else {
            Err(anyhow::anyhow!("Undefined variable: {}", name))
        }
    }

    /// Walk a dotted field path under a record/object value.
    fn read_field_path(value: &Value, path: &str) -> Value {
        let mut current = value.clone();
        for part in path.split('.') {
            current = match &current {
                Value::Record { fields, .. } | Value::Object { fields, .. } => {
                    fields.get(&part.to_lowercase()).cloned().unwrap_or(Value::Nil)
                }
                _ => Value::Nil,
            };
        }
        current
    }

    /// Assign to a variable or a dotted record/object field path (e.g. `a.b := ...`).
    fn assign_variable(&mut self, name: &str, value: Value) -> Result<()> {
        if let Some((base, rest)) = name.split_once('.') {
            let mut base_val = self.runtime.get_variable_value(base).ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", base))?;
            Self::write_field_path(&mut base_val, rest, value);
            self.runtime.set_variable(base.to_string(), base_val);
            return Ok(());
        }
        self.runtime.set_variable(name.to_string(), value);
        Ok(())
    }

    fn write_field_path(root: &mut Value, path: &str, value: Value) {
        let mut segments: Vec<&str> = path.split('.').collect();
        fn drill(val: &mut Value, segs: &mut Vec<&str>, value: Value) {
            if segs.is_empty() {
                return;
            }
            let seg = segs.remove(0).to_lowercase();
            if segs.is_empty() {
                match val {
                    Value::Record { fields, .. } | Value::Object { fields, .. } => {
                        fields.insert(seg, value);
                    }
                    _ => {}
                }
                return;
            }
            match val {
                Value::Record { fields, .. } | Value::Object { fields, .. } => {
                    if let Some(child) = fields.get_mut(&seg) {
                        drill(child, segs, value);
                    }
                }
                _ => {}
            }
        }
        drill(root, &mut segments, value);
    }

    /// Get the current scope for testing purposes
    pub fn current_scope(&self) -> &Scope {
        self.runtime.current_scope()
    }

    /// Get a variable value for testing purposes
    pub fn get_variable_value(&self, name: &str) -> Option<Value> {
        self.runtime.get_variable_value(name)
    }

    /// Number of active scopes (for leak analysis)
    pub fn scope_count(&self) -> usize {
        self.runtime.scope_count()
    }

    /// All variables across all scopes
    pub fn all_scope_variables(&self) -> Vec<(String, Value)> {
        self.runtime.all_scope_variables()
    }

    /// Load uses clause units (simplified implementation)
    pub fn load_uses_clause(&mut self, uses: &[String]) -> Result<()> {
        for unit_name in uses {
            if self.runtime.is_verbose() {
                eprintln!("[interpreter] Loading unit: {}", unit_name);
            }
            // In a real implementation, this would load and parse the unit
            match unit_name.as_str() {
                "SysUtils" | "Classes" => {
                    // Skip these common units that don't exist in our test environment
                }
                _ => {
                    return Err(anyhow::anyhow!("Unit '{}' not found", unit_name));
                }
            }
        }
        Ok(())
    }

    /// Declare variables in a block
    pub fn declare_block_vars(&mut self, block: &Block) -> Result<()> {
        for var in &block.vars {
            let default_value = self.default_value_for_type(&var.variable_type)?;
            self.runtime.declare_variable(&var.name, default_value)?;
        }
        Ok(())
    }

    /// Create a default value for a given type
    fn default_value_for_type(&self, typ: &crate::ast::Type) -> Result<Value> {
        use crate::ast::{Type, SimpleType};
        Ok(match typ {
            Type::Simple(SimpleType::Integer) => Value::Integer(0),
            Type::Simple(SimpleType::Real) => Value::Real(0.0),
            Type::Simple(SimpleType::Boolean) => Value::Boolean(false),
            Type::Simple(SimpleType::String) => Value::String("".to_string()),
            Type::Simple(SimpleType::Char) => Value::Char('\0'),
            Type::Integer => Value::Integer(0),
            Type::Real => Value::Real(0.0),
            Type::Boolean => Value::Boolean(false),
            Type::Char => Value::Char('\0'),
            Type::String => Value::String("".to_string()),
            Type::WideString => Value::String("".to_string()),
            Type::Array { element_type, range, .. } => {
                let elem_default = self.default_value_for_type(element_type)?;
                if let Some((start, end)) = range {
                    let size = (end - start + 1).max(0) as usize;
                    Value::Array {
                        elements: vec![elem_default; size],
                        lower_bound: *start,
                    }
                } else {
                    Value::Array {
                        elements: vec![],
                        lower_bound: 0,
                    }
                }
            }
            Type::Record { fields, .. } => {
                let mut field_values = HashMap::new();
                for (name, field_type) in fields {
                    field_values.insert(name.clone(), self.default_value_for_type(field_type)?);
                }
                Value::Record { fields: field_values, variant_tag: None, type_name: None }
            }
            Type::Alias { name, target_type } => {
                // Stamp the declared type name onto records so operator overloads
                // and reflection can dispatch on the named type.
                let mut value = self.default_value_for_type(target_type)?;
                if let Value::Record { type_name, .. } = &mut value
                    && type_name.is_none() {
                        *type_name = Some(name.clone());
                    }
                value
            }
            Type::Enum { values } => {
                if let Some(first) = values.first() {
                    Value::Enum {
                        type_name: first.clone(),
                        ordinal: 0,
                    }
                } else {
                    Value::Nil
                }
            }
            Type::GenericInstance {
                base_type,
                type_arguments,
            } => {
                if let Some(def) = self.generic_types.get(base_type) {
                    let instantiated = def.instantiate(type_arguments)?;
                    self.default_value_for_type(&instantiated)?
                } else {
                    Value::Nil
                }
            }
            Type::Pointer(_) => Value::Nil,
            Type::Set { .. } => Value::Nil,
            Type::File { .. } => Value::Nil,
            _ => Value::Nil,
        })
    }

    /// Register type declarations (enum constants and generic types) in a block
    pub fn register_block_types(&mut self, block: &Block) -> Result<()> {
        for type_decl in &block.types {
            if let Type::Enum { values } = &type_decl.type_definition {
                let type_name = type_decl.name.clone();
                for (i, value_name) in values.iter().enumerate() {
                    self.runtime.set_variable(
                        value_name.clone(),
                        Value::Enum {
                            type_name: type_name.clone(),
                            ordinal: i as i64,
                        },
                    );
                }
            }

            if !type_decl.type_parameters.is_empty() {
                self.generic_types.insert(
                    type_decl.name.clone(),
                    GenericTypeDefinition::new(
                        type_decl.name.clone(),
                        type_decl.type_parameters.clone(),
                        type_decl.type_definition.clone(),
                    ),
                );
            }
        }
        Ok(())
    }

    /// Register classes in a block
    pub fn register_block_classes(&mut self, block: &Block) -> Result<()> {
        for class_decl in &block.classes {
            if self.classes.contains_key(&class_decl.name) {
                return Err(anyhow::anyhow!(
                    "Class '{}' already defined",
                    class_decl.name
                ));
            }
            if self.runtime.is_verbose() {
                eprintln!("[interpreter] Registering class: {}", class_decl.name);
            }
            self.classes
                .insert(class_decl.name.clone(), class_decl.clone());
        }

        for class_decl in &block.classes {
            for iface_name in &class_decl.interfaces {
                if self.interface_registry.get(iface_name).is_some()
                    && !self
                        .interface_registry
                        .class_implements(class_decl, iface_name, &self.classes)
                {
                    return Err(anyhow::anyhow!(
                        "Class '{}' does not implement interface '{}'",
                        class_decl.name,
                        iface_name
                    ));
                }
            }
        }
        Ok(())
    }

    /// Register a single class (validates interfaces when registry knows them)
    pub fn register_class(&mut self, class_decl: &ClassDecl) -> Result<()> {
        if self.classes.contains_key(&class_decl.name) {
            return Err(anyhow::anyhow!("Class '{}' already defined", class_decl.name));
        }

        if self.runtime.is_verbose() {
            eprintln!("[interpreter] Registering class: {}", class_decl.name);
        }

        self.classes
            .insert(class_decl.name.clone(), class_decl.clone());

        for iface_name in &class_decl.interfaces {
            if self.interface_registry.get(iface_name).is_some()
                && !self
                    .interface_registry
                    .class_implements(class_decl, iface_name, &self.classes)
            {
                return Err(anyhow::anyhow!(
                    "Class '{}' does not implement interface '{}'",
                    class_decl.name,
                    iface_name
                ));
            }
        }
        Ok(())
    }

    /// Register an interface declaration
    pub fn register_interface(&mut self, iface: crate::ast::InterfaceDecl) {
        self.interface_registry.register(iface);
    }

    /// Register a user-defined operator overload (the implementation is a function
    /// with the given name, already registered in the function table).
    pub fn register_operator_overload(&mut self, overload: OperatorOverload) {
        self.operator_registry.register(overload);
    }

    /// Map a runtime value to an AST `Type` used for overload keying.
    fn type_of_value(&self, v: &Value) -> Option<Type> {
        Some(match v {
            Value::Integer(_) => Type::Integer,
            Value::Real(_) => Type::Real,
            Value::Boolean(_) => Type::Boolean,
            Value::Char(_) => Type::Char,
            Value::String(_) => Type::String,
            Value::Record { type_name: Some(name), .. } => Type::Alias {
                name: name.clone(),
                target_type: Box::new(Type::Simple(SimpleType::Integer)),
            },
            Value::Object { class_name, .. } => Type::Alias {
                name: class_name.clone(),
                target_type: Box::new(Type::Simple(SimpleType::Integer)),
            },
            _ => return None,
        })
    }

    /// Try to resolve a binary op via a registered overload; returns None if no overload applies.
    fn try_operator_overload(&self, operator: &str, left: &Value, right: &Value) -> Option<String> {
        let op = OverloadableOperator::from_operator_str(operator)?;
        let lt = self.type_of_value(left)?;
        let rt = self.type_of_value(right)?;
        self.operator_registry
            .lookup(&op, &lt, Some(&rt))
            .map(|o| o.implementation.clone())
    }

    /// Register functions and procedures in a block
    pub fn register_block_functions(&mut self, block: &Block) -> Result<()> {
        FunctionConverter::register_from_block(
            &mut self.functions,
            &block.functions,
            &block.procedures,
        )
    }

    /// Execute a program
    pub fn run_program(&mut self, program: &Program) -> Result<()> {
        if self.runtime.is_verbose() {
            eprintln!("[interpreter] Running program '{}'", program.name);
        }

        // Load uses clause units
        self.load_uses_clause(&program.uses)?;

        // Register type declarations (enums)
        self.register_block_types(&program.block)?;

        // Declare variables
        self.declare_block_vars(&program.block)?;

        // Register classes
        self.register_block_classes(&program.block)?;

        // Register functions
        self.register_block_functions(&program.block)?;

        // Execute statements
        for stmt in &program.block.statements {
            self.execute_stmt(stmt)?;
        }

        Ok(())
    }

    /// Execute a statement
    pub fn execute_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Assignment { target, value } => {
                let val = self.eval_expr(value)?;
                match target {
                    Expr::Variable(name) => {
                        self.assign_variable(name, val)?;
                    },
                    Expr::FunctionCall { name, arguments } if name == "__index__" => {
                        if let Some((root_name, index_exprs)) = Self::collect_index_chain(target) {
                            let indices: Vec<i64> = index_exprs.iter()
                                .map(|e| self.eval_expr(e)?.as_integer())
                                .collect::<Result<_>>()?;
                            self.runtime.set_nested_element(root_name, &indices, val)?;
                        } else {
                            return Err(anyhow::anyhow!("Unsupported assignment target"));
                        }
                    },
                    Expr::Dereference { expression } => {
                        let ptr = self.eval_expr(expression)?;
                        match ptr {
                            Value::Pointer(id) => {
                                self.heap.insert(id, val);
                            },
                            Value::Nil => return Err(anyhow::anyhow!("dereference of nil pointer")),
                            other => return Err(anyhow::anyhow!("cannot dereference {:?}", other)),
                        }
                    },
                    _ => {
                        return Err(anyhow::anyhow!("Unsupported assignment target: {:?}", target));
                    }
                }
            },
            Stmt::ProcedureCall { name, arguments } => {
                let name_lower = name.to_lowercase();
                if name_lower == "exit" {
                    let val = if let Some(arg) = arguments.first() {
                        Some(self.eval_expr(arg)?)
                    } else {
                        None
                    };
                    return Err(anyhow::Error::new(EarlyReturn { value: val }));
                } else if name_lower == "inc" {
                    if let Some(first) = arguments.first() {
                        let var_name = match first {
                            Expr::Variable(n) => n.clone(),
                            _ => return Err(anyhow::anyhow!("inc requires a variable")),
                        };
                        let delta = if arguments.len() > 1 {
                            self.eval_expr(&arguments[1])?.as_integer()?
                        } else {
                            1
                        };
                        let current = self.runtime.get_variable_value(&var_name)
                            .ok_or_else(|| anyhow::anyhow!("inc: variable {} not found", var_name))?;
                        let new_val = match current {
                            Value::Integer(i) => Value::Integer(i + delta),
                            _ => return Err(anyhow::anyhow!("inc requires integer variable")),
                        };
                        self.runtime.set_variable(var_name, new_val);
                    }
                } else if name_lower == "dec" {
                    if let Some(first) = arguments.first() {
                        let var_name = match first {
                            Expr::Variable(n) => n.clone(),
                            _ => return Err(anyhow::anyhow!("dec requires a variable")),
                        };
                        let delta = if arguments.len() > 1 {
                            self.eval_expr(&arguments[1])?.as_integer()?
                        } else {
                            1
                        };
                        let current = self.runtime.get_variable_value(&var_name)
                            .ok_or_else(|| anyhow::anyhow!("dec: variable {} not found", var_name))?;
                        let new_val = match current {
                            Value::Integer(i) => Value::Integer(i - delta),
                            _ => return Err(anyhow::anyhow!("dec requires integer variable")),
                        };
                        self.runtime.set_variable(var_name, new_val);
                    }
                } else if name_lower == "new" {
                    // New(p): allocate a heap slot and bind its id to p.
                    match arguments.first() {
                        Some(Expr::Variable(var_name)) => {
                            let id = self.heap_alloc(Value::Nil);
                            self.runtime.set_variable(var_name.clone(), Value::Pointer(id));
                        }
                        _ => return Err(anyhow::anyhow!("new requires a pointer variable")),
                    }
                } else if name_lower == "dispose" {
                    // Dispose(p): release the heap slot and set p to nil.
                    if let Some(Expr::Variable(var_name)) = arguments.first() {
                        if let Some(Value::Pointer(id)) = self.runtime.get_variable_value(var_name) {
                            self.heap.remove(&id);
                        }
                        self.runtime.set_variable(var_name.clone(), Value::Nil);
                    } else {
                        return Err(anyhow::anyhow!("dispose requires a pointer variable"));
                    }
                } else if name_lower == "halt" {
                    return Err(anyhow::anyhow!("program halted"));
                } else if name_lower == "setlength" {
                    // setlength(var, n) — resize a string or array
                    if let Some(Expr::Variable(var_name)) = arguments.first() {
                        let new_len = self.eval_expr(&arguments[1])?.as_integer()?;
                        if new_len < 0 {
                            return Err(anyhow::anyhow!("setlength: negative length"));
                        }
                        match self.runtime.get_variable_value(var_name) {
                            Some(Value::String(s)) => {
                                self.runtime.set_variable(
                                    var_name.clone(),
                                    Value::String(s.chars().take(new_len as usize).collect()),
                                );
                            }
                            Some(Value::Array { elements, lower_bound, .. }) => {
                                let new_size = new_len as usize;
                                let mut new_elements = elements.clone();
                                new_elements.resize(new_size, Value::Nil);
                                self.runtime.set_variable(
                                    var_name.clone(),
                                    Value::Array { elements: new_elements, lower_bound },
                                );
                            }
                            _ => {
                                return Err(anyhow::anyhow!(
                                    "setlength: {} is not a string or array",
                                    var_name
                                ));
                            }
                        }
                    } else {
                        return Err(anyhow::anyhow!("setlength requires a variable"));
                    }
                } else {
                    let args: Vec<Value> = arguments.iter()
                        .map(|arg| self.eval_expr(arg))
                        .collect::<Result<_>>()?;

                    self.call_procedure(name, &args)?;
                }
            },
            Stmt::Block(block) => {
                self.scope_manager.enter_scope();
                for stmt in &block.statements {
                    self.execute_stmt(stmt)?;
                }
                self.scope_manager.exit_scope();
            },
            Stmt::If { condition, then_branch, else_branch } => {
                let cond_val = self.eval_expr(condition)?;
                if self.is_truthy(&cond_val) {
                    for stmt in then_branch {
                        self.execute_stmt(stmt)?;
                    }
                } else if let Some(else_branch) = else_branch {
                    for stmt in else_branch {
                        self.execute_stmt(stmt)?;
                    }
                }
            },
            Stmt::While { condition, body } => {
                loop {
                    let cond = self.eval_expr(condition)?;
                    if !self.is_truthy(&cond) {
                        break;
                    }
                    for stmt in body.iter() {
                        self.execute_stmt(stmt)?;
                    }
                }
            },
            Stmt::For { var_name, start, end, direction, body } => {
                let start_val = self.eval_expr(start)?;
                let end_val = self.eval_expr(end)?;
                
                let (start_num, end_num) = match (start_val, end_val) {
                    (Value::Integer(s), Value::Integer(e)) => (s, e),
                    _ => return Err(anyhow::anyhow!("For loop bounds must be integers")),
                };
                
                if matches!(direction, ForDirection::To) {
                    for i in start_num..=end_num {
                        self.runtime.set_variable(var_name.clone(), Value::Integer(i));
                        for stmt in body.clone() {
                            self.execute_stmt(&stmt)?;
                        }
                    }
                } else {
                    for i in (end_num..=start_num).rev() {
                        self.runtime.set_variable(var_name.clone(), Value::Integer(i));
                        for stmt in body {
                            self.execute_stmt(stmt)?;
                        }
                    }
                }
            },
            Stmt::Repeat { body, until_condition } => {
                loop {
                    for stmt in body {
                        self.execute_stmt(stmt)?;
                    }
                    let until_result = self.eval_expr(until_condition)?;
                    if self.is_truthy(&until_result) {
                        break;
                    }
                }
            },
            Stmt::Case { expression, branches, else_branch } => {
                let expr_val = self.eval_expr(expression)?;
                let mut matched = false;
                for branch in branches {
                    for val_expr in &branch.values {
                        let branch_val = self.eval_expr(val_expr)?;
                        if self.values_equal(&expr_val, &branch_val) {
                            for stmt in &branch.body {
                                self.execute_stmt(stmt)?;
                            }
                            matched = true;
                            break;
                        }
                    }
                    if matched {
                        break;
                    }
                }
                if !matched
                    && let Some(else_stmts) = else_branch {
                        for stmt in else_stmts {
                            self.execute_stmt(stmt)?;
                        }
                    }
            },
            Stmt::Try { try_block, except_clauses, finally_block } => {
                let result = (|| {
                    for stmt in try_block {
                        self.execute_stmt(stmt)?;
                    }
                    Ok(()) as Result<()>
                })();
                if let Err(e) = result {
                    let mut handled = false;
                    for clause in except_clauses {
                        if clause.exception_type.is_none() || 
                           e.to_string().to_lowercase().contains(&clause.exception_type.as_ref().unwrap().to_lowercase()) {
                            for stmt in &clause.body {
                                self.execute_stmt(stmt)?;
                            }
                            handled = true;
                            break;
                        }
                    }
                    if !handled {
                        return Err(e);
                    }
                }
                if let Some(finally_stmts) = finally_block {
                    for stmt in finally_stmts {
                        self.execute_stmt(stmt)?;
                    }
                }
            },
            Stmt::Raise { exception, message: _ } => {
                let exc_str = if let Some(exc) = exception {
                    format!("{:?}", self.eval_expr(exc)?)
                } else {
                    "Re-raise".to_string()
                };
                return Err(anyhow::anyhow!("Exception: {}", exc_str));
            },
            Stmt::With { variable, statements } => {
                let var_val = self.eval_expr(variable)?;
                if let Value::Record { fields, .. } | Value::Object { fields, .. } = var_val {
                    self.scope_manager.enter_scope();
                    for (k, v) in fields {
                        self.runtime.set_variable(k.clone(), v.clone());
                    }
                    for stmt in statements {
                        self.execute_stmt(stmt)?;
                    }
                    self.scope_manager.exit_scope();
                }
            },
            Stmt::Empty => {},
            Stmt::Goto { .. } | Stmt::Label { .. } => {
                return Err(anyhow::anyhow!("Goto/Label not yet supported"));
            },
        }
        
        Ok(())
    }

    /// Evaluate an expression
    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Literal(literal) => self.eval_literal(literal),
            Expr::Variable(name) => self.eval_variable(name),
            Expr::BinaryOp { left, operator, right } => {
                match operator.as_str() {
                    "and" => {
                        let left_val = self.eval_expr(left)?;
                        if !self.is_truthy(&left_val) {
                            Ok(Value::Boolean(false))
                        } else {
                            let right_val = self.eval_expr(right)?;
                            Ok(Value::Boolean(self.is_truthy(&right_val)))
                        }
                    },
                    "or" => {
                        let left_val = self.eval_expr(left)?;
                        if self.is_truthy(&left_val) {
                            Ok(Value::Boolean(true))
                        } else {
                            let right_val = self.eval_expr(right)?;
                            Ok(Value::Boolean(self.is_truthy(&right_val)))
                        }
                    },
                    "xor" => {
                        let left_val = self.eval_expr(left)?;
                        let right_val = self.eval_expr(right)?;
                        Ok(Value::Boolean(self.is_truthy(&left_val) ^ self.is_truthy(&right_val)))
                    },
                    _ => {
                        let left_val = self.eval_expr(left)?;
                        let right_val = self.eval_expr(right)?;
                        // Operator overloading for record/object types.
                        if let Some(impl_name) = self.try_operator_overload(operator, &left_val, &right_val) {
                            return self.call_function(&impl_name, &[left_val, right_val]);
                        }
                        match operator.as_str() {
                            "+" => self.add_values(&left_val, &right_val),
                            "-" => self.sub_values(&left_val, &right_val),
                            "*" => self.mul_values(&left_val, &right_val),
                            "/" => self.div_values(&left_val, &right_val),
                            "div" => self.int_div_values(&left_val, &right_val),
                            "mod" => self.mod_values(&left_val, &right_val),
                            "shl" => self.shl_values(&left_val, &right_val),
                            "shr" => self.shr_values(&left_val, &right_val),
                            "=" => Ok(Value::Boolean(self.values_equal(&left_val, &right_val))),
                            "<>" => Ok(Value::Boolean(!self.values_equal(&left_val, &right_val))),
                            "<" | "<=" | ">" | ">=" => self.compare_values(&left_val, &right_val, operator),
                            "in" => {
                                match (&left_val, &right_val) {
                                    (Value::Integer(n), Value::Set { elements }) => {
                                        Ok(Value::Boolean(elements.contains(n)))
                                    }
                                    (Value::Enum { ordinal, .. }, Value::Set { elements }) => {
                                        Ok(Value::Boolean(elements.contains(ordinal)))
                                    }
                                    _ => Err(anyhow::anyhow!("in requires ordinal value and set")),
                                }
                            }
                            _ => Err(anyhow::anyhow!("Unsupported operator: {}", operator)),
                        }
                    }
                }
            },
            Expr::UnaryOp { operator, operand } => {
                let right_val = self.eval_expr(operand)?;
                match operator.as_str() {
                    "-" => {
                        match right_val {
                            Value::Integer(i) => Ok(Value::Integer(-i)),
                            Value::Real(f) => Ok(Value::Real(-f)),
                            _ => Err(anyhow::anyhow!("Unary minus requires numeric operand")),
                        }
                    },
                    "+" => Ok(right_val), // Unary plus
                    "not" => Ok(Value::Boolean(!self.is_truthy(&right_val))),
                    _ => Err(anyhow::anyhow!("Unsupported unary operator: {}", operator)),
                }
            },
            Expr::Set { elements } => {
                let mut set = std::collections::HashSet::new();
                for e in elements {
                    let val = self.eval_expr(e)?;
                    set.insert(val.as_integer()?);
                }
                Ok(Value::Set { elements: set })
            },
            Expr::FunctionCall { name, arguments } => {
                // Inline pointer dereference-then-call is handled by __index__/dot logic elsewhere;
                // closures stored in a variable are callable directly.
                let args: Vec<Value> = arguments.iter()
                    .map(|arg| self.eval_expr(arg))
                    .collect::<Result<_>>()?;
                
                self.call_function(name, &args)
            },
            Expr::AddressOf { expression } => {
                let val = self.eval_expr(expression)?;
                let id = self.heap_alloc(val);
                Ok(Value::Pointer(id))
            },
            Expr::Dereference { expression } => {
                let ptr = self.eval_expr(expression)?;
                match ptr {
                    Value::Pointer(id) => self.heap.get(&id).cloned().ok_or_else(|| anyhow::anyhow!("dangling pointer")),
                    Value::Nil => Err(anyhow::anyhow!("dereference of nil pointer")),
                    other => Err(anyhow::anyhow!("cannot dereference non-pointer value {:?}", other)),
                }
            },
            Expr::TypeCast { target_type, expression } => self.eval_type_cast(target_type, expression),
            Expr::SizeOf { type_or_expression } => {
                // Best-effort: size in bytes of the evaluated value's kind.
                let val = self.eval_expr(type_or_expression)?;
                Ok(Value::Integer(Self::value_size(&val)))
            },
            Expr::Is { expression, type_name } => {
                let val = self.eval_expr(expression)?;
                let matches = match &val {
                    Value::Object { class_name, .. } => self.class_is_a(class_name, type_name),
                    _ => false,
                };
                Ok(Value::Boolean(matches))
            },
            Expr::As { expression, type_name } => {
                let val = self.eval_expr(expression)?;
                if matches!(&val, Value::Object { class_name, .. } if self.class_is_a(class_name, type_name)) {
                    Ok(val)
                } else {
                    Err(anyhow::anyhow!("Invalid type cast to {}", type_name))
                }
            },
            Expr::Inherited { .. } => Err(anyhow::anyhow!("inherited is only valid inside a method body")),
            Expr::Lambda { params, body, return_type, .. } => {
                // Capture the currently-visible variable environment.
                let captured = self.runtime.all_scope_variables();
                Ok(Value::Closure {
                    params: params.iter().map(|p| (p.name.clone(), p.is_var)).collect(),
                    body: (**body).clone(),
                    is_function: return_type.is_some(),
                    return_type_name: match return_type {
                        Some(Type::Integer) | Some(Type::Simple(SimpleType::Integer)) => "integer".to_string(),
                        Some(Type::Real) | Some(Type::Simple(SimpleType::Real)) => "real".to_string(),
                        Some(Type::Boolean) | Some(Type::Simple(SimpleType::Boolean)) => "boolean".to_string(),
                        Some(Type::Char) | Some(Type::Simple(SimpleType::Char)) => "char".to_string(),
                        Some(Type::String) | Some(Type::Simple(SimpleType::String)) => "string".to_string(),
                        _ => String::new(),
                    },
                    captured,
                })
            },
        }
    }

    /// Compute a rough byte size for a runtime value (for `SizeOf`).
    fn value_size(val: &Value) -> i64 {
        match val {
            Value::Integer(_) | Value::Pointer(_) | Value::Enum { .. } => 8,
            Value::Real(_) => 8,
            Value::Boolean(_) => 1,
            Value::Char(_) => 1,
            Value::String(s) => s.len() as i64,
            Value::Array { elements, .. } => elements.iter().map(Self::value_size).sum(),
            Value::Record { fields, .. } => fields.values().map(Self::value_size).sum(),
            Value::Set { elements } => elements.len() as i64,
            Value::Nil => 0,
            Value::Object { fields, .. } => fields.values().map(Self::value_size).sum::<i64>() + 8,
            Value::Closure { .. } => 0,
        }
    }

    /// Walk the class inheritance chain to test `is`/`as` conformance.
    fn class_is_a(&self, class_name: &str, target: &str) -> bool {
        if class_name.eq_ignore_ascii_case(target) {
            return true;
        }
        let mut current = class_name.to_lowercase();
        while let Some(class) = self.classes.get(&current) {
            match &class.parent {
                Some(parent) => {
                    if parent.eq_ignore_ascii_case(target) {
                        return true;
                    }
                    current = parent.to_lowercase();
                }
                None => return false,
            }
        }
        false
    }

    /// Evaluate a type cast expression.
    fn eval_type_cast(&mut self, target_type: &Type, expression: &Expr) -> Result<Value> {
        let val = self.eval_expr(expression)?;
        Ok(match target_type {
            Type::Integer | Type::Simple(SimpleType::Integer) => Value::Integer(val.as_integer()?),
            Type::Real | Type::Simple(SimpleType::Real) => Value::Real(val.as_real()?),
            Type::Boolean | Type::Simple(SimpleType::Boolean) => Value::Boolean(val.as_boolean()?),
            Type::Char | Type::Simple(SimpleType::Char) => match val {
                Value::Integer(i) => Value::Char(char::from_u32(i as u32).unwrap_or('\0')),
                Value::String(s) => Value::Char(s.chars().next().unwrap_or('\0')),
                other => other,
            },
            Type::String | Type::Simple(SimpleType::String) => match val {
                Value::Integer(i) => Value::String(i.to_string()),
                Value::Real(r) => Value::String(r.to_string()),
                Value::Char(c) => Value::String(c.to_string()),
                other => other,
            },
            _ => val,
        })
    }

    /// Evaluate a literal value
    pub fn eval_literal(&mut self, literal: &Literal) -> Result<Value> {
        match literal {
            Literal::Integer(i) => Ok(Value::Integer(*i)),
            Literal::Real(f) => Ok(Value::Real(*f)),
            Literal::String(s) => Ok(Value::String(self.string_pool.intern(s))),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::Char(c) => Ok(Value::Char(*c)),
            Literal::Nil => Ok(Value::Nil),
            Literal::WideString(_) => todo!("WideString literals not implemented"),
            Literal::Set(_) => todo!("Set literals not implemented"),
        }
    }

    /// Call a built-in or user function
    pub fn call_function(&mut self, name: &str, args: &[Value]) -> Result<Value> {
        // If a variable of this name holds a closure, invoke it directly.
        if let Some(Value::Closure { .. }) = self.runtime.get_variable_value(name) {
            return self.call_closure(name, args);
        }

        // Check built-in functions first
        if let Some((_, _, func)) = self.builtins.get_function(name) {
            return func(args);
        }

        if self.ffi.has(name) {
            return self.ffi.call(name, args);
        }
        
        // Check user-defined functions
        if let Some(user_func) = self.functions.get_function(name) {
            // Validate argument count
            if user_func.params().len() != args.len() {
                return Err(anyhow::anyhow!(
                    "Function '{}' expects {} arguments, got {}",
                    name,
                    user_func.params().len(),
                    args.len()
                ));
            }
            
            // Create new scope and push parameters
            self.scope_manager.enter_scope();
            self.runtime.enter_scope();
            for (param, arg) in user_func.params().iter().zip(args.iter()) {
                self.runtime.set_variable(param.0.clone(), arg.clone());
            }
            
            let is_function = user_func.is_function();
            let func_name_lower = name.to_lowercase();
            
            // For functions, declare a result variable with the function's name
            if is_function {
                let default = match user_func.return_type_name() {
                    "integer" => Value::Integer(0),
                    "boolean" => Value::Boolean(false),
                    "real" | "float" => Value::Real(0.0),
                    "char" => Value::Char('\0'),
                    "string" => Value::String("".to_string()),
                    _ => Value::Nil,
                };
                self.runtime.set_variable(func_name_lower.clone(), default);
            }
            
            // Execute function body
            let body_statements = user_func.body().statements.clone();
            let body_result = self.execute_block_stmts(&body_statements);
            
            // Read return value before exiting scope
            let return_value = if is_function {
                self.runtime.get_variable_value(&func_name_lower).unwrap_or(Value::Nil)
            } else {
                Value::Nil
            };
            
            self.runtime.exit_scope();
            self.scope_manager.exit_scope();
            
            match body_result {
                Ok(()) => {
                    if is_function {
                        Ok(return_value)
                    } else {
                        Ok(Value::Nil)
                    }
                },
                Err(e) => {
                    if let Some(early) = e.downcast_ref::<EarlyReturn>() {
                        if let Some(val) = &early.value {
                            Ok(val.clone())
                        } else {
                            Ok(return_value)
                        }
                    } else {
                        Err(e)
                    }
                }
            }
        } else {
            Err(anyhow::anyhow!("Undefined function: {}", name))
        }
    }

    /// Call a procedure (function that returns Nil)
    pub fn call_procedure(&mut self, name: &str, args: &[Value]) -> Result<()> {
        let result = self.call_function(name, args)?;
        if result != Value::Nil {
            return Err(anyhow::anyhow!("Procedure {} returned a value", name));
        }
        Ok(())
    }

    /// Invoke a closure stored in variable `name`.
    fn call_closure(&mut self, name: &str, args: &[Value]) -> Result<Value> {
        let closure = match self.runtime.get_variable_value(name) {
            Some(Value::Closure { .. }) => {
                
                self.runtime.get_variable_value(name).unwrap()
            }
            _ => return Err(anyhow::anyhow!("{} is not callable", name)),
        };
        let Value::Closure { params, body, is_function, return_type_name, captured } = closure else {
            return Err(anyhow::anyhow!("{} is not callable", name));
        };

        if params.len() != args.len() {
            return Err(anyhow::anyhow!(
                "Closure expects {} arguments, got {}", params.len(), args.len()
            ));
        }

        self.scope_manager.enter_scope();
        self.runtime.enter_scope();
        // Restore captured environment first so parameters shadow them.
        for (k, v) in &captured {
            self.runtime.set_variable(k.clone(), v.clone());
        }
        for ((pname, _), arg) in params.iter().zip(args.iter()) {
            self.runtime.set_variable(pname.clone(), arg.clone());
        }
        if is_function {
            let default = match return_type_name.as_str() {
                "integer" => Value::Integer(0),
                "boolean" => Value::Boolean(false),
                "real" | "float" => Value::Real(0.0),
                "char" => Value::Char('\0'),
                "string" => Value::String(String::new()),
                _ => Value::Nil,
            };
            // Pascal allows returning via either `Result` or the function/closure name.
            self.runtime.set_variable("result".to_string(), default.clone());
            self.runtime.set_variable(name.to_lowercase(), default);
        }

        let body_statements = body.statements.clone();
        let body_result = self.execute_block_stmts(&body_statements);
        let return_value = if is_function {
            self.runtime
                .get_variable_value("result")
                .or_else(|| self.runtime.get_variable_value(&name.to_lowercase()))
                .unwrap_or(Value::Nil)
        } else {
            Value::Nil
        };

        self.runtime.exit_scope();
        self.scope_manager.exit_scope();

        match body_result {
            Ok(()) => Ok(return_value),
            Err(e) => {
                if let Some(early) = e.downcast_ref::<EarlyReturn>() {
                    Ok(early.value.clone().unwrap_or(return_value))
                } else {
                    Err(e)
                }
            }
        }
    }

    /// Execute multiple statements (for blocks)
    pub fn execute_block_stmts(&mut self, statements: &[Stmt]) -> Result<()> {
        for stmt in statements {
            self.execute_stmt(stmt)?;
        }
        Ok(())
    }

    /// Helper methods for value operations
    fn add_values(&self, left: &Value, right: &Value) -> Result<Value> {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l + r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Real(*l as f64 + *r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Real(*l + *r as f64)),
            (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
            (Value::Set { elements: l }, Value::Set { elements: r }) => {
                let mut result = l.clone();
                result.extend(r);
                Ok(Value::Set { elements: result })
            }
            _ => Err(anyhow::anyhow!("Incompatible types for addition")),
        }
    }

    fn sub_values(&self, left: &Value, right: &Value) -> Result<Value> {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l - r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l - r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Real(*l as f64 - *r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Real(*l - *r as f64)),
            (Value::Set { elements: l }, Value::Set { elements: r }) => {
                let mut result = l.clone();
                result.retain(|e| !r.contains(e));
                Ok(Value::Set { elements: result })
            }
            _ => Err(anyhow::anyhow!("Incompatible types for subtraction")),
        }
    }

    fn mul_values(&self, left: &Value, right: &Value) -> Result<Value> {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
            (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l * r)),
            (Value::Integer(l), Value::Real(r)) => Ok(Value::Real(*l as f64 * *r)),
            (Value::Real(l), Value::Integer(r)) => Ok(Value::Real(*l * *r as f64)),
            (Value::Set { elements: l }, Value::Set { elements: r }) => {
                let result: std::collections::HashSet<i64> = l.iter().filter(|e| r.contains(e)).copied().collect();
                Ok(Value::Set { elements: result })
            }
            _ => Err(anyhow::anyhow!("Incompatible types for multiplication")),
        }
    }

    fn div_values(&self, left: &Value, right: &Value) -> Result<Value> {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => {
                if *r == 0 {
                    return Err(anyhow::anyhow!("Division by zero"));
                }
                Ok(Value::Integer(l / r))
            },
            (Value::Real(l), Value::Real(r)) => {
                if *r == 0.0 {
                    return Err(anyhow::anyhow!("Division by zero"));
                }
                Ok(Value::Real(l / r))
            },
            (Value::Integer(l), Value::Real(r)) => {
                if *r == 0.0 {
                    return Err(anyhow::anyhow!("Division by zero"));
                }
                Ok(Value::Real(*l as f64 / *r))
            },
            (Value::Real(l), Value::Integer(r)) => {
                if *r == 0 {
                    return Err(anyhow::anyhow!("Division by zero"));
                }
                Ok(Value::Real(*l / *r as f64))
            },
            _ => Err(anyhow::anyhow!("Incompatible types for division")),
        }
    }

    fn int_div_values(&self, left: &Value, right: &Value) -> Result<Value> {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => {
                if *r == 0 {
                    return Err(anyhow::anyhow!("Division by zero"));
                }
                Ok(Value::Integer(l / r))
            },
            _ => Err(anyhow::anyhow!("div requires integer operands")),
        }
    }

    fn mod_values(&self, left: &Value, right: &Value) -> Result<Value> {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => {
                if *r == 0 {
                    return Err(anyhow::anyhow!("Division by zero"));
                }
                Ok(Value::Integer(l % r))
            },
            _ => Err(anyhow::anyhow!("mod requires integer operands")),
        }
    }

    fn shl_values(&self, left: &Value, right: &Value) -> Result<Value> {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l << r)),
            _ => Err(anyhow::anyhow!("shl requires integer operands")),
        }
    }

    fn shr_values(&self, left: &Value, right: &Value) -> Result<Value> {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l >> r)),
            _ => Err(anyhow::anyhow!("shr requires integer operands")),
        }
    }

    fn compare_values(&self, left: &Value, right: &Value, operator: &str) -> Result<Value> {
        let result = match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => {
                match operator {
                    "<" => l < r,
                    "<=" => l <= r,
                    ">" => l > r,
                    ">=" => l >= r,
                    _ => false,
                }
            },
            (Value::Real(l), Value::Real(r)) => {
                match operator {
                    "<" => l < r,
                    "<=" => l <= r,
                    ">" => l > r,
                    ">=" => l >= r,
                    _ => false,
                }
            },
            (Value::String(l), Value::String(r)) => {
                match operator {
                    "<" => l < r,
                    "<=" => l <= r,
                    ">" => l > r,
                    ">=" => l >= r,
                    _ => false,
                }
            },
            (Value::Char(l), Value::Char(r)) => {
                match operator {
                    "<" => l < r,
                    "<=" => l <= r,
                    ">" => l > r,
                    ">=" => l >= r,
                    _ => false,
                }
            },
            (Value::Boolean(l), Value::Boolean(r)) => {
                match operator {
                    "<" => !*l && *r,
                    "<=" => !*l || *r,
                    ">" => *l && !*r,
                    ">=" => *l || !*r,
                    _ => false,
                }
            },
            (Value::Enum { ordinal: l, .. }, Value::Enum { ordinal: r, .. }) => {
                match operator {
                    "<" => l < r,
                    "<=" => l <= r,
                    ">" => l > r,
                    ">=" => l >= r,
                    _ => false,
                }
            },
            _ => return Err(anyhow::anyhow!("Incompatible types for comparison")),
        };
        Ok(Value::Boolean(result))
    }

    fn values_equal(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => l == r,
            (Value::Real(l), Value::Real(r)) => (l - r).abs() < f64::EPSILON,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Boolean(l), Value::Boolean(r)) => l == r,
            (Value::Char(l), Value::Char(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            (Value::Enum { ordinal: l, .. }, Value::Enum { ordinal: r, .. }) => l == r,
            (Value::Set { elements: l }, Value::Set { elements: r }) => l == r,
            _ => false,
        }
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Boolean(b) => *b,
            Value::Integer(i) => *i != 0,
            Value::Real(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array { elements: arr, .. } => !arr.is_empty(),
            Value::Nil => false,
            Value::Char(_) => true,
            Value::Object { .. } => true,
            Value::Record { .. } => true,
            Value::Enum { .. } => true,
            Value::Set { elements } => !elements.is_empty(),
            Value::Pointer(_) => true,
            Value::Closure { .. } => true,
        }
    }

    /// Enable debug mode
    pub fn set_debug_mode(&mut self, debug: bool) {
        self.runtime.set_verbose(debug);
        self.scope_manager.set_debug_mode(debug);
    }

    /// Set debug breakpoint callback
    pub fn set_debug_breakpoint_check(&mut self, callback: DebugBreakpointCheck) {
        self.debug_breakpoint_check = Some(callback);
    }

    /// Set debug breakpoint handler
    pub fn set_debug_breakpoint_handler(&mut self, handler: DebugBreakpointHandler) {
        self.debug_breakpoint_handler = Some(handler);
    }

    /// Collect index chain from nested __index__ expressions.
    /// Returns (root_variable_name, index_expressions).
    fn collect_index_chain(expr: &Expr) -> Option<(&str, Vec<&Expr>)> {
        match expr {
            Expr::Variable(name) => Some((name.as_str(), vec![])),
            Expr::FunctionCall { name, arguments } if name == "__index__" && arguments.len() == 2 => {
                let (root, mut indices) = Self::collect_index_chain(&arguments[0])?;
                indices.push(&arguments[1]);
                Some((root, indices))
            },
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, Statement, Expr, Literal, FieldVisibility};

    #[test]
    fn test_interpreter_creation() {
        let interp = Interpreter::new(false);
        assert!(!interp.runtime.is_verbose());
    }

    #[test]
    fn test_variable_declaration() {
        let mut interp = Interpreter::new(false);
        
        let block = Block {
            consts: vec![],
            types: vec![],
            vars: vec![crate::ast::VariableDecl {
                name: "x".to_string(),
                variable_type: crate::ast::Type::Simple(crate::ast::SimpleType::Integer),
                initial_value: None,
                visibility: FieldVisibility::Public,
                is_absolute: false,
                absolute_address: None,
            }],
            procedures: vec![],
            functions: vec![],
            classes: vec![],
            statements: vec![],
        };

        assert!(interp.declare_block_vars(&block).is_ok());
        assert!(interp.get_variable_value("x").is_some());
    }

    #[test]
    fn test_arithmetic_operations() {
        let mut interp = Interpreter::new(false);
        
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Literal::Integer(5))),
            operator: "+".to_string(),
            right: Box::new(Expr::Literal(Literal::Integer(3))),
        };

        let result = interp.eval_expr(&expr).unwrap();
        assert_eq!(result, Value::Integer(8));
    }

    #[test]
    fn test_variable_assignment() {
        let mut interp = Interpreter::new(false);
        
        let assignment = Statement::Assignment {
            target: Expr::Variable("x".to_string()),
            value: Expr::Literal(Literal::Integer(42)),
        };

        interp.execute_stmt(&assignment).unwrap();
        assert_eq!(interp.get_variable_value("x"), Some(Value::Integer(42)));
    }
}