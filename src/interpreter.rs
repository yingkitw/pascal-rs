//! Tree-walking interpreter for Pascal programs
//!
//! Executes Pascal AST directly without compilation.

use crate::ast::{Block, Expr, ForDirection, Literal, Program, Stmt};
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::io::{self, Write};

/// A Pascal exception that propagates through the interpreter
#[derive(Debug, Clone)]
pub struct PascalException {
    pub class_name: String,
    pub message: String,
}

impl std::fmt::Display for PascalException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.class_name, self.message)
    }
}

impl std::error::Error for PascalException {}

/// Early return signal (for exit from functions/procedures)
#[derive(Debug, Clone)]
pub struct EarlyReturn {
    pub value: Option<Value>,
}

impl std::fmt::Display for EarlyReturn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "EarlyReturn")
    }
}

impl std::error::Error for EarlyReturn {}

/// Runtime value
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Real(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Nil,
    Object {
        class_name: String,
        fields: HashMap<String, Value>,
    },
    Array {
        elements: Vec<Value>,
        lower_bound: i64,
    },
    Record {
        fields: HashMap<String, Value>,
    },
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Real(r) => write!(f, "{}", r),
            Value::Boolean(b) => write!(f, "{}", if *b { "TRUE" } else { "FALSE" }),
            Value::Char(c) => write!(f, "{}", c),
            Value::String(s) => write!(f, "{}", s),
            Value::Nil => write!(f, "nil"),
            Value::Object { class_name, .. } => write!(f, "<{}>", class_name),
            Value::Array { elements, .. } => {
                let items: Vec<std::string::String> = elements.iter().map(|v| format!("{}", v)).collect();
                write!(f, "({})", items.join(", "))
            }
            Value::Record { fields } => {
                let items: Vec<std::string::String> = fields.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "record({})", items.join("; "))
            }
        }
    }
}

impl Value {
    fn as_integer(&self) -> Result<i64> {
        match self {
            Value::Integer(n) => Ok(*n),
            Value::Real(r) => Ok(*r as i64),
            Value::Boolean(b) => Ok(if *b { 1 } else { 0 }),
            Value::Char(c) => Ok(*c as i64),
            _ => Err(anyhow!("Cannot convert {:?} to integer", self)),
        }
    }

    fn as_real(&self) -> Result<f64> {
        match self {
            Value::Integer(n) => Ok(*n as f64),
            Value::Real(r) => Ok(*r),
            _ => Err(anyhow!("Cannot convert {:?} to real", self)),
        }
    }

    fn as_boolean(&self) -> Result<bool> {
        match self {
            Value::Boolean(b) => Ok(*b),
            Value::Integer(n) => Ok(*n != 0),
            _ => Err(anyhow!("Cannot convert {:?} to boolean", self)),
        }
    }

    fn is_real(&self) -> bool {
        matches!(self, Value::Real(_))
    }
}

/// Variable scope
#[derive(Debug, Clone)]
pub struct Scope {
    variables: HashMap<String, Value>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    /// Get a variable value by name (for testing purposes)
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(&name.to_lowercase())
    }
}

/// User-defined function/procedure
#[derive(Debug, Clone)]
struct UserFunction {
    params: Vec<(String, bool)>, // (name, is_var)
    body: Block,
    is_function: bool,
    return_type_name: String,
}

/// Pascal interpreter
pub struct Interpreter {
    scopes: Vec<Scope>,
    functions: HashMap<String, UserFunction>,
    classes: HashMap<String, crate::ast::ClassDecl>,
    verbose: bool,
}

impl Interpreter {
    /// Create a new interpreter
    pub fn new(verbose: bool) -> Self {
        let mut global = Scope::new();
        // Pre-define some common constants
        global.variables.insert("maxint".to_string(), Value::Integer(i64::MAX));
        global.variables.insert("true".to_string(), Value::Boolean(true));
        global.variables.insert("false".to_string(), Value::Boolean(false));

        Self {
            scopes: vec![global],
            functions: HashMap::new(),
            classes: HashMap::new(),
            verbose,
        }
    }

    /// Get the current (innermost) scope for testing purposes
    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("Scope stack should never be empty")
    }

    /// Get a variable value for testing purposes
    pub fn get_variable_value(&self, name: &str) -> Option<Value> {
        let name_lower = name.to_lowercase();
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(&name_lower) {
                return Some(val.clone());
            }
        }
        None
    }

    /// Run a parsed program
    pub fn run_program(&mut self, program: &Program) -> Result<()> {
        if self.verbose {
            eprintln!("[interpreter] Running program '{}'", program.name);
        }

        // Load uses clause units
        self.load_uses_clause(&program.uses)?;

        // Register declared variables with default values
        self.declare_block_vars(&program.block)?;

        // Register classes
        self.register_block_classes(&program.block)?;

        // Register user-defined functions/procedures
        self.register_block_functions(&program.block)?;

        // Execute statements
        for stmt in &program.block.statements {
            self.execute_stmt(stmt)?;
        }

        Ok(())
    }

    /// Load units from a uses clause
    fn load_uses_clause(&mut self, uses: &[String]) -> Result<()> {
        for unit_name in uses {
            let unit_lower = unit_name.to_lowercase();
            // Skip built-in units (SysUtils, Classes, etc.)
            match unit_lower.as_str() {
                "system" | "sysutils" | "classes" | "types" | "math"
                | "strutils" | "dateutils" | "variants" | "crt" => {
                    if self.verbose {
                        eprintln!("[interpreter] Skipping built-in unit '{}'", unit_name);
                    }
                    continue;
                }
                _ => {}
            }

            // Try to find and load the unit file
            let file_name = format!("{}.pas", unit_lower);
            if let Ok(source) = std::fs::read_to_string(&file_name) {
                if self.verbose {
                    eprintln!("[interpreter] Loading unit '{}' from {}", unit_name, file_name);
                }
                self.load_unit_source(&source)?;
            } else {
                if self.verbose {
                    eprintln!("[interpreter] Unit '{}' not found ({}), skipping", unit_name, file_name);
                }
            }
        }
        Ok(())
    }

    /// Load a unit from source code — parse and import its declarations
    fn load_unit_source(&mut self, source: &str) -> Result<()> {
        let mut parser = crate::parser::Parser::new(source);
        // Try parsing as a unit first
        if let Ok(unit) = parser.parse_unit() {
            // Import interface declarations
            for var_decl in &unit.interface.variables {
                let default = Value::Integer(0);
                self.set_variable(&var_decl.name, default);
            }
            for func in &unit.interface.functions {
                let params: Vec<(String, bool)> = func
                    .parameters
                    .iter()
                    .map(|p| (p.name.clone(), p.is_var))
                    .collect();
                self.functions.insert(
                    func.name.to_lowercase(),
                    UserFunction {
                        params,
                        body: func.block.clone(),
                        is_function: true,
                        return_type_name: func.name.clone(),
                    },
                );
            }
            for proc in &unit.interface.procedures {
                let params: Vec<(String, bool)> = proc
                    .parameters
                    .iter()
                    .map(|p| (p.name.clone(), p.is_var))
                    .collect();
                self.functions.insert(
                    proc.name.to_lowercase(),
                    UserFunction {
                        params,
                        body: proc.block.clone(),
                        is_function: false,
                        return_type_name: String::new(),
                    },
                );
            }
            for class_decl in &unit.interface.classes {
                self.classes.insert(class_decl.name.to_lowercase(), class_decl.clone());
            }
            for const_decl in &unit.interface.constants {
                let val = self.literal_to_value(&const_decl.value);
                self.set_variable(&const_decl.name, val);
            }
            // Execute initialization section if present
            if let Some(ref init_stmts) = unit.implementation.initialization {
                for stmt in init_stmts {
                    self.execute_stmt(stmt)?;
                }
            }
            return Ok(());
        }

        // Try parsing as a program (simple include)
        let mut parser2 = crate::parser::Parser::new(source);
        if let Ok(prog) = parser2.parse_program() {
            self.declare_block_vars(&prog.block)?;
            self.register_block_functions(&prog.block)?;
            self.register_block_classes(&prog.block)?;
        }

        Ok(())
    }

    /// Declare variables from a block (always in current scope)
    fn declare_block_vars(&mut self, block: &Block) -> Result<()> {
        for var_decl in &block.vars {
            let default = if let Some(init) = &var_decl.initial_value {
                self.eval_expr(init)?
            } else {
                Value::Integer(0) // Default
            };
            self.set_local_variable(&var_decl.name, default);
        }
        for const_decl in &block.consts {
            let val = self.literal_to_value(&const_decl.value);
            self.set_local_variable(&const_decl.name, val);
        }
        Ok(())
    }

    /// Register classes from a block
    fn register_block_classes(&mut self, block: &Block) -> Result<()> {
        for class_decl in &block.classes {
            let name_lower = class_decl.name.to_lowercase();
            if self.verbose {
                eprintln!("[interpreter] Registering class '{}'", class_decl.name);
            }
            self.classes.insert(name_lower, class_decl.clone());
        }
        Ok(())
    }

    /// Register functions/procedures from a block
    fn register_block_functions(&mut self, block: &Block) -> Result<()> {
        for func in &block.functions {
            let params: Vec<(String, bool)> = func
                .parameters
                .iter()
                .map(|p| (p.name.clone(), p.is_var))
                .collect();
            self.functions.insert(
                func.name.to_lowercase(),
                UserFunction {
                    params,
                    body: func.block.clone(),
                    is_function: true,
                    return_type_name: func.name.clone(),
                },
            );
        }
        for proc in &block.procedures {
            let params: Vec<(String, bool)> = proc
                .parameters
                .iter()
                .map(|p| (p.name.clone(), p.is_var))
                .collect();
            self.functions.insert(
                proc.name.to_lowercase(),
                UserFunction {
                    params,
                    body: proc.block.clone(),
                    is_function: false,
                    return_type_name: String::new(),
                },
            );
        }
        Ok(())
    }

    /// Execute a statement
    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Assignment { target, value } => {
                let val = self.eval_expr(value)?;
                if self.verbose {
                    eprintln!("[interpreter] {} := {:?}", target, val);
                }
                self.set_variable(target, val);
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.eval_expr(condition)?.as_boolean()?;
                if cond {
                    for s in then_branch {
                        self.execute_stmt(s)?;
                    }
                } else if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        self.execute_stmt(s)?;
                    }
                }
            }

            Stmt::While { condition, body } => {
                loop {
                    let cond = self.eval_expr(condition)?.as_boolean()?;
                    if !cond {
                        break;
                    }
                    for s in body {
                        self.execute_stmt(s)?;
                    }
                }
            }

            Stmt::Repeat {
                body,
                until_condition,
            } => {
                loop {
                    for s in body {
                        self.execute_stmt(s)?;
                    }
                    let cond = self.eval_expr(until_condition)?.as_boolean()?;
                    if cond {
                        break;
                    }
                }
            }

            Stmt::For {
                var_name,
                start,
                end,
                body,
                direction,
            } => {
                let start_val = self.eval_expr(start)?.as_integer()?;
                let end_val = self.eval_expr(end)?.as_integer()?;

                match direction {
                    ForDirection::To => {
                        let mut i = start_val;
                        while i <= end_val {
                            self.set_variable(var_name, Value::Integer(i));
                            for s in body {
                                self.execute_stmt(s)?;
                            }
                            i += 1;
                        }
                    }
                    ForDirection::DownTo => {
                        let mut i = start_val;
                        while i >= end_val {
                            self.set_variable(var_name, Value::Integer(i));
                            for s in body {
                                self.execute_stmt(s)?;
                            }
                            i -= 1;
                        }
                    }
                }
            }

            Stmt::Case {
                expression,
                branches,
                else_branch,
            } => {
                let val = self.eval_expr(expression)?;
                let mut matched = false;
                for branch in branches {
                    for case_val in &branch.values {
                        let cv = self.eval_expr(case_val)?;
                        if val == cv {
                            matched = true;
                            for s in &branch.body {
                                self.execute_stmt(s)?;
                            }
                            break;
                        }
                    }
                    if matched {
                        break;
                    }
                }
                if !matched {
                    if let Some(else_stmts) = else_branch {
                        for s in else_stmts {
                            self.execute_stmt(s)?;
                        }
                    }
                }
            }

            Stmt::ProcedureCall { name, arguments } => {
                self.call_procedure(name, arguments)?;
            }

            Stmt::Block(block) => {
                self.scopes.push(Scope::new());
                self.declare_block_vars(block)?;
                for s in &block.statements {
                    self.execute_stmt(s)?;
                }
                self.scopes.pop();
            }

            Stmt::Try {
                try_block,
                except_clauses,
                finally_block,
            } => {
                self.execute_try(try_block, except_clauses, finally_block)?;
            }

            Stmt::Raise {
                exception,
                message: _,
            } => {
                self.execute_raise(exception)?;
            }

            Stmt::With {
                variable,
                statements,
            } => {
                // `with` makes record/object fields accessible as local variables
                // For simplicity: evaluate the variable, if it's an object/record,
                // push its fields as a new scope
                let val = self.eval_expr(variable)?;
                match val {
                    Value::Object { ref fields, .. } | Value::Record { ref fields } => {
                        let mut scope = Scope::new();
                        for (k, v) in fields {
                            scope.variables.insert(k.clone(), v.clone());
                        }
                        self.scopes.push(scope);
                        for s in statements {
                            self.execute_stmt(s)?;
                        }
                        // Write back modified fields
                        let modified = self.scopes.pop().unwrap();
                        if let Expr::Variable(var_name) = variable {
                            for (k, v) in &modified.variables {
                                let dotted = format!("{}.{}", var_name, k);
                                self.set_variable(&dotted, v.clone());
                            }
                        }
                    }
                    _ => {
                        // Just execute statements normally
                        for s in statements {
                            self.execute_stmt(s)?;
                        }
                    }
                }
            }

            Stmt::Empty => {}

            _ => {
                if self.verbose {
                    eprintln!("[interpreter] Unsupported statement: {:?}", stmt);
                }
            }
        }
        Ok(())
    }

    /// Execute try/except/finally
    fn execute_try(
        &mut self,
        try_block: &[Stmt],
        except_clauses: &[crate::ast::ExceptClause],
        finally_block: &Option<Vec<Stmt>>,
    ) -> Result<()> {
        // Execute try block
        let try_result = self.execute_block_stmts(try_block);

        match try_result {
            Ok(()) => {
                // No exception — run finally if present
                if let Some(finally_stmts) = finally_block {
                    self.execute_block_stmts(finally_stmts)?;
                }
            }
            Err(err) => {
                // Check if it's a PascalException
                let pascal_exc = err.downcast_ref::<PascalException>().cloned();

                if let Some(exc) = pascal_exc {
                    // Try to match except clauses
                    let mut handled = false;
                    for clause in except_clauses {
                        let matches = match &clause.exception_type {
                            Some(etype) => {
                                // Match by class name (case-insensitive)
                                etype.eq_ignore_ascii_case(&exc.class_name)
                                    || etype.eq_ignore_ascii_case("exception")
                            }
                            None => true, // Default handler catches all
                        };

                        if matches {
                            // Bind exception variable if specified
                            if let Some(var_name) = &clause.variable {
                                self.set_variable(var_name, Value::String(exc.message.clone()));
                            }
                            self.execute_block_stmts(&clause.body)?;
                            handled = true;
                            break;
                        }
                    }

                    // Run finally
                    if let Some(finally_stmts) = finally_block {
                        self.execute_block_stmts(finally_stmts)?;
                    }

                    // If not handled, re-raise
                    if !handled {
                        return Err(anyhow::Error::new(exc));
                    }
                } else {
                    // Not a Pascal exception — run finally then propagate
                    if let Some(finally_stmts) = finally_block {
                        let _ = self.execute_block_stmts(finally_stmts);
                    }
                    return Err(err);
                }
            }
        }

        Ok(())
    }

    /// Execute raise statement
    fn execute_raise(&mut self, exception: &Option<Expr>) -> Result<()> {
        match exception {
            Some(expr) => {
                // Evaluate the expression to get exception info
                // For now, support: raise Exception.Create('message')
                // or raise SomeVar
                let val = self.eval_expr(expr)?;
                let msg = match val {
                    Value::String(s) => s,
                    other => format!("{}", other),
                };
                Err(anyhow::Error::new(PascalException {
                    class_name: "Exception".to_string(),
                    message: msg,
                }))
            }
            None => {
                // Re-raise (bare raise;) — create generic exception
                Err(anyhow::Error::new(PascalException {
                    class_name: "Exception".to_string(),
                    message: "Re-raised exception".to_string(),
                }))
            }
        }
    }

    /// Evaluate a dotted function call: ClassName.Create() or obj.Method()
    fn eval_dotted_call(&mut self, name: &str, arguments: &[Expr]) -> Result<Value> {
        let name_lower = name.to_lowercase();
        let dot_pos = name_lower.find('.').ok_or_else(|| anyhow!("Expected dotted name"))?;
        let receiver = &name_lower[..dot_pos];
        let method = &name_lower[dot_pos + 1..];

        // Check if receiver is a class name (constructor call)
        if let Some(class_decl) = self.classes.get(receiver).cloned() {
            if method == "create" {
                return self.create_object(&class_decl, arguments);
            }
        }

        // Check if receiver is a variable holding an object (method call)
        let obj = self.get_variable(receiver)?;
        if let Value::Object { ref class_name, .. } = obj {
            let class_name = class_name.clone();
            // Use virtual dispatch: find method starting from runtime class
            if let Some((method_decl, _owner_class)) =
                self.find_method_in_hierarchy(&class_name, method)
            {
                if method_decl.is_destructor {
                    self.set_variable(receiver, Value::Nil);
                    return Ok(Value::Nil);
                }
                if let Some(ref block) = method_decl.block {
                    let mut arg_values = Vec::new();
                    for arg in arguments {
                        arg_values.push(self.eval_expr(arg)?);
                    }

                    self.scopes.push(Scope::new());
                    self.set_variable("self", obj.clone());
                    for (i, param) in method_decl.parameters.iter().enumerate() {
                        let val = arg_values.get(i).cloned().unwrap_or(Value::Integer(0));
                        self.set_variable(&param.name, val);
                    }
                    self.declare_block_vars(block)?;
                    let is_function = method_decl.return_type.is_some();
                    if is_function {
                        self.set_variable("result", Value::Integer(0));
                    }
                    for stmt in &block.statements {
                        self.execute_stmt(stmt)?;
                    }
                    let result = if is_function {
                        self.get_variable("result").unwrap_or(Value::Nil)
                    } else {
                        Value::Nil
                    };
                    self.scopes.pop();
                    return Ok(result);
                }
            }
            return Err(anyhow!(
                "Method '{}' not found in class '{}'",
                method,
                class_name
            ));
        }

        Err(anyhow!("Cannot call '{}'", name))
    }

    /// Create a new object instance from a class declaration
    fn create_object(&mut self, class_decl: &crate::ast::ClassDecl, arguments: &[Expr]) -> Result<Value> {
        let mut fields = HashMap::new();

        // Collect fields from parent class (single inheritance)
        if let Some(ref parent_name) = class_decl.parent {
            if let Some(parent_decl) = self.classes.get(&parent_name.to_lowercase()).cloned() {
                for field in &parent_decl.fields {
                    fields.insert(field.name.to_lowercase(), Value::Integer(0));
                }
            }
        }

        // Collect own fields
        for field in &class_decl.fields {
            fields.insert(field.name.to_lowercase(), Value::Integer(0));
        }

        let obj = Value::Object {
            class_name: class_decl.name.clone(),
            fields,
        };

        // Run constructor if it exists
        for method in &class_decl.methods {
            if method.is_constructor {
                if let Some(ref block) = method.block {
                    let mut arg_values = Vec::new();
                    for arg in arguments {
                        arg_values.push(self.eval_expr(arg)?);
                    }

                    self.scopes.push(Scope::new());
                    self.set_variable("self", obj.clone());
                    for (i, param) in method.parameters.iter().enumerate() {
                        let val = arg_values.get(i).cloned().unwrap_or(Value::Integer(0));
                        self.set_variable(&param.name, val);
                    }
                    self.declare_block_vars(block)?;
                    for stmt in &block.statements {
                        self.execute_stmt(stmt)?;
                    }
                    // Get the potentially modified Self
                    let result_obj = self.get_variable("self").unwrap_or(obj.clone());
                    self.scopes.pop();
                    return Ok(result_obj);
                }
            }
        }

        Ok(obj)
    }

    /// Check if a class is an instance of (or inherits from) a target type
    fn is_instance_of(&self, class_name: &str, target_type: &str) -> bool {
        let cn = class_name.to_lowercase();
        let tt = target_type.to_lowercase();
        if cn == tt {
            return true;
        }
        // Walk inheritance chain
        if let Some(class_decl) = self.classes.get(&cn) {
            if let Some(ref parent) = class_decl.parent {
                return self.is_instance_of(parent, target_type);
            }
        }
        false
    }

    /// Find a method in a class hierarchy, respecting virtual/override dispatch
    /// Starts from the actual runtime class and walks up to find the method
    fn find_method_in_hierarchy(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Option<(crate::ast::MethodDecl, String)> {
        let cn = class_name.to_lowercase();
        let mn = method_name.to_lowercase();

        if let Some(class_decl) = self.classes.get(&cn) {
            // Check own methods
            for m in &class_decl.methods {
                if m.name.to_lowercase() == mn {
                    return Some((m.clone(), class_decl.name.clone()));
                }
            }
            // Walk up to parent
            if let Some(ref parent) = class_decl.parent {
                return self.find_method_in_hierarchy(parent, method_name);
            }
        }
        None
    }

    /// Resolve a property read — check if there's a getter
    #[allow(dead_code)]
    fn resolve_property_read(
        &mut self,
        class_name: &str,
        prop_name: &str,
        obj: &Value,
    ) -> Option<Result<Value>> {
        let cn = class_name.to_lowercase();
        let pn = prop_name.to_lowercase();

        if let Some(class_decl) = self.classes.get(&cn).cloned() {
            for prop in &class_decl.properties {
                if prop.name.to_lowercase() == pn {
                    match &prop.read_specifier {
                        Some(crate::ast::MethodSpecifier::Field(field_name)) => {
                            // Direct field read
                            if let Value::Object { fields, .. } = obj {
                                return Some(Ok(fields
                                    .get(&field_name.to_lowercase())
                                    .cloned()
                                    .unwrap_or(Value::Integer(0))));
                            }
                        }
                        Some(crate::ast::MethodSpecifier::Method(getter_name)) => {
                            // Call getter method
                            let dotted = format!("{}.{}", cn, getter_name);
                            return Some(self.eval_dotted_call(&dotted, &[]));
                        }
                        None => {}
                    }
                }
            }
            // Check parent
            if let Some(ref parent) = class_decl.parent {
                return self.resolve_property_read(parent, prop_name, obj);
            }
        }
        None
    }

    /// Resolve a property write — check if there's a setter
    #[allow(dead_code)]
    fn resolve_property_write(
        &mut self,
        class_name: &str,
        prop_name: &str,
        obj_name: &str,
        value: Value,
    ) -> Option<Result<()>> {
        let cn = class_name.to_lowercase();
        let pn = prop_name.to_lowercase();

        if let Some(class_decl) = self.classes.get(&cn).cloned() {
            for prop in &class_decl.properties {
                if prop.name.to_lowercase() == pn {
                    match &prop.write_specifier {
                        Some(crate::ast::MethodSpecifier::Field(field_name)) => {
                            // Direct field write
                            let dotted = format!("{}.{}", obj_name, field_name);
                            self.set_variable(&dotted, value);
                            return Some(Ok(()));
                        }
                        Some(crate::ast::MethodSpecifier::Method(_setter_name)) => {
                            // Call setter method — we'd need to pass value as arg
                            // For now, store as field
                            let dotted = format!("{}.{}", obj_name, prop_name);
                            self.set_variable(&dotted, value);
                            return Some(Ok(()));
                        }
                        None => {}
                    }
                }
            }
            if let Some(ref parent) = class_decl.parent {
                return self.resolve_property_write(parent, prop_name, obj_name, value);
            }
        }
        None
    }

    /// Execute a list of statements (helper for try/except/finally)
    fn execute_block_stmts(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            self.execute_stmt(stmt)?;
        }
        Ok(())
    }

    /// Call a built-in or user-defined procedure
    fn call_procedure(&mut self, name: &str, arguments: &[Expr]) -> Result<()> {
        let name_lower = name.to_lowercase();
        match name_lower.as_str() {
            "write" => {
                for arg in arguments {
                    let val = self.eval_expr(arg)?;
                    print!("{}", val);
                }
                io::stdout().flush()?;
            }
            "writeln" => {
                for arg in arguments {
                    let val = self.eval_expr(arg)?;
                    print!("{}", val);
                }
                println!();
            }
            "readln" | "read" => {
                // Read a line from stdin and assign to variable arguments
                let mut input = String::new();
                io::stdin().read_line(&mut input)?;
                let input = input.trim().to_string();
                if let Some(first_arg) = arguments.first() {
                    if let Expr::Variable(var_name) = first_arg {
                        // Try to parse as integer, then real, then keep as string
                        let val = if let Ok(n) = input.parse::<i64>() {
                            Value::Integer(n)
                        } else if let Ok(r) = input.parse::<f64>() {
                            Value::Real(r)
                        } else {
                            Value::String(input)
                        };
                        self.set_variable(var_name, val);
                    }
                }
            }
            "inc" => {
                if let Some(Expr::Variable(var_name)) = arguments.first() {
                    let current = self.get_variable(var_name)?.as_integer()?;
                    let amount = if arguments.len() > 1 {
                        self.eval_expr(&arguments[1])?.as_integer()?
                    } else {
                        1
                    };
                    self.set_variable(var_name, Value::Integer(current + amount));
                }
            }
            "dec" => {
                if let Some(Expr::Variable(var_name)) = arguments.first() {
                    let current = self.get_variable(var_name)?.as_integer()?;
                    let amount = if arguments.len() > 1 {
                        self.eval_expr(&arguments[1])?.as_integer()?
                    } else {
                        1
                    };
                    self.set_variable(var_name, Value::Integer(current - amount));
                }
            }
            "halt" => {
                let code = if let Some(arg) = arguments.first() {
                    self.eval_expr(arg)?.as_integer()? as i32
                } else {
                    0
                };
                std::process::exit(code);
            }
            "exit" => {
                // Exit from current function/procedure
                // If there's an argument, it sets the return value
                let val = if let Some(arg) = arguments.first() {
                    Some(self.eval_expr(arg)?)
                } else {
                    None
                };
                return Err(anyhow::Error::new(EarlyReturn { value: val }));
            }
            "setlength" => {
                // SetLength(arr, newlen)
                if let Some(Expr::Variable(var_name)) = arguments.first() {
                    let new_len = self.eval_expr(&arguments[1])?.as_integer()? as usize;
                    let current = self.get_variable(var_name).ok();
                    match current {
                        Some(Value::Array { mut elements, lower_bound }) => {
                            elements.resize(new_len, Value::Integer(0));
                            self.set_variable(var_name, Value::Array { elements, lower_bound });
                        }
                        Some(Value::String(mut s)) => {
                            s.truncate(new_len);
                            while s.len() < new_len {
                                s.push(' ');
                            }
                            self.set_variable(var_name, Value::String(s));
                        }
                        _ => {
                            // Create new array
                            let elements = vec![Value::Integer(0); new_len];
                            self.set_variable(var_name, Value::Array { elements, lower_bound: 0 });
                        }
                    }
                }
            }
            _ => {
                // Try dotted procedure calls: obj.Method()
                if name_lower.contains('.') {
                    self.eval_dotted_call(name, arguments)?;
                    return Ok(());
                }
                // Try user-defined function/procedure
                if let Some(user_func) = self.functions.get(&name_lower).cloned() {
                    self.call_user_function(&user_func, arguments)?;
                } else if self.verbose {
                    eprintln!("[interpreter] Unknown procedure: {}", name);
                }
            }
        }
        Ok(())
    }

    /// Call a user-defined function/procedure
    fn call_user_function(&mut self, func: &UserFunction, arguments: &[Expr]) -> Result<Value> {
        // Evaluate arguments
        let mut arg_values = Vec::new();
        for arg in arguments {
            arg_values.push(self.eval_expr(arg)?);
        }

        // Push new scope
        self.scopes.push(Scope::new());

        // Bind parameters (local to this scope — must not clobber parent)
        for (i, (param_name, _is_var)) in func.params.iter().enumerate() {
            let val = arg_values.get(i).cloned().unwrap_or(Value::Integer(0));
            self.set_local_variable(param_name, val);
        }

        // Declare local variables
        self.declare_block_vars(&func.body)?;

        // Register nested functions/procedures
        self.register_block_functions(&func.body)?;
        self.register_block_classes(&func.body)?;

        // If it's a function, initialize the result variable (local to this scope)
        if func.is_function {
            self.set_local_variable(&func.return_type_name, Value::Integer(0));
            self.set_local_variable("result", Value::Integer(0));
        }

        // Execute body — handle EarlyReturn from exit()
        let exec_result = (|| -> Result<()> {
            for stmt in &func.body.statements {
                self.execute_stmt(stmt)?;
            }
            Ok(())
        })();

        match exec_result {
            Ok(()) => {}
            Err(err) => {
                if let Some(early) = err.downcast_ref::<EarlyReturn>() {
                    // exit() was called — set return value if provided
                    if let Some(ref val) = early.value {
                        if func.is_function {
                            self.set_variable(&func.return_type_name, val.clone());
                        }
                    }
                } else {
                    self.scopes.pop();
                    return Err(err);
                }
            }
        }

        // Get return value
        let result = if func.is_function {
            self.get_variable(&func.return_type_name)
                .unwrap_or(Value::Integer(0))
        } else {
            Value::Nil
        };

        // Pop scope
        self.scopes.pop();

        Ok(result)
    }

    /// Evaluate an expression
    fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Literal(lit) => Ok(self.literal_to_value(lit)),

            Expr::Variable(name) => self.get_variable(name),

            Expr::BinaryOp {
                operator,
                left,
                right,
            } => {
                let lval = self.eval_expr(left)?;
                let rval = self.eval_expr(right)?;
                self.eval_binary_op(operator, &lval, &rval)
            }

            Expr::UnaryOp { operator, operand } => {
                let val = self.eval_expr(operand)?;
                self.eval_unary_op(operator, &val)
            }

            Expr::FunctionCall { name, arguments } => self.eval_function_call(name, arguments),

            Expr::Is {
                expression,
                type_name,
            } => {
                let val = self.eval_expr(expression)?;
                let result = match &val {
                    Value::Object { class_name, .. } => {
                        self.is_instance_of(class_name, type_name)
                    }
                    _ => false,
                };
                Ok(Value::Boolean(result))
            }

            Expr::As {
                expression,
                type_name,
            } => {
                let val = self.eval_expr(expression)?;
                match &val {
                    Value::Object { class_name, .. } => {
                        if self.is_instance_of(class_name, type_name) {
                            Ok(val)
                        } else {
                            Err(anyhow!(
                                "Invalid typecast: {} is not a {}",
                                class_name,
                                type_name
                            ))
                        }
                    }
                    _ => Err(anyhow!("'as' requires an object expression")),
                }
            }

            Expr::Inherited { member } => {
                // Look up Self, find parent class, call parent method
                let self_val = self.get_variable("self")?;
                if let Value::Object { ref class_name, .. } = self_val {
                    let class_name = class_name.clone();
                    if let Some(class_decl) = self.classes.get(&class_name.to_lowercase()).cloned() {
                        if let Some(ref parent_name) = class_decl.parent {
                            if let Some(method_name) = member {
                                // inherited MethodName — call parent's method
                                let dotted = format!("{}.{}", parent_name, method_name);
                                return self.eval_dotted_call(&dotted, &[]);
                            }
                        }
                    }
                }
                Ok(Value::Nil)
            }

            _ => Err(anyhow!("Unsupported expression: {:?}", expr)),
        }
    }

    /// Evaluate a function call expression
    fn eval_function_call(&mut self, name: &str, arguments: &[Expr]) -> Result<Value> {
        let name_lower = name.to_lowercase();
        match name_lower.as_str() {
            "abs" => {
                let val = self.eval_expr(&arguments[0])?;
                match val {
                    Value::Integer(n) => Ok(Value::Integer(n.abs())),
                    Value::Real(r) => Ok(Value::Real(r.abs())),
                    _ => Err(anyhow!("abs() requires numeric argument")),
                }
            }
            "sqr" => {
                let val = self.eval_expr(&arguments[0])?;
                match val {
                    Value::Integer(n) => Ok(Value::Integer(n * n)),
                    Value::Real(r) => Ok(Value::Real(r * r)),
                    _ => Err(anyhow!("sqr() requires numeric argument")),
                }
            }
            "sqrt" => {
                let val = self.eval_expr(&arguments[0])?.as_real()?;
                Ok(Value::Real(val.sqrt()))
            }
            "sin" => {
                let val = self.eval_expr(&arguments[0])?.as_real()?;
                Ok(Value::Real(val.sin()))
            }
            "cos" => {
                let val = self.eval_expr(&arguments[0])?.as_real()?;
                Ok(Value::Real(val.cos()))
            }
            "ln" => {
                let val = self.eval_expr(&arguments[0])?.as_real()?;
                Ok(Value::Real(val.ln()))
            }
            "exp" => {
                let val = self.eval_expr(&arguments[0])?.as_real()?;
                Ok(Value::Real(val.exp()))
            }
            "round" => {
                let val = self.eval_expr(&arguments[0])?.as_real()?;
                Ok(Value::Integer(val.round() as i64))
            }
            "trunc" => {
                let val = self.eval_expr(&arguments[0])?.as_real()?;
                Ok(Value::Integer(val.trunc() as i64))
            }
            "ord" => {
                let val = self.eval_expr(&arguments[0])?;
                match val {
                    Value::Char(c) => Ok(Value::Integer(c as i64)),
                    Value::Boolean(b) => Ok(Value::Integer(if b { 1 } else { 0 })),
                    Value::Integer(n) => Ok(Value::Integer(n)),
                    _ => Err(anyhow!("ord() requires ordinal argument")),
                }
            }
            "chr" => {
                let val = self.eval_expr(&arguments[0])?.as_integer()?;
                Ok(Value::Char(val as u8 as char))
            }
            "length" => {
                let val = self.eval_expr(&arguments[0])?;
                match val {
                    Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                    Value::Array { elements, .. } => Ok(Value::Integer(elements.len() as i64)),
                    _ => Err(anyhow!("length() requires string or array argument")),
                }
            }
            "high" => {
                let val = self.eval_expr(&arguments[0])?;
                match val {
                    Value::Array { elements, lower_bound } => {
                        Ok(Value::Integer(lower_bound + elements.len() as i64 - 1))
                    }
                    Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                    _ => Err(anyhow!("high() requires array or string")),
                }
            }
            "low" => {
                let val = self.eval_expr(&arguments[0])?;
                match val {
                    Value::Array { lower_bound, .. } => Ok(Value::Integer(lower_bound)),
                    Value::String(_) => Ok(Value::Integer(1)),
                    _ => Err(anyhow!("low() requires array or string")),
                }
            }
            "odd" => {
                let val = self.eval_expr(&arguments[0])?.as_integer()?;
                Ok(Value::Boolean(val % 2 != 0))
            }
            "succ" => {
                let val = self.eval_expr(&arguments[0])?.as_integer()?;
                Ok(Value::Integer(val + 1))
            }
            "pred" => {
                let val = self.eval_expr(&arguments[0])?.as_integer()?;
                Ok(Value::Integer(val - 1))
            }
            "random" => {
                if arguments.is_empty() {
                    Ok(Value::Real(rand_simple()))
                } else {
                    let max = self.eval_expr(&arguments[0])?.as_integer()?;
                    Ok(Value::Integer((rand_simple() * max as f64) as i64))
                }
            }
            "upcase" => {
                let val = self.eval_expr(&arguments[0])?;
                match val {
                    Value::Char(c) => Ok(Value::Char(c.to_uppercase().next().unwrap_or(c))),
                    Value::String(s) => Ok(Value::String(s.to_uppercase())),
                    _ => Err(anyhow!("upcase() requires char or string")),
                }
            }
            "lowercase" => {
                let val = self.eval_expr(&arguments[0])?;
                match val {
                    Value::Char(c) => Ok(Value::Char(c.to_lowercase().next().unwrap_or(c))),
                    Value::String(s) => Ok(Value::String(s.to_lowercase())),
                    _ => Err(anyhow!("lowercase() requires char or string")),
                }
            }
            "concat" => {
                let mut result = String::new();
                for arg in arguments {
                    let val = self.eval_expr(arg)?;
                    result.push_str(&format!("{}", val));
                }
                Ok(Value::String(result))
            }
            "copy" => {
                let s = match self.eval_expr(&arguments[0])? {
                    Value::String(s) => s,
                    _ => return Err(anyhow!("copy() requires string")),
                };
                let start = self.eval_expr(&arguments[1])?.as_integer()? as usize;
                let len = self.eval_expr(&arguments[2])?.as_integer()? as usize;
                let start_idx = if start > 0 { start - 1 } else { 0 };
                let end_idx = (start_idx + len).min(s.len());
                Ok(Value::String(s[start_idx..end_idx].to_string()))
            }
            "pos" => {
                let substr = match self.eval_expr(&arguments[0])? {
                    Value::String(s) => s,
                    _ => return Err(anyhow!("pos() requires string")),
                };
                let s = match self.eval_expr(&arguments[1])? {
                    Value::String(s) => s,
                    _ => return Err(anyhow!("pos() requires string")),
                };
                Ok(Value::Integer(
                    s.find(&substr).map(|i| i as i64 + 1).unwrap_or(0),
                ))
            }
            "inttostr" => {
                let val = self.eval_expr(&arguments[0])?.as_integer()?;
                Ok(Value::String(val.to_string()))
            }
            "strtoint" => {
                let val = match self.eval_expr(&arguments[0])? {
                    Value::String(s) => s.parse::<i64>().unwrap_or(0),
                    _ => return Err(anyhow!("strtoint() requires string")),
                };
                Ok(Value::Integer(val))
            }
            "__index__" => {
                // Array/string indexing: __index__(collection, index)
                let collection = self.eval_expr(&arguments[0])?;
                let index = self.eval_expr(&arguments[1])?.as_integer()?;
                match collection {
                    Value::Array { ref elements, lower_bound } => {
                        let idx = (index - lower_bound) as usize;
                        if idx < elements.len() {
                            Ok(elements[idx].clone())
                        } else {
                            Err(anyhow!("Array index out of bounds: {} (length {})", index, elements.len()))
                        }
                    }
                    Value::String(ref s) => {
                        // Pascal strings are 1-indexed
                        let idx = (index - 1) as usize;
                        if idx < s.len() {
                            Ok(Value::Char(s.as_bytes()[idx] as char))
                        } else {
                            Err(anyhow!("String index out of bounds: {} (length {})", index, s.len()))
                        }
                    }
                    _ => Err(anyhow!("Cannot index into {:?}", collection)),
                }
            }
            _ => {
                // Try dotted name: ClassName.Create() or obj.Method()
                if name_lower.contains('.') {
                    return self.eval_dotted_call(name, arguments);
                }
                // Try user-defined function
                if let Some(user_func) = self.functions.get(&name_lower).cloned() {
                    self.call_user_function(&user_func, arguments)
                } else {
                    Err(anyhow!("Unknown function: {}", name))
                }
            }
        }
    }

    /// Evaluate binary operation
    fn eval_binary_op(&self, op: &str, left: &Value, right: &Value) -> Result<Value> {
        // String concatenation
        if op == "+" {
            if let (Value::String(l), Value::String(r)) = (left, right) {
                return Ok(Value::String(format!("{}{}", l, r)));
            }
            if let Value::String(l) = left {
                return Ok(Value::String(format!("{}{}", l, right)));
            }
            if let Value::String(r) = right {
                return Ok(Value::String(format!("{}{}", left, r)));
            }
        }

        // Real arithmetic if either operand is real
        if left.is_real() || right.is_real() || op == "/" {
            let l = left.as_real()?;
            let r = right.as_real()?;
            return match op {
                "+" => Ok(Value::Real(l + r)),
                "-" => Ok(Value::Real(l - r)),
                "*" => Ok(Value::Real(l * r)),
                "/" => {
                    if r == 0.0 {
                        Err(anyhow!("Division by zero"))
                    } else {
                        Ok(Value::Real(l / r))
                    }
                }
                "=" => Ok(Value::Boolean((l - r).abs() < f64::EPSILON)),
                "<>" => Ok(Value::Boolean((l - r).abs() >= f64::EPSILON)),
                "<" => Ok(Value::Boolean(l < r)),
                ">" => Ok(Value::Boolean(l > r)),
                "<=" => Ok(Value::Boolean(l <= r)),
                ">=" => Ok(Value::Boolean(l >= r)),
                _ => Err(anyhow!("Unsupported real operation: {}", op)),
            };
        }

        // Integer arithmetic
        let l = left.as_integer()?;
        let r = right.as_integer()?;
        match op {
            "+" => Ok(Value::Integer(l + r)),
            "-" => Ok(Value::Integer(l - r)),
            "*" => Ok(Value::Integer(l * r)),
            "div" => {
                if r == 0 {
                    Err(anyhow!("Division by zero"))
                } else {
                    Ok(Value::Integer(l / r))
                }
            }
            "mod" => {
                if r == 0 {
                    Err(anyhow!("Division by zero"))
                } else {
                    Ok(Value::Integer(l % r))
                }
            }
            "shl" => Ok(Value::Integer(l << r)),
            "shr" => Ok(Value::Integer(l >> r)),
            "and" => {
                let lb = left.as_boolean()?;
                let rb = right.as_boolean()?;
                Ok(Value::Boolean(lb && rb))
            }
            "or" => {
                let lb = left.as_boolean()?;
                let rb = right.as_boolean()?;
                Ok(Value::Boolean(lb || rb))
            }
            "xor" => {
                let lb = left.as_boolean()?;
                let rb = right.as_boolean()?;
                Ok(Value::Boolean(lb ^ rb))
            }
            "=" => Ok(Value::Boolean(l == r)),
            "<>" => Ok(Value::Boolean(l != r)),
            "<" => Ok(Value::Boolean(l < r)),
            ">" => Ok(Value::Boolean(l > r)),
            "<=" => Ok(Value::Boolean(l <= r)),
            ">=" => Ok(Value::Boolean(l >= r)),
            _ => Err(anyhow!("Unsupported operation: {}", op)),
        }
    }

    /// Evaluate unary operation
    fn eval_unary_op(&self, op: &str, val: &Value) -> Result<Value> {
        match op {
            "-" => match val {
                Value::Integer(n) => Ok(Value::Integer(-n)),
                Value::Real(r) => Ok(Value::Real(-r)),
                _ => Err(anyhow!("Cannot negate {:?}", val)),
            },
            "+" => Ok(val.clone()),
            "not" => {
                let b = val.as_boolean()?;
                Ok(Value::Boolean(!b))
            }
            _ => Err(anyhow!("Unknown unary operator: {}", op)),
        }
    }

    /// Convert literal to runtime value
    fn literal_to_value(&self, lit: &Literal) -> Value {
        match lit {
            Literal::Integer(n) => Value::Integer(*n),
            Literal::Real(r) => Value::Real(*r),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::Char(c) => Value::Char(*c),
            Literal::String(s) => Value::String(s.clone()),
            Literal::WideString(s) => Value::String(s.clone()),
            Literal::Nil => Value::Nil,
            Literal::Set(_) => Value::Nil, // Simplified
        }
    }

    /// Get a variable value (searches scopes from innermost to outermost)
    /// Supports dotted names for object field access: obj.field
    fn get_variable(&self, name: &str) -> Result<Value> {
        let name_lower = name.to_lowercase();

        // Handle dotted field access: obj.field or record.field
        if let Some(dot_pos) = name_lower.find('.') {
            let obj_name = &name_lower[..dot_pos];
            let field_name = &name_lower[dot_pos + 1..];
            let obj = self.get_variable(obj_name)?;
            match &obj {
                Value::Object { fields, class_name: _, .. } => {
                    if let Some(val) = fields.get(field_name) {
                        return Ok(val.clone());
                    }
                    // Try property read (can't call &mut self here, so skip for const)
                    // Property reads are handled in eval_expr for dotted variables
                    return Err(anyhow!("Object '{}' has no field '{}'", obj_name, field_name));
                }
                Value::Record { fields } => {
                    if let Some(val) = fields.get(field_name) {
                        return Ok(val.clone());
                    }
                    return Err(anyhow!("Record '{}' has no field '{}'", obj_name, field_name));
                }
                _ => return Err(anyhow!("'{}' is not an object or record", obj_name)),
            }
        }

        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(&name_lower) {
                return Ok(val.clone());
            }
        }
        Err(anyhow!("Undefined variable: {}", name))
    }

    /// Set a variable in the current (innermost) scope only — no outward search.
    /// Used for function parameters, return variables, and local declarations
    /// to avoid clobbering parent scope variables in recursive calls.
    fn set_local_variable(&mut self, name: &str, value: Value) {
        let name_lower = name.to_lowercase();
        if let Some(scope) = self.scopes.last_mut() {
            scope.variables.insert(name_lower, value);
        }
    }

    /// Set a variable value (in the innermost scope that has it, or current scope)
    /// Supports dotted names for object field assignment: obj.field := value
    fn set_variable(&mut self, name: &str, value: Value) {
        let name_lower = name.to_lowercase();

        // Handle dotted field assignment: obj.field := value or record.field := value
        if let Some(dot_pos) = name_lower.find('.') {
            let obj_name = name_lower[..dot_pos].to_string();
            let field_name = name_lower[dot_pos + 1..].to_string();
            for scope in self.scopes.iter_mut().rev() {
                if let Some(obj_val) = scope.variables.get_mut(&obj_name) {
                    match obj_val {
                        Value::Object { fields, .. } => {
                            fields.insert(field_name, value);
                            return;
                        }
                        Value::Record { fields } => {
                            fields.insert(field_name, value);
                            return;
                        }
                        _ => {}
                    }
                }
            }
            return;
        }

        // Search existing scopes from innermost
        for scope in self.scopes.iter_mut().rev() {
            if scope.variables.contains_key(&name_lower) {
                scope.variables.insert(name_lower, value);
                return;
            }
        }
        // If not found, insert in current (innermost) scope
        if let Some(scope) = self.scopes.last_mut() {
            scope.variables.insert(name_lower, value);
        }
    }
}

/// Simple pseudo-random number generator (no external dependency)
fn rand_simple() -> f64 {
    use std::time::SystemTime;
    let seed = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .subsec_nanos();
    (seed as f64 % 1000.0) / 1000.0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, FieldVisibility, VariableDecl};

    fn make_program(name: &str, stmts: Vec<Stmt>) -> Program {
        Program {
            name: name.to_string(),
            uses: vec![],
            block: Block::with_statements(stmts),
        }
    }

    fn make_program_with_vars(
        name: &str,
        vars: Vec<VariableDecl>,
        stmts: Vec<Stmt>,
    ) -> Program {
        Program {
            name: name.to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars,
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                statements: stmts,
            },
        }
    }

    fn var_decl(name: &str) -> VariableDecl {
        VariableDecl {
            name: name.to_string(),
            variable_type: crate::ast::Type::Integer,
            initial_value: None,
            visibility: FieldVisibility::Public,
            is_absolute: false,
            absolute_address: None,
        }
    }

    #[test]
    fn test_assignment_and_variable() {
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![Stmt::Assignment {
                target: "x".to_string(),
                value: Expr::Literal(Literal::Integer(42)),
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(42));
    }

    #[test]
    fn test_arithmetic() {
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![Stmt::Assignment {
                target: "x".to_string(),
                value: Expr::BinaryOp {
                    operator: "+".to_string(),
                    left: Box::new(Expr::Literal(Literal::Integer(10))),
                    right: Box::new(Expr::Literal(Literal::Integer(32))),
                },
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(42));
    }

    #[test]
    fn test_if_true() {
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![Stmt::If {
                condition: Expr::Literal(Literal::Boolean(true)),
                then_branch: vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Literal(Literal::Integer(1)),
                }],
                else_branch: Some(vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Literal(Literal::Integer(2)),
                }]),
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(1));
    }

    #[test]
    fn test_if_false() {
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![Stmt::If {
                condition: Expr::Literal(Literal::Boolean(false)),
                then_branch: vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Literal(Literal::Integer(1)),
                }],
                else_branch: Some(vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Literal(Literal::Integer(2)),
                }]),
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(2));
    }

    #[test]
    fn test_while_loop() {
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![
                Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Literal(Literal::Integer(0)),
                },
                Stmt::While {
                    condition: Expr::BinaryOp {
                        operator: "<".to_string(),
                        left: Box::new(Expr::Variable("x".to_string())),
                        right: Box::new(Expr::Literal(Literal::Integer(5))),
                    },
                    body: vec![Stmt::Assignment {
                        target: "x".to_string(),
                        value: Expr::BinaryOp {
                            operator: "+".to_string(),
                            left: Box::new(Expr::Variable("x".to_string())),
                            right: Box::new(Expr::Literal(Literal::Integer(1))),
                        },
                    }],
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(5));
    }

    #[test]
    fn test_for_loop() {
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("i"), var_decl("sum")],
            vec![
                Stmt::Assignment {
                    target: "sum".to_string(),
                    value: Expr::Literal(Literal::Integer(0)),
                },
                Stmt::For {
                    var_name: "i".to_string(),
                    start: Expr::Literal(Literal::Integer(1)),
                    end: Expr::Literal(Literal::Integer(10)),
                    direction: ForDirection::To,
                    body: vec![Stmt::Assignment {
                        target: "sum".to_string(),
                        value: Expr::BinaryOp {
                            operator: "+".to_string(),
                            left: Box::new(Expr::Variable("sum".to_string())),
                            right: Box::new(Expr::Variable("i".to_string())),
                        },
                    }],
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("sum").unwrap(), Value::Integer(55));
    }

    #[test]
    fn test_repeat_until() {
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![
                Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Literal(Literal::Integer(0)),
                },
                Stmt::Repeat {
                    body: vec![Stmt::Assignment {
                        target: "x".to_string(),
                        value: Expr::BinaryOp {
                            operator: "+".to_string(),
                            left: Box::new(Expr::Variable("x".to_string())),
                            right: Box::new(Expr::Literal(Literal::Integer(1))),
                        },
                    }],
                    until_condition: Expr::BinaryOp {
                        operator: ">=".to_string(),
                        left: Box::new(Expr::Variable("x".to_string())),
                        right: Box::new(Expr::Literal(Literal::Integer(3))),
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(3));
    }

    #[test]
    fn test_string_concat() {
        let mut interp = Interpreter::new(false);
        let result = interp
            .eval_binary_op(
                "+",
                &Value::String("Hello".to_string()),
                &Value::String(" World".to_string()),
            )
            .unwrap();
        assert_eq!(result, Value::String("Hello World".to_string()));
    }

    #[test]
    fn test_comparison_ops() {
        let interp = Interpreter::new(false);
        assert_eq!(
            interp.eval_binary_op("=", &Value::Integer(5), &Value::Integer(5)).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            interp.eval_binary_op("<>", &Value::Integer(5), &Value::Integer(3)).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            interp.eval_binary_op("<", &Value::Integer(3), &Value::Integer(5)).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            interp.eval_binary_op(">", &Value::Integer(5), &Value::Integer(3)).unwrap(),
            Value::Boolean(true)
        );
    }

    #[test]
    fn test_unary_ops() {
        let interp = Interpreter::new(false);
        assert_eq!(
            interp.eval_unary_op("-", &Value::Integer(5)).unwrap(),
            Value::Integer(-5)
        );
        assert_eq!(
            interp.eval_unary_op("not", &Value::Boolean(true)).unwrap(),
            Value::Boolean(false)
        );
    }

    #[test]
    fn test_division() {
        let interp = Interpreter::new(false);
        // Integer division
        assert_eq!(
            interp.eval_binary_op("div", &Value::Integer(10), &Value::Integer(3)).unwrap(),
            Value::Integer(3)
        );
        // Real division
        let result = interp
            .eval_binary_op("/", &Value::Integer(10), &Value::Integer(4))
            .unwrap();
        if let Value::Real(r) = result {
            assert!((r - 2.5).abs() < f64::EPSILON);
        } else {
            panic!("Expected real result");
        }
    }

    #[test]
    fn test_try_except_catches_raise() {
        // try raise 'error'; except x := 1; end;
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![Stmt::Try {
                try_block: vec![Stmt::Raise {
                    exception: Some(Expr::Literal(Literal::String("error".to_string()))),
                    message: None,
                }],
                except_clauses: vec![crate::ast::ExceptClause {
                    exception_type: None,
                    variable: None,
                    body: vec![Stmt::Assignment {
                        target: "x".to_string(),
                        value: Expr::Literal(Literal::Integer(1)),
                    }],
                }],
                finally_block: None,
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(1));
    }

    #[test]
    fn test_try_finally_runs_on_success() {
        // try x := 10; finally x := x + 1; end;
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![Stmt::Try {
                try_block: vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Literal(Literal::Integer(10)),
                }],
                except_clauses: vec![],
                finally_block: Some(vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::BinaryOp {
                        operator: "+".to_string(),
                        left: Box::new(Expr::Variable("x".to_string())),
                        right: Box::new(Expr::Literal(Literal::Integer(1))),
                    },
                }]),
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(11));
    }

    #[test]
    fn test_try_finally_runs_on_exception() {
        // try raise 'err'; finally y := 99; end; — exception propagates but finally runs
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("y")],
            vec![Stmt::Try {
                try_block: vec![Stmt::Raise {
                    exception: Some(Expr::Literal(Literal::String("err".to_string()))),
                    message: None,
                }],
                except_clauses: vec![],
                finally_block: Some(vec![Stmt::Assignment {
                    target: "y".to_string(),
                    value: Expr::Literal(Literal::Integer(99)),
                }]),
            }],
        );

        let mut interp = Interpreter::new(false);
        // Should propagate the exception
        let result = interp.run_program(&prog);
        assert!(result.is_err());
        // But finally should have run
        assert_eq!(interp.get_variable("y").unwrap(), Value::Integer(99));
    }

    #[test]
    fn test_try_except_with_variable_binding() {
        // try raise 'hello'; except on E: Exception do x := 1; end;
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![Stmt::Try {
                try_block: vec![Stmt::Raise {
                    exception: Some(Expr::Literal(Literal::String("hello".to_string()))),
                    message: None,
                }],
                except_clauses: vec![crate::ast::ExceptClause {
                    exception_type: Some("Exception".to_string()),
                    variable: Some("e".to_string()),
                    body: vec![Stmt::Assignment {
                        target: "x".to_string(),
                        value: Expr::Literal(Literal::Integer(42)),
                    }],
                }],
                finally_block: None,
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(42));
        // The exception variable 'e' should have the message
        assert_eq!(
            interp.get_variable("e").unwrap(),
            Value::String("hello".to_string())
        );
    }

    #[test]
    fn test_try_except_finally_combined() {
        // try raise 'err'; except x := 1; finally y := 2; end;
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x"), var_decl("y")],
            vec![Stmt::Try {
                try_block: vec![Stmt::Raise {
                    exception: Some(Expr::Literal(Literal::String("err".to_string()))),
                    message: None,
                }],
                except_clauses: vec![crate::ast::ExceptClause {
                    exception_type: None,
                    variable: None,
                    body: vec![Stmt::Assignment {
                        target: "x".to_string(),
                        value: Expr::Literal(Literal::Integer(1)),
                    }],
                }],
                finally_block: Some(vec![Stmt::Assignment {
                    target: "y".to_string(),
                    value: Expr::Literal(Literal::Integer(2)),
                }]),
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(1));
        assert_eq!(interp.get_variable("y").unwrap(), Value::Integer(2));
    }

    #[test]
    fn test_no_exception_skips_except() {
        // try x := 10; except x := 99; end; — no exception, except not run
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("x")],
            vec![Stmt::Try {
                try_block: vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Literal(Literal::Integer(10)),
                }],
                except_clauses: vec![crate::ast::ExceptClause {
                    exception_type: None,
                    variable: None,
                    body: vec![Stmt::Assignment {
                        target: "x".to_string(),
                        value: Expr::Literal(Literal::Integer(99)),
                    }],
                }],
                finally_block: None,
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(10));
    }

    #[test]
    fn test_raise_bare_reraise() {
        // raise; — bare re-raise should produce an exception
        let prog = make_program_with_vars(
            "Test",
            vec![],
            vec![Stmt::Raise {
                exception: None,
                message: None,
            }],
        );

        let mut interp = Interpreter::new(false);
        let result = interp.run_program(&prog);
        assert!(result.is_err());
    }

    // ========== Class support tests ==========

    use crate::ast::{ClassDecl, FieldDecl, MethodDecl, Parameter, Type as AstType};

    fn make_simple_class(name: &str, fields: Vec<(&str, FieldVisibility)>) -> ClassDecl {
        ClassDecl {
            name: name.to_string(),
            parent: None,
            interfaces: vec![],
            fields: fields
                .into_iter()
                .map(|(n, vis)| FieldDecl {
                    name: n.to_string(),
                    field_type: AstType::Integer,
                    visibility: vis,
                })
                .collect(),
            methods: vec![],
            properties: vec![],
            visibility: FieldVisibility::Public,
            is_abstract: false,
            is_sealed: false,
        }
    }

    fn make_program_with_classes(
        name: &str,
        vars: Vec<VariableDecl>,
        classes: Vec<ClassDecl>,
        stmts: Vec<Stmt>,
    ) -> Program {
        Program {
            name: name.to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars,
                procedures: vec![],
                functions: vec![],
                classes,
                statements: stmts,
            },
        }
    }

    #[test]
    fn test_class_create_object() {
        // type TPoint = class x, y: integer; end;
        // var p: TPoint;
        // p := TPoint.Create;
        let class = make_simple_class("TPoint", vec![
            ("x", FieldVisibility::Public),
            ("y", FieldVisibility::Public),
        ]);

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("p")],
            vec![class],
            vec![Stmt::Assignment {
                target: "p".to_string(),
                value: Expr::FunctionCall {
                    name: "TPoint.Create".to_string(),
                    arguments: vec![],
                },
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        let p = interp.get_variable("p").unwrap();
        match p {
            Value::Object { class_name, fields } => {
                assert_eq!(class_name, "TPoint");
                assert!(fields.contains_key("x"));
                assert!(fields.contains_key("y"));
            }
            _ => panic!("Expected Object, got {:?}", p),
        }
    }

    #[test]
    fn test_class_field_access() {
        // p := TPoint.Create; p.x := 10; p.y := 20;
        let class = make_simple_class("TPoint", vec![
            ("x", FieldVisibility::Public),
            ("y", FieldVisibility::Public),
        ]);

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("p"), var_decl("sum")],
            vec![class],
            vec![
                Stmt::Assignment {
                    target: "p".to_string(),
                    value: Expr::FunctionCall {
                        name: "TPoint.Create".to_string(),
                        arguments: vec![],
                    },
                },
                Stmt::Assignment {
                    target: "p.x".to_string(),
                    value: Expr::Literal(Literal::Integer(10)),
                },
                Stmt::Assignment {
                    target: "p.y".to_string(),
                    value: Expr::Literal(Literal::Integer(20)),
                },
                // sum := p.x + p.y
                Stmt::Assignment {
                    target: "sum".to_string(),
                    value: Expr::BinaryOp {
                        operator: "+".to_string(),
                        left: Box::new(Expr::Variable("p.x".to_string())),
                        right: Box::new(Expr::Variable("p.y".to_string())),
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("sum").unwrap(), Value::Integer(30));
    }

    #[test]
    fn test_class_with_method() {
        // type TCounter = class
        //   count: integer;
        //   function GetCount: integer; begin result := self.count; end;
        // end;
        let class = ClassDecl {
            name: "TCounter".to_string(),
            parent: None,
            interfaces: vec![],
            fields: vec![FieldDecl {
                name: "count".to_string(),
                field_type: AstType::Integer,
                visibility: FieldVisibility::Public,
            }],
            methods: vec![MethodDecl {
                name: "GetCount".to_string(),
                parameters: vec![],
                return_type: Some(AstType::Integer),
                block: Some(Block::with_statements(vec![Stmt::Assignment {
                    target: "result".to_string(),
                    value: Expr::Variable("self.count".to_string()),
                }])),
                visibility: FieldVisibility::Public,
                is_class_method: false,
                is_virtual: false,
                is_abstract: false,
                is_override: false,
                is_overload: false,
                is_static: false,
                is_constructor: false,
                is_destructor: false,
            }],
            properties: vec![],
            visibility: FieldVisibility::Public,
            is_abstract: false,
            is_sealed: false,
        };

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("c"), var_decl("x")],
            vec![class],
            vec![
                Stmt::Assignment {
                    target: "c".to_string(),
                    value: Expr::FunctionCall {
                        name: "TCounter.Create".to_string(),
                        arguments: vec![],
                    },
                },
                Stmt::Assignment {
                    target: "c.count".to_string(),
                    value: Expr::Literal(Literal::Integer(42)),
                },
                // x := c.GetCount()
                Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::FunctionCall {
                        name: "c.GetCount".to_string(),
                        arguments: vec![],
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(42));
    }

    #[test]
    fn test_class_inheritance_fields() {
        // type TBase = class x: integer; end;
        // type TChild = class(TBase) y: integer; end;
        // var c: TChild; c := TChild.Create; — should have both x and y
        let base = make_simple_class("TBase", vec![("x", FieldVisibility::Public)]);
        let mut child = make_simple_class("TChild", vec![("y", FieldVisibility::Public)]);
        child.parent = Some("TBase".to_string());

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("c")],
            vec![base, child],
            vec![Stmt::Assignment {
                target: "c".to_string(),
                value: Expr::FunctionCall {
                    name: "TChild.Create".to_string(),
                    arguments: vec![],
                },
            }],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        let c = interp.get_variable("c").unwrap();
        match c {
            Value::Object { class_name, fields } => {
                assert_eq!(class_name, "TChild");
                assert!(fields.contains_key("x"), "Should inherit x from TBase");
                assert!(fields.contains_key("y"), "Should have own field y");
            }
            _ => panic!("Expected Object"),
        }
    }

    #[test]
    fn test_class_constructor_with_body() {
        // type TPoint = class
        //   x, y: integer;
        //   constructor Create(ax, ay: integer);
        //   begin self.x := ax; self.y := ay; end;
        // end;
        let class = ClassDecl {
            name: "TPoint".to_string(),
            parent: None,
            interfaces: vec![],
            fields: vec![
                FieldDecl { name: "x".to_string(), field_type: AstType::Integer, visibility: FieldVisibility::Public },
                FieldDecl { name: "y".to_string(), field_type: AstType::Integer, visibility: FieldVisibility::Public },
            ],
            methods: vec![MethodDecl {
                name: "Create".to_string(),
                parameters: vec![
                    Parameter { name: "ax".to_string(), param_type: AstType::Integer, is_var: false, is_const: false, is_out: false, default_value: None },
                    Parameter { name: "ay".to_string(), param_type: AstType::Integer, is_var: false, is_const: false, is_out: false, default_value: None },
                ],
                return_type: None,
                block: Some(Block::with_statements(vec![
                    Stmt::Assignment {
                        target: "self.x".to_string(),
                        value: Expr::Variable("ax".to_string()),
                    },
                    Stmt::Assignment {
                        target: "self.y".to_string(),
                        value: Expr::Variable("ay".to_string()),
                    },
                ])),
                visibility: FieldVisibility::Public,
                is_class_method: false,
                is_virtual: false,
                is_abstract: false,
                is_override: false,
                is_overload: false,
                is_static: false,
                is_constructor: true,
                is_destructor: false,
            }],
            properties: vec![],
            visibility: FieldVisibility::Public,
            is_abstract: false,
            is_sealed: false,
        };

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("p"), var_decl("sum")],
            vec![class],
            vec![
                Stmt::Assignment {
                    target: "p".to_string(),
                    value: Expr::FunctionCall {
                        name: "TPoint.Create".to_string(),
                        arguments: vec![
                            Expr::Literal(Literal::Integer(3)),
                            Expr::Literal(Literal::Integer(4)),
                        ],
                    },
                },
                Stmt::Assignment {
                    target: "sum".to_string(),
                    value: Expr::BinaryOp {
                        operator: "+".to_string(),
                        left: Box::new(Expr::Variable("p.x".to_string())),
                        right: Box::new(Expr::Variable("p.y".to_string())),
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("sum").unwrap(), Value::Integer(7));
    }

    // ========== is/as type check tests ==========

    #[test]
    fn test_is_type_check_same_class() {
        let class = make_simple_class("TAnimal", vec![("name", FieldVisibility::Public)]);
        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("a"), var_decl("result")],
            vec![class],
            vec![
                Stmt::Assignment {
                    target: "a".to_string(),
                    value: Expr::FunctionCall {
                        name: "TAnimal.Create".to_string(),
                        arguments: vec![],
                    },
                },
                Stmt::Assignment {
                    target: "result".to_string(),
                    value: Expr::Is {
                        expression: Box::new(Expr::Variable("a".to_string())),
                        type_name: "TAnimal".to_string(),
                    },
                },
            ],
        );
        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("result").unwrap(), Value::Boolean(true));
    }

    #[test]
    fn test_is_type_check_inheritance() {
        let base = make_simple_class("TAnimal", vec![("name", FieldVisibility::Public)]);
        let mut child = make_simple_class("TDog", vec![("breed", FieldVisibility::Public)]);
        child.parent = Some("TAnimal".to_string());

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("d"), var_decl("r1"), var_decl("r2")],
            vec![base, child],
            vec![
                Stmt::Assignment {
                    target: "d".to_string(),
                    value: Expr::FunctionCall {
                        name: "TDog.Create".to_string(),
                        arguments: vec![],
                    },
                },
                // d is TAnimal => true (child is parent)
                Stmt::Assignment {
                    target: "r1".to_string(),
                    value: Expr::Is {
                        expression: Box::new(Expr::Variable("d".to_string())),
                        type_name: "TAnimal".to_string(),
                    },
                },
                // d is TDog => true
                Stmt::Assignment {
                    target: "r2".to_string(),
                    value: Expr::Is {
                        expression: Box::new(Expr::Variable("d".to_string())),
                        type_name: "TDog".to_string(),
                    },
                },
            ],
        );
        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("r1").unwrap(), Value::Boolean(true));
        assert_eq!(interp.get_variable("r2").unwrap(), Value::Boolean(true));
    }

    #[test]
    fn test_as_typecast_valid() {
        let base = make_simple_class("TAnimal", vec![]);
        let mut child = make_simple_class("TDog", vec![("breed", FieldVisibility::Public)]);
        child.parent = Some("TAnimal".to_string());

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("d"), var_decl("a")],
            vec![base, child],
            vec![
                Stmt::Assignment {
                    target: "d".to_string(),
                    value: Expr::FunctionCall {
                        name: "TDog.Create".to_string(),
                        arguments: vec![],
                    },
                },
                // a := d as TAnimal — valid upcast
                Stmt::Assignment {
                    target: "a".to_string(),
                    value: Expr::As {
                        expression: Box::new(Expr::Variable("d".to_string())),
                        type_name: "TAnimal".to_string(),
                    },
                },
            ],
        );
        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        // Should succeed without error
        match interp.get_variable("a").unwrap() {
            Value::Object { class_name, .. } => assert_eq!(class_name, "TDog"),
            other => panic!("Expected Object, got {:?}", other),
        }
    }

    // ========== virtual/override dispatch tests ==========

    #[test]
    fn test_virtual_override_dispatch() {
        // TBase has virtual method Speak that returns 1
        // TChild overrides Speak to return 2
        // Calling Speak on a TChild instance should return 2
        let base = ClassDecl {
            name: "TBase".to_string(),
            parent: None,
            interfaces: vec![],
            fields: vec![],
            methods: vec![MethodDecl {
                name: "Speak".to_string(),
                parameters: vec![],
                return_type: Some(AstType::Integer),
                block: Some(Block::with_statements(vec![Stmt::Assignment {
                    target: "result".to_string(),
                    value: Expr::Literal(Literal::Integer(1)),
                }])),
                visibility: FieldVisibility::Public,
                is_class_method: false,
                is_virtual: true,
                is_abstract: false,
                is_override: false,
                is_overload: false,
                is_static: false,
                is_constructor: false,
                is_destructor: false,
            }],
            properties: vec![],
            visibility: FieldVisibility::Public,
            is_abstract: false,
            is_sealed: false,
        };

        let child = ClassDecl {
            name: "TChild".to_string(),
            parent: Some("TBase".to_string()),
            interfaces: vec![],
            fields: vec![],
            methods: vec![MethodDecl {
                name: "Speak".to_string(),
                parameters: vec![],
                return_type: Some(AstType::Integer),
                block: Some(Block::with_statements(vec![Stmt::Assignment {
                    target: "result".to_string(),
                    value: Expr::Literal(Literal::Integer(2)),
                }])),
                visibility: FieldVisibility::Public,
                is_class_method: false,
                is_virtual: false,
                is_abstract: false,
                is_override: true,
                is_overload: false,
                is_static: false,
                is_constructor: false,
                is_destructor: false,
            }],
            properties: vec![],
            visibility: FieldVisibility::Public,
            is_abstract: false,
            is_sealed: false,
        };

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("obj"), var_decl("x")],
            vec![base, child],
            vec![
                Stmt::Assignment {
                    target: "obj".to_string(),
                    value: Expr::FunctionCall {
                        name: "TChild.Create".to_string(),
                        arguments: vec![],
                    },
                },
                Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::FunctionCall {
                        name: "obj.Speak".to_string(),
                        arguments: vec![],
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        // Should call TChild.Speak (override), returning 2
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(2));
    }

    #[test]
    fn test_inherited_method_from_parent() {
        // TBase has method Greet returning 10
        // TChild has no Greet — should inherit from TBase
        let base = ClassDecl {
            name: "TBase".to_string(),
            parent: None,
            interfaces: vec![],
            fields: vec![],
            methods: vec![MethodDecl {
                name: "Greet".to_string(),
                parameters: vec![],
                return_type: Some(AstType::Integer),
                block: Some(Block::with_statements(vec![Stmt::Assignment {
                    target: "result".to_string(),
                    value: Expr::Literal(Literal::Integer(10)),
                }])),
                visibility: FieldVisibility::Public,
                is_class_method: false,
                is_virtual: true,
                is_abstract: false,
                is_override: false,
                is_overload: false,
                is_static: false,
                is_constructor: false,
                is_destructor: false,
            }],
            properties: vec![],
            visibility: FieldVisibility::Public,
            is_abstract: false,
            is_sealed: false,
        };

        let child = ClassDecl {
            name: "TChild".to_string(),
            parent: Some("TBase".to_string()),
            interfaces: vec![],
            fields: vec![],
            methods: vec![],
            properties: vec![],
            visibility: FieldVisibility::Public,
            is_abstract: false,
            is_sealed: false,
        };

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("obj"), var_decl("x")],
            vec![base, child],
            vec![
                Stmt::Assignment {
                    target: "obj".to_string(),
                    value: Expr::FunctionCall {
                        name: "TChild.Create".to_string(),
                        arguments: vec![],
                    },
                },
                Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::FunctionCall {
                        name: "obj.Greet".to_string(),
                        arguments: vec![],
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(10));
    }

    // ========== Property tests ==========

    #[test]
    fn test_property_read_via_field() {
        use crate::ast::{PropertyDecl, MethodSpecifier};
        let class = ClassDecl {
            name: "TObj".to_string(),
            parent: None,
            interfaces: vec![],
            fields: vec![FieldDecl {
                name: "fvalue".to_string(),
                field_type: AstType::Integer,
                visibility: FieldVisibility::Private,
            }],
            methods: vec![],
            properties: vec![PropertyDecl {
                name: "Value".to_string(),
                property_type: AstType::Integer,
                read_specifier: Some(MethodSpecifier::Field("fvalue".to_string())),
                write_specifier: Some(MethodSpecifier::Field("fvalue".to_string())),
                stored_field: None,
                default_value: None,
                visibility: FieldVisibility::Public,
                is_indexed: false,
                index_parameters: vec![],
            }],
            visibility: FieldVisibility::Public,
            is_abstract: false,
            is_sealed: false,
        };

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("o"), var_decl("x")],
            vec![class],
            vec![
                Stmt::Assignment {
                    target: "o".to_string(),
                    value: Expr::FunctionCall {
                        name: "TObj.Create".to_string(),
                        arguments: vec![],
                    },
                },
                // Set the backing field directly
                Stmt::Assignment {
                    target: "o.fvalue".to_string(),
                    value: Expr::Literal(Literal::Integer(42)),
                },
                // Read via field access (property backed by field)
                Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Variable("o.fvalue".to_string()),
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(42));
    }

    // ========== Array tests ==========

    #[test]
    fn test_array_create_and_index() {
        // Create array, set elements, read back
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("arr"), var_decl("x")],
            vec![
                // arr := setlength(arr, 5) — via procedure call
                Stmt::ProcedureCall {
                    name: "setlength".to_string(),
                    arguments: vec![
                        Expr::Variable("arr".to_string()),
                        Expr::Literal(Literal::Integer(5)),
                    ],
                },
                // Read arr length
                Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::FunctionCall {
                        name: "length".to_string(),
                        arguments: vec![Expr::Variable("arr".to_string())],
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(5));
    }

    #[test]
    fn test_array_indexing() {
        // __index__(arr, i) for array element access
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("arr"), var_decl("x")],
            vec![
                Stmt::ProcedureCall {
                    name: "setlength".to_string(),
                    arguments: vec![
                        Expr::Variable("arr".to_string()),
                        Expr::Literal(Literal::Integer(3)),
                    ],
                },
                // Read arr[0] (should be 0, default)
                Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::FunctionCall {
                        name: "__index__".to_string(),
                        arguments: vec![
                            Expr::Variable("arr".to_string()),
                            Expr::Literal(Literal::Integer(0)),
                        ],
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(0));
    }

    #[test]
    fn test_array_high_low() {
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("arr"), var_decl("h"), var_decl("l")],
            vec![
                Stmt::ProcedureCall {
                    name: "setlength".to_string(),
                    arguments: vec![
                        Expr::Variable("arr".to_string()),
                        Expr::Literal(Literal::Integer(10)),
                    ],
                },
                Stmt::Assignment {
                    target: "h".to_string(),
                    value: Expr::FunctionCall {
                        name: "high".to_string(),
                        arguments: vec![Expr::Variable("arr".to_string())],
                    },
                },
                Stmt::Assignment {
                    target: "l".to_string(),
                    value: Expr::FunctionCall {
                        name: "low".to_string(),
                        arguments: vec![Expr::Variable("arr".to_string())],
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("h").unwrap(), Value::Integer(9));
        assert_eq!(interp.get_variable("l").unwrap(), Value::Integer(0));
    }

    // ========== Record tests ==========

    #[test]
    fn test_record_field_access() {
        // Manually create a record value and test field access
        let mut interp = Interpreter::new(false);
        let mut fields = HashMap::new();
        fields.insert("x".to_string(), Value::Integer(10));
        fields.insert("y".to_string(), Value::Integer(20));
        interp.set_variable("rec", Value::Record { fields });

        assert_eq!(interp.get_variable("rec.x").unwrap(), Value::Integer(10));
        assert_eq!(interp.get_variable("rec.y").unwrap(), Value::Integer(20));

        // Set field
        interp.set_variable("rec.x", Value::Integer(99));
        assert_eq!(interp.get_variable("rec.x").unwrap(), Value::Integer(99));
    }

    // ========== String indexing tests ==========

    #[test]
    fn test_string_indexing() {
        // s := 'Hello'; x := s[1] => 'H'
        let prog = make_program_with_vars(
            "Test",
            vec![var_decl("s"), var_decl("x")],
            vec![
                Stmt::Assignment {
                    target: "s".to_string(),
                    value: Expr::Literal(Literal::String("Hello".to_string())),
                },
                Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::FunctionCall {
                        name: "__index__".to_string(),
                        arguments: vec![
                            Expr::Variable("s".to_string()),
                            Expr::Literal(Literal::Integer(1)),
                        ],
                    },
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Char('H'));
    }

    // ========== With statement tests ==========

    #[test]
    fn test_with_statement() {
        // Create object, use with to access fields directly
        let class = make_simple_class("TPoint", vec![
            ("x", FieldVisibility::Public),
            ("y", FieldVisibility::Public),
        ]);

        let prog = make_program_with_classes(
            "Test",
            vec![var_decl("p"), var_decl("sum")],
            vec![class],
            vec![
                Stmt::Assignment {
                    target: "p".to_string(),
                    value: Expr::FunctionCall {
                        name: "TPoint.Create".to_string(),
                        arguments: vec![],
                    },
                },
                Stmt::Assignment {
                    target: "p.x".to_string(),
                    value: Expr::Literal(Literal::Integer(3)),
                },
                Stmt::Assignment {
                    target: "p.y".to_string(),
                    value: Expr::Literal(Literal::Integer(7)),
                },
                // with p do sum := x + y
                Stmt::With {
                    variable: Expr::Variable("p".to_string()),
                    statements: vec![Stmt::Assignment {
                        target: "sum".to_string(),
                        value: Expr::BinaryOp {
                            operator: "+".to_string(),
                            left: Box::new(Expr::Variable("x".to_string())),
                            right: Box::new(Expr::Variable("y".to_string())),
                        },
                    }],
                },
            ],
        );

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("sum").unwrap(), Value::Integer(10));
    }

    // ========== Exit tests ==========

    #[test]
    fn test_exit_from_function() {
        use crate::ast::FunctionDecl;
        // function Foo: integer; begin exit(42); result := 99; end;
        let prog = Program {
            name: "Test".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![var_decl("x")],
                procedures: vec![],
                functions: vec![FunctionDecl {
                    name: "Foo".to_string(),
                    parameters: vec![],
                    return_type: AstType::Integer,
                    block: Block::with_statements(vec![
                        Stmt::ProcedureCall {
                            name: "exit".to_string(),
                            arguments: vec![Expr::Literal(Literal::Integer(42))],
                        },
                        // This should NOT execute
                        Stmt::Assignment {
                            target: "Foo".to_string(),
                            value: Expr::Literal(Literal::Integer(99)),
                        },
                    ]),
                    visibility: FieldVisibility::Public,
                    is_external: false,
                    external_name: None,
                    is_inline: false,
                    is_forward: false,
                    is_class_method: false,
                    is_virtual: false,
                    is_override: false,
                    is_overload: false,
                }],
                classes: vec![],
                statements: vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::FunctionCall {
                        name: "Foo".to_string(),
                        arguments: vec![],
                    },
                }],
            },
        };

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(42));
    }

    // ========== Nested function scoping tests ==========

    #[test]
    fn test_nested_function_call() {
        use crate::ast::FunctionDecl;
        // function Outer: integer;
        //   function Inner: integer; begin Inner := 5; end;
        // begin Outer := Inner * 2; end;
        // x := Outer;
        let inner_func = FunctionDecl {
            name: "Inner".to_string(),
            parameters: vec![],
            return_type: AstType::Integer,
            block: Block::with_statements(vec![Stmt::Assignment {
                target: "Inner".to_string(),
                value: Expr::Literal(Literal::Integer(5)),
            }]),
            visibility: FieldVisibility::Public,
            is_external: false,
            external_name: None,
            is_inline: false,
            is_forward: false,
            is_class_method: false,
            is_virtual: false,
            is_override: false,
            is_overload: false,
        };

        let outer_block = Block {
            consts: vec![],
            types: vec![],
            vars: vec![],
            procedures: vec![],
            functions: vec![inner_func],
            classes: vec![],
            statements: vec![Stmt::Assignment {
                target: "Outer".to_string(),
                value: Expr::BinaryOp {
                    operator: "*".to_string(),
                    left: Box::new(Expr::FunctionCall {
                        name: "Inner".to_string(),
                        arguments: vec![],
                    }),
                    right: Box::new(Expr::Literal(Literal::Integer(2))),
                },
            }],
        };

        let prog = Program {
            name: "Test".to_string(),
            uses: vec![],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![var_decl("x")],
                procedures: vec![],
                functions: vec![FunctionDecl {
                    name: "Outer".to_string(),
                    parameters: vec![],
                    return_type: AstType::Integer,
                    block: outer_block,
                    visibility: FieldVisibility::Public,
                    is_external: false,
                    external_name: None,
                    is_inline: false,
                    is_forward: false,
                    is_class_method: false,
                    is_virtual: false,
                    is_override: false,
                    is_overload: false,
                }],
                classes: vec![],
                statements: vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::FunctionCall {
                        name: "Outer".to_string(),
                        arguments: vec![],
                    },
                }],
            },
        };

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(10));
    }

    // ========== Uses clause tests ==========

    #[test]
    fn test_uses_builtin_units_skipped() {
        // uses SysUtils, Classes — should not error
        let prog = Program {
            name: "Test".to_string(),
            uses: vec!["SysUtils".to_string(), "Classes".to_string()],
            block: Block {
                consts: vec![],
                types: vec![],
                vars: vec![var_decl("x")],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                statements: vec![Stmt::Assignment {
                    target: "x".to_string(),
                    value: Expr::Literal(Literal::Integer(1)),
                }],
            },
        };

        let mut interp = Interpreter::new(false);
        interp.run_program(&prog).unwrap();
        assert_eq!(interp.get_variable("x").unwrap(), Value::Integer(1));
    }
}
