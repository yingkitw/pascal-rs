//! Tree-walking interpreter for Pascal programs
//!
//! Executes Pascal AST directly without compilation.

use crate::ast::{Block, Expr, ForDirection, Literal, Program, Stmt};
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::io::{self, Write};

/// Runtime value
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Real(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Nil,
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

        // Register declared variables with default values
        self.declare_block_vars(&program.block)?;

        // Register user-defined functions/procedures
        self.register_block_functions(&program.block)?;

        // Execute statements
        for stmt in &program.block.statements {
            self.execute_stmt(stmt)?;
        }

        Ok(())
    }

    /// Declare variables from a block
    fn declare_block_vars(&mut self, block: &Block) -> Result<()> {
        for var_decl in &block.vars {
            let default = if let Some(init) = &var_decl.initial_value {
                self.eval_expr(init)?
            } else {
                Value::Integer(0) // Default
            };
            self.set_variable(&var_decl.name, default);
        }
        for const_decl in &block.consts {
            let val = self.literal_to_value(&const_decl.value);
            self.set_variable(&const_decl.name, val);
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

            Stmt::Empty => {}

            _ => {
                if self.verbose {
                    eprintln!("[interpreter] Unsupported statement: {:?}", stmt);
                }
            }
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
            _ => {
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

        // Bind parameters
        for (i, (param_name, _is_var)) in func.params.iter().enumerate() {
            let val = arg_values.get(i).cloned().unwrap_or(Value::Integer(0));
            self.set_variable(param_name, val);
        }

        // Declare local variables
        self.declare_block_vars(&func.body)?;

        // If it's a function, initialize the result variable
        if func.is_function {
            self.set_variable(&func.return_type_name, Value::Integer(0));
        }

        // Execute body
        for stmt in &func.body.statements {
            self.execute_stmt(stmt)?;
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
                    _ => Err(anyhow!("length() requires string argument")),
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
            _ => {
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
    fn get_variable(&self, name: &str) -> Result<Value> {
        let name_lower = name.to_lowercase();
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(&name_lower) {
                return Ok(val.clone());
            }
        }
        Err(anyhow!("Undefined variable: {}", name))
    }

    /// Set a variable value (in the innermost scope that has it, or current scope)
    fn set_variable(&mut self, name: &str, value: Value) {
        let name_lower = name.to_lowercase();
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
}
