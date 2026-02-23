//! Core formatting engine for Pascal code
//!
//! Traverses the AST and generates formatted source code according
//! to the specified configuration.

use crate::ast::*;
use crate::formatter::config::*;

/// Core formatter that converts AST nodes to formatted source code
pub struct Formatter {
    config: FormatConfig,
    output: String,
    indent_level: usize,
    current_line_start: usize,
    needs_indent: bool,
}

impl Formatter {
    /// Create a new formatter with the given configuration
    pub fn new(config: FormatConfig) -> Self {
        Self {
            config,
            output: String::new(),
            indent_level: 0,
            current_line_start: 0,
            needs_indent: true,
        }
    }

    /// Format a complete program
    pub fn format_program(&mut self, program: &Program) -> Result<String, anyhow::Error> {
        self.write_program(program)?;
        Ok(self.output.clone())
    }

    /// Format a complete unit
    pub fn format_unit(&mut self, unit: &Unit) -> Result<String, anyhow::Error> {
        self.write_unit(unit)?;
        Ok(self.output.clone())
    }

    /// Write indentation if needed
    fn write_indent(&mut self) -> Result<(), anyhow::Error> {
        if self.needs_indent {
            match self.config.indent_style {
                IndentStyle::Spaces => {
                    for _ in 0..(self.indent_level * self.config.indent_width) {
                        self.output.push(' ');
                    }
                }
                IndentStyle::Tabs => {
                    for _ in 0..self.indent_level {
                        self.output.push('\t');
                    }
                }
            }
            self.needs_indent = false;
            self.current_line_start = self.output.len();
        }
        Ok(())
    }

    /// Write a string with proper indentation handling
    fn write_str(&mut self, s: &str) -> Result<(), anyhow::Error> {
        for ch in s.chars() {
            if ch == '\n' {
                self.output.push('\n');
                self.needs_indent = true;
            } else {
                self.write_indent()?;
                self.output.push(ch);
            }
        }
        Ok(())
    }

    /// Write a space according to spacing rules
    fn write_space(&mut self) -> Result<(), anyhow::Error> {
        self.write_indent()?;
        self.output.push(' ');
        Ok(())
    }

    /// Write newline(s) according to blank line rules
    fn write_newline(&mut self, count: usize) -> Result<(), anyhow::Error> {
        for _ in 0..count {
            self.output.push('\n');
            self.needs_indent = true;
        }
        Ok(())
    }

    /// Write a keyword with proper spacing
    fn write_keyword(&mut self, keyword: &str) -> Result<(), anyhow::Error> {
        self.write_indent()?;
        self.output.push_str(keyword);
        Ok(())
    }

    /// Write an identifier
    fn write_identifier(&mut self, ident: &str) -> Result<(), anyhow::Error> {
        self.write_indent()?;
        self.output.push_str(ident);
        Ok(())
    }

    /// Write a string literal
    fn write_string_literal(&mut self, literal: &str) -> Result<(), anyhow::Error> {
        self.write_indent()?;
        self.output.push('\'');
        self.output.push_str(literal);
        self.output.push('\'');
        Ok(())
    }

    /// Write program structure
    fn write_program(&mut self, program: &Program) -> Result<(), anyhow::Error> {
        // Program header
        self.write_keyword("program")?;
        self.write_space()?;
        self.write_identifier(&program.name)?;
        self.output.push(';');
        self.write_newline(2)?;

        // Uses clause
        if !program.uses.is_empty() {
            self.write_keyword("uses")?;
            for (i, use_name) in program.uses.iter().enumerate() {
                if i > 0 {
                    self.output.push(',');
                    self.write_space()?;
                }
                self.write_identifier(use_name)?;
            }
            self.output.push(';');
            self.write_newline(2)?;
        }

        // Program block
        self.write_block(&program.block)?;

        self.output.push('.');
        self.write_newline(1)?;
        Ok(())
    }

    /// Write unit structure
    fn write_unit(&mut self, unit: &Unit) -> Result<(), anyhow::Error> {
        // Unit header
        self.write_keyword("unit")?;
        self.write_space()?;
        self.write_identifier(&unit.name)?;
        self.output.push(';');
        self.write_newline(2)?;

        // Interface section
        self.write_keyword("interface")?;
        self.write_newline(2)?;
        self.write_unit_interface(&unit.interface)?;

        // Implementation section
        self.write_keyword("implementation")?;
        self.write_newline(2)?;
        self.write_unit_implementation(&unit.implementation)?;

        // Unit termination
        self.write_keyword("end")?;
        self.output.push('.');
        self.write_newline(1)?;
        Ok(())
    }

    /// Write unit interface
    fn write_unit_interface(&mut self, interface: &UnitInterface) -> Result<(), anyhow::Error> {
        // Uses clause
        if !interface.uses.is_empty() {
            self.write_keyword("uses")?;
            for (i, use_name) in interface.uses.iter().enumerate() {
                if i > 0 {
                    self.output.push(',');
                    self.write_space()?;
                }
                self.write_identifier(use_name)?;
            }
            self.output.push(';');
            self.write_newline(2)?;
        }

        // Type declarations
        for type_decl in &interface.types {
            self.write_type_decl(type_decl)?;
            self.write_newline(self.config.blank_lines.between_declarations)?;
        }

        // Constant declarations
        for const_decl in &interface.constants {
            self.write_const_decl(const_decl)?;
            self.write_newline(self.config.blank_lines.between_declarations)?;
        }

        // Variable declarations
        for var_decl in &interface.variables {
            self.write_var_decl(var_decl)?;
            self.write_newline(self.config.blank_lines.between_declarations)?;
        }

        // Procedure declarations
        for proc_decl in &interface.procedures {
            self.write_proc_decl(proc_decl, true)?;
            self.write_newline(self.config.blank_lines.between_routines)?;
        }

        // Function declarations
        for func_decl in &interface.functions {
            self.write_func_decl(func_decl, true)?;
            self.write_newline(self.config.blank_lines.between_routines)?;
        }

        // Class declarations
        for class_decl in &interface.classes {
            self.write_class_decl(class_decl)?;
            self.write_newline(self.config.blank_lines.between_routines)?;
        }

        Ok(())
    }

    /// Write unit implementation
    fn write_unit_implementation(
        &mut self,
        impl_section: &UnitImplementation,
    ) -> Result<(), anyhow::Error> {
        // Uses clause
        if !impl_section.uses.is_empty() {
            self.write_keyword("uses")?;
            for (i, use_name) in impl_section.uses.iter().enumerate() {
                if i > 0 {
                    self.output.push(',');
                    self.write_space()?;
                }
                self.write_identifier(use_name)?;
            }
            self.output.push(';');
            self.write_newline(2)?;
        }

        // Similar to interface but with implementations
        // Implementation declarations would be written here
        // For now, we'll write a placeholder
        self.write_newline(1)?;
        Ok(())
    }

    /// Write block (begin...end)
    fn write_block(&mut self, block: &Block) -> Result<(), anyhow::Error> {
        // Block declarations
        for const_decl in &block.consts {
            self.write_keyword("const")?;
            self.write_space()?;
            self.write_const_decl(const_decl)?;
            self.write_newline(self.config.blank_lines.between_declarations)?;
        }

        for type_decl in &block.types {
            self.write_type_decl(type_decl)?;
            self.write_newline(self.config.blank_lines.between_declarations)?;
        }

        for var_decl in &block.vars {
            self.write_keyword("var")?;
            self.write_space()?;
            self.write_var_decl(var_decl)?;
            self.write_newline(self.config.blank_lines.between_declarations)?;
        }

        for proc_decl in &block.procedures {
            self.write_proc_decl(proc_decl, false)?;
            self.write_newline(self.config.blank_lines.between_routines)?;
        }

        for func_decl in &block.functions {
            self.write_func_decl(func_decl, false)?;
            self.write_newline(self.config.blank_lines.between_routines)?;
        }

        // Statements
        if !block.statements.is_empty() {
            self.write_keyword("begin")?;
            self.write_newline(1)?;

            self.indent_level += 1;

            for (i, statement) in block.statements.iter().enumerate() {
                self.write_statement(statement)?;

                if i < block.statements.len() - 1 {
                    self.output.push(';');
                    self.write_newline(1)?;
                }
            }

            self.indent_level -= 1;

            self.write_newline(1)?;
            self.write_keyword("end")?;
        }

        Ok(())
    }

    /// Write a statement
    fn write_statement(&mut self, stmt: &Statement) -> Result<(), anyhow::Error> {
        match stmt {
            Statement::Assignment { target, value } => {
                self.write_identifier(target)?;
                self.write_space()?;
                self.output.push_str(":=");
                self.write_space()?;
                self.write_expression(value)?;
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.write_keyword("if")?;
                self.write_space()?;
                self.write_expression(condition)?;
                self.write_space()?;
                self.write_keyword("then")?;

                // Handle then branch
                self.write_newline(1)?;
                self.indent_level += 1;
                for stmt in then_branch {
                    self.write_statement(stmt)?;
                    self.output.push(';');
                    self.write_newline(1)?;
                }
                self.indent_level -= 1;

                // Handle else branch
                if let Some(else_branch) = else_branch {
                    self.write_keyword("else")?;
                    self.write_newline(1)?;
                    self.indent_level += 1;
                    for stmt in else_branch {
                        self.write_statement(stmt)?;
                        self.output.push(';');
                        self.write_newline(1)?;
                    }
                    self.indent_level -= 1;
                }
            }
            Statement::While { condition, body } => {
                self.write_keyword("while")?;
                self.write_space()?;
                self.write_expression(condition)?;
                self.write_space()?;
                self.write_keyword("do")?;

                self.write_newline(1)?;
                self.indent_level += 1;
                for stmt in body {
                    self.write_statement(stmt)?;
                    self.output.push(';');
                    self.write_newline(1)?;
                }
                self.indent_level -= 1;
            }
            Statement::For {
                var_name,
                start,
                end,
                direction,
                body,
            } => {
                self.write_keyword("for")?;
                self.write_space()?;
                self.write_identifier(var_name)?;
                self.write_space()?;
                self.output.push_str(":=");
                self.write_space()?;
                self.write_expression(start)?;
                self.write_space()?;

                match direction {
                    ForDirection::To => self.write_keyword("to"),
                    ForDirection::DownTo => self.write_keyword("downto"),
                }?;

                self.write_space()?;
                self.write_expression(end)?;
                self.write_space()?;
                self.write_keyword("do")?;

                self.write_newline(1)?;
                self.indent_level += 1;
                for stmt in body {
                    self.write_statement(stmt)?;
                    self.output.push(';');
                    self.write_newline(1)?;
                }
                self.indent_level -= 1;
            }
            Statement::Block(block) => {
                self.write_block(block)?;
            }
            Statement::ProcedureCall { name, arguments } => {
                self.write_identifier(name)?;

                if !arguments.is_empty() {
                    self.output.push('(');

                    for (i, arg) in arguments.iter().enumerate() {
                        if i > 0 {
                            self.output.push(',');
                            self.write_space()?;
                        }
                        self.write_expression(arg)?;
                    }

                    self.output.push(')');
                }
            }
            Statement::Empty => {
                // Empty statement - no output
            }
            _ => {
                // Placeholder for other statement types
                self.write_indent()?;
                self.output
                    .push_str("// TODO: Implement statement formatting");
            }
        }

        Ok(())
    }

    /// Write an expression
    fn write_expression(&mut self, expr: &Expression) -> Result<(), anyhow::Error> {
        match expr {
            Expression::Variable(name) => {
                self.write_identifier(name)?;
            }
            Expression::Literal(literal) => match literal {
                Literal::Integer(value) => {
                    self.write_indent()?;
                    self.output.push_str(&value.to_string());
                }
                Literal::String(value) => {
                    self.write_string_literal(value)?;
                }
                _ => {
                    self.output.push_str("/* TODO: literal */");
                }
            },
            Expression::BinaryOp {
                left,
                operator,
                right,
            } => {
                self.write_expression(left)?;

                if self.config.operator_spacing.arithmetic
                    || self.config.operator_spacing.comparison
                    || self.config.operator_spacing.logical
                {
                    self.write_space()?;
                }

                self.output.push_str(operator);

                if self.config.operator_spacing.arithmetic
                    || self.config.operator_spacing.comparison
                    || self.config.operator_spacing.logical
                {
                    self.write_space()?;
                }

                self.write_expression(right)?;
            }
            Expression::UnaryOp { operator, operand } => {
                self.output.push_str(operator);
                self.write_expression(operand)?;
            }
            _ => {
                // Placeholder for other expression types
                self.output.push_str("/* TODO: expression */");
            }
        }

        Ok(())
    }

    /// Write type declaration
    fn write_type_decl(&mut self, type_decl: &TypeDecl) -> Result<(), anyhow::Error> {
        self.write_identifier(&type_decl.name)?;

        if self.config.operator_spacing.colon {
            self.write_space()?;
        }
        self.output.push(':');
        self.write_space()?;
        self.write_type(&type_decl.type_definition)?;

        self.output.push(';');
        Ok(())
    }

    /// Write constant declaration
    fn write_const_decl(&mut self, const_decl: &ConstDecl) -> Result<(), anyhow::Error> {
        self.write_identifier(&const_decl.name)?;

        if self.config.operator_spacing.assignment {
            self.write_space()?;
        }
        self.output.push('=');

        if self.config.operator_spacing.assignment {
            self.write_space()?;
        }

        match &const_decl.value {
            Literal::Integer(val) => {
                self.output.push_str(&val.to_string());
            }
            Literal::String(val) => {
                self.write_string_literal(val)?;
            }
            _ => {
                self.output.push_str("/* TODO: const value */");
            }
        }

        self.output.push(';');
        Ok(())
    }

    /// Write variable declaration
    fn write_var_decl(&mut self, var_decl: &VariableDecl) -> Result<(), anyhow::Error> {
        self.write_identifier(&var_decl.name)?;

        if self.config.operator_spacing.colon {
            self.write_space()?;
        }
        self.output.push(':');
        self.write_space()?;
        self.write_type(&var_decl.variable_type)?;
        self.output.push(';');
        Ok(())
    }

    /// Write procedure declaration
    fn write_proc_decl(
        &mut self,
        proc_decl: &ProcedureDecl,
        interface_only: bool,
    ) -> Result<(), anyhow::Error> {
        self.write_keyword("procedure")?;
        self.write_space()?;
        self.write_identifier(&proc_decl.name)?;

        // Parameters
        self.write_parameters(&proc_decl.parameters)?;

        if interface_only {
            self.output.push(';');
            self.write_newline(1)?;
        } else {
            self.output.push(';');
            self.write_newline(1)?;

            // Procedure body would be written here
            if !proc_decl.is_forward {
                self.write_block(&proc_decl.block)?;
            } else {
                self.output.push_str(" forward;");
            }
        }

        Ok(())
    }

    /// Write function declaration
    fn write_func_decl(
        &mut self,
        func_decl: &FunctionDecl,
        interface_only: bool,
    ) -> Result<(), anyhow::Error> {
        self.write_keyword("function")?;
        self.write_space()?;
        self.write_identifier(&func_decl.name)?;

        // Parameters
        self.write_parameters(&func_decl.parameters)?;

        if self.config.operator_spacing.colon {
            self.write_space()?;
        }
        self.output.push(':');
        self.write_space()?;
        self.write_type(&func_decl.return_type)?;

        if interface_only {
            self.output.push(';');
            self.write_newline(1)?;
        } else {
            self.output.push(';');
            self.write_newline(1)?;

            // Function body would be written here
            if !func_decl.is_forward {
                self.write_block(&func_decl.block)?;
            } else {
                self.output.push_str(" forward;");
            }
        }

        Ok(())
    }

    /// Write class declaration
    fn write_class_decl(&mut self, class_decl: &ClassDecl) -> Result<(), anyhow::Error> {
        self.write_keyword("type")?;
        self.write_space()?;
        self.write_identifier(&class_decl.name)?;
        self.write_space()?;
        self.write_keyword("class")?;

        self.write_newline(1)?;
        self.indent_level += 1;

        // Class members would be written here

        self.indent_level -= 1;
        self.write_keyword("end")?;
        self.output.push(';');
        Ok(())
    }

    /// Write parameter list
    fn write_parameters(&mut self, params: &[Parameter]) -> Result<(), anyhow::Error> {
        if !params.is_empty() {
            self.output.push('(');

            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    self.output.push(';');
                    self.write_space()?;
                }

                self.write_identifier(&param.name)?;

                if self.config.operator_spacing.colon {
                    self.write_space()?;
                }
                self.output.push(':');
                self.write_space()?;
                self.write_type(&param.param_type)?;
            }

            self.output.push(')');
        }
        Ok(())
    }

    /// Write type definition
    fn write_type(&mut self, type_def: &Type) -> Result<(), anyhow::Error> {
        match type_def {
            Type::Simple(simple) => match simple {
                SimpleType::Integer => self.output.push_str("Integer"),
                SimpleType::Real => self.output.push_str("Real"),
                SimpleType::Boolean => self.output.push_str("Boolean"),
                SimpleType::Char => self.output.push_str("Char"),
                SimpleType::String => self.output.push_str("String"),
            },
            Type::Integer => self.output.push_str("Integer"),
            Type::Real => self.output.push_str("Real"),
            Type::Boolean => self.output.push_str("Boolean"),
            Type::Char => self.output.push_str("Char"),
            Type::String => self.output.push_str("String"),
            Type::Alias { name, .. } => {
                self.write_identifier(name)?;
            }
            Type::Array { element_type, .. } => {
                self.output.push_str("array");
                self.write_space()?;
                self.output.push_str("of");
                self.write_space()?;
                self.write_type(element_type)?;
            }
            Type::Record { fields, .. } => {
                self.output.push_str("record");
                self.write_newline(1)?;
                self.indent_level += 1;

                for (name, field_type) in fields {
                    self.write_identifier(name)?;
                    if self.config.operator_spacing.colon {
                        self.write_space()?;
                    }
                    self.output.push(':');
                    self.write_space()?;
                    self.write_type(field_type)?;
                    self.output.push(';');
                    self.write_newline(1)?;
                }

                self.indent_level -= 1;
                self.write_keyword("end")?;
            }
            _ => {
                self.output.push_str("/* TODO: type */");
            }
        }

        Ok(())
    }
}
