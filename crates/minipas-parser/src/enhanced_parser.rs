use crate::traits::*;
use minipas_ast::enhanced_ast::*;
use minipas_lexer::{EnhancedLexer, EnhancedToken, EnhancedLexerCapability};
use std::collections::HashMap;

/// Enhanced parser based on Free Pascal Compiler parsing logic
/// Provides comprehensive Pascal language support
pub struct EnhancedParser<'a> {
    lexer: EnhancedLexer<'a>,
    current_token: Option<(usize, EnhancedToken, usize)>,
    errors: Vec<String>,
    warnings: Vec<String>,
    symbol_table: HashMap<String, SymbolInfo>,
    current_scope: Vec<String>,
    type_table: HashMap<String, EnhancedType>,
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub symbol_type: SymbolType,
    pub scope_level: usize,
    pub is_forward: bool,
    pub is_external: bool,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub enum SymbolType {
    Variable(EnhancedType),
    Constant(Expression),
    Type(EnhancedType),
    Procedure(Vec<Parameter>, Option<EnhancedType>),
    Function(Vec<Parameter>, EnhancedType),
    Class(String),
    Interface(String),
    Unit(String),
}

impl<'a> EnhancedParser<'a> {
    /// Creates a new enhanced parser
    pub fn new(input: &'a str) -> Self {
        let mut parser = Self {
            lexer: EnhancedLexer::new(input),
            current_token: None,
            errors: Vec::new(),
            warnings: Vec::new(),
            symbol_table: HashMap::new(),
            current_scope: Vec::new(),
            type_table: HashMap::new(),
        };
        
        // Initialize built-in types
        parser.initialize_builtin_types();
        parser
    }
    
    /// Initializes built-in types
    fn initialize_builtin_types(&mut self) {
        self.type_table.insert("integer".to_string(), EnhancedType::Integer);
        self.type_table.insert("real".to_string(), EnhancedType::Real);
        self.type_table.insert("boolean".to_string(), EnhancedType::Boolean);
        self.type_table.insert("char".to_string(), EnhancedType::Char);
        self.type_table.insert("string".to_string(), EnhancedType::String);
        self.type_table.insert("widestring".to_string(), EnhancedType::WideString);
    }
    
    /// Parses a complete Pascal program
    pub fn parse_program(&mut self) -> Result<EnhancedAst, String> {
        self.advance()?;
        
        match self.current_token.as_ref() {
            Some((_, EnhancedToken::Program, _)) => {
                self.parse_program_declaration()
            }
            Some((_, EnhancedToken::Unit, _)) => {
                self.parse_unit_declaration()
            }
            Some((_, EnhancedToken::Library, _)) => {
                self.parse_library_declaration()
            }
            Some((_, EnhancedToken::Package, _)) => {
                self.parse_package_declaration()
            }
            _ => Err("Expected program, unit, library, or package declaration".to_string())
        }
    }
    
    /// Parses a program declaration
    fn parse_program_declaration(&mut self) -> Result<EnhancedAst, String> {
        // Parse 'program' keyword
        self.expect_token(EnhancedToken::Program)?;
        
        // Parse program name
        let name = self.expect_identifier()?;
        
        // Parse optional program parameters
        let program_params = if self.check_token(EnhancedToken::LeftParen) {
            self.parse_program_parameters()?
        } else {
            Vec::new()
        };
        
        // Parse semicolon
        self.expect_token(EnhancedToken::Semicolon)?;
        
        // Parse uses clause
        let uses = if self.check_token(EnhancedToken::Uses) {
            self.parse_uses_clause()?
        } else {
            Vec::new()
        };
        
        // Parse program block
        let block = self.parse_block()?;
        
        // Parse final period
        self.expect_token(EnhancedToken::Point)?;
        
        Ok(EnhancedAst::Program(Program {
            name,
            uses,
            block,
        }))
    }
    
    /// Parses a unit declaration
    fn parse_unit_declaration(&mut self) -> Result<EnhancedAst, String> {
        // Parse 'unit' keyword
        self.expect_token(EnhancedToken::Unit)?;
        
        // Parse unit name
        let name = self.expect_identifier()?;
        
        // Parse semicolon
        self.expect_token(EnhancedToken::Semicolon)?;
        
        // Parse interface section
        let interface_section = if self.check_token(EnhancedToken::Interface) {
            Some(self.parse_interface_section()?)
        } else {
            None
        };
        
        // Parse implementation section
        let implementation_section = if self.check_token(EnhancedToken::Implementation) {
            Some(self.parse_implementation_section()?)
        } else {
            None
        };
        
        // Parse initialization section
        let initialization_section = if self.check_token(EnhancedToken::Begin) {
            Some(self.parse_block()?)
        } else {
            None
        };
        
        // Parse finalization section
        let finalization_section = if self.check_token(EnhancedToken::Final) {
            Some(self.parse_block()?)
        } else {
            None
        };
        
        // Parse final period
        self.expect_token(EnhancedToken::Point)?;
        
        Ok(EnhancedAst::Unit(Unit {
            name,
            uses: Vec::new(), // TODO: Parse uses clause
            interface_section,
            implementation_section,
            initialization_section,
            finalization_section,
        }))
    }
    
    /// Parses a library declaration
    fn parse_library_declaration(&mut self) -> Result<EnhancedAst, String> {
        // Parse 'library' keyword
        self.expect_token(EnhancedToken::Library)?;
        
        // Parse library name
        let name = self.expect_identifier()?;
        
        // Parse semicolon
        self.expect_token(EnhancedToken::Semicolon)?;
        
        // Parse uses clause
        let uses = if self.check_token(EnhancedToken::Uses) {
            self.parse_uses_clause()?
        } else {
            Vec::new()
        };
        
        // Parse exports clause
        let exports = if self.check_token(EnhancedToken::Exports) {
            self.parse_exports_clause()?
        } else {
            Vec::new()
        };
        
        // Parse library block
        let block = self.parse_block()?;
        
        // Parse final period
        self.expect_token(EnhancedToken::Point)?;
        
        Ok(EnhancedAst::Library(Library {
            name,
            uses,
            exports,
            block,
        }))
    }
    
    /// Parses a package declaration
    fn parse_package_declaration(&mut self) -> Result<EnhancedAst, String> {
        // Parse 'package' keyword
        self.expect_token(EnhancedToken::Package)?;
        
        // Parse package name
        let name = self.expect_identifier()?;
        
        // Parse semicolon
        self.expect_token(EnhancedToken::Semicolon)?;
        
        // Parse requires clause
        let requires = if self.check_token(EnhancedToken::Requires) {
            self.parse_requires_clause()?
        } else {
            Vec::new()
        };
        
        // Parse contains clause
        let contains = if self.check_token(EnhancedToken::Contains) {
            self.parse_contains_clause()?
        } else {
            Vec::new()
        };
        
        // Parse package block
        let block = self.parse_block()?;
        
        // Parse final period
        self.expect_token(EnhancedToken::Point)?;
        
        Ok(EnhancedAst::Package(Package {
            name,
            uses: Vec::new(), // TODO: Parse uses clause
            requires,
            contains,
            block,
        }))
    }
    
    /// Parses a block (const, type, var, procedure/function declarations, statements)
    fn parse_block(&mut self) -> Result<Block, String> {
        let mut block = Block {
            consts: Vec::new(),
            types: Vec::new(),
            vars: Vec::new(),
            procedures: Vec::new(),
            functions: Vec::new(),
            statements: Vec::new(),
            labels: Vec::new(),
        };
        
        // Parse constant declarations
        while self.check_token(EnhancedToken::Const) {
            self.advance()?;
            let consts = self.parse_constant_declarations()?;
            block.consts.extend(consts);
        }
        
        // Parse type declarations
        while self.check_token(EnhancedToken::Type) {
            self.advance()?;
            let types = self.parse_type_declarations()?;
            block.types.extend(types);
        }
        
        // Parse variable declarations
        while self.check_token(EnhancedToken::Var) {
            self.advance()?;
            let vars = self.parse_variable_declarations()?;
            block.vars.extend(vars);
        }
        
        // Parse procedure/function declarations
        while self.check_token(EnhancedToken::Procedure) || self.check_token(EnhancedToken::Function) {
            if self.check_token(EnhancedToken::Procedure) {
                self.advance()?;
                let procedure = self.parse_procedure_declaration()?;
                block.procedures.push(procedure);
            } else {
                self.advance()?;
                let function = self.parse_function_declaration()?;
                block.functions.push(function);
            }
        }
        
        // Parse statements
        if self.check_token(EnhancedToken::Begin) {
            self.advance()?;
            let statements = self.parse_statement_list()?;
            block.statements = statements;
            self.expect_token(EnhancedToken::End)?;
        }
        
        Ok(block)
    }
    
    /// Parses constant declarations
    fn parse_constant_declarations(&mut self) -> Result<Vec<Constant>, String> {
        let mut constants = Vec::new();
        
        loop {
            let name = self.expect_identifier()?;
            
            let constant_type = if self.check_token(EnhancedToken::Colon) {
                self.advance()?;
                Some(self.parse_type()?)
            } else {
                None
            };
            
            self.expect_token(EnhancedToken::Equal)?;
            let value = self.parse_expression()?;
            self.expect_token(EnhancedToken::Semicolon)?;
            
            constants.push(Constant {
                name,
                constant_type,
                value,
            });
            
            if !self.check_token(EnhancedToken::Identifier("".to_string())) {
                break;
            }
        }
        
        Ok(constants)
    }
    
    /// Parses type declarations
    fn parse_type_declarations(&mut self) -> Result<Vec<TypeDefinition>, String> {
        let mut types = Vec::new();
        
        loop {
            let name = self.expect_identifier()?;
            self.expect_token(EnhancedToken::Equal)?;
            let type_definition = self.parse_type()?;
            self.expect_token(EnhancedToken::Semicolon)?;
            
            types.push(TypeDefinition {
                name,
                type_definition,
            });
            
            if !self.check_token(EnhancedToken::Identifier("".to_string())) {
                break;
            }
        }
        
        Ok(types)
    }
    
    /// Parses variable declarations
    fn parse_variable_declarations(&mut self) -> Result<Vec<Variable>, String> {
        let mut variables = Vec::new();
        
        loop {
            let mut var_names = Vec::new();
            
            // Parse variable names
            loop {
                let name = self.expect_identifier()?;
                var_names.push(name);
                
                if self.check_token(EnhancedToken::Comma) {
                    self.advance()?;
                } else {
                    break;
                }
            }
            
            self.expect_token(EnhancedToken::Colon)?;
            let variable_type = self.parse_type()?;
            
            let initial_value = if self.check_token(EnhancedToken::Equal) {
                self.advance()?;
                Some(self.parse_expression()?)
            } else {
                None
            };
            
            self.expect_token(EnhancedToken::Semicolon)?;
            
            // Create variable for each name
            for name in var_names {
                variables.push(Variable {
                    name,
                    variable_type: variable_type.clone(),
                    initial_value: initial_value.clone(),
                    is_absolute: false,
                    absolute_address: None,
                    is_external: false,
                    external_name: None,
                    is_public: false,
                    is_exported: false,
                });
            }
            
            if !self.check_token(EnhancedToken::Identifier("".to_string())) {
                break;
            }
        }
        
        Ok(variables)
    }
    
    /// Parses a type
    fn parse_type(&mut self) -> Result<EnhancedType, String> {
        match self.current_token.as_ref() {
            Some((_, EnhancedToken::Identifier(name), _)) => {
                self.advance()?;
                if let Some(&typ) = self.type_table.get(name) {
                    Ok(typ.clone())
                } else {
                    Ok(EnhancedType::Alias {
                        name: name.clone(),
                        target_type: Box::new(EnhancedType::Custom(name.clone())),
                    })
                }
            }
            Some((_, EnhancedToken::Array, _)) => {
                self.parse_array_type()
            }
            Some((_, EnhancedToken::Record, _)) => {
                self.parse_record_type()
            }
            Some((_, EnhancedToken::Set, _)) => {
                self.parse_set_type()
            }
            Some((_, EnhancedToken::File, _)) => {
                self.parse_file_type()
            }
            Some((_, EnhancedToken::Pointer, _)) => {
                self.parse_pointer_type()
            }
            Some((_, EnhancedToken::Class, _)) => {
                self.parse_class_type()
            }
            Some((_, EnhancedToken::Object, _)) => {
                self.parse_object_type()
            }
            Some((_, EnhancedToken::Interface, _)) => {
                self.parse_interface_type()
            }
            _ => Err("Expected type".to_string())
        }
    }
    
    /// Parses an array type
    fn parse_array_type(&mut self) -> Result<EnhancedType, String> {
        self.expect_token(EnhancedToken::Array)?;
        self.expect_token(EnhancedToken::LeftBracket)?;
        
        let mut dimensions = Vec::new();
        
        loop {
            let lower_bound = if self.check_token(EnhancedToken::Identifier("".to_string())) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            
            self.expect_token(EnhancedToken::PointPoint)?;
            
            let upper_bound = if self.check_token(EnhancedToken::Identifier("".to_string())) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            
            dimensions.push(ArrayDimension {
                lower_bound,
                upper_bound,
            });
            
            if self.check_token(EnhancedToken::Comma) {
                self.advance()?;
            } else {
                break;
            }
        }
        
        self.expect_token(EnhancedToken::RightBracket)?;
        self.expect_token(EnhancedToken::Of)?;
        
        let element_type = Box::new(self.parse_type()?);
        
        Ok(EnhancedType::Array {
            element_type,
            dimensions,
        })
    }
    
    /// Parses a record type
    fn parse_record_type(&mut self) -> Result<EnhancedType, String> {
        self.expect_token(EnhancedToken::Record)?;
        
        let is_packed = if self.check_token(EnhancedToken::Packed) {
            self.advance()?;
            true
        } else {
            false
        };
        
        let mut fields = Vec::new();
        
        while !self.check_token(EnhancedToken::End) {
            let field = self.parse_record_field()?;
            fields.push(field);
            
            if self.check_token(EnhancedToken::Semicolon) {
                self.advance()?;
            }
        }
        
        self.expect_token(EnhancedToken::End)?;
        
        Ok(EnhancedType::Record {
            fields,
            is_packed,
        })
    }
    
    /// Parses a record field
    fn parse_record_field(&mut self) -> Result<RecordField, String> {
        let name = self.expect_identifier()?;
        self.expect_token(EnhancedToken::Colon)?;
        let field_type = self.parse_type()?;
        
        Ok(RecordField {
            name,
            field_type,
            offset: None,
            case_variant: None,
        })
    }
    
    /// Parses a set type
    fn parse_set_type(&mut self) -> Result<EnhancedType, String> {
        self.expect_token(EnhancedToken::Set)?;
        self.expect_token(EnhancedToken::Of)?;
        
        let base_type = Box::new(self.parse_type()?);
        
        Ok(EnhancedType::Set {
            base_type,
        })
    }
    
    /// Parses a file type
    fn parse_file_type(&mut self) -> Result<EnhancedType, String> {
        self.expect_token(EnhancedToken::File)?;
        
        let element_type = if self.check_token(EnhancedToken::Of) {
            self.advance()?;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };
        
        Ok(EnhancedType::File {
            element_type,
        })
    }
    
    /// Parses a pointer type
    fn parse_pointer_type(&mut self) -> Result<EnhancedType, String> {
        self.expect_token(EnhancedToken::Caret)?;
        
        let target_type = Box::new(self.parse_type()?);
        
        Ok(EnhancedType::Pointer {
            target_type,
        })
    }
    
    /// Parses a class type
    fn parse_class_type(&mut self) -> Result<EnhancedType, String> {
        self.expect_token(EnhancedToken::Class)?;
        
        let name = self.expect_identifier()?;
        
        let parent = if self.check_token(EnhancedToken::LeftParen) {
            self.advance()?;
            let parent_name = self.expect_identifier()?;
            self.expect_token(EnhancedToken::RightParen)?;
            Some(parent_name)
        } else {
            None
        };
        
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut properties = Vec::new();
        
        if self.check_token(EnhancedToken::LeftParen) {
            self.advance()?;
            
            while !self.check_token(EnhancedToken::RightParen) {
                // Parse class members
                // TODO: Implement class member parsing
                self.advance()?;
            }
            
            self.expect_token(EnhancedToken::RightParen)?;
        }
        
        Ok(EnhancedType::Class {
            name,
            parent,
            fields,
            methods,
            properties,
            visibility: Visibility::Public,
        })
    }
    
    /// Parses an object type
    fn parse_object_type(&mut self) -> Result<EnhancedType, String> {
        self.expect_token(EnhancedToken::Object)?;
        
        let name = self.expect_identifier()?;
        
        let parent = if self.check_token(EnhancedToken::LeftParen) {
            self.advance()?;
            let parent_name = self.expect_identifier()?;
            self.expect_token(EnhancedToken::RightParen)?;
            Some(parent_name)
        } else {
            None
        };
        
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        
        if self.check_token(EnhancedToken::LeftParen) {
            self.advance()?;
            
            while !self.check_token(EnhancedToken::RightParen) {
                // Parse object members
                // TODO: Implement object member parsing
                self.advance()?;
            }
            
            self.expect_token(EnhancedToken::RightParen)?;
        }
        
        Ok(EnhancedType::Object {
            name,
            parent,
            fields,
            methods,
            visibility: Visibility::Public,
        })
    }
    
    /// Parses an interface type
    fn parse_interface_type(&mut self) -> Result<EnhancedType, String> {
        self.expect_token(EnhancedToken::Interface)?;
        
        let name = self.expect_identifier()?;
        
        let parent = if self.check_token(EnhancedToken::LeftParen) {
            self.advance()?;
            let parent_name = self.expect_identifier()?;
            self.expect_token(EnhancedToken::RightParen)?;
            Some(parent_name)
        } else {
            None
        };
        
        let mut methods = Vec::new();
        let mut properties = Vec::new();
        
        if self.check_token(EnhancedToken::LeftParen) {
            self.advance()?;
            
            while !self.check_token(EnhancedToken::RightParen) {
                // Parse interface members
                // TODO: Implement interface member parsing
                self.advance()?;
            }
            
            self.expect_token(EnhancedToken::RightParen)?;
        }
        
        Ok(EnhancedType::Interface {
            name,
            parent,
            methods,
            properties,
        })
    }
    
    /// Parses a procedure declaration
    fn parse_procedure_declaration(&mut self) -> Result<Procedure, String> {
        let name = self.expect_identifier()?;
        
        let parameters = if self.check_token(EnhancedToken::LeftParen) {
            self.parse_parameter_list()?
        } else {
            Vec::new()
        };
        
        self.expect_token(EnhancedToken::Semicolon)?;
        
        let body = if self.check_token(EnhancedToken::Begin) {
            Some(self.parse_block()?)
        } else {
            None
        };
        
        Ok(Procedure {
            name,
            parameters,
            calling_convention: CallingConvention::Default,
            is_forward: body.is_none(),
            is_external: false,
            external_name: None,
            is_public: false,
            is_exported: false,
            is_inline: false,
            is_assembler: false,
            body,
        })
    }
    
    /// Parses a function declaration
    fn parse_function_declaration(&mut self) -> Result<Function, String> {
        let name = self.expect_identifier()?;
        
        let parameters = if self.check_token(EnhancedToken::LeftParen) {
            self.parse_parameter_list()?
        } else {
            Vec::new()
        };
        
        self.expect_token(EnhancedToken::Colon)?;
        let return_type = self.parse_type()?;
        self.expect_token(EnhancedToken::Semicolon)?;
        
        let body = if self.check_token(EnhancedToken::Begin) {
            Some(self.parse_block()?)
        } else {
            None
        };
        
        Ok(Function {
            name,
            parameters,
            return_type,
            calling_convention: CallingConvention::Default,
            is_forward: body.is_none(),
            is_external: false,
            external_name: None,
            is_public: false,
            is_exported: false,
            is_inline: false,
            is_assembler: false,
            body,
        })
    }
    
    /// Parses a parameter list
    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, String> {
        self.expect_token(EnhancedToken::LeftParen)?;
        
        let mut parameters = Vec::new();
        
        while !self.check_token(EnhancedToken::RightParen) {
            let mut param_names = Vec::new();
            
            // Parse parameter names
            loop {
                let name = self.expect_identifier()?;
                param_names.push(name);
                
                if self.check_token(EnhancedToken::Comma) {
                    self.advance()?;
                } else {
                    break;
                }
            }
            
            let is_var = if self.check_token(EnhancedToken::Var) {
                self.advance()?;
                true
            } else {
                false
            };
            
            let is_const = if self.check_token(EnhancedToken::Const) {
                self.advance()?;
                true
            } else {
                false
            };
            
            let is_out = if self.check_token(EnhancedToken::Out) {
                self.advance()?;
                true
            } else {
                false
            };
            
            self.expect_token(EnhancedToken::Colon)?;
            let parameter_type = self.parse_type()?;
            
            let default_value = if self.check_token(EnhancedToken::Equal) {
                self.advance()?;
                Some(self.parse_expression()?)
            } else {
                None
            };
            
            // Create parameter for each name
            for name in param_names {
                parameters.push(Parameter {
                    name,
                    parameter_type: parameter_type.clone(),
                    is_var,
                    is_const,
                    is_out,
                    default_value: default_value.clone(),
                    is_array_of_const: false,
                });
            }
            
            if self.check_token(EnhancedToken::Semicolon) {
                self.advance()?;
            }
        }
        
        self.expect_token(EnhancedToken::RightParen)?;
        
        Ok(parameters)
    }
    
    /// Parses a statement list
    fn parse_statement_list(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        
        while !self.check_token(EnhancedToken::End) {
            let statement = self.parse_statement()?;
            statements.push(statement);
            
            if self.check_token(EnhancedToken::Semicolon) {
                self.advance()?;
            }
        }
        
        Ok(statements)
    }
    
    /// Parses a statement
    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.current_token.as_ref() {
            Some((_, EnhancedToken::Begin, _)) => {
                self.advance()?;
                let statements = self.parse_statement_list()?;
                self.expect_token(EnhancedToken::End)?;
                Ok(Statement::Block(Block {
                    consts: Vec::new(),
                    types: Vec::new(),
                    vars: Vec::new(),
                    procedures: Vec::new(),
                    functions: Vec::new(),
                    statements,
                    labels: Vec::new(),
                }))
            }
            Some((_, EnhancedToken::If, _)) => {
                self.parse_if_statement()
            }
            Some((_, EnhancedToken::While, _)) => {
                self.parse_while_statement()
            }
            Some((_, EnhancedToken::Repeat, _)) => {
                self.parse_repeat_statement()
            }
            Some((_, EnhancedToken::For, _)) => {
                self.parse_for_statement()
            }
            Some((_, EnhancedToken::Case, _)) => {
                self.parse_case_statement()
            }
            Some((_, EnhancedToken::Try, _)) => {
                self.parse_try_statement()
            }
            Some((_, EnhancedToken::With, _)) => {
                self.parse_with_statement()
            }
            Some((_, EnhancedToken::Goto, _)) => {
                self.parse_goto_statement()
            }
            Some((_, EnhancedToken::Exit, _)) => {
                self.parse_exit_statement()
            }
            Some((_, EnhancedToken::Break, _)) => {
                self.advance()?;
                Ok(Statement::Break)
            }
            Some((_, EnhancedToken::Continue, _)) => {
                self.advance()?;
                Ok(Statement::Continue)
            }
            Some((_, EnhancedToken::Halt, _)) => {
                self.parse_halt_statement()
            }
            _ => {
                // Try to parse assignment or procedure call
                self.parse_assignment_or_call()
            }
        }
    }
    
    /// Parses an if statement
    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::If)?;
        let condition = self.parse_expression()?;
        self.expect_token(EnhancedToken::Then)?;
        let then_branch = self.parse_block()?;
        
        let else_branch = if self.check_token(EnhancedToken::Else) {
            self.advance()?;
            Some(self.parse_block()?)
        } else {
            None
        };
        
        Ok(Statement::If {
            condition,
            then_branch,
            else_branch,
        })
    }
    
    /// Parses a while statement
    fn parse_while_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::While)?;
        let condition = self.parse_expression()?;
        self.expect_token(EnhancedToken::Do)?;
        let body = self.parse_block()?;
        
        Ok(Statement::While {
            condition,
            body,
        })
    }
    
    /// Parses a repeat statement
    fn parse_repeat_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::Repeat)?;
        let body = self.parse_block()?;
        self.expect_token(EnhancedToken::Until)?;
        let condition = self.parse_expression()?;
        
        Ok(Statement::Repeat {
            body,
            condition,
        })
    }
    
    /// Parses a for statement
    fn parse_for_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::For)?;
        let variable = self.expect_identifier()?;
        self.expect_token(EnhancedToken::Assignment)?;
        let start_value = self.parse_expression()?;
        
        let direction = if self.check_token(EnhancedToken::To) {
            self.advance()?;
            ForDirection::To
        } else if self.check_token(EnhancedToken::Downto) {
            self.advance()?;
            ForDirection::Downto
        } else {
            return Err("Expected 'to' or 'downto'".to_string());
        };
        
        let end_value = self.parse_expression()?;
        self.expect_token(EnhancedToken::Do)?;
        let body = self.parse_block()?;
        
        Ok(Statement::For {
            variable,
            start_value,
            end_value,
            step: None,
            body,
            direction,
        })
    }
    
    /// Parses a case statement
    fn parse_case_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::Case)?;
        let expression = self.parse_expression()?;
        self.expect_token(EnhancedToken::Of)?;
        
        let mut cases = Vec::new();
        
        while !self.check_token(EnhancedToken::End) && !self.check_token(EnhancedToken::Else) {
            let values = self.parse_case_values()?;
            self.expect_token(EnhancedToken::Colon)?;
            let body = self.parse_block()?;
            
            cases.push(CaseBranch {
                values,
                body,
            });
            
            if self.check_token(EnhancedToken::Semicolon) {
                self.advance()?;
            }
        }
        
        let else_branch = if self.check_token(EnhancedToken::Else) {
            self.advance()?;
            Some(self.parse_block()?)
        } else {
            None
        };
        
        self.expect_token(EnhancedToken::End)?;
        
        Ok(Statement::Case {
            expression,
            cases,
            else_branch,
        })
    }
    
    /// Parses case values
    fn parse_case_values(&mut self) -> Result<Vec<Expression>, String> {
        let mut values = Vec::new();
        
        loop {
            let value = self.parse_expression()?;
            values.push(value);
            
            if self.check_token(EnhancedToken::Comma) {
                self.advance()?;
            } else {
                break;
            }
        }
        
        Ok(values)
    }
    
    /// Parses a try statement
    fn parse_try_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::Try)?;
        let body = self.parse_block()?;
        
        let except_clauses = if self.check_token(EnhancedToken::Except) {
            self.advance()?;
            self.parse_except_clauses()?
        } else {
            Vec::new()
        };
        
        let finally_clause = if self.check_token(EnhancedToken::Finally) {
            self.advance()?;
            Some(self.parse_block()?)
        } else {
            None
        };
        
        Ok(Statement::Try {
            body,
            except_clauses,
            finally_clause,
        })
    }
    
    /// Parses except clauses
    fn parse_except_clauses(&mut self) -> Result<Vec<ExceptClause>, String> {
        let mut clauses = Vec::new();
        
        while !self.check_token(EnhancedToken::End) && !self.check_token(EnhancedToken::Finally) {
            let exception_type = if self.check_token(EnhancedToken::Identifier("".to_string())) {
                Some(self.expect_identifier()?)
            } else {
                None
            };
            
            let variable = if self.check_token(EnhancedToken::Colon) {
                self.advance()?;
                Some(self.expect_identifier()?)
            } else {
                None
            };
            
            self.expect_token(EnhancedToken::Do)?;
            let body = self.parse_block()?;
            
            clauses.push(ExceptClause {
                exception_type,
                variable,
                body,
            });
            
            if self.check_token(EnhancedToken::Semicolon) {
                self.advance()?;
            }
        }
        
        Ok(clauses)
    }
    
    /// Parses a with statement
    fn parse_with_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::With)?;
        let expressions = self.parse_with_expressions()?;
        self.expect_token(EnhancedToken::Do)?;
        let body = self.parse_block()?;
        
        Ok(Statement::With {
            expressions,
            body,
        })
    }
    
    /// Parses with expressions
    fn parse_with_expressions(&mut self) -> Result<Vec<Expression>, String> {
        let mut expressions = Vec::new();
        
        loop {
            let expr = self.parse_expression()?;
            expressions.push(expr);
            
            if self.check_token(EnhancedToken::Comma) {
                self.advance()?;
            } else {
                break;
            }
        }
        
        Ok(expressions)
    }
    
    /// Parses a goto statement
    fn parse_goto_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::Goto)?;
        let label = self.expect_identifier()?;
        
        Ok(Statement::Goto {
            label,
        })
    }
    
    /// Parses an exit statement
    fn parse_exit_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::Exit)?;
        
        let return_value = if self.check_token(EnhancedToken::LeftParen) {
            self.advance()?;
            let value = Some(self.parse_expression()?);
            self.expect_token(EnhancedToken::RightParen)?;
            value
        } else {
            None
        };
        
        Ok(Statement::Exit {
            return_value,
        })
    }
    
    /// Parses a halt statement
    fn parse_halt_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(EnhancedToken::Halt)?;
        
        let exit_code = if self.check_token(EnhancedToken::LeftParen) {
            self.advance()?;
            let code = Some(self.parse_expression()?);
            self.expect_token(EnhancedToken::RightParen)?;
            code
        } else {
            None
        };
        
        Ok(Statement::Halt {
            exit_code,
        })
    }
    
    /// Parses assignment or procedure call
    fn parse_assignment_or_call(&mut self) -> Result<Statement, String> {
        let target = self.parse_expression()?;
        
        if self.check_token(EnhancedToken::Assignment) {
            self.advance()?;
            let value = self.parse_expression()?;
            Ok(Statement::Assignment {
                target,
                value,
            })
        } else {
            // This is a procedure call
            Ok(Statement::ProcedureCall {
                name: "".to_string(), // TODO: Extract name from target
                arguments: Vec::new(),
            })
        }
    }
    
    /// Parses an expression
    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_logical_or_expression()
    }
    
    /// Parses logical OR expression
    fn parse_logical_or_expression(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_logical_and_expression()?;
        
        while self.check_token(EnhancedToken::OpOr) {
            self.advance()?;
            let right = self.parse_logical_and_expression()?;
            left = Expression::BinaryOp {
                left: Box::new(left),
                operator: BinaryOperator::Or,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// Parses logical AND expression
    fn parse_logical_and_expression(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_equality_expression()?;
        
        while self.check_token(EnhancedToken::OpAnd) {
            self.advance()?;
            let right = self.parse_equality_expression()?;
            left = Expression::BinaryOp {
                left: Box::new(left),
                operator: BinaryOperator::And,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// Parses equality expression
    fn parse_equality_expression(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_relational_expression()?;
        
        while self.check_token(EnhancedToken::Equal) || self.check_token(EnhancedToken::NotEqual) {
            let operator = if self.check_token(EnhancedToken::Equal) {
                self.advance()?;
                BinaryOperator::Equal
            } else {
                self.advance()?;
                BinaryOperator::NotEqual
            };
            
            let right = self.parse_relational_expression()?;
            left = Expression::BinaryOp {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// Parses relational expression
    fn parse_relational_expression(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_additive_expression()?;
        
        while self.check_token(EnhancedToken::LessThan) || 
              self.check_token(EnhancedToken::GreaterThan) ||
              self.check_token(EnhancedToken::LessEqual) ||
              self.check_token(EnhancedToken::GreaterEqual) ||
              self.check_token(EnhancedToken::OpIn) ||
              self.check_token(EnhancedToken::OpIs) ||
              self.check_token(EnhancedToken::OpAs) {
            
            let operator = if self.check_token(EnhancedToken::LessThan) {
                self.advance()?;
                BinaryOperator::LessThan
            } else if self.check_token(EnhancedToken::GreaterThan) {
                self.advance()?;
                BinaryOperator::GreaterThan
            } else if self.check_token(EnhancedToken::LessEqual) {
                self.advance()?;
                BinaryOperator::LessEqual
            } else if self.check_token(EnhancedToken::GreaterEqual) {
                self.advance()?;
                BinaryOperator::GreaterEqual
            } else if self.check_token(EnhancedToken::OpIn) {
                self.advance()?;
                BinaryOperator::In
            } else if self.check_token(EnhancedToken::OpIs) {
                self.advance()?;
                BinaryOperator::Is
            } else {
                self.advance()?;
                BinaryOperator::As
            };
            
            let right = self.parse_additive_expression()?;
            left = Expression::BinaryOp {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// Parses additive expression
    fn parse_additive_expression(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_multiplicative_expression()?;
        
        while self.check_token(EnhancedToken::Plus) || self.check_token(EnhancedToken::Minus) {
            let operator = if self.check_token(EnhancedToken::Plus) {
                self.advance()?;
                BinaryOperator::Add
            } else {
                self.advance()?;
                BinaryOperator::Subtract
            };
            
            let right = self.parse_multiplicative_expression()?;
            left = Expression::BinaryOp {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// Parses multiplicative expression
    fn parse_multiplicative_expression(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_unary_expression()?;
        
        while self.check_token(EnhancedToken::Star) || 
              self.check_token(EnhancedToken::Slash) ||
              self.check_token(EnhancedToken::OpDiv) ||
              self.check_token(EnhancedToken::OpMod) ||
              self.check_token(EnhancedToken::OpShl) ||
              self.check_token(EnhancedToken::OpShr) ||
              self.check_token(EnhancedToken::OpXor) {
            
            let operator = if self.check_token(EnhancedToken::Star) {
                self.advance()?;
                BinaryOperator::Multiply
            } else if self.check_token(EnhancedToken::Slash) {
                self.advance()?;
                BinaryOperator::Divide
            } else if self.check_token(EnhancedToken::OpDiv) {
                self.advance()?;
                BinaryOperator::IntDivide
            } else if self.check_token(EnhancedToken::OpMod) {
                self.advance()?;
                BinaryOperator::Modulus
            } else if self.check_token(EnhancedToken::OpShl) {
                self.advance()?;
                BinaryOperator::Shl
            } else if self.check_token(EnhancedToken::OpShr) {
                self.advance()?;
                BinaryOperator::Shr
            } else {
                self.advance()?;
                BinaryOperator::Xor
            };
            
            let right = self.parse_unary_expression()?;
            left = Expression::BinaryOp {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    /// Parses unary expression
    fn parse_unary_expression(&mut self) -> Result<Expression, String> {
        if self.check_token(EnhancedToken::Plus) {
            self.advance()?;
            let expr = self.parse_primary_expression()?;
            Ok(Expression::UnaryOp {
                operator: UnaryOperator::Plus,
                operand: Box::new(expr),
            })
        } else if self.check_token(EnhancedToken::Minus) {
            self.advance()?;
            let expr = self.parse_primary_expression()?;
            Ok(Expression::UnaryOp {
                operator: UnaryOperator::Minus,
                operand: Box::new(expr),
            })
        } else if self.check_token(EnhancedToken::OpNot) {
            self.advance()?;
            let expr = self.parse_primary_expression()?;
            Ok(Expression::UnaryOp {
                operator: UnaryOperator::Not,
                operand: Box::new(expr),
            })
        } else if self.check_token(EnhancedToken::At) {
            self.advance()?;
            let expr = self.parse_primary_expression()?;
            Ok(Expression::AddressOf {
                variable: Box::new(expr),
            })
        } else if self.check_token(EnhancedToken::Caret) {
            self.advance()?;
            let expr = self.parse_primary_expression()?;
            Ok(Expression::PointerDeref {
                pointer: Box::new(expr),
            })
        } else {
            self.parse_primary_expression()
        }
    }
    
    /// Parses primary expression
    fn parse_primary_expression(&mut self) -> Result<Expression, String> {
        match self.current_token.as_ref() {
            Some((_, EnhancedToken::IntegerLiteral(value), _)) => {
                self.advance()?;
                Ok(Expression::Literal(Literal::Integer(*value)))
            }
            Some((_, EnhancedToken::RealLiteral(value), _)) => {
                self.advance()?;
                Ok(Expression::Literal(Literal::Real(*value)))
            }
            Some((_, EnhancedToken::StringLiteral(value), _)) => {
                self.advance()?;
                Ok(Expression::Literal(Literal::String(value.clone())))
            }
            Some((_, EnhancedToken::CharLiteral(value), _)) => {
                self.advance()?;
                Ok(Expression::Literal(Literal::Char(*value)))
            }
            Some((_, EnhancedToken::True, _)) => {
                self.advance()?;
                Ok(Expression::Literal(Literal::Boolean(true)))
            }
            Some((_, EnhancedToken::False, _)) => {
                self.advance()?;
                Ok(Expression::Literal(Literal::Boolean(false)))
            }
            Some((_, EnhancedToken::Nil, _)) => {
                self.advance()?;
                Ok(Expression::Literal(Literal::Nil))
            }
            Some((_, EnhancedToken::Identifier(name), _)) => {
                self.advance()?;
                Ok(Expression::Identifier(name.clone()))
            }
            Some((_, EnhancedToken::LeftParen, _)) => {
                self.advance()?;
                let expr = self.parse_expression()?;
                self.expect_token(EnhancedToken::RightParen)?;
                Ok(expr)
            }
            _ => Err("Expected expression".to_string())
        }
    }
    
    /// Parses program parameters
    fn parse_program_parameters(&mut self) -> Result<Vec<String>, String> {
        self.expect_token(EnhancedToken::LeftParen)?;
        
        let mut params = Vec::new();
        
        while !self.check_token(EnhancedToken::RightParen) {
            let param = self.expect_identifier()?;
            params.push(param);
            
            if self.check_token(EnhancedToken::Comma) {
                self.advance()?;
            }
        }
        
        self.expect_token(EnhancedToken::RightParen)?;
        
        Ok(params)
    }
    
    /// Parses uses clause
    fn parse_uses_clause(&mut self) -> Result<Vec<UseClause>, String> {
        self.expect_token(EnhancedToken::Uses)?;
        
        let mut uses = Vec::new();
        
        while !self.check_token(EnhancedToken::Semicolon) {
            let unit_name = self.expect_identifier()?;
            
            let alias = if self.check_token(EnhancedToken::Identifier("".to_string())) {
                Some(self.expect_identifier()?)
            } else {
                None
            };
            
            uses.push(UseClause {
                unit_name,
                alias,
                is_in_interface: false,
            });
            
            if self.check_token(EnhancedToken::Comma) {
                self.advance()?;
            }
        }
        
        self.expect_token(EnhancedToken::Semicolon)?;
        
        Ok(uses)
    }
    
    /// Parses exports clause
    fn parse_exports_clause(&mut self) -> Result<Vec<ExportClause>, String> {
        self.expect_token(EnhancedToken::Exports)?;
        
        let mut exports = Vec::new();
        
        while !self.check_token(EnhancedToken::Semicolon) {
            let name = self.expect_identifier()?;
            
            let alias = if self.check_token(EnhancedToken::Identifier("".to_string())) {
                Some(self.expect_identifier()?)
            } else {
                None
            };
            
            let index = if self.check_token(EnhancedToken::IntegerLiteral(0)) {
                self.advance()?;
                Some(0)
            } else {
                None
            };
            
            exports.push(ExportClause {
                name,
                alias,
                index,
                is_name: false,
            });
            
            if self.check_token(EnhancedToken::Comma) {
                self.advance()?;
            }
        }
        
        self.expect_token(EnhancedToken::Semicolon)?;
        
        Ok(exports)
    }
    
    /// Parses requires clause
    fn parse_requires_clause(&mut self) -> Result<Vec<String>, String> {
        self.expect_token(EnhancedToken::Requires)?;
        
        let mut requires = Vec::new();
        
        while !self.check_token(EnhancedToken::Semicolon) {
            let name = self.expect_identifier()?;
            requires.push(name);
            
            if self.check_token(EnhancedToken::Comma) {
                self.advance()?;
            }
        }
        
        self.expect_token(EnhancedToken::Semicolon)?;
        
        Ok(requires)
    }
    
    /// Parses contains clause
    fn parse_contains_clause(&mut self) -> Result<Vec<String>, String> {
        self.expect_token(EnhancedToken::Contains)?;
        
        let mut contains = Vec::new();
        
        while !self.check_token(EnhancedToken::Semicolon) {
            let name = self.expect_identifier()?;
            contains.push(name);
            
            if self.check_token(EnhancedToken::Comma) {
                self.advance()?;
            }
        }
        
        self.expect_token(EnhancedToken::Semicolon)?;
        
        Ok(contains)
    }
    
    /// Parses interface section
    fn parse_interface_section(&mut self) -> Result<InterfaceSection, String> {
        self.expect_token(EnhancedToken::Interface)?;
        
        let mut interface = InterfaceSection {
            consts: Vec::new(),
            types: Vec::new(),
            vars: Vec::new(),
            procedures: Vec::new(),
            functions: Vec::new(),
        };
        
        // Parse interface declarations
        while !self.check_token(EnhancedToken::Implementation) && !self.check_token(EnhancedToken::Begin) {
            if self.check_token(EnhancedToken::Const) {
                self.advance()?;
                let consts = self.parse_constant_declarations()?;
                interface.consts.extend(consts);
            } else if self.check_token(EnhancedToken::Type) {
                self.advance()?;
                let types = self.parse_type_declarations()?;
                interface.types.extend(types);
            } else if self.check_token(EnhancedToken::Var) {
                self.advance()?;
                let vars = self.parse_variable_declarations()?;
                interface.vars.extend(vars);
            } else if self.check_token(EnhancedToken::Procedure) {
                self.advance()?;
                let procedure = self.parse_procedure_declaration()?;
                interface.procedures.push(procedure);
            } else if self.check_token(EnhancedToken::Function) {
                self.advance()?;
                let function = self.parse_function_declaration()?;
                interface.functions.push(function);
            } else {
                self.advance()?;
            }
        }
        
        Ok(interface)
    }
    
    /// Parses implementation section
    fn parse_implementation_section(&mut self) -> Result<ImplementationSection, String> {
        self.expect_token(EnhancedToken::Implementation)?;
        
        let mut implementation = ImplementationSection {
            consts: Vec::new(),
            types: Vec::new(),
            vars: Vec::new(),
            procedures: Vec::new(),
            functions: Vec::new(),
        };
        
        // Parse implementation declarations
        while !self.check_token(EnhancedToken::Begin) && !self.check_token(EnhancedToken::Final) {
            if self.check_token(EnhancedToken::Const) {
                self.advance()?;
                let consts = self.parse_constant_declarations()?;
                implementation.consts.extend(consts);
            } else if self.check_token(EnhancedToken::Type) {
                self.advance()?;
                let types = self.parse_type_declarations()?;
                implementation.types.extend(types);
            } else if self.check_token(EnhancedToken::Var) {
                self.advance()?;
                let vars = self.parse_variable_declarations()?;
                implementation.vars.extend(vars);
            } else if self.check_token(EnhancedToken::Procedure) {
                self.advance()?;
                let procedure = self.parse_procedure_declaration()?;
                implementation.procedures.push(procedure);
            } else if self.check_token(EnhancedToken::Function) {
                self.advance()?;
                let function = self.parse_function_declaration()?;
                implementation.functions.push(function);
            } else {
                self.advance()?;
            }
        }
        
        Ok(implementation)
    }
    
    /// Advances to the next token
    fn advance(&mut self) -> Result<(), String> {
        if let Some(result) = self.lexer.next_token() {
            match result {
                Ok(token) => {
                    self.current_token = Some(token);
                    Ok(())
                }
                Err(error) => {
                    self.errors.push(error);
                    Err("Lexer error".to_string())
                }
            }
        } else {
            self.current_token = None;
            Ok(())
        }
    }
    
    /// Checks if the current token matches the expected token
    fn check_token(&self, expected: EnhancedToken) -> bool {
        match &self.current_token {
            Some((_, token, _)) => std::mem::discriminant(token) == std::mem::discriminant(&expected),
            None => false,
        }
    }
    
    /// Expects a specific token and advances
    fn expect_token(&mut self, expected: EnhancedToken) -> Result<(), String> {
        if self.check_token(expected) {
            self.advance()?;
            Ok(())
        } else {
            Err(format!("Expected {:?}", expected))
        }
    }
    
    /// Expects an identifier and returns its value
    fn expect_identifier(&mut self) -> Result<String, String> {
        match &self.current_token {
            Some((_, EnhancedToken::Identifier(name), _)) => {
                let name = name.clone();
                self.advance()?;
                Ok(name)
            }
            _ => Err("Expected identifier".to_string())
        }
    }
}

impl<'a> ParserCapability for EnhancedParser<'a> {
    fn new(input: &str) -> Self {
        Self::new(input)
    }
    
    fn parse(&mut self) -> Result<minipas_ast::Program, String> {
        // Convert EnhancedAst to Program for compatibility
        match self.parse_program()? {
            EnhancedAst::Program(program) => {
                // Convert to old Program format
                Ok(minipas_ast::Program {
                    name: program.name,
                    uses: program.uses.into_iter().map(|u| u.unit_name).collect(),
                    block: minipas_ast::Block {
                        consts: program.block.consts.into_iter().map(|c| minipas_ast::ConstDecl {
                            name: c.name,
                            value: minipas_ast::Expr::Literal(minipas_ast::Literal::String("".to_string())), // TODO: Convert properly
                        }).collect(),
                        types: program.block.types.into_iter().map(|t| minipas_ast::TypeDecl {
                            name: t.name,
                            typ: minipas_ast::Type::Custom("".to_string()), // TODO: Convert properly
                        }).collect(),
                        vars: program.block.vars.into_iter().map(|v| minipas_ast::VariableDecl {
                            name: v.name,
                            var_type: minipas_ast::Type::Custom("".to_string()), // TODO: Convert properly
                            initializer: None,
                            is_threadvar: false,
                            absolute_address: None,
                            external_name: None,
                        }).collect(),
                        procedures: program.block.procedures.into_iter().map(|p| minipas_ast::ProcedureDecl {
                            name: p.name,
                            params: p.parameters.into_iter().map(|param| minipas_ast::Parameter {
                                name: param.name,
                                param_type: minipas_ast::Type::Custom("".to_string()), // TODO: Convert properly
                                is_var: param.is_var,
                                is_const: param.is_const,
                                is_out: param.is_out,
                                default_value: None,
                            }).collect(),
                            block: minipas_ast::Block {
                                consts: Vec::new(),
                                types: Vec::new(),
                                vars: Vec::new(),
                                procedures: Vec::new(),
                                functions: Vec::new(),
                                statements: Vec::new(),
                            },
                            is_forward: p.is_forward,
                            is_external: p.is_external,
                            external_name: p.external_name,
                            external_library: None,
                            is_inline: p.is_inline,
                            is_assembler: p.is_assembler,
                            calling_convention: Some(minipas_ast::CallingConvention::Default),
                        }).collect(),
                        functions: program.block.functions.into_iter().map(|f| minipas_ast::FunctionDecl {
                            name: f.name,
                            params: f.parameters.into_iter().map(|param| minipas_ast::Parameter {
                                name: param.name,
                                param_type: minipas_ast::Type::Custom("".to_string()), // TODO: Convert properly
                                is_var: param.is_var,
                                is_const: param.is_const,
                                is_out: param.is_out,
                                default_value: None,
                            }).collect(),
                            return_type: minipas_ast::Type::Custom("".to_string()), // TODO: Convert properly
                            block: minipas_ast::Block {
                                consts: Vec::new(),
                                types: Vec::new(),
                                vars: Vec::new(),
                                procedures: Vec::new(),
                                functions: Vec::new(),
                                statements: Vec::new(),
                            },
                            is_forward: f.is_forward,
                            is_external: f.is_external,
                            external_name: f.external_name,
                            external_library: None,
                            is_inline: f.is_inline,
                            is_assembler: f.is_assembler,
                            calling_convention: Some(minipas_ast::CallingConvention::Default),
                        }).collect(),
                        statements: Vec::new(),
                    },
                })
            }
            _ => Err("Expected program".to_string())
        }
    }
}

impl<'a> SymbolTable for EnhancedParser<'a> {
    fn add_symbol(&mut self, name: String, symbol_type: SymbolType) {
        let symbol_info = SymbolInfo {
            name: name.clone(),
            symbol_type,
            scope_level: self.current_scope.len(),
            is_forward: false,
            is_external: false,
            visibility: Visibility::Public,
        };
        self.symbol_table.insert(name, symbol_info);
    }
    
    fn lookup_symbol(&self, name: &str) -> Option<&SymbolInfo> {
        self.symbol_table.get(name)
    }
    
    fn enter_scope(&mut self, name: String) {
        self.current_scope.push(name);
    }
    
    fn exit_scope(&mut self) {
        self.current_scope.pop();
    }
}

impl<'a> SyntaxValidator for EnhancedParser<'a> {
    fn validate_syntax(&self) -> bool {
        self.errors.is_empty()
    }
    
    fn get_syntax_errors(&self) -> Vec<String> {
        self.errors.clone()
    }
}

impl<'a> ErrorRecovery for EnhancedParser<'a> {
    fn recover_from_error(&mut self) -> bool {
        // Simple error recovery: skip to next statement
        while let Some((_, token, _)) = &self.current_token {
            if matches!(token, EnhancedToken::Semicolon | EnhancedToken::End | EnhancedToken::Begin) {
                break;
            }
            if let Err(_) = self.advance() {
                return false;
            }
        }
        true
    }
    
    fn has_recoverable_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}
