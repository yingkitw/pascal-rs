//! Declaration parsing for the Pascal parser

use crate::ast::{Block, FunctionDecl, Parameter, ProcedureDecl, SimpleType, Type, VariableDecl, FieldVisibility, Unit, UnitInterface, UnitImplementation, ClassDecl, TypeDecl, ConstDecl, FieldDecl, MethodDecl, PropertyDecl};
use crate::parser::{ParseResult, Parser};
use crate::tokens::Token;
use crate::ParseError;

impl<'a> Parser<'a> {
    /// Parse a complete program
    pub fn parse_program(&mut self) -> ParseResult<crate::ast::Program> {
        let name = self.parse_program_header()?;
        let uses = self.parse_uses_clause()?;
        let block = self.parse_block()?;

        // Expect final period
        self.consume(Token::Dot)?;

        Ok(crate::ast::Program { name, uses, block })
    }

    /// Parse a complete unit
    pub fn parse_unit(&mut self) -> ParseResult<Unit> {
        let name = self.parse_unit_header()?;
        let interface = self.parse_interface_section()?;
        let implementation = self.parse_implementation_section()?;

        // Expect final period
        self.consume(Token::Dot)?;

        Ok(Unit {
            name,
            uses: interface.uses.clone(),
            interface,
            implementation,
        })
    }

    /// Parse program header: program Name;
    fn parse_program_header(&mut self) -> ParseResult<String> {
        self.consume(Token::Program)?;

        let name = match self.peek() {
            Some(Token::Identifier(_)) => {
                if let Some(Token::Identifier(name)) = self.current_token.take() {
                    self.advance();
                    name
                } else {
                    unreachable!()
                }
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    "expected program name".to_string(),
                ))
            }
        };

        self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin, Token::Uses]);
        Ok(name)
    }

    /// Parse unit header: unit UnitName;
    fn parse_unit_header(&mut self) -> ParseResult<String> {
        self.consume(Token::Unit)?;

        let name = match self.peek() {
            Some(Token::Identifier(_)) => {
                if let Some(Token::Identifier(name)) = self.current_token.take() {
                    self.advance();
                    name
                } else {
                    unreachable!()
                }
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    "expected unit name".to_string(),
                ))
            }
        };

        self.consume_or_skip(Token::Semicolon, &[Token::Interface, Token::Uses]);
        Ok(name)
    }

    /// Parse uses clause: uses Unit1, Unit2;
    pub fn parse_uses_clause(&mut self) -> ParseResult<Vec<String>> {
        let mut uses = Vec::new();

        if self.check(Token::Uses) {
            self.advance();

            loop {
                if let Some(Token::Identifier(name)) = self.current_token.take() {
                    uses.push(name);
                    self.advance();

                    match self.peek() {
                        Some(Token::Comma) => {
                            self.advance();
                            continue;
                        }
                        Some(Token::Semicolon) => {
                            self.advance();
                            break;
                        }
                        _ => break,
                    }
                } else {
                    break;
                }
            }
        }

        Ok(uses)
    }

    /// Parse a block (declarations + compound statement)
    /// Declaration sections (const, type, var, function, procedure) can appear in any order.
    pub fn parse_block(&mut self) -> ParseResult<Block> {
        let mut consts = vec![];
        let mut types = vec![];
        let mut vars = vec![];
        let mut procedures = vec![];
        let mut functions = vec![];
        let mut classes = vec![];

        loop {
            if self.check(Token::Const) {
                self.advance();
                while let Some(Token::Identifier(name)) = self.peek() {
                    let name = name.clone();
                    self.advance();
                    self.consume_or_skip(Token::Equal, &[Token::Var, Token::Begin]);

                    if let Some(literal) = self.parse_literal_int()? {
                        consts.push(crate::ast::ConstDecl {
                            name,
                            value: literal,
                            visibility: FieldVisibility::Public,
                        });
                    }

                    self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin]);
                }
            } else if self.check(Token::Type) {
                self.advance();
                while let Some(Token::Identifier(name)) = self.peek() {
                    let name = name.clone();
                    self.advance();
                    self.consume_or_skip(Token::Equal, &[Token::Var, Token::Begin]);

                    // Check if this is a class declaration
                    if self.check(Token::Class) {
                        // Parse as class declaration, injecting the name
                        if let Ok(mut class_decl) = self.parse_class_decl() {
                            class_decl.name = name;
                            classes.push(class_decl);
                        }
                    } else if let Ok(type_def) = self.parse_type() {
                        types.push(crate::ast::TypeDecl {
                            name,
                            type_definition: type_def,
                            visibility: FieldVisibility::Public,
                        });
                    }

                    self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin]);
                }
            } else if self.check(Token::Var) {
                self.advance();
                while let Some(Token::Identifier(_)) = self.peek() {
                    let mut names = vec![];
                    loop {
                        if let Some(Token::Identifier(name)) = self.current_token.take() {
                            names.push(name);
                            self.advance();
                        }
                        if self.check(Token::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.consume_or_skip(Token::Colon, &[Token::Begin]);

                    let var_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));

                    for name in names {
                        vars.push(VariableDecl {
                            name,
                            variable_type: var_type.clone(),
                            initial_value: None,
                            visibility: FieldVisibility::Public,
                            is_absolute: false,
                            absolute_address: None,
                        });
                    }

                    self.consume_or_skip(Token::Semicolon, &[Token::Begin]);
                }
            } else if self.check(Token::Procedure) {
                self.advance();
                let proc = self.parse_procedure_decl()?;
                procedures.push(proc);
            } else if self.check(Token::Function) {
                self.advance();
                let func = self.parse_function_decl()?;
                functions.push(func);
            } else {
                break;
            }
        }

        // Parse compound statement
        let statements = self.parse_compound_statement_int()?;

        Ok(Block {
            consts,
            types,
            vars,
            procedures,
            functions,
            classes,
            statements,
        })
    }

    /// Parse interface section
    fn parse_interface_section(&mut self) -> ParseResult<UnitInterface> {
        self.consume(Token::Interface)?;

        let uses = self.parse_uses_clause()?;

        let mut types = vec![];
        let mut constants = vec![];
        let mut variables = vec![];
        let mut procedures = vec![];
        let mut functions = vec![];
        let mut classes = vec![];
        let interfaces = vec![];

        loop {
            match self.peek() {
                Some(Token::Type) => {
                    self.advance();
                    while let Some(Token::Identifier(name)) = self.peek() {
                        let name = name.clone();
                        self.advance();
                        self.consume_or_skip(Token::Equal, &[Token::Var, Token::Procedure, Token::Function, Token::Implementation]);

                        if let Ok(type_def) = self.parse_type() {
                            types.push(TypeDecl {
                                name,
                                type_definition: type_def,
                                visibility: FieldVisibility::Public,
                            });
                        }

                        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Implementation]);
                    }
                }
                Some(Token::Const) => {
                    self.advance();
                    while let Some(Token::Identifier(name)) = self.peek() {
                        let name = name.clone();
                        self.advance();
                        self.consume_or_skip(Token::Equal, &[Token::Var, Token::Procedure, Token::Function, Token::Implementation]);

                        if let Some(literal) = self.parse_literal_int()? {
                            constants.push(ConstDecl {
                                name,
                                value: literal,
                                visibility: FieldVisibility::Public,
                            });
                        }

                        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Implementation]);
                    }
                }
                Some(Token::Var) => {
                    self.advance();
                    while let Some(Token::Identifier(_)) = self.peek() {
                        let mut names = vec![];
                        loop {
                            if let Some(Token::Identifier(name)) = self.current_token.take() {
                                names.push(name);
                                self.advance();
                            }
                            if self.check(Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.consume_or_skip(Token::Colon, &[Token::Procedure, Token::Function, Token::Implementation]);

                        let var_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));

                        for name in names {
                            variables.push(VariableDecl {
                                name,
                                variable_type: var_type.clone(),
                                initial_value: None,
                                visibility: FieldVisibility::Public,
                                is_absolute: false,
                                absolute_address: None,
                            });
                        }

                        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Implementation]);
                    }
                }
                Some(Token::Procedure) => {
                    self.advance();
                    let proc = self.parse_procedure_decl_interface()?;
                    procedures.push(proc);
                }
                Some(Token::Function) => {
                    self.advance();
                    let func = self.parse_function_decl_interface()?;
                    functions.push(func);
                }
                Some(Token::Class) => {
                    self.advance();
                    if let Ok(class) = self.parse_class_decl() {
                        classes.push(class);
                    }
                    self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Implementation]);
                }
                Some(Token::Implementation) => {
                    break;
                }
                _ => {
                    break;
                }
            }
        }

        Ok(UnitInterface {
            uses,
            types,
            constants,
            variables,
            procedures,
            functions,
            classes,
            interfaces,
        })
    }

    /// Parse procedure declaration in interface (forward declaration)
    fn parse_procedure_decl_interface(&mut self) -> ParseResult<ProcedureDecl> {
        let name = match self.peek() {
            Some(Token::Identifier(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err(ParseError::UnexpectedToken("expected procedure name".to_string())),
        };

        let parameters = self.parse_parameters()?;
        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Implementation]);

        Ok(ProcedureDecl {
            name,
            parameters,
            block: Block::empty(),
            visibility: FieldVisibility::Public,
            is_external: false,
            external_name: None,
            is_inline: false,
            is_forward: true,  // Interface declarations are forward
            is_class_method: false,
            is_virtual: false,
            is_override: false,
            is_overload: false,
        })
    }

    /// Parse function declaration in interface (forward declaration)
    fn parse_function_decl_interface(&mut self) -> ParseResult<FunctionDecl> {
        let name = match self.peek() {
            Some(Token::Identifier(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err(ParseError::UnexpectedToken("expected function name".to_string())),
        };

        let parameters = self.parse_parameters()?;

        // Parse return type: ': type'
        self.consume_or_skip(Token::Colon, &[Token::Semicolon, Token::Implementation]);
        let return_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));
        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Implementation]);

        Ok(FunctionDecl {
            name,
            parameters,
            return_type,
            block: Block::empty(),
            visibility: FieldVisibility::Public,
            is_external: false,
            external_name: None,
            is_inline: false,
            is_forward: true,  // Interface declarations are forward
            is_class_method: false,
            is_virtual: false,
            is_override: false,
            is_overload: false,
        })
    }

    /// Parse implementation section
    fn parse_implementation_section(&mut self) -> ParseResult<UnitImplementation> {
        self.consume(Token::Implementation)?;

        let uses = self.parse_uses_clause()?;

        let mut types = vec![];
        let mut constants = vec![];
        let mut variables = vec![];
        let mut procedures = vec![];
        let mut functions = vec![];
        let mut classes = vec![];
        let interfaces = vec![];

        let mut initialization = None;
        let mut finalization = None;

        loop {
            match self.peek() {
                Some(Token::Type) => {
                    self.advance();
                    while let Some(Token::Identifier(name)) = self.peek() {
                        let name = name.clone();
                        self.advance();
                        self.consume_or_skip(Token::Equal, &[Token::Var, Token::Procedure, Token::Function, Token::Begin]);

                        if let Ok(type_def) = self.parse_type() {
                            types.push(TypeDecl {
                                name,
                                type_definition: type_def,
                                visibility: FieldVisibility::Private,
                            });
                        }

                        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);
                    }
                }
                Some(Token::Const) => {
                    self.advance();
                    while let Some(Token::Identifier(name)) = self.peek() {
                        let name = name.clone();
                        self.advance();
                        self.consume_or_skip(Token::Equal, &[Token::Var, Token::Procedure, Token::Function, Token::Begin]);

                        if let Some(literal) = self.parse_literal_int()? {
                            constants.push(ConstDecl {
                                name,
                                value: literal,
                                visibility: FieldVisibility::Private,
                            });
                        }

                        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);
                    }
                }
                Some(Token::Var) => {
                    self.advance();
                    while let Some(Token::Identifier(_)) = self.peek() {
                        let mut names = vec![];
                        loop {
                            if let Some(Token::Identifier(name)) = self.current_token.take() {
                                names.push(name);
                                self.advance();
                            }
                            if self.check(Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.consume_or_skip(Token::Colon, &[Token::Procedure, Token::Function, Token::Begin]);

                        let var_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));

                        for name in names {
                            variables.push(VariableDecl {
                                name,
                                variable_type: var_type.clone(),
                                initial_value: None,
                                visibility: FieldVisibility::Private,
                                is_absolute: false,
                                absolute_address: None,
                            });
                        }

                        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);
                    }
                }
                Some(Token::Procedure) => {
                    self.advance();
                    let proc = self.parse_procedure_decl()?;
                    procedures.push(proc);
                }
                Some(Token::Function) => {
                    self.advance();
                    let func = self.parse_function_decl()?;
                    functions.push(func);
                }
                Some(Token::Class) => {
                    self.advance();
                    if let Ok(class) = self.parse_class_decl() {
                        classes.push(class);
                    }
                    self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);
                }
                Some(Token::Begin) => {
                    // Initialization section
                    self.advance();
                    let mut init_stmts = vec![];
                    while !self.check(Token::End) && self.peek().is_some() {
                        if let Ok(Some(stmt)) = self.parse_statement() {
                            init_stmts.push(stmt);
                        }
                        if self.check(Token::Semicolon) {
                            self.advance();
                        }
                    }
                    self.consume(Token::End)?;
                    initialization = Some(init_stmts);

                    // Check for finalization section
                    if self.check(Token::Finalization) {
                        self.advance();
                        let mut final_stmts = vec![];
                        while !self.check(Token::End) && self.peek().is_some() {
                            if let Ok(Some(stmt)) = self.parse_statement() {
                                final_stmts.push(stmt);
                            }
                            if self.check(Token::Semicolon) {
                                self.advance();
                            }
                        }
                        self.consume(Token::End)?;
                        finalization = Some(final_stmts);
                    }
                    break;
                }
                Some(Token::End) => {
                    // Consume the final 'end' of the unit
                    self.advance();
                    break;
                }
                _ => {
                    break;
                }
            }
        }

        Ok(UnitImplementation {
            uses,
            types,
            constants,
            variables,
            procedures,
            functions,
            classes,
            interfaces,
            initialization,
            finalization,
        })
    }

    /// Parse class declaration: class(Parent) ... end
    /// Supports visibility sections, constructor/destructor, virtual/override/abstract,
    /// and property declarations using proper Pascal syntax.
    fn parse_class_decl(&mut self) -> ParseResult<ClassDecl> {
        let name = match self.peek() {
            Some(Token::Identifier(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err(ParseError::UnexpectedToken("expected class name".to_string())),
        };

        // Parse '= class' (the '=' was already consumed by the type section parser)
        // The 'class' keyword was already consumed before calling this method

        // Parse optional parent class: (TParent)
        let mut parent = None;
        let mut iface_list = vec![];
        if self.check(Token::LeftParen) {
            self.advance();
            // First identifier is parent class
            if let Some(Token::Identifier(n)) = self.peek() {
                let n = n.clone();
                self.advance();
                parent = Some(n);
            }
            // Additional comma-separated identifiers are interfaces
            while self.check(Token::Comma) {
                self.advance();
                if let Some(Token::Identifier(n)) = self.peek() {
                    let n = n.clone();
                    self.advance();
                    iface_list.push(n);
                }
            }
            self.consume_or_skip(Token::RightParen, &[Token::End, Token::Private, Token::Public, Token::Protected, Token::Published]);
        }

        let mut fields = vec![];
        let mut methods = vec![];
        let mut properties = vec![];
        let mut current_visibility = FieldVisibility::Public;
        let is_abstract_class = false;

        // Parse class body until 'end'
        while !self.check(Token::End) && self.peek().is_some() {
            match self.peek() {
                // Visibility sections
                Some(Token::Private) => { self.advance(); current_visibility = FieldVisibility::Private; }
                Some(Token::Protected) => { self.advance(); current_visibility = FieldVisibility::Protected; }
                Some(Token::Public) => { self.advance(); current_visibility = FieldVisibility::Public; }
                Some(Token::Published) => { self.advance(); current_visibility = FieldVisibility::Published; }

                // Constructor
                Some(Token::Constructor) => {
                    self.advance();
                    let method = self.parse_class_method(current_visibility.clone(), true, false)?;
                    methods.push(method);
                }
                // Destructor
                Some(Token::Destructor) => {
                    self.advance();
                    let method = self.parse_class_method(current_visibility.clone(), false, true)?;
                    methods.push(method);
                }
                // Function method
                Some(Token::Function) => {
                    self.advance();
                    let mut method = self.parse_class_method(current_visibility.clone(), false, false)?;
                    // Parse return type
                    self.consume_or_skip(Token::Colon, &[Token::Semicolon, Token::End]);
                    let return_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));
                    method.return_type = Some(return_type);
                    self.consume_or_skip(Token::Semicolon, &[Token::End, Token::Private, Token::Public, Token::Protected, Token::Published, Token::Function, Token::Procedure, Token::Constructor, Token::Destructor, Token::Property]);
                    // Parse method modifiers after semicolon
                    self.parse_method_modifiers(&mut method);
                    methods.push(method);
                }
                // Procedure method
                Some(Token::Procedure) => {
                    self.advance();
                    let mut method = self.parse_class_method(current_visibility.clone(), false, false)?;
                    self.consume_or_skip(Token::Semicolon, &[Token::End, Token::Private, Token::Public, Token::Protected, Token::Published, Token::Function, Token::Procedure, Token::Constructor, Token::Destructor, Token::Property]);
                    self.parse_method_modifiers(&mut method);
                    methods.push(method);
                }
                // Property
                Some(Token::Property) => {
                    self.advance();
                    if let Ok(prop) = self.parse_property_decl(current_visibility.clone()) {
                        properties.push(prop);
                    }
                }
                // Field declarations (identifier: type)
                Some(Token::Identifier(_)) => {
                    let mut names = vec![];
                    loop {
                        if let Some(Token::Identifier(fname)) = self.current_token.take() {
                            names.push(fname);
                            self.advance();
                        }
                        if self.check(Token::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.consume_or_skip(Token::Colon, &[Token::Semicolon, Token::End]);
                    let field_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));
                    for fname in names {
                        fields.push(FieldDecl {
                            name: fname,
                            field_type: field_type.clone(),
                            visibility: current_visibility.clone(),
                        });
                    }
                    self.consume_or_skip(Token::Semicolon, &[Token::End, Token::Private, Token::Public, Token::Protected, Token::Published]);
                }
                _ => break,
            }
        }

        self.consume_or_skip(Token::End, &[Token::Semicolon]);

        Ok(ClassDecl {
            name,
            parent,
            interfaces: iface_list,
            fields,
            methods,
            properties,
            visibility: FieldVisibility::Public,
            is_abstract: is_abstract_class,
            is_sealed: false,
        })
    }

    /// Parse a method declaration inside a class (name + params, no return type yet for function)
    fn parse_class_method(&mut self, visibility: FieldVisibility, is_constructor: bool, is_destructor: bool) -> ParseResult<MethodDecl> {
        let name = match self.peek() {
            Some(Token::Identifier(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err(ParseError::UnexpectedToken("expected method name".to_string())),
        };

        let params = self.parse_parameters()?;

        // For constructor/destructor, consume semicolon and modifiers here
        if is_constructor || is_destructor {
            self.consume_or_skip(Token::Semicolon, &[Token::End, Token::Private, Token::Public, Token::Protected, Token::Published, Token::Function, Token::Procedure, Token::Constructor, Token::Destructor, Token::Property]);
            let mut method = MethodDecl {
                name,
                parameters: params,
                return_type: None,
                block: None,
                visibility,
                is_class_method: false,
                is_virtual: false,
                is_abstract: false,
                is_override: false,
                is_overload: false,
                is_static: false,
                is_constructor,
                is_destructor,
            };
            self.parse_method_modifiers(&mut method);
            return Ok(method);
        }

        Ok(MethodDecl {
            name,
            parameters: params,
            return_type: None,
            block: None,
            visibility,
            is_class_method: false,
            is_virtual: false,
            is_abstract: false,
            is_override: false,
            is_overload: false,
            is_static: false,
            is_constructor: false,
            is_destructor: false,
        })
    }

    /// Parse method modifiers: virtual; override; abstract; overload; static;
    fn parse_method_modifiers(&mut self, method: &mut MethodDecl) {
        loop {
            match self.peek() {
                Some(Token::Virtual) => {
                    self.advance();
                    method.is_virtual = true;
                    if self.check(Token::Semicolon) { self.advance(); }
                }
                Some(Token::Override) => {
                    self.advance();
                    method.is_override = true;
                    if self.check(Token::Semicolon) { self.advance(); }
                }
                Some(Token::Abstract) => {
                    self.advance();
                    method.is_abstract = true;
                    method.is_virtual = true;
                    if self.check(Token::Semicolon) { self.advance(); }
                }
                Some(Token::Identifier(s)) if s == "overload" => {
                    self.advance();
                    method.is_overload = true;
                    if self.check(Token::Semicolon) { self.advance(); }
                }
                Some(Token::Identifier(s)) if s == "static" => {
                    self.advance();
                    method.is_static = true;
                    if self.check(Token::Semicolon) { self.advance(); }
                }
                _ => break,
            }
        }
    }

    /// Parse property declaration: property Name: Type read GetName write SetName;
    fn parse_property_decl(&mut self, visibility: FieldVisibility) -> ParseResult<PropertyDecl> {
        use crate::ast::MethodSpecifier;

        let name = match self.peek() {
            Some(Token::Identifier(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err(ParseError::UnexpectedToken("expected property name".to_string())),
        };

        self.consume_or_skip(Token::Colon, &[Token::Semicolon, Token::End]);
        let property_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));

        let mut read_specifier = None;
        let mut write_specifier = None;
        let default_value = None;

        // Parse read/write/default specifiers
        loop {
            match self.peek() {
                Some(Token::Identifier(s)) if s == "read" => {
                    self.advance();
                    if let Some(Token::Identifier(getter)) = self.peek() {
                        let getter = getter.clone();
                        self.advance();
                        read_specifier = Some(MethodSpecifier::Field(getter));
                    }
                }
                Some(Token::Identifier(s)) if s == "write" => {
                    self.advance();
                    if let Some(Token::Identifier(setter)) = self.peek() {
                        let setter = setter.clone();
                        self.advance();
                        write_specifier = Some(MethodSpecifier::Method(setter));
                    }
                }
                Some(Token::Identifier(s)) if s == "default" => {
                    self.advance();
                    // Skip default value for now
                }
                _ => break,
            }
        }

        self.consume_or_skip(Token::Semicolon, &[Token::End, Token::Private, Token::Public, Token::Protected, Token::Published, Token::Property]);

        Ok(PropertyDecl {
            name,
            property_type,
            read_specifier,
            write_specifier,
            stored_field: None,
            default_value,
            visibility,
            is_indexed: false,
            index_parameters: vec![],
        })
    }

    /// Parse procedure declaration (after 'procedure' keyword consumed)
    pub fn parse_procedure_decl(&mut self) -> ParseResult<ProcedureDecl> {
        let name = match self.peek() {
            Some(Token::Identifier(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err(ParseError::UnexpectedToken("expected procedure name".to_string())),
        };

        let parameters = self.parse_parameters()?;
        self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin, Token::Forward]);

        // Check for forward declaration
        if self.check(Token::Forward) {
            self.advance();
            self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);
            return Ok(ProcedureDecl {
                name,
                parameters,
                block: Block::empty(),
                visibility: FieldVisibility::Public,
                is_external: false,
                external_name: None,
                is_inline: false,
                is_forward: true,
                is_class_method: false,
                is_virtual: false,
                is_override: false,
                is_overload: false,
            });
        }

        let block = self.parse_block()?;
        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);

        Ok(ProcedureDecl {
            name,
            parameters,
            block,
            visibility: FieldVisibility::Public,
            is_external: false,
            external_name: None,
            is_inline: false,
            is_forward: false,
            is_class_method: false,
            is_virtual: false,
            is_override: false,
            is_overload: false,
        })
    }

    /// Parse function declaration (after 'function' keyword consumed)
    pub fn parse_function_decl(&mut self) -> ParseResult<FunctionDecl> {
        let name = match self.peek() {
            Some(Token::Identifier(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err(ParseError::UnexpectedToken("expected function name".to_string())),
        };

        let parameters = self.parse_parameters()?;

        // Parse return type: ': type'
        self.consume_or_skip(Token::Colon, &[Token::Semicolon, Token::Begin]);
        let return_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));
        self.consume_or_skip(Token::Semicolon, &[Token::Var, Token::Begin, Token::Forward]);

        // Check for forward declaration
        if self.check(Token::Forward) {
            self.advance();
            self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);
            return Ok(FunctionDecl {
                name,
                parameters,
                return_type,
                block: Block::empty(),
                visibility: FieldVisibility::Public,
                is_external: false,
                external_name: None,
                is_inline: false,
                is_forward: true,
                is_class_method: false,
                is_virtual: false,
                is_override: false,
                is_overload: false,
            });
        }

        let block = self.parse_block()?;
        self.consume_or_skip(Token::Semicolon, &[Token::Procedure, Token::Function, Token::Begin]);

        Ok(FunctionDecl {
            name,
            parameters,
            return_type,
            block,
            visibility: FieldVisibility::Public,
            is_external: false,
            external_name: None,
            is_inline: false,
            is_forward: false,
            is_class_method: false,
            is_virtual: false,
            is_override: false,
            is_overload: false,
        })
    }

    /// Parse parameter list: (a, b: integer; c: real) or empty
    fn parse_parameters(&mut self) -> ParseResult<Vec<Parameter>> {
        let mut params = vec![];

        if !self.check(Token::LeftParen) {
            return Ok(params);
        }
        self.advance(); // consume '('

        if self.check(Token::RightParen) {
            self.advance();
            return Ok(params);
        }

        loop {
            let is_var = if self.check(Token::Var) {
                self.advance();
                true
            } else {
                false
            };

            let is_const = if self.check(Token::Const) {
                self.advance();
                true
            } else {
                false
            };

            // Collect comma-separated parameter names
            let mut names = vec![];
            loop {
                if let Some(Token::Identifier(name)) = self.peek() {
                    let name = name.clone();
                    self.advance();
                    names.push(name);
                }
                if self.check(Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            // Parse ': type'
            self.consume_or_skip(Token::Colon, &[Token::Semicolon, Token::RightParen]);
            let param_type = self.parse_type().unwrap_or(Type::Simple(SimpleType::Integer));

            for name in names {
                params.push(Parameter {
                    name,
                    param_type: param_type.clone(),
                    is_var,
                    is_const,
                    is_out: false,
                    default_value: None,
                });
            }

            if self.check(Token::Semicolon) {
                self.advance();
            } else {
                break;
            }
        }

        self.consume_or_skip(Token::RightParen, &[Token::Semicolon, Token::Colon, Token::Begin]);
        Ok(params)
    }

    /// Parse a type
    pub fn parse_type(&mut self) -> ParseResult<Type> {
        Ok(match self.peek() {
            Some(Token::Integer) => {
                self.advance();
                Type::Simple(SimpleType::Integer)
            }
            Some(Token::Real) => {
                self.advance();
                Type::Simple(SimpleType::Real)
            }
            Some(Token::Boolean) => {
                self.advance();
                Type::Simple(SimpleType::Boolean)
            }
            Some(Token::Char) => {
                self.advance();
                Type::Simple(SimpleType::Char)
            }
            Some(Token::String) => {
                self.advance();
                Type::Simple(SimpleType::String)
            }
            Some(Token::Array) => {
                self.advance();
                self.consume_or_skip(Token::LeftBracket, &[Token::Of, Token::Semicolon]);
                // Parse index type (simplified)
                let index_type = Box::new(Type::Simple(SimpleType::Integer));
                self.consume_or_skip(Token::RightBracket, &[Token::Of, Token::Semicolon]);
                self.consume_or_skip(Token::Of, &[Token::Semicolon]);
                let element_type = Box::new(self.parse_type()?);
                Type::Array {
                    index_type,
                    element_type,
                    range: None,
                }
            }
            Some(Token::Record) => {
                self.advance();
                let mut fields = std::collections::HashMap::new();

                while !self.check(Token::End) && self.peek().is_some() {
                    if let Some(Token::Identifier(_)) = self.peek() {
                        // Collect comma-separated field names
                        let mut names = vec![];
                        loop {
                            if let Some(Token::Identifier(name)) = self.peek() {
                                let name = name.clone();
                                self.advance();
                                names.push(name);
                            }
                            if self.check(Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.consume_or_skip(Token::Colon, &[Token::End, Token::Semicolon]);
                        if let Ok(field_type) = self.parse_type() {
                            for name in names {
                                fields.insert(name, Box::new(field_type.clone()));
                            }
                        }
                        self.consume_or_skip(Token::Semicolon, &[Token::End]);
                    } else {
                        break;
                    }
                }

                self.consume_or_skip(Token::End, &[Token::Semicolon]);
                Type::Record {
                    fields,
                    is_packed: false,
                }
            }
            Some(Token::Pointer) => {
                self.advance();
                Type::Pointer(Box::new(self.parse_type()?))
            }
            Some(Token::Class) => {
                // class type — delegate to parse_class_decl but we need a dummy name
                // This is used when parsing: type TFoo = class ... end;
                // The name was already consumed by the type section, so we push it back
                // Actually, parse_type is called after '=' so we see 'class' here.
                // We return a named alias that the caller resolves.
                self.advance();
                // For forward class declaration: type TFoo = class;
                if self.check(Token::Semicolon) {
                    return Ok(Type::Alias {
                        name: "class".to_string(),
                        target_type: Box::new(Type::Simple(SimpleType::Integer)),
                    });
                }
                // Full class — we need to parse the body inline
                // Parse optional parent
                let mut _parent = None;
                if self.check(Token::LeftParen) {
                    self.advance();
                    if let Some(Token::Identifier(n)) = self.peek() {
                        _parent = Some(n.clone());
                        self.advance();
                    }
                    self.consume_or_skip(Token::RightParen, &[Token::End, Token::Private, Token::Public]);
                }
                // Skip the class body for type parsing — consume until 'end'
                let mut depth = 1;
                while depth > 0 && self.peek().is_some() {
                    match self.peek() {
                        Some(Token::End) => {
                            depth -= 1;
                            if depth == 0 {
                                self.advance();
                                break;
                            }
                            self.advance();
                        }
                        Some(Token::Begin) | Some(Token::Record) | Some(Token::Case) => {
                            self.advance();
                            depth += 1;
                        }
                        _ => { self.advance(); }
                    }
                }
                Type::Alias {
                    name: "class".to_string(),
                    target_type: Box::new(Type::Simple(SimpleType::Integer)),
                }
            }
            Some(Token::Identifier(name)) => {
                let name = name.clone();
                self.advance();
                Type::Alias {
                    name,
                    target_type: Box::new(Type::Simple(SimpleType::Integer)),
                }
            }
            _ => Type::Simple(SimpleType::Integer), // Default
        })
    }

    /// Parse compound statement for blocks - delegates to the full statement parser
    fn parse_compound_statement_int(&mut self) -> ParseResult<Vec<crate::ast::Statement>> {
        self.parse_compound_statement()
    }

    /// Parse a literal value
    fn parse_literal_int(&mut self) -> ParseResult<Option<crate::ast::Literal>> {
        match self.peek() {
            Some(Token::IntegerLiteral(_)) => {
                if let Some(Token::IntegerLiteral(val)) = self.current_token.take() {
                    self.advance();
                    Ok(Some(crate::ast::Literal::Integer(val)))
                } else {
                    Ok(None)
                }
            }
            Some(Token::RealLiteral(_)) => {
                if let Some(Token::RealLiteral(val)) = self.current_token.take() {
                    self.advance();
                    Ok(Some(crate::ast::Literal::Real(val)))
                } else {
                    Ok(None)
                }
            }
            Some(Token::StringLiteral(_)) => {
                if let Some(Token::StringLiteral(val)) = self.current_token.take() {
                    self.advance();
                    Ok(Some(crate::ast::Literal::String(val)))
                } else {
                    Ok(None)
                }
            }
            Some(Token::CharLiteral(_)) => {
                if let Some(Token::CharLiteral(val)) = self.current_token.take() {
                    self.advance();
                    Ok(Some(crate::ast::Literal::Char(val)))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_program_header() {
        let source = "program Test;";
        let mut parser = Parser::new(source);
        let name = parser.parse_program_header();
        assert!(name.is_ok());
        assert_eq!(name.unwrap(), "Test");
    }

    #[test]
    fn test_parse_uses_clause() {
        let source = "uses SysUtils, Classes;";
        let mut parser = Parser::new(source);
        let uses = parser.parse_uses_clause();
        assert!(uses.is_ok());
        assert_eq!(uses.unwrap().len(), 2);
    }

    #[test]
    fn test_parse_type() {
        let source = "integer";
        let mut parser = Parser::new(source);
        let typ = parser.parse_type();
        assert!(typ.is_ok());
    }

    #[test]
    fn test_parse_unit() {
        let source = r#"
unit TestUnit;

interface

uses
  SysUtils;

function Add(a, b: integer): integer;

implementation

function Add(a, b: integer): integer;
begin
  result := a + b;
end;

end.
"#;

        let mut parser = Parser::new(source);
        let result = parser.parse_unit();
        assert!(result.is_ok(), "Parse error: {:?}", result.err());

        let unit = result.unwrap();
        assert_eq!(unit.name, "TestUnit");
        assert_eq!(unit.interface.functions.len(), 1);
        assert_eq!(unit.interface.functions[0].name, "Add");
    }
}
