//! Advanced type system features
//! 
//! Generic types, type inference improvements, operator overloading

use minipas_ast::{Type, Expr, BinaryOp, UnaryOp};
use std::collections::HashMap;
use anyhow::{Result, anyhow};

/// Generic type parameter
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParameter {
    pub name: String,
    pub constraints: Vec<TypeConstraint>,
}

/// Type constraint for generics
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeConstraint {
    /// Must implement a specific interface
    Interface(String),
    /// Must be a numeric type
    Numeric,
    /// Must be comparable
    Comparable,
    /// Must be a reference type
    Reference,
}

/// Generic type
#[derive(Debug, Clone)]
pub struct GenericType {
    pub base_type: String,
    pub type_params: Vec<TypeParameter>,
    pub instantiations: Vec<(Vec<String>, Type)>,
}

impl GenericType {
    /// Create a new generic type
    pub fn new(base_type: String, type_params: Vec<TypeParameter>) -> Self {
        Self {
            base_type,
            type_params,
            instantiations: Vec::new(),
        }
    }
    
    /// Instantiate generic type with concrete types
    pub fn instantiate(&mut self, concrete_types: Vec<Type>) -> Result<Type> {
        // Check number of type parameters
        if concrete_types.len() != self.type_params.len() {
            return Err(anyhow!(
                "Expected {} type parameters, got {}",
                self.type_params.len(),
                concrete_types.len()
            ));
        }
        
        // Check constraints
        for (param, concrete) in self.type_params.iter().zip(concrete_types.iter()) {
            self.check_constraints(param, concrete)?;
        }
        
        // Create type key
        let type_key: Vec<String> = concrete_types.iter()
            .map(|t| format!("{:?}", t))
            .collect();
        
        // Check if already instantiated
        if let Some((_, instantiated)) = self.instantiations.iter()
            .find(|(key, _)| key == &type_key)
        {
            return Ok(instantiated.clone());
        }
        
        // Create new instantiation
        let instantiated = Type::Custom(
            format!("{}<{}>", self.base_type, type_key.join(", "))
        );
        
        self.instantiations.push((type_key, instantiated.clone()));
        Ok(instantiated)
    }
    
    /// Check type constraints
    fn check_constraints(&self, param: &TypeParameter, concrete: &Type) -> Result<()> {
        for constraint in &param.constraints {
            match constraint {
                TypeConstraint::Numeric => {
                    if !matches!(concrete, Type::Integer | Type::Real) {
                        return Err(anyhow!("Type must be numeric"));
                    }
                }
                TypeConstraint::Comparable => {
                    // Most types are comparable
                }
                TypeConstraint::Reference => {
                    if !matches!(concrete, Type::Pointer(_) | Type::String(_)) {
                        return Err(anyhow!("Type must be a reference type"));
                    }
                }
                TypeConstraint::Interface(_) => {
                    // Would check interface implementation
                }
            }
        }
        Ok(())
    }
}

/// Type inference engine
pub struct TypeInference {
    type_vars: HashMap<String, Type>,
    constraints: Vec<TypeConstraint>,
    next_type_var: u32,
}

impl TypeInference {
    /// Create a new type inference engine
    pub fn new() -> Self {
        Self {
            type_vars: HashMap::new(),
            constraints: Vec::new(),
            next_type_var: 0,
        }
    }
    
    /// Create a fresh type variable
    pub fn fresh_type_var(&mut self) -> Type {
        let var_name = format!("T{}", self.next_type_var);
        self.next_type_var += 1;
        Type::Custom(var_name)
    }
    
    /// Infer type from expression
    pub fn infer_expr(&mut self, expr: &Expr) -> Result<Type> {
        match expr {
            Expr::Literal(lit) => {
                Ok(match lit {
                    minipas_ast::Literal::Integer(_) => Type::Integer,
                    minipas_ast::Literal::Real(_) => Type::Real,
                    minipas_ast::Literal::Boolean(_) => Type::Boolean,
                    minipas_ast::Literal::Char(_) => Type::Char,
                    minipas_ast::Literal::String(_) => Type::String(None),
                    _ => Type::Integer,
                })
            }
            
            Expr::Identifier(parts) => {
                // Look up in type environment
                if let Some(name) = parts.first() {
                    if let Some(typ) = self.type_vars.get(name) {
                        Ok(typ.clone())
                    } else {
                        // Create fresh type variable
                        let typ = self.fresh_type_var();
                        self.type_vars.insert(name.clone(), typ.clone());
                        Ok(typ)
                    }
                } else {
                    Err(anyhow!("Empty identifier"))
                }
            }
            
            Expr::BinaryOp { op, left, right } => {
                let left_type = self.infer_expr(left)?;
                let right_type = self.infer_expr(right)?;
                
                // Unify types
                self.unify(&left_type, &right_type)?;
                
                match op {
                    BinaryOp::Add | BinaryOp::Subtract | 
                    BinaryOp::Multiply | BinaryOp::Divide => {
                        Ok(left_type)
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual |
                    BinaryOp::Less | BinaryOp::LessOrEqual |
                    BinaryOp::Greater | BinaryOp::GreaterOrEqual => {
                        Ok(Type::Boolean)
                    }
                    _ => Ok(left_type),
                }
            }
            
            _ => Ok(Type::Integer),
        }
    }
    
    /// Unify two types
    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<()> {
        match (t1, t2) {
            (Type::Integer, Type::Integer) => Ok(()),
            (Type::Real, Type::Real) => Ok(()),
            (Type::Boolean, Type::Boolean) => Ok(()),
            (Type::Integer, Type::Real) | (Type::Real, Type::Integer) => {
                // Allow integer to real promotion
                Ok(())
            }
            _ => Err(anyhow!("Cannot unify types {:?} and {:?}", t1, t2)),
        }
    }
    
    /// Resolve all type variables
    pub fn resolve(&self) -> HashMap<String, Type> {
        self.type_vars.clone()
    }
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

/// Operator overload definition
#[derive(Debug, Clone)]
pub struct OperatorOverload {
    pub operator: OverloadableOperator,
    pub left_type: Type,
    pub right_type: Option<Type>,
    pub return_type: Type,
    pub implementation: String,
}

/// Overloadable operators
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OverloadableOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    Less,
    Greater,
    Index,
    Call,
}

impl OverloadableOperator {
    /// Convert from BinaryOp
    pub fn from_binary_op(op: &BinaryOp) -> Option<Self> {
        match op {
            BinaryOp::Add => Some(Self::Add),
            BinaryOp::Subtract => Some(Self::Subtract),
            BinaryOp::Multiply => Some(Self::Multiply),
            BinaryOp::Divide => Some(Self::Divide),
            BinaryOp::Equal => Some(Self::Equal),
            BinaryOp::NotEqual => Some(Self::NotEqual),
            BinaryOp::Less => Some(Self::Less),
            BinaryOp::Greater => Some(Self::Greater),
            _ => None,
        }
    }
}

/// Operator overload registry
pub struct OperatorRegistry {
    overloads: Vec<OperatorOverload>,
}

impl OperatorRegistry {
    /// Create a new operator registry
    pub fn new() -> Self {
        Self {
            overloads: Vec::new(),
        }
    }
    
    /// Register an operator overload
    pub fn register(&mut self, overload: OperatorOverload) {
        self.overloads.push(overload);
    }
    
    /// Look up operator overload
    pub fn lookup(&self, op: &OverloadableOperator, left: &Type, right: Option<&Type>) -> Option<&OperatorOverload> {
        self.overloads.iter().find(|o| {
            o.operator == *op && 
            format!("{:?}", o.left_type) == format!("{:?}", left) &&
            o.right_type.as_ref().map(|t| format!("{:?}", t)) == right.map(|t| format!("{:?}", t))
        })
    }
    
    /// Check if operator is overloaded for types
    pub fn is_overloaded(&self, op: &OverloadableOperator, left: &Type, right: Option<&Type>) -> bool {
        self.lookup(op, left, right).is_some()
    }
}

impl Default for OperatorRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Type class for ad-hoc polymorphism
#[derive(Debug, Clone)]
pub struct TypeClass {
    pub name: String,
    pub methods: Vec<TypeClassMethod>,
    pub instances: Vec<TypeClassInstance>,
}

#[derive(Debug, Clone)]
pub struct TypeClassMethod {
    pub name: String,
    pub signature: Type,
}

#[derive(Debug, Clone)]
pub struct TypeClassInstance {
    pub typ: Type,
    pub implementations: HashMap<String, String>,
}

impl TypeClass {
    /// Create a new type class
    pub fn new(name: String) -> Self {
        Self {
            name,
            methods: Vec::new(),
            instances: Vec::new(),
        }
    }
    
    /// Add a method to the type class
    pub fn add_method(&mut self, name: String, signature: Type) {
        self.methods.push(TypeClassMethod { name, signature });
    }
    
    /// Add an instance for a type
    pub fn add_instance(&mut self, typ: Type, implementations: HashMap<String, String>) {
        self.instances.push(TypeClassInstance {
            typ,
            implementations,
        });
    }
    
    /// Check if type implements this type class
    pub fn has_instance(&self, typ: &Type) -> bool {
        self.instances.iter().any(|inst| format!("{:?}", inst.typ) == format!("{:?}", typ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_generic_type() {
        let mut generic = GenericType::new(
            "Array".to_string(),
            vec![TypeParameter {
                name: "T".to_string(),
                constraints: vec![],
            }],
        );
        
        let int_array = generic.instantiate(vec![Type::Integer]).unwrap();
        assert!(matches!(int_array, Type::Custom { .. }));
    }
    
    #[test]
    fn test_type_inference() {
        let mut inference = TypeInference::new();
        
        let expr = Expr::Literal(minipas_ast::Literal::Integer(42));
        let typ = inference.infer_expr(&expr).unwrap();
        
        assert_eq!(typ, Type::Integer);
    }
    
    #[test]
    fn test_operator_overload() {
        let mut registry = OperatorRegistry::new();
        
        registry.register(OperatorOverload {
            operator: OverloadableOperator::Add,
            left_type: Type::String(None),
            right_type: Some(Type::String(None)),
            return_type: Type::String(None),
            implementation: "string_concat".to_string(),
        });
        
        assert!(registry.is_overloaded(
            &OverloadableOperator::Add,
            &Type::String(None),
            Some(&Type::String(None))
        ));
    }
}
