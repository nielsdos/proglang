use crate::ast::Ast;
use crate::span::Spanned;
use crate::type_system::Type;
use std::collections::HashMap;

#[derive(Debug)]
pub struct FunctionInfo<'ast> {
    body: &'ast Spanned<Ast<'ast>>,
    variable_types: HashMap<&'ast str, Type>,
    return_type: Type,
}

pub enum VariableUpdateError {
    TypeMismatch(Type),
}

impl<'ast> FunctionInfo<'ast> {
    pub fn new(body: &'ast Spanned<Ast<'ast>>, return_type: Type) -> Self {
        Self {
            body,
            variable_types: HashMap::new(),
            return_type,
        }
    }

    pub fn query_variable_type(&self, identifier: &'ast str) -> Option<&Type> {
        self.variable_types.get(identifier)
    }

    pub fn update_variable_type(&mut self, identifier: &'ast str, ty: Type) -> Result<(), VariableUpdateError> {
        let old_type = self.variable_types.insert(identifier, ty);
        if let Some(old_type) = old_type {
            if ty == old_type {
                Ok(())
            } else {
                Err(VariableUpdateError::TypeMismatch(old_type))
            }
        } else {
            Ok(())
        }
    }

    pub fn variables(&self) -> impl Iterator<Item = (&'ast str, &Type)> {
        self.variable_types.iter().map(|(k, v)| (*k, v))
    }

    pub fn body(&self) -> &'ast Spanned<Ast<'ast>> {
        self.body
    }

    pub fn return_type(&self) -> Type {
        self.return_type
    }
}
