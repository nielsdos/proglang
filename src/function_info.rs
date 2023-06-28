use crate::ast::Ast;
use crate::span::Spanned;
use crate::type_system::Type;
use std::collections::HashMap;

#[derive(Debug)]
pub struct FunctionInfo<'ast> {
    body: &'ast Spanned<Ast<'ast>>,
    variable_types: HashMap<&'ast str, Type>,
}

impl<'ast> FunctionInfo<'ast> {
    pub fn new(body: &'ast Spanned<Ast<'ast>>) -> Self {
        Self { body, variable_types: HashMap::new() }
    }

    pub fn query_variable_type(&self, identifier: &'ast str) -> Option<&Type> {
        self.variable_types.get(identifier)
    }

    pub fn update_variable_type(&mut self, identifier: &'ast str, ty: Type) {
        let old_type = self.variable_types.insert(identifier, ty);
        if let Some(old_type) = old_type {
            // TODO: make fallible
            println!("{} {}", ty, old_type);
            //assert_eq!(ty, old_type);
        }
    }

    pub fn variables(&self) -> impl Iterator<Item = (&'ast str, &Type)> {
        self.variable_types.iter().map(|(k, v)| (*k, v))
    }

    pub fn body(&self) -> &'ast Spanned<Ast<'ast>> {
        self.body
    }
}
