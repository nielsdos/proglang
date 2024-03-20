use crate::syntax::ast::{Ast, AstHandle};
use crate::syntax::span::Spanned;
use crate::types::type_system::Type;
use std::collections::HashMap;

#[derive(Debug)]
pub struct ArgumentInfo<'ast> {
    name: &'ast str,
    ty: Type,
}

#[derive(Debug)]
pub struct FunctionInfo<'ast> {
    body: &'ast Spanned<Ast<'ast>>,
    variable_types: HashMap<AstHandle, Type>,
    args: &'ast [Spanned<ArgumentInfo<'ast>>],
    return_type: Type,
}

#[derive(Debug)]
pub enum VariableUpdateError {
    TypeMismatch(Type),
}

impl<'ast> ArgumentInfo<'ast> {
    pub fn new(name: &'ast str, ty: Type) -> Self {
        Self { name, ty }
    }

    #[inline]
    pub fn name(&self) -> &'ast str {
        self.name
    }

    #[inline]
    pub fn ty(&self) -> &Type {
        &self.ty
    }

    // TODO: hacky?
    pub fn as_handle(&self) -> AstHandle {
        self as *const _ as AstHandle
    }
}

impl<'ast> FunctionInfo<'ast> {
    pub fn new(body: &'ast Spanned<Ast<'ast>>, args: &'ast [Spanned<ArgumentInfo<'ast>>], return_type: Type) -> Self {
        Self {
            body,
            variable_types: HashMap::new(),
            args,
            return_type,
        }
    }

    pub fn query_variable_type(&self, handle: AstHandle) -> Option<&Type> {
        self.variable_types.get(&handle)
    }

    pub fn update_variable_type(&mut self, handle: AstHandle, ty: Type) -> Result<(), VariableUpdateError> {
        // TODO: avoid clone?
        let old_type = self.variable_types.insert(handle, ty.clone());
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

    pub fn variables(&self) -> impl Iterator<Item = (AstHandle, &Type)> {
        self.variable_types.iter().map(|(k, v)| (*k, v))
    }

    #[inline]
    pub fn body(&self) -> &'ast Spanned<Ast<'ast>> {
        self.body
    }

    #[inline]
    pub fn return_type(&self) -> &Type {
        &self.return_type
    }

    #[inline]
    pub fn args(&self) -> &'ast [Spanned<ArgumentInfo<'ast>>] {
        self.args
    }
}
