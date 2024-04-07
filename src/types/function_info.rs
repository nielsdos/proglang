use crate::syntax::ast::{Ast, BindingType};
use crate::syntax::span::Spanned;
use crate::types::type_system::{FunctionType, Type};
use crate::util::handle::Handle;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct ArgumentInfo<'ast> {
    name: &'ast str,
    ty: Type<'ast>,
    binding: BindingType,
    default_value: Option<Spanned<Ast<'ast>>>
}

#[derive(Debug)]
pub struct FunctionInfo<'ast> {
    body: &'ast Spanned<Ast<'ast>>,
    variable_types: HashMap<Handle, Type<'ast>>,
    args: &'ast [Spanned<ArgumentInfo<'ast>>],
    name: &'ast str,
    declaration_handle: Handle,
    function_type: Rc<FunctionType<'ast>>,
    // TODO: flags field instead?
    always_inline: bool,
}

#[derive(Debug)]
pub enum VariableUpdateError<'ast> {
    TypeMismatch(Type<'ast>),
}

impl<'ast> ArgumentInfo<'ast> {
    pub fn new(name: &'ast str, ty: Type<'ast>, binding: BindingType, default_value: Option<Spanned<Ast<'ast>>>) -> Self {
        Self { name, ty, binding, default_value }
    }

    #[inline]
    pub fn name(&self) -> &'ast str {
        self.name
    }

    #[inline]
    pub fn ty(&self) -> &Type<'ast> {
        &self.ty
    }

    #[inline]
    pub fn binding(&self) -> BindingType {
        self.binding
    }

    // TODO: this isn't stable upon resize???
    #[inline]
    pub fn as_handle(&self) -> Handle {
        self as *const _ as Handle
    }
}

impl<'ast> FunctionInfo<'ast> {
    pub fn new(name: &'ast str, body: &'ast Spanned<Ast<'ast>>, args: &'ast [Spanned<ArgumentInfo<'ast>>], return_type: Type<'ast>, declaration_handle: Handle) -> Self {
        let function_type = FunctionType {
            return_type,
            arg_types: args.iter().map(|arg| arg.0.ty.clone()).collect(),
        };

        Self {
            body,
            variable_types: HashMap::new(),
            args,
            name,
            declaration_handle,
            function_type: Rc::new(function_type),
            always_inline: false,
        }
    }

    pub fn set_always_inline(&mut self, always_inline: bool) {
        self.always_inline = always_inline;
    }

    pub fn is_always_inline(&self) -> bool {
        self.always_inline
    }

    pub fn query_variable_type(&self, handle: Handle) -> Option<&Type<'ast>> {
        self.variable_types.get(&handle)
    }

    pub fn update_variable_type(&mut self, handle: Handle, ty: Type<'ast>) -> Result<(), VariableUpdateError<'ast>> {
        match self.variable_types.entry(handle) {
            Entry::Occupied(entry) => {
                if *entry.get() == ty {
                    Ok(())
                } else {
                    Err(VariableUpdateError::TypeMismatch(entry.get().clone()))
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(ty);
                Ok(())
            }
        }
    }

    pub fn variables(&self) -> impl Iterator<Item = (Handle, &Type<'_>)> {
        self.variable_types.iter().map(|(k, v)| (*k, v))
    }

    #[inline]
    pub fn body(&self) -> &'ast Spanned<Ast<'ast>> {
        self.body
    }

    #[inline]
    pub fn args(&self) -> &'ast [Spanned<ArgumentInfo<'ast>>] {
        self.args
    }

    #[inline]
    pub fn declaration_handle(&self) -> Handle {
        self.declaration_handle
    }

    #[inline]
    pub fn function_type(&self) -> Rc<FunctionType<'ast>> {
        self.function_type.clone()
    }

    #[inline]
    pub fn return_type(&self) -> &Type<'ast> {
        &self.function_type.return_type
    }

    #[inline]
    pub fn name(&self) -> &'ast str {
        self.name
    }
}
