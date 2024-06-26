use crate::syntax::ast::ClassField;
use crate::syntax::span::{Span, Spanned};
use crate::types::type_system::Type;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug)]
pub struct ClassFieldInfo<'ast> {
    class_field: &'ast Spanned<ClassField<'ast>>,
    index: u32,
}

#[derive(Debug)]
pub struct ClassInfo<'ast> {
    name: &'ast str,
    fields: FxHashMap<&'ast str, ClassFieldInfo<'ast>>,
}

impl<'ast> ClassInfo<'ast> {
    pub fn new(name: &'ast str) -> Self {
        Self { name, fields: Default::default() }
    }

    /// Tries to add a new field to the class info. If it already exists, returns the existing field.
    pub fn add_field(&mut self, class_field: &'ast Spanned<ClassField<'ast>>) -> Option<Span> {
        let index = self.fields.len();
        match self.fields.entry(class_field.0.name) {
            Entry::Occupied(entry) => Some(entry.get().span()),
            Entry::Vacant(entry) => {
                entry.insert(ClassFieldInfo {
                    class_field,
                    index: index.try_into().expect("index overflow"),
                });
                None
            }
        }
    }

    pub fn field(&self, name: &'ast str) -> Option<&ClassFieldInfo<'ast>> {
        self.fields.get(name)
    }

    pub fn fields_iter(&self) -> impl Iterator<Item = &ClassFieldInfo<'ast>> {
        let mut tmp = self.fields.values().collect::<Vec<_>>();
        tmp.sort_by(|a, b| a.index.cmp(&b.index));
        tmp.into_iter()
    }

    pub fn name(&self) -> &'ast str {
        self.name
    }
}

impl<'ast> ClassFieldInfo<'ast> {
    pub fn name(&self) -> &'ast str {
        self.class_field.0.name
    }

    pub fn ty(&self) -> &'ast Type<'ast> {
        &self.class_field.0.ty
    }

    pub fn span(&self) -> Span {
        self.class_field.1
    }

    pub fn index(&self) -> u32 {
        self.index
    }
}
