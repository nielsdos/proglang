//! Type graph construction
//! This construct a directed graph that serves 2 purposes:
//! 1) Checks for cycles caused by structs (e.g. type A contains a field of type B and vice versa)
//! 2) Detects undeclared but used types

use crate::analysis::semantic_analysis::ClassMap;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::syntax::span::Span;
use crate::types::class_info::ClassInfo;
use crate::types::type_system::Type;
use std::collections::HashSet;

pub struct TypeGraphPass<'c> {
    class_map: &'c ClassMap<'c>,
    semantic_error_list: &'c mut SemanticErrorList,
    visited: HashSet<&'c str>,
    recursion_stack: HashSet<&'c str>,
}

impl<'c> TypeGraphPass<'c> {
    pub fn new(class_map: &'c ClassMap<'c>, semantic_error_list: &'c mut SemanticErrorList) -> Self {
        Self {
            class_map,
            semantic_error_list,
            visited: Default::default(),
            recursion_stack: Default::default(),
        }
    }

    pub fn check(&mut self) {
        for (name, info) in self.class_map.iter() {
            self.visit_aux(name, info, (0..0).into());
        }
    }

    fn recursion_stack_vec(&self) -> Vec<&'c str> {
        let mut result = Vec::with_capacity(self.recursion_stack.len());
        for &name in &self.recursion_stack {
            result.push(name);
        }
        result
    }

    fn visit_aux(&mut self, name: &'c str, info: &'c ClassInfo, span: Span) {
        if self.visited.insert(name) {
            self.recursion_stack.insert(name);
            self.visit(info);
        } else if self.recursion_stack.contains(name) {
            self.semantic_error_list.report_error(
                span,
                format!("This field will cause a struct cycle, which leads to an infinitely nested type. Break the cycle using a class instead of a struct type. Checked: {}", self.recursion_stack_vec().join(", ").to_string()),
            );
        }
        self.recursion_stack.remove(name);
    }

    fn visit(&mut self, info: &'c ClassInfo) {
        for field in info.fields_iter() {
            match field.ty() {
                Type::UserType(name) => match self.class_map.get(name) {
                    None => {
                        self.semantic_error_list.report_error(field.span(), format!("The type '{}' was not found", name));
                    }
                    Some(info) => {
                        self.visit_aux(name, info, field.span());
                    }
                },
                _ => {}
            }
        }
    }
}
