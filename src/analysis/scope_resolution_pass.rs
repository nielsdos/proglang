use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::syntax::ast::{Assignment, AstHandle, FunctionDeclaration, Identifier};
use crate::syntax::span::Span;
use std::collections::HashMap;

#[derive(Default)]
pub struct ScopeReferenceMap {
    pub(crate) references: HashMap<AstHandle, AstHandle>,
}

pub(crate) struct ScopeResolutionPass<'ast, 'f> {
    /// Every item of the vec maps an identifier to the handle declaring the identifier.
    /// The topmost item is the currently active scope.
    pub(crate) environment_stack: Vec<HashMap<&'ast str, AstHandle>>,
    /// Maps an identifier handle to its declaration handle.
    pub(crate) reference_map: ScopeReferenceMap,

    pub(crate) semantic_error_list: &'f mut SemanticErrorList,
}

impl<'ast, 'f> ScopeResolutionPass<'ast, 'f> {
    pub fn new(semantic_error_list: &'f mut SemanticErrorList) -> Self {
        Self {
            environment_stack: Default::default(),
            reference_map: Default::default(),
            semantic_error_list,
        }
    }

    fn push_scope(&mut self) {
        self.environment_stack.push(Default::default());
    }

    fn pop_scope(&mut self) {
        self.environment_stack.pop();
    }

    fn bind(&mut self, identifier: &'ast str, declaration: AstHandle) {
        self.environment_stack.last_mut().expect("at least one scope must be active").insert(identifier, declaration);
    }

    fn resolve(&self, identifier: &str) -> Option<AstHandle> {
        for scope in self.environment_stack.iter().rev() {
            if let Some(handle) = scope.get(identifier) {
                return Some(*handle);
            }
        }
        None
    }

    fn reference(&mut self, referencing_handle: AstHandle, refereed_handle: AstHandle) {
        self.reference_map.references.insert(referencing_handle, refereed_handle);
    }
}

impl<'ast, 'f> ScopeResolutionPass<'ast, 'f> {
    fn declare(&mut self, identifier: &'ast str, declaration: AstHandle, span: Span) {
        // TODO: once map's try_insert stabilizes, use that API in `bind`!
        if self.resolve(identifier).is_none() {
            self.bind(identifier, declaration);
            self.reference(declaration, declaration);
        } else {
            self.semantic_error_list
                .report_error(span, format!("identifier '{}' is already declared in the current scope", identifier));
        }
    }

    fn check_binding(&mut self, handle: AstHandle, identifier: &str, span: Span) {
        if let Some(declaration_handle) = self.resolve(identifier) {
            self.reference(handle, declaration_handle);
        } else {
            // TODO: improve span to only highlight the identifier?
            self.semantic_error_list.report_error(span, format!("identifier '{}' was not found in the current scope", identifier));
        }
    }
}

impl<'ast, 'f> SemanticAnalysisPass<'ast, ()> for ScopeResolutionPass<'ast, 'f> {
    fn visit_identifier(&mut self, handle: AstHandle, node: &'ast Identifier<'ast>, span: Span) {
        self.check_binding(handle, node.0, span);
    }

    fn visit_assignment(&mut self, handle: AstHandle, node: &'ast Assignment<'ast>, span: Span) {
        self.visit(&node.1);
        self.check_binding(handle, node.0, span);
    }

    fn visit_declaration(&mut self, handle: AstHandle, node: &'ast Assignment<'ast>, span: Span) {
        self.visit(&node.1);
        // TODO: improve span?
        self.declare(node.0, handle, span);
    }

    fn visit_function_declaration(&mut self, _: AstHandle, node: &'ast FunctionDeclaration<'ast>, _: Span) {
        self.push_scope();
        for (arg, arg_span) in &node.args {
            self.declare(arg.name(), arg.as_handle(), *arg_span);
        }
        self.visit(&node.statements);
        self.pop_scope();
    }
}
