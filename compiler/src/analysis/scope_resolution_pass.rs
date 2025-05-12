use crate::analysis::semantic_analysis::FunctionMap;
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::syntax::ast::{Assignment, Declaration, FunctionDeclaration, Identifier, StatementList};
use crate::syntax::span::Span;
use crate::util::handle::Handle;
use rustc_hash::FxHashMap;

#[derive(Default)]
pub struct ScopeReferenceMap {
    pub(crate) references: FxHashMap<Handle, Handle>,
}

pub(crate) struct ScopeResolutionPass<'ast, 'f> {
    /// Every item of the vec maps an identifier to the handle declaring the identifier.
    /// The topmost item is the currently active scope.
    environment_stack: Vec<FxHashMap<&'ast str, Handle>>,
    /// Maps an identifier handle to its declaration handle.
    reference_map: ScopeReferenceMap,

    semantic_error_list: &'f mut SemanticErrorList,
}

impl<'ast, 'f> ScopeResolutionPass<'ast, 'f> {
    pub fn new(semantic_error_list: &'f mut SemanticErrorList) -> Self {
        Self {
            environment_stack: Default::default(),
            reference_map: Default::default(),
            semantic_error_list,
        }
    }

    pub fn register_functions(&mut self, function_map: &'ast FunctionMap<'ast>) {
        assert!(self.environment_stack.is_empty());
        self.push_scope();
        for function_declaration in function_map.values() {
            self.bind(function_declaration.name(), function_declaration.declaration_handle());
        }
    }

    pub fn into_reference_map(self) -> ScopeReferenceMap {
        self.reference_map
    }

    fn push_scope(&mut self) {
        self.environment_stack.push(Default::default());
    }

    fn pop_scope(&mut self) {
        self.environment_stack.pop();
    }

    fn bind(&mut self, identifier: &'ast str, declaration: Handle) {
        self.environment_stack.last_mut().expect("at least one scope must be active").insert(identifier, declaration);
    }

    fn resolve(&self, identifier: &str) -> Option<Handle> {
        for scope in self.environment_stack.iter().rev() {
            if let Some(handle) = scope.get(identifier) {
                return Some(*handle);
            }
        }
        None
    }

    fn reference(&mut self, referencing_handle: Handle, refereed_handle: Handle) {
        self.reference_map.references.insert(referencing_handle, refereed_handle);
    }
}

impl<'ast, 'f> ScopeResolutionPass<'ast, 'f> {
    fn declare(&mut self, identifier: &'ast str, declaration: Handle, span: Span) {
        // TODO: once map's try_insert stabilizes, use that API in `bind`!
        if self.resolve(identifier).is_none() {
            self.bind(identifier, declaration);
            self.reference(declaration, declaration);
        } else {
            self.semantic_error_list
                .report_error(span, format!("identifier '{}' is already declared in the current scope", identifier));
        }
    }

    fn check_binding(&mut self, handle: Handle, identifier: &str, span: Span) {
        if let Some(declaration_handle) = self.resolve(identifier) {
            self.reference(handle, declaration_handle);
        } else {
            self.semantic_error_list.report_error(span, format!("identifier '{}' was not found in the current scope", identifier));
        }
    }
}

impl<'ast, 'f> SemanticAnalysisPass<'ast, ()> for ScopeResolutionPass<'ast, 'f> {
    fn visit_identifier(&mut self, handle: Handle, node: &'ast Identifier<'ast>, span: Span) {
        self.check_binding(handle, node.0, span);
    }

    fn visit_assignment(&mut self, handle: Handle, node: &'ast Assignment<'ast>, _: Span) {
        self.visit(&node.1);
        self.check_binding(handle, node.0 .0, node.0 .1);
    }

    fn visit_declaration(&mut self, handle: Handle, node: &'ast Declaration<'ast>, span: Span) {
        self.visit(&node.assignment.1);
        self.declare(node.assignment.0 .0, handle, span);
    }

    fn visit_statement_list(&mut self, _: Handle, node: &'ast StatementList<'ast>, _: Span) {
        self.push_scope();
        for statement in &node.0 {
            self.visit(statement);
        }
        self.pop_scope();
    }

    fn visit_function_declaration(&mut self, _: Handle, node: &'ast FunctionDeclaration<'ast>, _: Span) {
        self.push_scope();
        for (arg, arg_span) in &node.args {
            self.declare(arg.name(), arg.as_handle(), *arg_span);
        }
        self.visit(&node.statements);
        self.pop_scope();
    }
}
