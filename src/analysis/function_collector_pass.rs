use crate::analysis::semantic_analysis::FunctionMap;
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::syntax::ast::FunctionDeclaration;
use crate::syntax::span::Span;
use crate::types::function_info::FunctionInfo;
use crate::util::handle::Handle;
use std::collections::HashMap;

pub(crate) struct FunctionCollectorPass<'f, 'ast> {
    pub(crate) function_map: FunctionMap<'ast>,
    pub(crate) semantic_error_list: &'f mut SemanticErrorList,
    pub(crate) seen_function_names: HashMap<&'ast str, Span>,
}

impl<'f, 'ast> FunctionCollectorPass<'f, 'ast> {
    pub fn new(semantic_error_list: &'f mut SemanticErrorList) -> Self {
        Self {
            function_map: Default::default(),
            semantic_error_list,
            seen_function_names: Default::default(),
        }
    }
}

impl<'f, 'ast> SemanticAnalysisPass<'ast, ()> for FunctionCollectorPass<'f, 'ast> {
    fn visit_function_declaration(&mut self, handle: Handle, node: &'ast FunctionDeclaration<'ast>, span: Span) {
        // TODO: in the future, when we support lambdas and closures, we should visit the function bodies
        match self.seen_function_names.get(node.name) {
            Some(previous_span) => {
                self.semantic_error_list.report_error_with_note(
                    span,
                    format!("the function '{}' was already declared in this scope", node.name),
                    *previous_span,
                    "previously declared here".into(),
                );
            }
            None => {
                self.seen_function_names.insert(node.name, span);
                self.function_map
                    .insert(handle, FunctionInfo::new(node.name, &node.statements, &node.args, node.return_type.clone(), handle));
            }
        }
    }
}
