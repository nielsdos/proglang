use crate::analysis::semantic_analysis::ClassMap;
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::syntax::ast::Class;
use crate::syntax::span::Span;
use crate::util::handle::Handle;
use std::collections::HashMap;

pub(crate) struct ClassCollectorPass<'f, 'ast> {
    class_map: ClassMap<'ast>,
    semantic_error_list: &'f mut SemanticErrorList,
    seen_class_names: HashMap<&'ast str, Span>,
}

impl<'f, 'ast> ClassCollectorPass<'f, 'ast> {
    pub fn new(semantic_error_list: &'f mut SemanticErrorList) -> Self {
        Self {
            class_map: Default::default(),
            semantic_error_list,
            seen_class_names: Default::default(),
        }
    }

    pub fn into_class_map(self) -> ClassMap<'ast> {
        self.class_map
    }
}

impl<'f, 'ast> SemanticAnalysisPass<'ast, ()> for ClassCollectorPass<'f, 'ast> {
    fn visit_class(&mut self, handle: Handle, node: &'ast Class<'ast>, span: Span) {
        match self.seen_class_names.get(node.name) {
            Some(previous_span) => {
                self.semantic_error_list
                    .report_error_with_note(span, format!("the class '{}' was already declared", node.name), *previous_span, "previously declared here".into());
            }
            None => {
                // TODO: check for duplicate fields
                self.seen_class_names.insert(node.name, span);
                self.class_map.insert(handle, node);
            }
        }
    }
}
