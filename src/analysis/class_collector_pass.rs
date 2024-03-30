use crate::analysis::semantic_analysis::ClassMap;
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::syntax::ast::Class;
use crate::syntax::span::Span;
use crate::types::class_info::ClassInfo;
use crate::util::handle::Handle;
use std::collections::HashMap;

pub(crate) struct ClassCollectorPass<'f, 'ast> {
    class_map: ClassMap<'ast>,
    semantic_error_list: &'f mut SemanticErrorList,
    // TODO: merge with class_map itself
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
    fn visit_class(&mut self, _: Handle, node: &'ast Class<'ast>, span: Span) {
        match self.seen_class_names.get(node.name) {
            Some(previous_span) => {
                self.semantic_error_list
                    .report_error_with_note(span, format!("the class '{}' was already declared", node.name), *previous_span, "previously declared here".into());
            }
            None => {
                self.seen_class_names.insert(node.name, span);
                let mut class_info = ClassInfo::new(node.name);
                for field in &node.fields {
                    if let Some(existing_field_span) = class_info.add_field(field) {
                        self.semantic_error_list.report_error_with_note(
                            field.1,
                            format!("the field '{}' was already declared", field.0.name),
                            existing_field_span,
                            "previously declared here".into(),
                        );
                    }
                }
                self.class_map.insert(node.name, class_info);
            }
        }
    }
}
