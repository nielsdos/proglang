use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::analysis::unique_function_identifier::UniqueFunctionIdentifier;
use crate::syntax::ast::{AstHandle, FunctionDeclaration};
use crate::syntax::span::Span;
use crate::types::function_info::FunctionInfo;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub(crate) struct FunctionCollectorPass<'f, 'ast> {
    pub(crate) function_map: HashMap<UniqueFunctionIdentifier<'ast>, FunctionInfo<'ast>>,
    pub(crate) semantic_error_list: &'f mut SemanticErrorList,
}

impl<'f, 'ast> SemanticAnalysisPass<'ast, ()> for FunctionCollectorPass<'f, 'ast> {
    fn visit_function_declaration(&mut self, _: AstHandle, node: &'ast FunctionDeclaration<'ast>, span: Span) {
        // TODO: in the future, when we support lambdas and closures, we should visit the function bodies
        match self.function_map.entry(UniqueFunctionIdentifier(node.name)) {
            Entry::Occupied(o) => {
                self.semantic_error_list.report_error_with_note(
                    span,
                    format!("the function '{}' was already declared in this scope", node.name),
                    o.get().body().1,
                    "previously declared here".into(),
                );
            }
            Entry::Vacant(v) => {
                v.insert(FunctionInfo::new(&node.statements, &node.args, node.return_type));
            }
        }
    }
}
