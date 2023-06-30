use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::types::UniqueFunctionIdentifier;
use crate::ast::{AstHandle, FunctionDeclaration};
use crate::function_info::FunctionInfo;
use crate::span::Span;
use std::collections::HashMap;

pub(crate) struct FunctionCollectorPass<'ast> {
    pub(crate) function_map: HashMap<UniqueFunctionIdentifier<'ast>, FunctionInfo<'ast>>,
}

impl<'ast> SemanticAnalysisPass<'ast, ()> for FunctionCollectorPass<'ast> {
    fn visit_function_declaration(&mut self, _: AstHandle, node: &'ast FunctionDeclaration<'ast>, _: Span) {
        // TODO: in the future, when we support lambdas and closures, we should visit the function bodies
        self.function_map.insert(UniqueFunctionIdentifier(node.name), FunctionInfo::new(&node.statements, node.return_type));
    }
}
