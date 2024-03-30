use crate::analysis::semantic_analysis::FunctionMap;
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::builtin::Builtins;
use crate::syntax::ast::FunctionDeclaration;
use crate::syntax::span::Span;
use crate::types::function_info::FunctionInfo;
use crate::util::handle::Handle;
use std::collections::HashMap;

pub(crate) struct FunctionCollectorPass<'f, 'ast> {
    function_map: FunctionMap<'ast>,
    semantic_error_list: &'f mut SemanticErrorList,
    seen_function_names: HashMap<&'ast str, Span>,
}

impl<'f, 'ast> FunctionCollectorPass<'f, 'ast> {
    pub fn new(semantic_error_list: &'f mut SemanticErrorList) -> Self {
        Self {
            function_map: Default::default(),
            semantic_error_list,
            seen_function_names: Default::default(),
        }
    }

    pub fn into_function_map(self) -> FunctionMap<'ast> {
        self.function_map
    }

    pub fn register_internal_functions(&mut self, builtins: &'ast Builtins<'ast>) {
        let float = builtins.get_builtin_float();
        let declaration_handle = float.name() as *const _ as *const ();

        self.seen_function_names.insert(float.name(), (0..0).into());
        let mut function_info = FunctionInfo::new(float.name(), float.body(), float.args(), float.return_type().clone(), declaration_handle);
        function_info.set_always_inline(true);
        for arg in float.args() {
            let _ = function_info.update_variable_type(arg.0.as_handle(), arg.0.ty().clone());
        }
        self.function_map.insert(declaration_handle, function_info);
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
