use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::syntax::ast::{AstHandle, FunctionDeclaration, IfStatement, ReturnStatement, StatementList};
use crate::syntax::span::Span;
use crate::types::type_system::Type;

pub(crate) struct ReturnCheckPass<'f> {
    pub(crate) semantic_error_list: &'f mut SemanticErrorList,
}

impl<'f, 'ast> SemanticAnalysisPass<'ast, bool> for ReturnCheckPass<'f> {
    fn visit_statement_list(&mut self, _: AstHandle, node: &'ast StatementList<'ast>, _: Span) -> bool {
        for statement in &node.0 {
            if self.visit(statement) {
                return true;
            }
        }
        false
    }

    fn visit_function_declaration(&mut self, _: AstHandle, node: &'ast FunctionDeclaration<'ast>, span: Span) -> bool {
        if node.return_type == Type::Void {
            return false;
        }

        if !self.visit(&node.statements) {
            self.semantic_error_list.report_error(span, format!("function '{}' does not always return a value", node.name));
            return false;
        }
        // Return value doesn't matter, only the error reporting matters
        false
    }

    fn visit_if_statement(&mut self, _: AstHandle, node: &'ast IfStatement<'ast>, _: Span) -> bool {
        if let Some(else_statements) = &node.else_statements {
            self.visit(&node.then_statements) && self.visit(else_statements)
        } else {
            false
        }
    }

    fn visit_return_statement(&mut self, _: AstHandle, _: &'ast ReturnStatement<'ast>, _: Span) -> bool {
        true
    }
}
