use crate::ast::{Assignment, Ast, AstHandle, BinaryOperation, FunctionDeclaration, Identifier, IfStatement, LiteralBool, LiteralDouble, LiteralInt, StatementList, UnaryOperation};
use crate::span::{Span, Spanned};

pub trait SemanticAnalysisPass<'ast, T: Default> {
    fn visit(&mut self, node: &'ast Spanned<Ast<'ast>>) -> T {
        let handle = node.0.as_handle();
        match &node.0 {
            Ast::LiteralInt(inner) => self.visit_literal_int(handle, inner, node.1),
            Ast::LiteralDouble(inner) => self.visit_literal_double(handle, inner, node.1),
            Ast::LiteralBool(inner) => self.visit_literal_bool(handle, inner, node.1),
            Ast::Identifier(inner) => self.visit_identifier(handle, inner, node.1),
            Ast::BinaryOperation(inner) => self.visit_binary_operation(handle, inner, node.1),
            Ast::UnaryOperation(inner) => self.visit_unary_operation(handle, inner, node.1),
            Ast::Assignment(inner) => self.visit_assignment(handle, inner, node.1),
            Ast::StatementList(inner) => self.visit_statement_list(handle, inner, node.1),
            Ast::FunctionDeclaration(inner) => self.visit_function_declaration(handle, inner, node.1),
            Ast::IfStatement(inner) => self.visit_if_statement(handle, inner, node.1),
            Ast::Todo => todo!(),
        }
    }

    fn visit_literal_int(&mut self, _: AstHandle, _: &'ast LiteralInt, _: Span) -> T {
        T::default()
    }

    fn visit_literal_double(&mut self, _: AstHandle, _: &'ast LiteralDouble, _: Span) -> T {
        T::default()
    }

    fn visit_literal_bool(&mut self, _: AstHandle, _: &'ast LiteralBool, _: Span) -> T {
        T::default()
    }

    fn visit_identifier(&mut self, _: AstHandle, _: &'ast Identifier<'ast>, _: Span) -> T {
        T::default()
    }

    fn visit_binary_operation(&mut self, _: AstHandle, node: &'ast BinaryOperation<'ast>, _: Span) -> T {
        self.visit(&node.0);
        self.visit(&node.2);
        T::default()
    }

    fn visit_unary_operation(&mut self, _: AstHandle, node: &'ast UnaryOperation<'ast>, _: Span) -> T {
        self.visit(&node.1);
        T::default()
    }

    fn visit_assignment(&mut self, _: AstHandle, node: &'ast Assignment<'ast>, _: Span) -> T {
        self.visit(&node.1);
        T::default()
    }

    fn visit_statement_list(&mut self, _: AstHandle, node: &'ast StatementList<'ast>, _: Span) -> T {
        for statement in &node.0 {
            self.visit(statement);
        }
        T::default()
    }

    fn visit_function_declaration(&mut self, _: AstHandle, node: &'ast FunctionDeclaration<'ast>, _: Span) -> T {
        self.visit(&node.statements);
        T::default()
    }

    fn visit_if_statement(&mut self, _: AstHandle, node: &'ast IfStatement<'ast>, _: Span) -> T {
        self.visit(&node.condition);
        self.visit(&node.statements);
        T::default()
    }
}
