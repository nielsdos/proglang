use crate::syntax::ast::{
    Ast, BinaryOperation, ComplexAssignment, Declaration, FunctionCall, FunctionDeclaration, Identifier, IfStatement, LiteralBool, LiteralFloat, LiteralInt, MemberAccess, ReturnStatement,
    StatementList, TableConstructor, UnaryOperation, VariableAssignment, WhileLoop,
};
use crate::syntax::span::{Span, Spanned};
use crate::util::handle::Handle;

pub trait SemanticAnalysisPass<'ast, T: Default> {
    fn visit(&mut self, node: &'ast Spanned<Ast<'ast>>) -> T {
        let handle = node.0.as_handle();
        match &node.0 {
            Ast::LiteralInt(inner) => self.visit_literal_int(handle, inner, node.1),
            Ast::LiteralFloat(inner) => self.visit_literal_double(handle, inner, node.1),
            Ast::LiteralBool(inner) => self.visit_literal_bool(handle, inner, node.1),
            Ast::Identifier(inner) => self.visit_identifier(handle, inner, node.1),
            Ast::BinaryOperation(inner) => self.visit_binary_operation(handle, inner, node.1),
            Ast::UnaryOperation(inner) => self.visit_unary_operation(handle, inner, node.1),
            Ast::VariableAssignment(inner) => self.visit_variable_assignment(handle, inner, node.1),
            Ast::ComplexAssignment(inner) => self.visit_complex_assignment(handle, inner, node.1),
            Ast::Declaration(inner) => self.visit_declaration(handle, inner, node.1),
            Ast::StatementList(inner) => self.visit_statement_list(handle, inner, node.1),
            Ast::FunctionDeclaration(inner) => self.visit_function_declaration(handle, inner, node.1),
            Ast::IfStatement(inner) => self.visit_if_statement(handle, inner, node.1),
            Ast::WhileLoop(inner) => self.visit_while_statement(handle, inner, node.1),
            Ast::ReturnStatement(inner) => self.visit_return_statement(handle, inner, node.1),
            Ast::FunctionCall(inner) => self.visit_function_call(handle, inner, node.1),
            Ast::MemberAccess(inner) => self.visit_member_access(handle, inner, node.1),
            Ast::TableConstructor(inner) => self.visit_table_constructor(handle, inner, node.1),
            _ => todo!(),
        }
    }

    fn visit_literal_int(&mut self, _: Handle, _: &'ast LiteralInt, _: Span) -> T {
        T::default()
    }

    fn visit_literal_double(&mut self, _: Handle, _: &'ast LiteralFloat, _: Span) -> T {
        T::default()
    }

    fn visit_literal_bool(&mut self, _: Handle, _: &'ast LiteralBool, _: Span) -> T {
        T::default()
    }

    fn visit_identifier(&mut self, _: Handle, _: &'ast Identifier<'ast>, _: Span) -> T {
        T::default()
    }

    fn visit_binary_operation(&mut self, _: Handle, node: &'ast BinaryOperation<'ast>, _: Span) -> T {
        self.visit(&node.0);
        self.visit(&node.2);
        T::default()
    }

    fn visit_unary_operation(&mut self, _: Handle, node: &'ast UnaryOperation<'ast>, _: Span) -> T {
        self.visit(&node.1);
        T::default()
    }

    fn visit_variable_assignment(&mut self, _: Handle, node: &'ast VariableAssignment<'ast>, _: Span) -> T {
        self.visit(&node.1);
        T::default()
    }

    fn visit_complex_assignment(&mut self, _: Handle, node: &'ast ComplexAssignment<'ast>, _: Span) -> T {
        self.visit(&node.0);
        self.visit(&node.1);
        T::default()
    }

    fn visit_declaration(&mut self, _: Handle, node: &'ast Declaration<'ast>, _: Span) -> T {
        self.visit(&node.assignment.1);
        T::default()
    }

    fn visit_statement_list(&mut self, _: Handle, node: &'ast StatementList<'ast>, _: Span) -> T {
        for statement in &node.0 {
            self.visit(statement);
        }
        T::default()
    }

    fn visit_function_declaration(&mut self, _: Handle, node: &'ast FunctionDeclaration<'ast>, _: Span) -> T {
        self.visit(&node.statements);
        T::default()
    }

    fn visit_if_statement(&mut self, _: Handle, node: &'ast IfStatement<'ast>, _: Span) -> T {
        self.visit(&node.condition);
        self.visit(&node.then_statements);
        if let Some(else_statements) = node.else_statements.as_ref() {
            self.visit(else_statements);
        }
        T::default()
    }

    fn visit_while_statement(&mut self, _: Handle, node: &'ast WhileLoop<'ast>, _: Span) -> T {
        self.visit(&node.condition);
        self.visit(&node.body_statements);
        T::default()
    }

    fn visit_return_statement(&mut self, _: Handle, node: &'ast ReturnStatement<'ast>, _: Span) -> T {
        if let Some(value) = &node.value {
            self.visit(value);
        }
        T::default()
    }

    fn visit_function_call(&mut self, _: Handle, node: &'ast FunctionCall<'ast>, _: Span) -> T {
        self.visit(&node.callee);
        for arg in &node.args {
            self.visit(&arg.value);
        }
        T::default()
    }

    fn visit_member_access(&mut self, _: Handle, node: &'ast MemberAccess<'ast>, _: Span) -> T {
        self.visit(&node.lhs);
        T::default()
    }

    fn visit_table_constructor(&mut self, _: Handle, node: &'ast TableConstructor<'ast>, _: Span) -> T {
        for field in &node.fields {
            // TODO: once we support arbitrary names, handle that here too
            self.visit(&field.initializer);
        }
        T::default()
    }
}
