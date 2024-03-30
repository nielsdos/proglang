use crate::analysis::scope_resolution_pass::ScopeReferenceMap;
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::syntax::ast::{
    Assignment, BinaryOperation, BinaryOperationKind, BindingType, FunctionCall, FunctionDeclaration, Identifier, IfStatement, LiteralBool, LiteralFloat, LiteralInt, ReturnStatement, StatementList,
    UnaryOperation,
};
use crate::syntax::ast::{Ast, Declaration};
use crate::syntax::span::Span;
use crate::types::function_info::{FunctionInfo, VariableUpdateError};
use crate::types::type_system::{FunctionType, Type};
use crate::util::handle::Handle;
use std::collections::HashMap;
use std::rc::Rc;

pub(crate) struct TypeCheckerPass<'ast, 'f> {
    pub(crate) current_function: Option<Handle>,
    pub(crate) function_map: &'f mut HashMap<Handle, FunctionInfo<'ast>>,
    pub(crate) scope_reference_map: &'f ScopeReferenceMap,
    pub(crate) binding_types: HashMap<Handle, BindingType>,
    pub(crate) indirect_call_function_types: HashMap<Handle, Rc<FunctionType>>,
    pub(crate) semantic_error_list: &'f mut SemanticErrorList,
}

impl<'ast, 'f> TypeCheckerPass<'ast, 'f> {
    pub fn new(function_map: &'f mut HashMap<Handle, FunctionInfo<'ast>>, scope_reference_map: &'f ScopeReferenceMap, semantic_error_list: &'f mut SemanticErrorList) -> Self {
        Self {
            current_function: None,
            function_map,
            scope_reference_map,
            binding_types: Default::default(),
            indirect_call_function_types: Default::default(),
            semantic_error_list,
        }
    }

    fn current_function_scope(&self) -> Option<&FunctionInfo<'ast>> {
        self.current_function.as_ref().and_then(|name| self.function_map.get(name))
    }

    fn current_function_scope_mut(&mut self) -> Option<&mut FunctionInfo<'ast>> {
        self.current_function.as_ref().and_then(|name| self.function_map.get_mut(name))
    }

    fn enter_function_scope(&mut self, function: Handle) {
        assert!(self.current_function.is_none());
        self.current_function = Some(function);
    }

    fn leave_function_scope(&mut self) {
        assert!(self.current_function.is_some());
        self.current_function = None;
    }

    fn map_ast_handle_to_declarator(&self, handle: Handle) -> Handle {
        *self.scope_reference_map.references.get(&handle).expect("scope resolution ensures this mapping exists")
    }

    fn query_handle_type(&self, handle: Handle) -> Type {
        if let Some(ty) = self.current_function_scope().expect("just entered a function").query_variable_type(handle) {
            ty.clone()
        } else {
            // TODO: generalise
            match self.function_map.get(&handle).map(|info| info.function_type()) {
                Some(ty) => Type::Function(ty.clone()),
                None => Type::Error,
            }
        }
    }
}

impl<'ast, 'f> SemanticAnalysisPass<'ast, Type> for TypeCheckerPass<'ast, 'f> {
    fn visit_literal_int(&mut self, _: Handle, _: &'ast LiteralInt, _: Span) -> Type {
        Type::Int
    }

    fn visit_literal_double(&mut self, _: Handle, _: &'ast LiteralFloat, _: Span) -> Type {
        Type::Double
    }

    fn visit_literal_bool(&mut self, _: Handle, _: &'ast LiteralBool, _: Span) -> Type {
        Type::Bool
    }

    fn visit_identifier(&mut self, handle: Handle, _: &'ast Identifier<'ast>, _: Span) -> Type {
        let referenced_handle = self.map_ast_handle_to_declarator(handle);
        self.query_handle_type(referenced_handle).clone()
    }

    fn visit_binary_operation(&mut self, _: Handle, node: &'ast BinaryOperation<'ast>, span: Span) -> Type {
        let lhs_type = self.visit(&node.0);
        let rhs_type = self.visit(&node.2);

        if lhs_type.is_error() || rhs_type.is_error() {
            return Type::Error;
        }

        if node.1.is_comparison_op() {
            Type::Bool
        } else {
            if !lhs_type.is_numeric() || !rhs_type.is_numeric() {
                self.semantic_error_list.report_error(
                    span,
                    format!(
                        "expected both operands to be numeric, but the left-hand side has type '{}' and the right-hand side has type '{}'",
                        lhs_type, rhs_type
                    ),
                );
                return Type::Error;
            }

            if lhs_type != rhs_type {
                self.semantic_error_list.report_error(
                    span,
                    format!(
                        "expected both operands to have the same type, but the left-hand side has type '{}' and the right-hand side has type '{}'",
                        lhs_type, rhs_type
                    ),
                );
                return Type::Error;
            }

            if lhs_type == Type::Double || rhs_type == Type::Double || node.1 == BinaryOperationKind::DoubleDivision || node.1 == BinaryOperationKind::Power {
                Type::Double
            } else {
                Type::Int
            }
        }
    }

    fn visit_unary_operation(&mut self, _: Handle, node: &'ast UnaryOperation<'ast>, span: Span) -> Type {
        let rhs_type = self.visit(&node.1);
        if !rhs_type.is_numeric() {
            // Don't report propagated errors
            if !rhs_type.is_error() {
                self.semantic_error_list.report_error(
                    span,
                    format!(
                        "unary operator '{}' expects a numeric type, found a value of type '{}' instead",
                        node.0.to_human_readable_str(),
                        rhs_type
                    ),
                );
            }
            Type::Error
        } else {
            rhs_type
        }
    }

    fn visit_assignment(&mut self, handle: Handle, node: &'ast Assignment<'ast>, span: Span) -> Type {
        let handle = self.map_ast_handle_to_declarator(handle);
        let binding_type = self.binding_types[&handle];

        let rhs_type = self.visit(&node.1);

        match binding_type {
            BindingType::ImmutableVariable => {
                self.semantic_error_list.report_error(span, format!("the variable '{}' is immutable", node.0));
                return rhs_type;
            }
            BindingType::NonVariable => {
                self.semantic_error_list.report_error(span, format!("cannot assign to a non-variable '{}'", node.0));
                return rhs_type;
            }
            _ => {}
        };

        match self.current_function_scope_mut().expect("must be in function context").update_variable_type(handle, rhs_type.clone()) {
            Err(VariableUpdateError::TypeMismatch(old_type)) => {
                self.semantic_error_list.report_error(
                    span,
                    format!(
                        "the variable '{}' has mismatching types: previously had type '{}', but this assigns a value of type '{}'",
                        node.0, old_type, rhs_type
                    ),
                );
                Type::Error
            }
            Ok(()) => rhs_type,
        }
    }

    fn visit_declaration(&mut self, handle: Handle, node: &'ast Declaration<'ast>, _: Span) -> Type {
        let rhs_type = self.visit(&node.assignment.1);
        self.current_function_scope_mut()
            .expect("must be in function context")
            .update_variable_type(handle, rhs_type.clone())
            .expect("cannot fail because the variable did not exist yet");
        self.binding_types.insert(handle, node.binding);
        rhs_type
    }

    fn visit_statement_list(&mut self, _: Handle, node: &'ast StatementList<'ast>, _: Span) -> Type {
        let mut last_return_span: Option<Span> = None;
        let mut found_dead_code_after_return = false;
        for statement in &node.0 {
            let span = statement.1;
            self.visit(statement);
            if let Some(last_return_span) = &last_return_span {
                if !found_dead_code_after_return {
                    self.semantic_error_list.report_error_with_note(
                        span,
                        "therefore, this statement and any following statements in this block are unreachable".to_string(),
                        *last_return_span,
                        "this is the last non-dead statement in this block".to_string(),
                    );
                    found_dead_code_after_return = true;
                }
            } else if matches!(statement, (Ast::ReturnStatement(_), _)) {
                last_return_span = Some(span);
            }
        }
        Type::Void
    }

    fn visit_function_declaration(&mut self, handle: Handle, node: &'ast FunctionDeclaration<'ast>, _: Span) -> Type {
        self.enter_function_scope(handle);
        let scope = self.current_function_scope_mut().expect("just entered a function");
        for arg in &node.args {
            scope
                .update_variable_type(arg.0.as_handle(), arg.0.ty().clone())
                .expect("cannot fail because the variable did not exist yet");
        }
        for arg in &node.args {
            self.binding_types.insert(arg.0.as_handle(), arg.0.binding());
        }
        self.visit(&node.statements);
        self.leave_function_scope();
        Type::Void
    }

    fn visit_if_statement(&mut self, _: Handle, node: &'ast IfStatement<'ast>, _: Span) -> Type {
        let condition_type = self.visit(&node.condition);
        if condition_type.is_error() {
            return Type::Error;
        }
        if condition_type != Type::Bool {
            self.semantic_error_list
                .report_error(node.condition.1, format!("expected a condition of type 'bool', but this condition has type '{}'", condition_type));
        }
        self.visit(&node.then_statements);
        if let Some(else_statements) = &node.else_statements {
            self.visit(else_statements);
        }
        Type::Void
    }

    fn visit_return_statement(&mut self, _: Handle, node: &'ast ReturnStatement<'ast>, span: Span) -> Type {
        // TODO: implicit casts?
        if let Some(value) = &node.value {
            let returned_type = self.visit(value);
            if returned_type.is_error() {
                return Type::Error;
            }
            let current_function = self.current_function_scope().expect("must be in function context");
            let function_return_type = current_function.return_type();
            if returned_type != *function_return_type {
                let error = match function_return_type {
                    Type::Void => "function must not return a value because its return type is void".to_string(),
                    _ => format!("function must return a value of type '{}', but this returns a value of type '{}'", function_return_type, returned_type),
                };
                self.semantic_error_list.report_error(span, error);
            }
        } else {
            let current_function = self.current_function_scope().expect("must be in function context");
            let function_return_type = current_function.return_type();
            if *function_return_type != Type::Void {
                self.semantic_error_list
                    .report_error(span, format!("function must return a value of type '{}', but this returns nothing", function_return_type));
            }
        }
        Type::Void
    }

    fn visit_function_call(&mut self, _: Handle, node: &'ast FunctionCall<'ast>, span: Span) -> Type {
        let callee_type = match self.visit(&node.callee) {
            Type::Function(function) => function,
            ty => {
                self.semantic_error_list
                    .report_error(node.callee.1, format!("expected a function, found a value of type '{}' instead", ty));
                return Type::Error;
            }
        };

        self.indirect_call_function_types.insert(node.callee.0.as_handle(), callee_type.clone());

        if node.args.len() != callee_type.arg_types.len() {
            let plural_letter = |x: usize| if x == 1 { "" } else { "s" };
            self.semantic_error_list.report_error(
                span,
                format!(
                    "expected {} argument{}, but this function call has {} argument{}",
                    callee_type.arg_types.len(),
                    plural_letter(callee_type.arg_types.len()),
                    node.args.len(),
                    plural_letter(node.args.len())
                ),
            );
        }

        for (arg, expected_type) in node.args.iter().zip(callee_type.arg_types.iter()) {
            let arg_type = self.visit(arg);
            if arg_type != *expected_type {
                self.semantic_error_list
                    .report_error(arg.1, format!("expected an argument of type '{}', but this argument has type '{}'", expected_type, arg_type));
            }
        }

        callee_type.return_type.clone()
    }
}
