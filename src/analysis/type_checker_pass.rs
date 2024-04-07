use crate::analysis::scope_resolution_pass::ScopeReferenceMap;
use crate::analysis::semantic_analysis::{ClassMap, MemberAccessMetadata};
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::SemanticErrorList;
use crate::syntax::ast::{
    Assignment, BinaryOperation, BinaryOperationKind, BindingType, FunctionCall, FunctionCallArg, FunctionDeclaration, Identifier, IfStatement, LiteralBool, LiteralFloat, LiteralInt, MemberAccess,
    ReturnStatement, StatementList, UnaryOperation,
};
use crate::syntax::ast::{Ast, Declaration};
use crate::syntax::span::{combine_span, Span, Spanned};
use crate::types::function_info::{FunctionInfo, VariableUpdateError};
use crate::types::type_system::{FunctionType, Type};
use crate::util::handle::Handle;
use std::collections::HashMap;
use std::rc::Rc;

pub(crate) struct TypeCheckerPass<'ast, 'f> {
    pub(crate) current_function: Option<Handle>,
    pub(crate) function_map: &'f mut HashMap<Handle, FunctionInfo<'ast>>,
    pub(crate) scope_reference_map: &'f ScopeReferenceMap,
    class_map: &'f ClassMap<'ast>,
    pub(crate) member_access_meta_data: HashMap<Handle, MemberAccessMetadata<'ast>>,
    pub(crate) binding_types: HashMap<Handle, BindingType>,
    pub(crate) indirect_call_function_types: HashMap<Handle, Rc<FunctionType<'ast>>>,
    pub(crate) call_argument_order: HashMap<Handle, Vec<Option<&'ast Spanned<Ast<'ast>>>>>,
    pub(crate) semantic_error_list: &'f mut SemanticErrorList,
}

impl<'ast, 'f> TypeCheckerPass<'ast, 'f> {
    pub fn new(
        function_map: &'f mut HashMap<Handle, FunctionInfo<'ast>>,
        scope_reference_map: &'f ScopeReferenceMap,
        class_map: &'f ClassMap<'ast>,
        semantic_error_list: &'f mut SemanticErrorList,
    ) -> Self {
        Self {
            current_function: None,
            function_map,
            scope_reference_map,
            class_map,
            member_access_meta_data: Default::default(),
            binding_types: Default::default(),
            indirect_call_function_types: Default::default(),
            call_argument_order: Default::default(),
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

    fn query_handle_type(&self, handle: Handle) -> Type<'ast> {
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

    fn report_type_existence(&mut self, ty: &'ast Type<'ast>, span: Span) {
        if let Type::UserType(name) = ty.dereference() {
            if !self.class_map.contains_key(name) {
                self.semantic_error_list.report_error(span, format!("type '{}' was not found", name));
            }
        }
    }

    fn check_argument_type(&mut self, arg: &'ast Spanned<Ast<'ast>>, expected_type: &Type<'ast>) {
        let arg_type = self.visit(arg);
        if arg_type != *expected_type {
            self.semantic_error_list
                .report_error(arg.1, format!("expected an argument of type '{}', but this argument has type '{}'", expected_type, arg_type));
        }
    }

    /// Called in case it will be impossible to resolve the order, but we can still type check the argument expressions
    fn visit_args_do_no_further_checks(&mut self, args: &'ast [FunctionCallArg<'ast>]) {
        for arg in args.iter() {
            self.visit(&arg.value);
        }
    }
}

impl<'ast, 'f> SemanticAnalysisPass<'ast, Type<'ast>> for TypeCheckerPass<'ast, 'f> {
    fn visit_literal_int(&mut self, _: Handle, _: &'ast LiteralInt, _: Span) -> Type<'ast> {
        Type::Int
    }

    fn visit_literal_double(&mut self, _: Handle, _: &'ast LiteralFloat, _: Span) -> Type<'ast> {
        Type::Double
    }

    fn visit_literal_bool(&mut self, _: Handle, _: &'ast LiteralBool, _: Span) -> Type<'ast> {
        Type::Bool
    }

    fn visit_identifier(&mut self, handle: Handle, _: &'ast Identifier<'ast>, _: Span) -> Type<'ast> {
        let referenced_handle = self.map_ast_handle_to_declarator(handle);
        self.query_handle_type(referenced_handle).clone()
    }

    fn visit_binary_operation(&mut self, _: Handle, node: &'ast BinaryOperation<'ast>, span: Span) -> Type<'ast> {
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

    fn visit_unary_operation(&mut self, _: Handle, node: &'ast UnaryOperation<'ast>, span: Span) -> Type<'ast> {
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

    fn visit_assignment(&mut self, handle: Handle, node: &'ast Assignment<'ast>, span: Span) -> Type<'ast> {
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

    fn visit_declaration(&mut self, handle: Handle, node: &'ast Declaration<'ast>, _: Span) -> Type<'ast> {
        let rhs_type = self.visit(&node.assignment.1);
        self.current_function_scope_mut()
            .expect("must be in function context")
            .update_variable_type(handle, rhs_type.clone())
            .expect("cannot fail because the variable did not exist yet");
        self.binding_types.insert(handle, node.binding);
        rhs_type
    }

    fn visit_statement_list(&mut self, _: Handle, node: &'ast StatementList<'ast>, _: Span) -> Type<'ast> {
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

    fn visit_function_declaration(&mut self, handle: Handle, node: &'ast FunctionDeclaration<'ast>, _: Span) -> Type<'ast> {
        self.enter_function_scope(handle);
        for arg in &node.args {
            self.report_type_existence(arg.0.ty(), arg.1);
        }
        let scope = self.current_function_scope_mut().expect("just entered a function");
        for arg in &node.args {
            scope
                .update_variable_type(arg.0.as_handle(), arg.0.ty().clone())
                .expect("cannot fail because the variable did not exist yet");
        }
        self.report_type_existence(&node.return_type.0, node.return_type.1);
        for arg in &node.args {
            self.binding_types.insert(arg.0.as_handle(), arg.0.binding());
        }
        self.visit(&node.statements);
        self.leave_function_scope();
        Type::Void
    }

    fn visit_if_statement(&mut self, _: Handle, node: &'ast IfStatement<'ast>, _: Span) -> Type<'ast> {
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

    fn visit_return_statement(&mut self, _: Handle, node: &'ast ReturnStatement<'ast>, span: Span) -> Type<'ast> {
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

    fn visit_function_call(&mut self, _: Handle, node: &'ast FunctionCall<'ast>, span: Span) -> Type<'ast> {
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

        let no_named_args = node.args.iter().all(|arg| arg.name.is_none());
        if !no_named_args {
            let handle = self.map_ast_handle_to_declarator(node.callee.0.as_handle());
            if let Some(function_info) = self.function_map.get(&handle) {
                // Check that all positional arguments come before named arguments
                let mut first_named_arg_span = None;
                let mut error = false;
                for arg in node.args.iter() {
                    if arg.name.is_none() {
                        if let Some(first_named_arg_span) = first_named_arg_span {
                            self.semantic_error_list.report_error_with_note(
                                arg.value.1,
                                "positional arguments must not come after named arguments".to_string(),
                                first_named_arg_span,
                                "first named argument passed here".to_string(),
                            );
                            error = true;
                        }
                    } else if first_named_arg_span.is_none() {
                        first_named_arg_span = Some(combine_span(arg.name.as_ref().unwrap().1, arg.value.1));
                    }
                }

                // Put the arguments in the right order
                if !error {
                    let mut ordered_args = Vec::with_capacity(node.args.len());
                    for _ in 0..node.args.len() {
                        ordered_args.push(None);
                    }
                    for (index, arg) in node.args.iter().enumerate() {
                        if let Some(name) = &arg.name {
                            if let Some(matched_arg) = function_info.args().iter().position(|arg| arg.0.name() == name.0 .0) {
                                if ordered_args[matched_arg].is_none() {
                                    ordered_args[matched_arg] = Some(&arg.value);
                                } else {
                                    self.semantic_error_list.report_error(name.1, format!("argument '{}' already passed", name.0 .0));
                                }
                            } else {
                                self.semantic_error_list
                                    .report_error(name.1, format!("argument '{}' not found in function '{}'", name.0 .0, function_info.name()));
                            }
                        } else {
                            ordered_args[index] = Some(&arg.value);
                        }
                    }

                    for (arg, expected_type) in ordered_args.iter().zip(callee_type.arg_types.iter()) {
                        if let Some(arg) = arg {
                            self.check_argument_type(arg, expected_type);
                        }
                    }

                    self.call_argument_order.insert(handle, ordered_args);
                } else {
                    self.visit_args_do_no_further_checks(&node.args);
                }
            } else {
                self.semantic_error_list.report_error(span, "named arguments are not supported in indirect calls".to_string());
                self.visit_args_do_no_further_checks(&node.args);
            }
        } else {
            for (arg, expected_type) in node.args.iter().zip(callee_type.arg_types.iter()) {
                self.check_argument_type(&arg.value, expected_type);
            }
        }

        callee_type.return_type.clone()
    }

    fn visit_member_access(&mut self, handle: Handle, node: &'ast MemberAccess<'ast>, _: Span) -> Type<'ast> {
        let object_type = self.visit(&node.lhs);
        if object_type.is_error() {
            return Type::Error;
        }

        let rhs_identifier = &node.rhs.0;

        // Automatic dereferencing on member access
        let member_info = match object_type.dereference() {
            Type::UserType(name) => self.class_map.get(name).expect("must exist because of type graph pass").field(rhs_identifier.0),
            ty => {
                self.semantic_error_list.report_error(node.rhs.1, format!("type '{}' does not support member access", ty));
                return Type::Error;
            }
        };

        match member_info {
            Some(member_info) => {
                self.member_access_meta_data.insert(
                    handle,
                    MemberAccessMetadata {
                        object_type: object_type.clone(),
                        member_type: member_info.ty().clone(),
                        index: member_info.index(),
                    },
                );
                member_info.ty().clone()
            }
            None => {
                self.semantic_error_list
                    .report_error(node.rhs.1, format!("field '{}' does not exist in type '{}'", rhs_identifier.0, object_type));
                Type::Error
            }
        }
    }
}
