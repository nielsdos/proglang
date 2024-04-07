use crate::analysis::semantic_analysis::SemanticAnalyser;
use crate::mid_ir::ir::{
    MidAssignment, MidBinaryOperation, MidDirectCall, MidExpression, MidFunction, MidIf, MidIndirectCall, MidMemberReference, MidPointerLoad, MidReturn, MidStatement, MidStatementList, MidTarget,
    MidVariableReference,
};
use crate::syntax::ast::{Assignment, Ast, Declaration, FunctionCall, LiteralBool, LiteralFloat, LiteralInt, StatementList, UnaryOperationKind};
use crate::syntax::span::Spanned;
use crate::types::function_info::FunctionInfo;
use crate::types::type_system::Type;
use crate::util::handle::Handle;
use std::collections::HashMap;

pub struct Construction<'ctx> {
    semantic_analyser: &'ctx SemanticAnalyser<'ctx>,
    variables: Vec<&'ctx Type<'ctx>>,
    handle_to_var_idx: HashMap<Handle, usize>,
    arg_idx_to_var_idx: Vec<usize>,
    function_info: &'ctx FunctionInfo<'ctx>,
}

impl<'ctx> Construction<'ctx> {
    pub fn new(semantic_analyser: &'ctx SemanticAnalyser<'ctx>, function_info: &'ctx FunctionInfo<'ctx>) -> Self {
        let mut variables = Vec::new();
        let mut handle_to_var_idx = HashMap::new();
        let mut arg_idx_to_var_idx = Vec::new();
        for (handle, ty) in function_info.variables() {
            handle_to_var_idx.insert(handle, variables.len());
            variables.push(ty);
        }
        for arg in function_info.args() {
            arg_idx_to_var_idx.push(handle_to_var_idx[&arg.0.as_handle()]);
        }
        Self {
            semantic_analyser,
            variables,
            handle_to_var_idx,
            arg_idx_to_var_idx,
            function_info,
        }
    }

    fn construct_args_from_function_call_node(&self, function_call: &'ctx FunctionCall<'ctx>) -> Vec<MidExpression<'ctx>> {
        function_call.args.iter().map(|expression| self.construct_from_expression(&expression.value)).collect::<Vec<_>>()
    }

    fn construct_from_expression(&self, expression: &'ctx Spanned<Ast>) -> MidExpression<'ctx> {
        match &expression.0 {
            Ast::LiteralBool(LiteralBool(value)) => MidExpression::LiteralBool(*value),
            Ast::LiteralInt(LiteralInt(value)) => MidExpression::LiteralInt(*value),
            Ast::LiteralFloat(LiteralFloat(value)) => MidExpression::LiteralFloat(*value),
            Ast::BinaryOperation(op) => MidExpression::BinaryOperation(MidBinaryOperation {
                lhs: Box::new(self.construct_from_expression(&op.0)),
                rhs: Box::new(self.construct_from_expression(&op.2)),
                op: op.1,
            }),
            Ast::UnaryOperation(op) => match op.0 {
                UnaryOperationKind::Plus => self.construct_from_expression(&op.1),
                UnaryOperationKind::Minus => MidExpression::UnaryNegateOperation(Box::new(self.construct_from_expression(&op.1))),
            },
            Ast::Identifier(_) => {
                let declaration_handle = self.semantic_analyser.identifier_to_declaration(expression.0.as_handle());
                if let Some(&variable_index) = self.handle_to_var_idx.get(&declaration_handle) {
                    MidExpression::VariableRead(MidVariableReference { variable_index })
                } else {
                    MidExpression::FunctionReference(declaration_handle)
                }
            }
            Ast::FunctionCall(function_call) => match self.construct_from_expression(&function_call.callee) {
                MidExpression::FunctionReference(handle) => {
                    let args = if let Some(order) = self.semantic_analyser.call_argument_order(handle) {
                        order.iter().map(|arg| self.construct_from_expression(arg.as_ref().unwrap())).collect::<Vec<_>>()
                    } else {
                        self.construct_args_from_function_call_node(function_call)
                    };

                    MidExpression::DirectCall(MidDirectCall {
                        declaration_handle_of_target: handle,
                        args,
                    })
                }
                other => {
                    let args = self.construct_args_from_function_call_node(function_call);
                    MidExpression::IndirectCall(MidIndirectCall {
                        expression: Box::new(other),
                        args,
                        ty: self
                            .semantic_analyser
                            .indirect_call_function_type(function_call.callee.0.as_handle())
                            .expect("verified in type checker")
                            .clone(),
                    })
                }
            },
            Ast::MemberAccess(member_access) => {
                let metadata = self.semantic_analyser.member_access_meta_data(expression.0.as_handle());
                let lhs = self.construct_from_expression(&member_access.lhs);

                let lhs = if !metadata.object_type.is_reference() {
                    match lhs {
                        MidExpression::VariableRead(variable_reference) => MidExpression::VariableReference(variable_reference),
                        MidExpression::PointerLoad(pointer_load) => *pointer_load.of,
                        _ => lhs,
                    }
                } else {
                    lhs
                };

                let member_reference = MidExpression::MemberReference(MidMemberReference {
                    of: Box::new(lhs),
                    index: metadata.index,
                    of_type: metadata.object_type.clone(),
                });

                MidExpression::PointerLoad(MidPointerLoad {
                    of: Box::new(member_reference),
                    of_type: metadata.member_type.clone(),
                })
            }
            _ => todo!(),
        }
    }

    fn construct_from_statement_list(&self, list: &'ctx StatementList) -> MidStatementList<'ctx> {
        MidStatementList {
            list: list.0.iter().map(|statement| self.construct_from_statement(statement)).collect::<Vec<_>>(),
        }
    }

    fn construct_from_ast_ensure_statement_list(&self, ast: &'ctx Spanned<Ast>) -> MidStatementList<'ctx> {
        match &ast.0 {
            Ast::StatementList(statement_list) => self.construct_from_statement_list(statement_list),
            _ => unreachable!(),
        }
    }

    fn construct_from_statement(&self, statement: &'ctx Spanned<Ast>) -> MidStatement<'ctx> {
        match &statement.0 {
            Ast::StatementList(list) => MidStatement::StatementList(self.construct_from_statement_list(list)),
            Ast::ReturnStatement(ret) => MidStatement::Return(MidReturn {
                value: ret.value.as_ref().map(|value| self.construct_from_expression(value)),
            }),
            Ast::Assignment(Assignment(_, expression))
            | Ast::Declaration(Declaration {
                assignment: Assignment(_, expression),
                binding: _,
            }) => {
                let declaration_handle = self.semantic_analyser.identifier_to_declaration(statement.0.as_handle());
                let variable_index = self.handle_to_var_idx[&declaration_handle];
                MidStatement::Assignment(MidAssignment {
                    target: MidTarget::Variable(MidVariableReference { variable_index }),
                    value: self.construct_from_expression(expression),
                })
            }
            Ast::IfStatement(if_statement) => MidStatement::If(MidIf {
                condition: self.construct_from_expression(&if_statement.condition),
                then_statements: self.construct_from_ast_ensure_statement_list(&if_statement.then_statements),
                else_statements: if_statement.else_statements.as_ref().map(|statements| self.construct_from_ast_ensure_statement_list(statements)),
            }),
            _ => unreachable!(),
        }
    }

    pub fn construct_from_function_declaration(self) -> MidFunction<'ctx> {
        MidFunction {
            statements: self.construct_from_statement(self.function_info.body()),
            variables: self.variables,
            arg_idx_to_var_idx: self.arg_idx_to_var_idx,
            name: self.function_info.name(),
            declaration_handle: self.function_info.declaration_handle(),
            function_type: self.function_info.function_type(),
            always_inline: self.function_info.is_always_inline(),
        }
    }
}
