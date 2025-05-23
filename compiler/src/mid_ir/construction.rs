use crate::analysis::semantic_analysis::SemanticAnalyser;
use crate::mid_ir::ir::{
    MidAssignment, MidBinaryOperation, MidBlock, MidCallExternalByName, MidDirectCall, MidExpression, MidFunction, MidIf, MidIndirectCall, MidReturn, MidStatement, MidStatementList, MidTarget,
    MidVariableReference, MidWhile,
};
use crate::syntax::ast::{Ast, Declaration, FunctionCall, FunctionCallArg, LiteralBool, LiteralFloat, LiteralInt, StatementList, UnaryOperationKind, VariableAssignment};
use crate::syntax::span::Spanned;
use crate::types::function_info::FunctionInfo;
use crate::types::type_system::Type;
use crate::util::handle::Handle;
use rustc_hash::FxHashMap;

pub struct Construction<'ctx> {
    semantic_analyser: &'ctx SemanticAnalyser<'ctx>,
    variables: Vec<&'ctx Type<'ctx>>,
    handle_to_var_idx: FxHashMap<Handle, usize>,
    arg_idx_to_var_idx: Vec<usize>,
    function_info: &'ctx FunctionInfo<'ctx>,
}

impl<'ctx> Construction<'ctx> {
    pub fn new(semantic_analyser: &'ctx SemanticAnalyser<'ctx>, function_info: &'ctx FunctionInfo<'ctx>) -> Self {
        let mut variables = Vec::new();
        let mut handle_to_var_idx = FxHashMap::default();
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

    fn construct_args_from_slice(&mut self, function_call_args: &'ctx [FunctionCallArg<'ctx>]) -> Vec<MidExpression<'ctx>> {
        [MidExpression::HiddenBasePtr]
            .into_iter()
            .chain(function_call_args.iter().map(|expression| self.construct_from_expression(&expression.value)))
            .collect::<Vec<_>>()
    }

    fn construct_args_from_function_call_node(&mut self, function_call: &'ctx FunctionCall<'ctx>) -> Vec<MidExpression<'ctx>> {
        self.construct_args_from_slice(function_call.args.as_slice())
    }

    /// Introduce a new temporary to pass around
    fn new_temp(&mut self, ty: &'ctx Type) -> MidVariableReference {
        let temp_idx = self.variables.len();
        self.variables.push(ty);
        MidVariableReference { variable_index: temp_idx }
    }

    fn construct_from_expression(&mut self, expression: &'ctx Spanned<Ast>) -> MidExpression<'ctx> {
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
                UnaryOperationKind::BitwiseNot => MidExpression::UnaryBitwiseNotOperation(Box::new(self.construct_from_expression(&op.1))),
            },
            Ast::Identifier(_) => {
                let declaration_handle = self.semantic_analyser.identifier_to_declaration(expression.0.as_handle());
                if let Some(&variable_index) = self.handle_to_var_idx.get(&declaration_handle) {
                    MidExpression::VariableRead(MidVariableReference { variable_index })
                } else {
                    MidExpression::FunctionReference(declaration_handle)
                }
            }
            Ast::TableConstructor(table_constructor) => {
                let constructor_call = MidExpression::CallExternalByName(MidCallExternalByName {
                    name: "rt_create_table",
                    args: vec![MidExpression::HiddenBasePtr, MidExpression::LiteralInt(table_constructor.fields.len() as i64)],
                });
                if table_constructor.fields.is_empty() {
                    constructor_call
                } else {
                    let temp_ref = self.new_temp(&Type::Table);

                    let mut statements = vec![MidStatement::Expression(MidExpression::Assignment(MidAssignment {
                        target: MidTarget::Variable(temp_ref),
                        value: Box::new(constructor_call),
                    }))];
                    for field in &table_constructor.fields {
                        statements.push(MidStatement::Expression(MidExpression::CallExternalByName(MidCallExternalByName {
                            name: "rt_set_in_table",
                            // TODO: type fun
                            args: vec![
                                MidExpression::HiddenBasePtr,
                                MidExpression::VariableRead(temp_ref),
                                MidExpression::LiteralString(field.name.0 .0),
                                MidExpression::LiteralInt(field.name.0 .0.len() as i64),
                                self.construct_from_expression(&field.initializer),
                            ],
                        })));
                    }
                    let statements = MidStatementList { list: statements };
                    MidExpression::Block(MidBlock {
                        statements,
                        yield_expr: Box::new(MidExpression::VariableRead(temp_ref)),
                    })
                }
            }
            Ast::FunctionCall(function_call) => match self.construct_from_expression(&function_call.callee) {
                MidExpression::FunctionReference(handle) => {
                    let args = if let Some(order) = self.semantic_analyser.call_argument_order(expression.0.as_handle()) {
                        [MidExpression::HiddenBasePtr]
                            .into_iter()
                            .chain(order.iter().map(|arg| self.construct_from_expression(arg.as_ref().unwrap())))
                            .collect::<Vec<_>>()
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
                let metadata = self.semantic_analyser.member_access_meta_data(expression.0.as_handle()); // TODO ???
                let lhs = self.construct_from_expression(&member_access.lhs);

                MidExpression::CallExternalByName(MidCallExternalByName {
                    name: "rt_get_from_table",
                    args: vec![
                        MidExpression::HiddenBasePtr,
                        lhs,
                        MidExpression::LiteralString(member_access.rhs.0 .0),
                        MidExpression::LiteralInt(member_access.rhs.0 .0.len() as i64),
                    ],
                })
            }
            Ast::ComplexAssignment(assignment) => match &assignment.0 .0 {
                Ast::MemberAccess(member_access) => {
                    let temp_ref = self.new_temp(&Type::Int); // TODO

                    let lhs = self.construct_from_expression(&member_access.lhs);
                    let rhs = self.construct_from_expression(&assignment.1);

                    let call = MidExpression::CallExternalByName(MidCallExternalByName {
                        name: "rt_set_in_table",
                        args: vec![
                            MidExpression::HiddenBasePtr,
                            lhs,
                            MidExpression::LiteralString(member_access.rhs.0 .0),
                            MidExpression::LiteralInt(member_access.rhs.0 .0.len() as i64),
                            MidExpression::VariableRead(temp_ref),
                        ],
                    });

                    MidExpression::Block(MidBlock {
                        statements: MidStatementList {
                            list: vec![
                                MidStatement::Expression(MidExpression::Assignment(MidAssignment {
                                    target: MidTarget::Variable(temp_ref),
                                    value: Box::new(rhs),
                                })),
                                MidStatement::Expression(call),
                            ],
                        },
                        yield_expr: Box::new(MidExpression::VariableRead(temp_ref)),
                    })
                }
                _ => unreachable!(),
            },
            Ast::VariableAssignment(VariableAssignment(_, rhs))
            | Ast::Declaration(Declaration {
                assignment: VariableAssignment(_, rhs),
                binding: _,
            }) => {
                let declaration_handle = self.semantic_analyser.identifier_to_declaration(expression.0.as_handle());
                let variable_index = self.handle_to_var_idx[&declaration_handle];
                MidExpression::Assignment(MidAssignment {
                    target: MidTarget::Variable(MidVariableReference { variable_index }),
                    value: Box::new(self.construct_from_expression(rhs)),
                })
            }
            _ => todo!(),
        }
    }

    fn construct_from_statement_list(&mut self, list: &'ctx StatementList) -> MidStatementList<'ctx> {
        MidStatementList {
            list: list.0.iter().map(|statement| self.construct_from_statement(statement)).collect::<Vec<_>>(),
        }
    }

    fn construct_from_ast_ensure_statement_list(&mut self, ast: &'ctx Spanned<Ast>) -> MidStatementList<'ctx> {
        match &ast.0 {
            Ast::StatementList(statement_list) => self.construct_from_statement_list(statement_list),
            _ => panic!("Not a statement list: {:?}", ast),
        }
    }

    fn construct_from_statement(&mut self, statement: &'ctx Spanned<Ast>) -> MidStatement<'ctx> {
        match &statement.0 {
            Ast::StatementList(list) => MidStatement::StatementList(self.construct_from_statement_list(list)),
            Ast::ReturnStatement(ret) => MidStatement::Return(MidReturn {
                value: ret.value.as_ref().map(|value| self.construct_from_expression(value)),
            }),
            Ast::IfStatement(if_statement) => MidStatement::If(MidIf {
                condition: self.construct_from_expression(&if_statement.condition),
                then_statements: self.construct_from_ast_ensure_statement_list(&if_statement.then_statements),
                else_statements: if_statement.else_statements.as_ref().map(|statements| self.construct_from_ast_ensure_statement_list(statements)),
            }),
            Ast::WhileLoop(while_loop) => MidStatement::While(MidWhile {
                condition: self.construct_from_expression(&while_loop.condition),
                body_statements: self.construct_from_ast_ensure_statement_list(&while_loop.body_statements),
                check_condition_first: while_loop.check_condition_first,
            }),
            _ => MidStatement::Expression(self.construct_from_expression(statement)),
        }
    }

    pub fn construct_from_function_declaration(mut self) -> MidFunction<'ctx> {
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
