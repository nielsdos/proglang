use crate::syntax::ast::BinaryOperationKind;
use crate::types::type_system::{FunctionType, Type};
use crate::util::handle::Handle;
use std::rc::Rc;

/// Access to a local variable
#[derive(Debug, Clone, Copy)]
pub struct MidVariableReference {
    pub variable_index: usize,
}

#[derive(Debug)]
pub struct MidPointerLoad<'t> {
    pub of: Box<MidExpression<'t>>,
    pub of_type: Type<'t>,
}

#[derive(Debug)]
pub struct MidBinaryOperation<'t> {
    pub lhs: Box<MidExpression<'t>>,
    pub op: BinaryOperationKind,
    pub rhs: Box<MidExpression<'t>>,
}

#[derive(Debug)]
pub struct MidDirectCall<'t> {
    pub declaration_handle_of_target: Handle,
    pub args: Vec<MidExpression<'t>>,
}

#[derive(Debug)]
pub struct MidIndirectCall<'t> {
    pub expression: Box<MidExpression<'t>>,
    pub args: Vec<MidExpression<'t>>,
    pub ty: Rc<FunctionType<'t>>,
}

#[derive(Debug)]
pub struct MidCallExternalByName<'t> {
    pub name: &'static str,
    pub args: Vec<MidExpression<'t>>,
}

#[derive(Debug)]
pub struct MidBlock<'t> {
    pub statements: MidStatementList<'t>,
    pub yield_expr: Box<MidExpression<'t>>,
}

#[derive(Debug)]
pub enum MidExpression<'t> {
    Assignment(MidAssignment<'t>),
    VariableRead(MidVariableReference),
    VariableReference(MidVariableReference),
    FunctionReference(Handle),
    BinaryOperation(MidBinaryOperation<'t>),
    UnaryNegateOperation(Box<MidExpression<'t>>),
    UnaryBitwiseNotOperation(Box<MidExpression<'t>>),
    CallExternalByName(MidCallExternalByName<'t>),
    DirectCall(MidDirectCall<'t>),
    IndirectCall(MidIndirectCall<'t>),
    Block(MidBlock<'t>),
    HiddenBasePtr,
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralBool(bool),
    LiteralString(&'t str),
}

/// Target of a memory access, could be a variable, array, ...
#[derive(Debug)]
pub enum MidTarget {
    Variable(MidVariableReference),
}

/// A statement list, i.e. often called a block
#[derive(Debug)]
pub struct MidStatementList<'t> {
    pub list: Vec<MidStatement<'t>>,
}

#[derive(Debug)]
pub struct MidAssignment<'t> {
    pub target: MidTarget,
    pub value: Box<MidExpression<'t>>,
}

#[derive(Debug)]
pub struct MidReturn<'t> {
    pub value: Option<MidExpression<'t>>,
}

#[derive(Debug)]
pub struct MidIf<'t> {
    pub condition: MidExpression<'t>,
    pub then_statements: MidStatementList<'t>,
    pub else_statements: Option<MidStatementList<'t>>,
}

#[derive(Debug)]
pub struct MidWhile<'t> {
    pub condition: MidExpression<'t>,
    pub body_statements: MidStatementList<'t>,
    pub check_condition_first: bool,
}

#[derive(Debug)]
pub enum MidStatement<'t> {
    StatementList(MidStatementList<'t>),
    Return(MidReturn<'t>),
    If(MidIf<'t>),
    While(MidWhile<'t>),
    Expression(MidExpression<'t>),
}

#[derive(Debug)]
pub struct MidFunction<'ctx> {
    pub statements: MidStatement<'ctx>,
    pub variables: Vec<&'ctx Type<'ctx>>,
    pub arg_idx_to_var_idx: Vec<usize>,
    pub name: &'ctx str,
    pub declaration_handle: Handle,
    pub function_type: Rc<FunctionType<'ctx>>,
    pub always_inline: bool,
}

impl<'ctx> MidFunction<'ctx> {
    pub fn return_type(&self) -> &Type<'ctx> {
        &self.function_type.return_type
    }
}
