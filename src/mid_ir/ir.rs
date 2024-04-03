use crate::syntax::ast::BinaryOperationKind;
use crate::types::type_system::{FunctionType, Type};
use crate::util::handle::Handle;
use std::rc::Rc;

/// Access to a local variable
#[derive(Debug)]
pub struct MidVariableReference {
    pub variable_index: usize,
}

/// Access to a member of a structure
#[derive(Debug)]
pub struct MidMemberReference<'t> {
    pub of: Box<MidExpression<'t>>,
    pub index: u32,
    pub of_type: Type<'t>,
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
pub enum MidExpression<'t> {
    VariableRead(MidVariableReference),
    VariableReference(MidVariableReference),
    MemberReference(MidMemberReference<'t>),
    PointerLoad(MidPointerLoad<'t>),
    FunctionReference(Handle),
    BinaryOperation(MidBinaryOperation<'t>),
    UnaryNegateOperation(Box<MidExpression<'t>>),
    DirectCall(MidDirectCall<'t>),
    IndirectCall(MidIndirectCall<'t>),
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralBool(bool),
    /// These are internal expression types not exposed to the language
    BuiltinSiToFp(Box<MidExpression<'t>>),
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
    pub value: MidExpression<'t>,
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
pub enum MidStatement<'t> {
    StatementList(MidStatementList<'t>),
    Assignment(MidAssignment<'t>),
    Return(MidReturn<'t>),
    If(MidIf<'t>),
}

#[derive(Debug)]
pub struct MidFunction<'ctx> {
    pub statements: MidStatement<'ctx>,
    pub variables: Vec<&'ctx Type<'ctx>>,
    pub arg_idx_to_var_idx: Vec<usize>,
    pub name: &'ctx str,
    pub declaration_handle: Handle,
    pub function_type: Rc<FunctionType<'ctx>>,
    // TODO: flags field instead?
    pub always_inline: bool,
}

impl<'ctx> MidFunction<'ctx> {
    pub fn return_type(&self) -> &Type<'ctx> {
        &self.function_type.return_type
    }
}
