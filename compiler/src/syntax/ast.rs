use crate::syntax::span::Spanned;
use crate::types::function_info::ArgumentInfo;
use crate::types::type_system::Type;
use crate::util::handle::Handle;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOperationKind {
    Addition,
    Subtraction,
    Product,
    DoubleDivision,
    WholeDivision,
    Power,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalRightShift,
    ArithmeticRightShift,
    LogicalLeftShift,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperationKind {
    Plus,
    Minus,
    BitwiseNot,
}

impl BinaryOperationKind {
    pub fn is_comparison_op(&self) -> bool {
        matches!(
            self,
            BinaryOperationKind::Equal
                | BinaryOperationKind::NotEqual
                | BinaryOperationKind::LessThan
                | BinaryOperationKind::GreaterThan
                | BinaryOperationKind::LessThanEqual
                | BinaryOperationKind::GreaterThanEqual
        )
    }

    pub fn is_logical_op(&self) -> bool {
        matches!(self, BinaryOperationKind::LogicalAnd | BinaryOperationKind::LogicalOr)
    }

    pub fn is_bitwise_op(&self) -> bool {
        matches!(
            self,
            BinaryOperationKind::BitwiseAnd
                | BinaryOperationKind::BitwiseOr
                | BinaryOperationKind::BitwiseXor
                | BinaryOperationKind::LogicalRightShift
                | BinaryOperationKind::ArithmeticRightShift
                | BinaryOperationKind::LogicalLeftShift
        )
    }
}

impl UnaryOperationKind {
    pub fn to_human_readable_str(&self) -> &str {
        match self {
            UnaryOperationKind::Plus => "+",
            UnaryOperationKind::Minus => "-",
            UnaryOperationKind::BitwiseNot => "~",
        }
    }
}

#[derive(Debug)]
pub struct LiteralInt(pub i64);
#[derive(Debug)]
pub struct LiteralFloat(pub f64);
#[derive(Debug)]
pub struct LiteralBool(pub bool);
#[derive(Debug)]
pub struct Identifier<'src>(pub &'src str);
#[derive(Debug)]
pub struct BinaryOperation<'src>(pub Box<Spanned<Ast<'src>>>, pub BinaryOperationKind, pub Box<Spanned<Ast<'src>>>);
#[derive(Debug)]
pub struct UnaryOperation<'src>(pub UnaryOperationKind, pub Box<Spanned<Ast<'src>>>);
#[derive(Debug)]
pub struct VariableAssignment<'src>(pub Spanned<&'src str>, pub Box<Spanned<Ast<'src>>>);
#[derive(Debug)]
pub struct ComplexAssignment<'src>(pub Box<Spanned<Ast<'src>>>, pub Box<Spanned<Ast<'src>>>);
#[derive(Debug, Copy, Clone)]
pub enum BindingType {
    MutableVariable,
    ImmutableVariable,
    NonVariable,
}
#[derive(Debug)]
pub struct Declaration<'src> {
    pub assignment: VariableAssignment<'src>,
    pub binding: BindingType,
}
#[derive(Debug)]
pub struct MemberStore<'src> {
    pub base: Box<Spanned<Ast<'src>>>,
    pub value: Box<Spanned<Ast<'src>>>,
}
#[derive(Debug)]
pub struct StatementList<'src>(pub Vec<Spanned<Ast<'src>>>);
#[derive(Debug)]
pub struct FunctionDeclaration<'src> {
    pub name: &'src str,
    pub statements: Box<Spanned<Ast<'src>>>,
    pub return_type: Spanned<Type<'src>>,
    pub args: Vec<Spanned<ArgumentInfo<'src>>>,
}
#[derive(Debug)]
pub struct IfStatement<'src> {
    pub condition: Box<Spanned<Ast<'src>>>,
    pub then_statements: Box<Spanned<Ast<'src>>>,
    pub else_statements: Option<Box<Spanned<Ast<'src>>>>,
}
#[derive(Debug)]
pub struct WhileLoop<'src> {
    pub condition: Box<Spanned<Ast<'src>>>,
    pub body_statements: Box<Spanned<Ast<'src>>>,
    pub check_condition_first: bool,
}
#[derive(Debug)]
pub struct ReturnStatement<'src> {
    pub value: Option<Box<Spanned<Ast<'src>>>>,
}
#[derive(Debug)]
pub struct FunctionCallArg<'ctx> {
    pub name: Option<Spanned<Identifier<'ctx>>>,
    pub value: Spanned<Ast<'ctx>>,
}
#[derive(Debug)]
pub struct FunctionCall<'ctx> {
    pub callee: Box<Spanned<Ast<'ctx>>>,
    pub args: Vec<FunctionCallArg<'ctx>>,
}
#[derive(Debug)]
pub struct MemberAccess<'src> {
    pub lhs: Box<Spanned<Ast<'src>>>,
    pub rhs: Spanned<Identifier<'src>>,
}
#[derive(Debug)]
pub struct TableField<'src> {
    pub name: Spanned<Identifier<'src>>,
    pub initializer: Box<Spanned<Ast<'src>>>,
}
#[derive(Debug)]
pub struct TableConstructor<'src> {
    pub fields: Vec<TableField<'src>>,
}

#[derive(Debug)]
pub enum Ast<'src> {
    LiteralInt(LiteralInt),
    LiteralFloat(LiteralFloat),
    LiteralBool(LiteralBool),
    Identifier(Identifier<'src>),
    BinaryOperation(BinaryOperation<'src>),
    UnaryOperation(UnaryOperation<'src>),
    VariableAssignment(VariableAssignment<'src>),
    ComplexAssignment(ComplexAssignment<'src>),
    MemberStore(MemberStore<'src>),
    Declaration(Declaration<'src>),
    StatementList(StatementList<'src>),
    FunctionDeclaration(FunctionDeclaration<'src>),
    IfStatement(IfStatement<'src>),
    WhileLoop(WhileLoop<'src>),
    ReturnStatement(ReturnStatement<'src>),
    FunctionCall(FunctionCall<'src>),
    MemberAccess(MemberAccess<'src>), // TODO: rename?
    TableConstructor(TableConstructor<'src>),
    /// Placeholder, useful for development purposes.
    Todo,
}

impl Ast<'_> {
    pub fn as_handle(&self) -> Handle {
        self as *const _ as _
    }
}
