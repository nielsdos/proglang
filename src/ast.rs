use crate::span::Spanned;
use crate::type_system::Type;

#[derive(Debug, Clone, Eq, PartialEq)]
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperationKind {
    Plus,
    Minus,
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
}

impl UnaryOperationKind {
    pub fn to_human_readable_str(&self) -> &str {
        match self {
            UnaryOperationKind::Plus => "+",
            UnaryOperationKind::Minus => "-",
        }
    }
}

#[derive(Debug)]
pub struct LiteralInt(pub i64);
#[derive(Debug)]
pub struct LiteralDouble(pub f64);
#[derive(Debug)]
pub struct LiteralBool(pub bool);
#[derive(Debug)]
pub struct Identifier<'src>(pub &'src str);
#[derive(Debug)]
pub struct BinaryOperation<'src>(pub Box<Spanned<Ast<'src>>>, pub BinaryOperationKind, pub Box<Spanned<Ast<'src>>>);
#[derive(Debug)]
pub struct UnaryOperation<'src>(pub UnaryOperationKind, pub Box<Spanned<Ast<'src>>>);
#[derive(Debug)]
pub struct Assignment<'src>(pub &'src str, pub Box<Spanned<Ast<'src>>>);
#[derive(Debug)]
pub struct StatementList<'src>(pub Vec<Spanned<Ast<'src>>>);
#[derive(Debug)]
pub struct FunctionDeclaration<'src> {
    pub name: &'src str,
    pub statements: Box<Spanned<Ast<'src>>>,
    pub return_type: Type,
}
#[derive(Debug)]
pub struct IfStatement<'src> {
    pub condition: Box<Spanned<Ast<'src>>>,
    pub statements: Box<Spanned<Ast<'src>>>,
}
#[derive(Debug)]
pub struct ReturnStatement<'src> {
    pub value: Option<Box<Spanned<Ast<'src>>>>,
}

#[derive(Debug)]
pub enum Ast<'src> {
    LiteralInt(LiteralInt),
    LiteralDouble(LiteralDouble),
    LiteralBool(LiteralBool),
    Identifier(Identifier<'src>),
    BinaryOperation(BinaryOperation<'src>),
    UnaryOperation(UnaryOperation<'src>),
    Assignment(Assignment<'src>),
    StatementList(StatementList<'src>),
    FunctionDeclaration(FunctionDeclaration<'src>),
    IfStatement(IfStatement<'src>),
    ReturnStatement(ReturnStatement<'src>),
    /// Placeholder, useful for development purposes.
    Todo,
}

pub type AstHandle = *const ();

impl Ast<'_> {
    pub fn as_handle(&self) -> AstHandle {
        self as *const _ as _
    }
}
