use crate::span::Spanned;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BinaryOperationKind {
    Addition,
    Subtraction,
    Product,
    DoubleDivision,
    IntDivision,
    Power,
    Equality,
}

#[derive(Debug, Clone)]
pub enum UnaryOperationKind {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum Ast<'src> {
    LiteralInt(i64),
    LiteralDouble(f64),
    Identifier(&'src str),
    BinaryOperation(
        Box<Spanned<Ast<'src>>>,
        BinaryOperationKind,
        Box<Spanned<Ast<'src>>>,
    ),
    UnaryOperation(UnaryOperationKind, Box<Spanned<Ast<'src>>>),
    Assignment(&'src str, Box<Spanned<Ast<'src>>>),
    StatementList(Vec<Spanned<Ast<'src>>>),
    FunctionDeclaration(&'src str, Box<Spanned<Ast<'src>>>),
    IfStatement {
        condition: Box<Spanned<Ast<'src>>>,
        statements: Box<Spanned<Ast<'src>>>,
    },
    /// Placeholder, useful for development purposes.
    Todo,
}

pub type AstHandle = *const ();

impl Ast<'_> {
    pub fn as_handle(&self) -> AstHandle {
        self as *const _ as _
    }
}
