use crate::span::Spanned;

#[derive(Debug, Clone)]
pub enum BinaryOperationKind {
    Addition,
    Subtraction,
    Product,
    Division,
    Power,
}

#[derive(Debug, Clone)]
pub enum UnaryOperationKind {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum Ast<'src> {
    LiteralInt(i64),
    Identifier(&'src str),
    BinaryOperation(
        Box<Spanned<Ast<'src>>>,
        BinaryOperationKind,
        Box<Spanned<Ast<'src>>>,
    ),
    UnaryOperation(UnaryOperationKind, Box<Spanned<Ast<'src>>>),
    Assignment(&'src str, Box<Spanned<Ast<'src>>>),
    StatementList(Vec<Spanned<Ast<'src>>>),
    Todo,
}
