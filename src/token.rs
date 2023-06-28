use crate::span::Spanned;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum TokenTree<'src> {
    Tree(Vec<TokenTree<'src>>),
    Leaf(Spanned<Token<'src>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'src> {
    LiteralInt(i64),
    LiteralDouble(f64),
    LiteralBool(bool),
    LiteralString(&'src str),
    Identifier(&'src str),
    Operator(char),
    StatementEnd,
    LeftParen,
    RightParen,
    DoubleStar,
    DoubleEqual,
    NotEqual,
    DoubleSlash,
    PlusEqual,
    MinusEqual,
    StarEqual,
    DoubleStarEqual,
    SlashEqual,
    PercentEqual,
    DoubleSlashEqual,
    If,
    For,
    While,
    Do,
    Fn,
    Return,
    Pub,
    Prot,
    Priv,
    Class,
    Interface,
    Import,
    BlockStart,
    BlockEnd,
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LiteralInt(int) => write!(f, "{}", int),
            Token::LiteralString(string) => write!(f, "{}", string),
            Token::LiteralBool(true) => write!(f, "true"),
            Token::LiteralBool(false) => write!(f, "false"),
            Token::LiteralDouble(double) => write!(f, "{}", double),
            Token::Identifier(ident) => write!(f, "{}", ident),
            Token::Operator(c) => write!(f, "{}", c),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::DoubleStar => write!(f, "**"),
            Token::DoubleEqual => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::DoubleSlash => write!(f, "//"),
            Token::PlusEqual => write!(f, "+="),
            Token::MinusEqual => write!(f, "-="),
            Token::StarEqual => write!(f, "*="),
            Token::DoubleStarEqual => write!(f, "**="),
            Token::SlashEqual => write!(f, "/="),
            Token::PercentEqual => write!(f, "%="),
            Token::DoubleSlashEqual => write!(f, "//="),
            Token::If => write!(f, "if"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::Do => write!(f, "do"),
            Token::StatementEnd => write!(f, "end of statement"),
            Token::Fn => write!(f, "fn"),
            Token::Return => write!(f, "return"),
            Token::Pub => write!(f, "pub"),
            Token::Prot => write!(f, "prot"),
            Token::Priv => write!(f, "priv"),
            Token::Class => write!(f, "class"),
            Token::Interface => write!(f, "interface"),
            Token::Import => write!(f, "import"),
            Token::BlockStart => write!(f, "start of block"),
            Token::BlockEnd => write!(f, "end of block"),
        }
    }
}
