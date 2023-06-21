use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'src> {
    LiteralInt(i64),
    LiteralString(&'src str),
    Identifier(&'src str),
    Operator(char),
    Newline,
    LeftParen,
    RightParen,
    DoubleStar,
    DoubleEqual,
    For,
    While,
    Do,
    True,
    False,
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LiteralInt(int) => write!(f, "{}", int),
            Token::LiteralString(string) => write!(f, "{}", string),
            Token::Identifier(ident) => write!(f, "{}", ident),
            Token::Operator(c) => write!(f, "{}", c),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::DoubleStar => write!(f, "**"),
            Token::DoubleEqual => write!(f, "=="),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::Do => write!(f, "do"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Newline => write!(f, "newline"),
        }
    }
}
