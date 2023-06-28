use std::fmt::{Display, Formatter};

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Double,
    Bool,
    Void,
    #[default]
    Error,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Double => write!(f, "double"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Error => write!(f, "?"),
        }
    }
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Double)
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Type::Error)
    }
}
