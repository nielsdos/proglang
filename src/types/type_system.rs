use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub return_type: Type,
    pub arg_types: Vec<Type>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Double,
    Bool,
    Void,
    Function(Rc<FunctionType>),
    #[default]
    Error,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Double => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Error => write!(f, "?"),
            Type::Function(func) => {
                write!(f, "fn(")?;
                for (i, arg) in func.arg_types.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") -> {}", func.return_type)
            }
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
