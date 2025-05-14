use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType<'a> {
    pub return_type: Type<'a>,
    pub arg_types: Vec<Type<'a>>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type<'a> {
    Int,
    Double,
    Bool,
    Table,
    Void,
    Function(Rc<FunctionType<'a>>),
    UserType(&'a str),
    #[default]
    Error,
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Double => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Table => write!(f, "table"),
            Type::Void => write!(f, "void"),
            Type::Error => write!(f, "<error>"),
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
            Type::UserType(name) => write!(f, "{}", name),
        }
    }
}

impl<'a> Type<'a> {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Double)
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Type::Error)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub fn is_structure(&self) -> bool {
        matches!(self, Type::UserType(_))
    }
}
