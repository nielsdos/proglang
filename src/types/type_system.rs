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
    Void,
    Function(Rc<FunctionType<'a>>),
    UserType(&'a str),
    Reference(Rc<Type<'a>>),
    #[default]
    Error,
}

impl<'a> Display for Type<'a> {
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
            Type::UserType(name) => write!(f, "{}", name),
            Type::Reference(ty) => write!(f, "&{}", ty),
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

    pub fn dereference(&self) -> &Type<'a> {
        match self {
            Type::Reference(ty) => &*ty,
            ty => ty,
        }
    }
}
