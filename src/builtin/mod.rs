use crate::syntax::ast::{Ast, BindingType, ReturnStatement};
use crate::syntax::span::Spanned;
use crate::types::function_info::ArgumentInfo;
use crate::types::type_system::Type;

fn span<T>(ast: T) -> Spanned<T> {
    (ast, (0..0).into())
}

pub struct Builtins<'b> {
    float: BuiltinRecord<'b>,
}

pub struct BuiltinRecord<'b> {
    name: &'b str,
    body: Spanned<Ast<'b>>,
    args: Vec<Spanned<ArgumentInfo<'b>>>,
    return_type: Type,
}

impl<'b> Default for Builtins<'b> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'b> Builtins<'b> {
    pub fn new() -> Self {
        Self { float: Self::create_builtin_float() }
    }

    fn create_builtin_float() -> BuiltinRecord<'b> {
        let args = vec![span(ArgumentInfo::new("value", Type::Int, BindingType::ImmutableVariable))];
        let body = span(Ast::ReturnStatement(ReturnStatement {
            value: Some(Box::new(span(Ast::BuiltinSiToFp(args[0].0.as_handle())))),
        }));
        BuiltinRecord {
            name: "float",
            body,
            args,
            return_type: Type::Double,
        }
    }

    pub fn get_builtin_float(&'b self) -> &'b BuiltinRecord<'b> {
        &self.float
    }
}

impl<'b> BuiltinRecord<'b> {
    pub fn name(&self) -> &'b str {
        self.name
    }

    pub fn body(&'b self) -> &'b Spanned<Ast<'b>> {
        &self.body
    }

    pub fn args(&'b self) -> &'b [Spanned<ArgumentInfo<'b>>] {
        &self.args
    }

    pub fn return_type(&self) -> &Type {
        &self.return_type
    }
}
