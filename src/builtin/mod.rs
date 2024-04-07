use crate::mid_ir::ir::{MidExpression, MidReturn, MidStatement, MidVariableReference};
use crate::syntax::ast::BindingType;
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
    body: MidStatement<'b>,
    args: Vec<Spanned<ArgumentInfo<'b>>>,
    return_type: Type<'b>,
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
        let args = vec![span(ArgumentInfo::new("value", Type::Int, BindingType::ImmutableVariable, None))];
        let body = MidStatement::Return(MidReturn {
            value: Some(MidExpression::BuiltinSiToFp(Box::new(MidExpression::VariableRead(MidVariableReference { variable_index: 0 })))),
        });
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

    pub fn body(&self) -> &MidStatement {
        &self.body
    }

    pub fn args(&'b self) -> &'b [Spanned<ArgumentInfo<'b>>] {
        &self.args
    }

    pub fn return_type(&self) -> &Type {
        &self.return_type
    }
}
