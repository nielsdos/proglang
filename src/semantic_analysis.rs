use crate::ast::{Ast, AstHandle, BinaryOperationKind};
use crate::function_info::FunctionInfo;
use crate::span::Spanned;
use crate::type_system::Type;
use std::collections::HashMap;

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct UniqueFunctionIdentifier<'ast>(&'ast str);

pub struct SemanticAnalyser<'ast> {
    ast: &'ast Spanned<Ast<'ast>>,
    type_table: HashMap<AstHandle, Type>,
    function_map: HashMap<UniqueFunctionIdentifier<'ast>, FunctionInfo<'ast>>,
    current_function: Option<UniqueFunctionIdentifier<'ast>>,
}

// TODO: error handling!
impl<'ast> SemanticAnalyser<'ast> {
    pub fn new(ast: &'ast Spanned<Ast<'ast>>) -> Self {
        Self {
            ast,
            type_table: HashMap::new(),
            function_map: Default::default(),
            current_function: None,
        }
    }

    pub fn analyse(&mut self) {
        self.visit(self.ast)
    }

    fn store_ast_type(&mut self, node: &'ast Ast<'ast>, ty: Type) {
        let old_value = self.type_table.insert(node.as_handle(), ty);
        assert!(old_value.is_none());
    }

    pub fn query_ast_type_spanned(&self, node: &'ast Spanned<Ast<'ast>>) -> Option<&Type> {
        self.query_ast_type(&node.0)
    }

    pub fn query_ast_type(&self, node: &'ast Ast<'ast>) -> Option<&Type> {
        self.type_table.get(&node.as_handle())
    }

    fn current_function_scope(&self) -> Option<&FunctionInfo<'ast>> {
        self.current_function
            .as_ref()
            .and_then(|name| self.function_map.get(name))
    }

    fn current_function_scope_mut(&mut self) -> Option<&mut FunctionInfo<'ast>> {
        self.current_function
            .as_ref()
            .and_then(|name| self.function_map.get_mut(name))
    }

    fn enter_function_scope(&mut self, function: UniqueFunctionIdentifier<'ast>) {
        assert!(self.current_function.is_none());
        self.current_function = Some(function);
    }

    fn leave_function_scope(&mut self) {
        assert!(self.current_function.is_some());
        self.current_function = None;
    }

    fn visit(&mut self, node: &'ast Spanned<Ast<'ast>>) {
        let node = &node.0;
        match &node {
            Ast::LiteralInt(_) => {
                self.store_ast_type(node, Type::Int);
            }
            Ast::LiteralDouble(_) => {
                self.store_ast_type(node, Type::Double);
            }
            Ast::Identifier(name) => {
                if let Some(ty) = self
                    .current_function_scope()
                    .expect("must be in function context")
                    .query_variable_type(name)
                {
                    self.store_ast_type(node, *ty);
                }
                // TODO: error case??
            }
            Ast::BinaryOperation(lhs, operation, rhs) => {
                self.visit(lhs);
                self.visit(rhs);

                println!("{:?}", node);

                let lhs_type = self.query_ast_type_spanned(lhs).expect("just visited lhs");
                let rhs_type = self.query_ast_type_spanned(rhs).expect("just visited rhs");

                println!("{:?} {:?}", lhs_type, rhs_type);

                let result_type = match operation {
                    BinaryOperationKind::Equality => Type::Bool,
                    _ => Type::Void, // TODO
                };
                self.store_ast_type(node, result_type);
            }
            Ast::UnaryOperation(_, rhs) => {
                self.visit(rhs);
                // TODO: typecheck
            }
            Ast::Assignment(identifier, rhs) => {
                self.visit(rhs);
                let rhs_type = *self.query_ast_type_spanned(rhs).expect("just visited rhs");
                self.current_function_scope_mut()
                    .expect("must be in function context")
                    .update_variable_type(identifier, rhs_type);
                // TODO: error case??
            }
            Ast::StatementList(statements) => {
                for statement in statements {
                    self.visit(statement);
                }
            }
            Ast::FunctionDeclaration(name, body) => {
                // TODO: name conflict check
                self.function_map
                    .insert(UniqueFunctionIdentifier(name), FunctionInfo::new(body));

                self.enter_function_scope(UniqueFunctionIdentifier(name));
                self.visit(body);
                self.leave_function_scope();
            }
            Ast::IfStatement {
                condition,
                statements,
            } => {
                self.visit(condition);
                let condition_type = self
                    .query_ast_type_spanned(condition)
                    .expect("just visited condition");
                println!("condition type: {:?}", condition_type);
                // TODO: check condition type
                self.visit(statements);
            }
            Ast::Todo => todo!(),
        }
    }

    pub fn function_list_iter(
        &self,
    ) -> impl Iterator<Item = (&'_ UniqueFunctionIdentifier<'_>, &'_ FunctionInfo<'_>)> {
        self.function_map.iter().map(|(k, v)| (k, v))
    }
}

impl<'f> UniqueFunctionIdentifier<'f> {
    pub fn as_str(&self) -> &'f str {
        self.0
    }
}
