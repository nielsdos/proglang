use crate::ast::{Ast, AstHandle};
use crate::span::Spanned;
use std::collections::HashMap;

pub struct SemanticAnalyser<'ast> {
    ast: &'ast Spanned<Ast<'ast>>,
    type_table: HashMap<AstHandle, Type>,
    variable_types: HashMap<&'ast str, Type>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    Int,
    Double,
    Void,
}

// TODO: error handling!
impl<'ast> SemanticAnalyser<'ast> {
    pub fn new(ast: &'ast Spanned<Ast<'ast>>) -> Self {
        Self {
            ast,
            type_table: HashMap::new(),
            variable_types: HashMap::new(),
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

    fn query_variable_type(&self, identifier: &'ast str) -> Option<&Type> {
        // TODO: take into account scope or smth like that?
        self.variable_types.get(identifier)
    }

    fn visit(&mut self, node: &'ast Spanned<Ast<'ast>>) {
        let node = &node.0;
        match &node {
            Ast::LiteralInt(_) => {
                self.store_ast_type(&node, Type::Int);
            }
            Ast::LiteralDouble(_) => {
                self.store_ast_type(&node, Type::Double);
            }
            Ast::Identifier(name) => {
                if let Some(ty) = self.query_variable_type(name) {
                    self.store_ast_type(&node, *ty);
                }
                // TODO: error case??
            }
            Ast::BinaryOperation(lhs, _, rhs) => {
                self.visit(&lhs);
                self.visit(&rhs);

                println!("{:?}", node);

                let lhs_type = self.query_ast_type_spanned(&lhs).expect("just visited lhs");
                let rhs_type = self.query_ast_type_spanned(&rhs).expect("just visited rhs");

                println!("{:?} {:?}", lhs_type, rhs_type);
            }
            Ast::UnaryOperation(_, rhs) => {
                self.visit(&rhs);
                // TODO: typecheck
            }
            Ast::Assignment(identifier, rhs) => {
                self.visit(&rhs);
                let rhs_type = self.query_ast_type_spanned(&rhs).expect("just visited rhs");
                self.variable_types.insert(identifier, *rhs_type);
                // TODO: error case??
            }
            Ast::StatementList(statements) => {
                for statement in statements {
                    self.visit(&statement);
                }
            }
            Ast::FunctionDeclaration(_, body) => {
                self.visit(&body);
            }
            Ast::IfStatement {
                condition,
                statements,
            } => {
                self.visit(&condition);
                // TODO: check condition type
                self.visit(&statements);
            }
            Ast::Todo => todo!(),
        }
    }
}
