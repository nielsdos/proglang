use crate::analysis::function_collector_pass::FunctionCollectorPass;
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::type_checker_pass::TypeCheckerPass;
use crate::analysis::types::{SemanticError, SemanticErrorList, UniqueFunctionIdentifier};
use crate::ast::{Ast, AstHandle};
use crate::function_info::FunctionInfo;
use crate::span::Spanned;
use crate::type_system::{ImplicitCast, Type};
use std::collections::HashMap;

pub struct SemanticAnalyser<'ast> {
    ast: &'ast Spanned<Ast<'ast>>,
    type_table: HashMap<AstHandle, Type>,
    implicit_cast_table: HashMap<AstHandle, ImplicitCast>,
    function_map: HashMap<UniqueFunctionIdentifier<'ast>, FunctionInfo<'ast>>,
    errors: Vec<SemanticError>,
}

impl<'ast> SemanticAnalyser<'ast> {
    pub fn new(ast: &'ast Spanned<Ast<'ast>>) -> Self {
        Self {
            ast,
            type_table: HashMap::new(),
            implicit_cast_table: Default::default(),
            function_map: Default::default(),
            errors: vec![],
        }
    }

    pub fn analyse(&mut self) {
        let mut semantic_error_list = SemanticErrorList::default();

        self.function_map = {
            let mut function_collector = FunctionCollectorPass { function_map: Default::default(), semantic_error_list: &mut semantic_error_list };
            function_collector.visit(self.ast);
            function_collector.function_map
        };

        let (type_table, implicit_cast_table) = {
            let mut type_checker = TypeCheckerPass {
                type_table: Default::default(),
                implicit_cast_table: Default::default(),
                current_function: None,
                function_map: &mut self.function_map,
                semantic_error_list: &mut semantic_error_list,
            };
            type_checker.visit(self.ast);
            (type_checker.type_table, type_checker.implicit_cast_table)
        };

        self.type_table = type_table;
        self.implicit_cast_table = implicit_cast_table;
        self.errors = semantic_error_list.into_vec();
    }

    pub fn errors(&self) -> &[SemanticError] {
        &self.errors
    }

    pub fn function_list_iter(&self) -> impl Iterator<Item = (&'_ UniqueFunctionIdentifier<'_>, &'_ FunctionInfo<'_>)> {
        self.function_map.iter().map(|(k, v)| (k, v))
    }

    pub fn implicit_cast_entry(&self, handle: AstHandle) -> Option<&ImplicitCast> {
        self.implicit_cast_table.get(&handle)
    }
}
