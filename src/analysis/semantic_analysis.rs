use crate::analysis::function_collector_pass::FunctionCollectorPass;
use crate::analysis::return_check_pass::ReturnCheckPass;
use crate::analysis::scope_resolution_pass::{ScopeReferenceMap, ScopeResolutionPass};
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::{SemanticError, SemanticErrorList};
use crate::analysis::type_checker_pass::TypeCheckerPass;
use crate::syntax::ast::Ast;
use crate::syntax::span::Spanned;
use crate::types::function_info::FunctionInfo;
use crate::types::type_system::{FunctionType, ImplicitCast};
use crate::util::handle::Handle;
use std::collections::HashMap;
use std::rc::Rc;

pub type FunctionMap<'ast> = HashMap<Handle, FunctionInfo<'ast>>;

pub struct SemanticAnalyser<'ast> {
    ast: &'ast Spanned<Ast<'ast>>,
    scope_reference_map: ScopeReferenceMap,
    implicit_cast_table: HashMap<Handle, ImplicitCast>,
    function_map: FunctionMap<'ast>,
    indirect_call_function_types: HashMap<Handle, Rc<FunctionType>>,
    errors: Vec<SemanticError>,
}

impl<'ast> SemanticAnalyser<'ast> {
    pub fn new(ast: &'ast Spanned<Ast<'ast>>) -> Self {
        Self {
            ast,
            scope_reference_map: Default::default(),
            implicit_cast_table: Default::default(),
            function_map: Default::default(),
            indirect_call_function_types: Default::default(),
            errors: vec![],
        }
    }

    pub fn analyse(&mut self) {
        let mut semantic_error_list = SemanticErrorList::default();

        self.function_map = {
            let mut function_collector = FunctionCollectorPass::new(&mut semantic_error_list);
            function_collector.visit(self.ast);
            function_collector.function_map
        };

        let scope_reference_map = {
            let mut scope_resolution = ScopeResolutionPass::new(&mut semantic_error_list);
            scope_resolution.register_functions(&self.function_map);
            scope_resolution.visit(self.ast);
            scope_resolution.reference_map
        };

        if semantic_error_list.has_errors() {
            self.errors = semantic_error_list.into_vec();
            return;
        }

        let (implicit_cast_table, indirect_call_function_types) = {
            let mut type_checker = TypeCheckerPass::new(&mut self.function_map, &scope_reference_map, &mut semantic_error_list);
            type_checker.visit(self.ast);
            (type_checker.implicit_cast_table, type_checker.indirect_call_function_types)
        };

        let mut return_check_pass = ReturnCheckPass::new(&mut semantic_error_list);
        return_check_pass.visit(self.ast);

        self.scope_reference_map = scope_reference_map;
        self.implicit_cast_table = implicit_cast_table;
        self.indirect_call_function_types = indirect_call_function_types;
        self.errors = semantic_error_list.into_vec();
    }

    pub fn errors(&self) -> &[SemanticError] {
        &self.errors
    }

    pub fn function_list_iter(&self) -> impl Iterator<Item = (&Handle, &'_ FunctionInfo<'_>)> {
        self.function_map.iter()
    }

    pub fn implicit_cast_entry(&self, handle: Handle) -> Option<&ImplicitCast> {
        self.implicit_cast_table.get(&handle)
    }

    pub fn identifier_to_declaration(&self, handle: Handle) -> Handle {
        *self.scope_reference_map.references.get(&handle).expect("declaration should exist")
    }

    pub fn try_identifier_to_declaration(&self, handle: Handle) -> Option<&Handle> {
        self.scope_reference_map.references.get(&handle)
    }

    pub fn indirect_call_function_type(&self, handle: Handle) -> Option<&Rc<FunctionType>> {
        self.indirect_call_function_types.get(&handle)
    }
}
