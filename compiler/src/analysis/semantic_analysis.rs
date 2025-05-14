use crate::analysis::function_collector_pass::FunctionCollectorPass;
use crate::analysis::return_check_pass::ReturnCheckPass;
use crate::analysis::scope_resolution_pass::{ScopeReferenceMap, ScopeResolutionPass};
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::{SemanticError, SemanticErrorList};
use crate::analysis::type_checker_pass::TypeCheckerPass;
use crate::syntax::ast::Ast;
use crate::syntax::span::Spanned;
use crate::types::function_info::FunctionInfo;
use crate::types::type_system::{FunctionType, Type};
use crate::util::handle::Handle;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Values;
use std::rc::Rc;

pub type FunctionMap<'ast> = FxHashMap<Handle, FunctionInfo<'ast>>;
pub type CallArgumentOrder<'ast> = FxHashMap<Handle, Vec<Option<&'ast Spanned<Ast<'ast>>>>>;

pub struct SemanticAnalyser<'ast> {
    ast: &'ast Spanned<Ast<'ast>>,
    scope_reference_map: ScopeReferenceMap,
    function_map: FunctionMap<'ast>,
    indirect_call_function_types: FxHashMap<Handle, Rc<FunctionType<'ast>>>,
    member_access_meta_data: FxHashMap<Handle, MemberAccessMetadata<'ast>>,
    call_argument_order: CallArgumentOrder<'ast>,
    errors: Vec<SemanticError>,
}

pub struct MemberAccessMetadata<'ast> {
    pub object_type: Type<'ast>,
    pub member_type: Type<'ast>,
    pub identifier: &'ast str,
}

// TODO: what can we throw away / simplify from this??
impl<'ast> SemanticAnalyser<'ast> {
    pub fn new(ast: &'ast Spanned<Ast<'ast>>) -> Self {
        Self {
            ast,
            scope_reference_map: Default::default(),
            function_map: Default::default(),
            indirect_call_function_types: Default::default(),
            member_access_meta_data: Default::default(),
            call_argument_order: Default::default(),
            errors: vec![],
        }
    }

    pub fn analyse(&mut self) {
        let mut semantic_error_list = SemanticErrorList::default();

        self.function_map = {
            let mut function_collector = FunctionCollectorPass::new(&mut semantic_error_list);
            function_collector.visit(self.ast);
            function_collector.into_function_map()
        };

        let scope_reference_map = {
            let mut scope_resolution = ScopeResolutionPass::new(&mut semantic_error_list);
            scope_resolution.register_functions(&self.function_map);
            scope_resolution.visit(self.ast);
            scope_resolution.into_reference_map()
        };

        if semantic_error_list.has_errors() {
            self.errors = semantic_error_list.into_vec();
            return;
        }

        let (indirect_call_function_types, member_access_meta_data, call_argument_order) = {
            let mut type_checker = TypeCheckerPass::new(&mut self.function_map, &scope_reference_map, &mut semantic_error_list);
            type_checker.visit(self.ast);
            (type_checker.indirect_call_function_types, type_checker.member_access_meta_data, type_checker.call_argument_order)
        };

        let mut return_check_pass = ReturnCheckPass::new(&mut semantic_error_list);
        return_check_pass.visit(self.ast);

        self.scope_reference_map = scope_reference_map;
        self.member_access_meta_data = member_access_meta_data;
        self.call_argument_order = call_argument_order;
        self.indirect_call_function_types = indirect_call_function_types;
        self.errors = semantic_error_list.into_vec();
    }

    pub fn errors(&self) -> &[SemanticError] {
        &self.errors
    }

    pub fn function_list_iter(&self) -> Values<'_, Handle, FunctionInfo<'_>> {
        self.function_map.values()
    }

    pub fn identifier_to_declaration(&self, handle: Handle) -> Handle {
        *self.scope_reference_map.references.get(&handle).expect("declaration should exist")
    }

    pub fn indirect_call_function_type(&self, handle: Handle) -> Option<&Rc<FunctionType>> {
        self.indirect_call_function_types.get(&handle)
    }

    pub fn member_access_meta_data(&self, handle: Handle) -> &MemberAccessMetadata<'ast> {
        &self.member_access_meta_data[&handle]
    }

    pub fn call_argument_order(&self, handle: Handle) -> Option<&[Option<&'ast Spanned<Ast<'ast>>>]> {
        self.call_argument_order.get(&handle).map(|v| v.as_slice())
    }
}
