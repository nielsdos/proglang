use crate::analysis::class_collector_pass::ClassCollectorPass;
use crate::analysis::function_collector_pass::FunctionCollectorPass;
use crate::analysis::return_check_pass::ReturnCheckPass;
use crate::analysis::scope_resolution_pass::{ScopeReferenceMap, ScopeResolutionPass};
use crate::analysis::semantic_analysis_pass::SemanticAnalysisPass;
use crate::analysis::semantic_error::{SemanticError, SemanticErrorList};
use crate::analysis::type_checker_pass::TypeCheckerPass;
use crate::builtin::Builtins;
use crate::syntax::ast::Ast;
use crate::syntax::span::Spanned;
use crate::types::class_info::ClassInfo;
use crate::types::function_info::FunctionInfo;
use crate::types::type_system::{FunctionType, Type};
use crate::util::handle::Handle;
use std::collections::{hash_map::Values, HashMap};
use std::rc::Rc;
use crate::analysis::type_graph_pass::TypeGraphPass;

pub type FunctionMap<'ast> = HashMap<Handle, FunctionInfo<'ast>>;
pub type ClassMap<'ast> = HashMap<&'ast str, ClassInfo<'ast>>;

pub struct SemanticAnalyser<'ast> {
    ast: &'ast Spanned<Ast<'ast>>,
    builtins: &'ast Builtins<'ast>,
    scope_reference_map: ScopeReferenceMap,
    function_map: FunctionMap<'ast>,
    class_map: ClassMap<'ast>,
    indirect_call_function_types: HashMap<Handle, Rc<FunctionType<'ast>>>,
    member_access_meta_data: HashMap<Handle, MemberAccessMetadata<'ast>>,
    errors: Vec<SemanticError>,
}

pub struct MemberAccessMetadata<'ast> {
    pub object_type: Type<'ast>,
    pub member_type: Type<'ast>,
    pub index: u32,
}

// TODO: what can we throw away / simplify from this??
impl<'ast> SemanticAnalyser<'ast> {
    pub fn new(ast: &'ast Spanned<Ast<'ast>>, builtins: &'ast Builtins<'ast>) -> Self {
        Self {
            ast,
            builtins,
            scope_reference_map: Default::default(),
            function_map: Default::default(),
            class_map: Default::default(),
            indirect_call_function_types: Default::default(),
            member_access_meta_data: Default::default(),
            errors: vec![],
        }
    }

    pub fn analyse(&mut self) {
        let mut semantic_error_list = SemanticErrorList::default();

        let class_map = {
            let mut class_collector = ClassCollectorPass::new(&mut semantic_error_list);
            class_collector.visit(self.ast);
            class_collector.into_class_map()
        };

        {
            let mut type_graph = TypeGraphPass::new(&class_map, &mut semantic_error_list);
            type_graph.check();
        }

        self.function_map = {
            let mut function_collector = FunctionCollectorPass::new(&mut semantic_error_list);
            function_collector.register_internal_functions(self.builtins);
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

        let (indirect_call_function_types, member_access_meta_data) = {
            let mut type_checker = TypeCheckerPass::new(&mut self.function_map, &scope_reference_map, &class_map, &mut semantic_error_list);
            type_checker.visit(self.ast);
            (type_checker.indirect_call_function_types, type_checker.member_access_meta_data)
        };

        let mut return_check_pass = ReturnCheckPass::new(&mut semantic_error_list);
        return_check_pass.visit(self.ast);

        self.class_map = class_map;
        self.scope_reference_map = scope_reference_map;
        self.member_access_meta_data = member_access_meta_data;
        self.indirect_call_function_types = indirect_call_function_types;
        self.errors = semantic_error_list.into_vec();
    }

    pub fn errors(&self) -> &[SemanticError] {
        &self.errors
    }

    pub fn class_map(&self) -> &ClassMap<'ast> {
        &self.class_map
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
}
