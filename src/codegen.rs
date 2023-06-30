use crate::analysis::semantic_analysis::SemanticAnalyser;
use crate::analysis::types::UniqueFunctionIdentifier;
use crate::codegen_llvm::{CodeGenContext, CodeGenLLVM};
use crate::function_info::FunctionInfo;

pub struct CodeGen<'c> {
    semantic_analyser: &'c SemanticAnalyser<'c>,
    llvm_codegen: CodeGenLLVM<'c>,
}

impl<'c> CodeGen<'c> {
    pub fn new(semantic_analyser: &'c SemanticAnalyser<'c>, codegen_context: &'c CodeGenContext, optimization_level: u32) -> Self {
        Self {
            semantic_analyser,
            llvm_codegen: CodeGenLLVM::new(codegen_context, semantic_analyser, optimization_level),
        }
    }

    pub fn codegen_program(&mut self) {
        self.llvm_codegen.add_module("main_module");
        self.codegen_functions();
    }

    fn codegen_functions(&mut self) {
        for (name, function_info) in self.semantic_analyser.function_list_iter() {
            self.codegen_function(name, function_info);
        }
    }

    fn codegen_function(&mut self, name: &UniqueFunctionIdentifier, function_info: &FunctionInfo) {
        self.llvm_codegen.codegen_function(name, function_info);
    }

    pub fn dump(&self) {
        self.llvm_codegen.dump();
    }
}
