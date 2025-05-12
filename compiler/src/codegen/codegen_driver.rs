use crate::analysis::semantic_analysis::ClassMap;
use crate::codegen::codegen_llvm::{CodeGenContext, CodeGenLLVM};
use crate::mid_ir::ir::MidFunction;

pub struct CodeGen<'c> {
    llvm_codegen: CodeGenLLVM<'c>,
    mid_functions: &'c [MidFunction<'c>],
}

impl<'c> CodeGen<'c> {
    pub fn new(codegen_context: &'c CodeGenContext, mid_functions: &'c [MidFunction<'c>], optimization_level: u32) -> Self {
        Self {
            llvm_codegen: CodeGenLLVM::new(codegen_context, optimization_level),
            mid_functions,
        }
    }

    pub fn codegen_program(&mut self, class_map: &'c ClassMap<'c>) {
        self.llvm_codegen.add_module("main_module");
        self.codegen_types(class_map);
        self.codegen_functions();
    }

    fn codegen_types(&mut self, class_map: &'c ClassMap<'c>) {
        self.llvm_codegen.codegen_types(class_map);
    }

    fn codegen_functions(&mut self) {
        for mid_function in self.mid_functions {
            self.declare_function(mid_function);
        }

        for mid_function in self.mid_functions {
            self.codegen_function(mid_function);
        }

        self.llvm_codegen.optimize();
    }

    fn declare_function(&mut self, mid_function: &'c MidFunction<'c>) {
        self.llvm_codegen.declare_function(mid_function);
    }

    fn codegen_function(&mut self, mid_function: &'c MidFunction<'c>) {
        self.llvm_codegen.codegen_function(mid_function);
    }

    pub fn dump(&self) {
        self.llvm_codegen.dump();
    }
}
