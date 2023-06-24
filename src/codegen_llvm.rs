use crate::function_info::FunctionInfo;
use crate::semantic_analysis::UniqueFunctionIdentifier;
use crate::type_system::Type;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;
use std::collections::HashMap;

pub struct CodeGenContext(Context);

impl CodeGenContext {
    pub fn new() -> Self {
        Self(Context::create())
    }
}

struct CodeGenInner<'ctx> {
    module: Module<'ctx>,
}

pub struct CodeGenLLVM<'ctx> {
    context: &'ctx CodeGenContext,
    modules: Vec<CodeGenInner<'ctx>>,
    type_to_llvm_type: HashMap<Type, AnyTypeEnum<'ctx>>,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn new(context: &'ctx CodeGenContext) -> Self {
        let mut type_to_llvm_type: HashMap<Type, AnyTypeEnum<'ctx>> = HashMap::new();

        // Store basic types
        type_to_llvm_type.insert(Type::Int, AnyTypeEnum::IntType(context.0.i64_type()));
        type_to_llvm_type.insert(Type::Double, AnyTypeEnum::FloatType(context.0.f64_type()));
        type_to_llvm_type.insert(
            Type::Bool,
            AnyTypeEnum::IntType(context.0.custom_width_int_type(1)),
        );
        type_to_llvm_type.insert(Type::Void, AnyTypeEnum::VoidType(context.0.void_type()));

        Self {
            context,
            modules: Vec::new(),
            type_to_llvm_type,
        }
    }

    pub fn add_module(&mut self, module_name: &'ctx str) {
        let inner = CodeGenInner::new(self.context.0.create_module(module_name));
        self.modules.push(inner);
    }

    pub fn codegen_function(
        &mut self,
        name: &UniqueFunctionIdentifier,
        function_info: &FunctionInfo,
    ) {
        let builder = self.context.0.create_builder();

        // TODO: hardcoded to first module right now
        self.modules[0].codegen_function(name, function_info, builder, self);
    }

    pub fn dump(&self) {
        for module in &self.modules {
            module.dump_module();
        }
    }
}

impl<'ctx> CodeGenInner<'ctx> {
    pub fn new(module: Module<'ctx>) -> Self {
        Self { module }
    }

    pub fn codegen_function(
        &self,
        name: &UniqueFunctionIdentifier,
        function_info: &FunctionInfo,
        builder: Builder<'ctx>,
        codegen: &CodeGenLLVM<'ctx>,
    ) {
        // Create function declaration and entry basic block
        let context = &codegen.context.0;
        let function_type = context.void_type().fn_type(&[], false);
        let function = self.module.add_function(name.as_str(), function_type, None);
        let basic_block = context.append_basic_block(function, "entry");
        builder.position_at_end(basic_block);

        // Create memory locations for the local variables
        for (variable_name, variable_type) in function_info.variables() {
            let variable_type = codegen.type_to_llvm_type[variable_type];
            let variable_memory = match variable_type {
                AnyTypeEnum::IntType(ty) => builder.build_alloca(ty, variable_name),
                AnyTypeEnum::FloatType(ty) => builder.build_alloca(ty, variable_name),
                AnyTypeEnum::ArrayType(ty) => builder.build_alloca(ty, variable_name),
                AnyTypeEnum::PointerType(ty) => builder.build_alloca(ty, variable_name),
                AnyTypeEnum::StructType(ty) => builder.build_alloca(ty, variable_name),
                AnyTypeEnum::VectorType(ty) => builder.build_alloca(ty, variable_name),
                _ => unreachable!("codegen for type not implemented"),
            };
        }

        // Emit instructions
        let body = function_info.body();
        println!("{:#?}", body);
    }

    pub fn dump_module(&self) {
        self.module.print_to_stderr();
    }
}
