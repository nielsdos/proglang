use crate::analysis::semantic_analysis::SemanticAnalyser;
use crate::analysis::types::UniqueFunctionIdentifier;
use crate::ast::{
    Assignment, Ast, AstHandle, BinaryOperation, BinaryOperationKind, Identifier, IfStatement, LiteralBool, LiteralDouble, LiteralInt, StatementList, UnaryOperation, UnaryOperationKind,
};
use crate::function_info::FunctionInfo;
use crate::span::Spanned;
use crate::type_system::{ImplicitCast, Type};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{BasicTypeEnum, VoidType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use std::collections::HashMap;
use std::path::Path;
use inkwell::intrinsics::Intrinsic;
use inkwell::OptimizationLevel;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple};

pub struct CodeGenContext(Context);

impl Default for CodeGenContext {
    fn default() -> Self {
        Self(Context::create())
    }
}

struct VariableInfo<'ctx> {
    ptr: PointerValue<'ctx>,
    ty: BasicTypeEnum<'ctx>,
}

struct CodeGenFunctionContext<'f, 'ctx> {
    builder: Builder<'ctx>,
    function_value: FunctionValue<'ctx>,
    variables: HashMap<&'f str, VariableInfo<'ctx>>,
}

struct CodeGenInner<'ctx> {
    module: Module<'ctx>,
    semantic_analyser: &'ctx SemanticAnalyser<'ctx>,
}

pub struct CodeGenLLVM<'ctx> {
    context: &'ctx CodeGenContext,
    semantic_analyser: &'ctx SemanticAnalyser<'ctx>,
    modules: Vec<CodeGenInner<'ctx>>,
    type_to_llvm_type: HashMap<Type, BasicTypeEnum<'ctx>>,
    void_type: VoidType<'ctx>,
    pass_managers: Vec<PassManager<Module<'ctx>>>,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn new(context: &'ctx CodeGenContext, semantic_analyser: &'ctx SemanticAnalyser) -> Self {
        let mut type_to_llvm_type: HashMap<Type, BasicTypeEnum<'ctx>> = HashMap::new();

        // Store basic types
        type_to_llvm_type.insert(Type::Int, BasicTypeEnum::IntType(context.0.i64_type()));
        type_to_llvm_type.insert(Type::Double, BasicTypeEnum::FloatType(context.0.f64_type()));
        type_to_llvm_type.insert(Type::Bool, BasicTypeEnum::IntType(context.0.custom_width_int_type(1)));

        Self {
            context,
            semantic_analyser,
            modules: Vec::new(),
            type_to_llvm_type,
            void_type: context.0.void_type(),
            pass_managers: Vec::new(),
        }
    }

    pub fn add_module(&mut self, module_name: &'ctx str) {
        let module = self.context.0.create_module(module_name);
        let inner = CodeGenInner::new(module, self.semantic_analyser);
        self.modules.push(inner);
        let pm = PassManager::create(());
        //pm.add_promote_memory_to_register_pass();
        // TODO: more passes
        self.pass_managers.push(pm);
    }

    pub fn codegen_function(&mut self, name: &UniqueFunctionIdentifier, function_info: &FunctionInfo) {
        let builder = self.context.0.create_builder();

        // TODO: hardcoded to first module right now
        self.modules[0].codegen_function(name, function_info, builder, self);
        self.pass_managers[0].run_on(&self.modules[0].module);
    }

    pub fn dump(&self) {
        for module in &self.modules {
            module.dump_module();
        }
    }
}

impl<'ctx> CodeGenInner<'ctx> {
    pub fn new(module: Module<'ctx>, semantic_analyser: &'ctx SemanticAnalyser<'ctx>) -> Self {
        Self { module, semantic_analyser }
    }

    pub fn codegen_function(&self, name: &UniqueFunctionIdentifier, function_info: &FunctionInfo, builder: Builder<'ctx>, codegen: &CodeGenLLVM<'ctx>) {
        // Create function declaration and entry basic block
        let context = &codegen.context.0;
        let function_type = codegen.void_type.fn_type(&[], false);
        let function_value = self.module.add_function(name.as_str(), function_type, None);
        let basic_block = context.append_basic_block(function_value, "entry");
        builder.position_at_end(basic_block);

        let mut variables = HashMap::new();

        // Create memory locations for the local variables
        for (variable_name, variable_type) in function_info.variables() {
            // TODO: default values maybe?
            let variable_type = codegen.type_to_llvm_type[variable_type];
            let variable_memory = builder.build_alloca(variable_type, variable_name);
            variables.insert(
                variable_name,
                VariableInfo {
                    ptr: variable_memory,
                    ty: variable_type,
                },
            );
        }

        // Emit instructions
        let body = function_info.body();
        let function_context = CodeGenFunctionContext { builder, function_value, variables };
        self.emit_instructions(body, &function_context, codegen);

        if function_context.is_bb_unterminated() {
            function_context.builder.build_return(None);
        }
    }

    fn emit_implicit_cast_if_necessary<'ast>(
        &self,
        handle: AstHandle,
        value: BasicValueEnum<'ctx>,
        function_context: &CodeGenFunctionContext<'ast, 'ctx>,
        codegen: &CodeGenLLVM<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        if let Some(implicit_cast_entry) = self.semantic_analyser.implicit_cast_entry(handle) {
            match implicit_cast_entry {
                ImplicitCast::IntZext => function_context
                    .builder
                    .build_int_z_extend(value.into_int_value(), codegen.type_to_llvm_type[&Type::Int].into_int_type(), "zext")
                    .as_basic_value_enum(),
                ImplicitCast::UnsignedIntToDouble => function_context
                    .builder
                    .build_unsigned_int_to_float(value.into_int_value(), codegen.type_to_llvm_type[&Type::Double].into_float_type(), "double")
                    .as_basic_value_enum(),
                ImplicitCast::SignedIntToDouble => function_context
                    .builder
                    .build_signed_int_to_float(value.into_int_value(), codegen.type_to_llvm_type[&Type::Double].into_float_type(), "double")
                    .as_basic_value_enum(),
                ImplicitCast::IntToBool => function_context
                    .builder
                    .build_int_compare(inkwell::IntPredicate::NE, value.into_int_value(), codegen.type_to_llvm_type[&Type::Int].into_int_type().const_int(0, false), "int_to_bool")
                    .as_basic_value_enum(),
                ImplicitCast::DoubleToBool => function_context
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::UNE, value.into_float_value(), codegen.type_to_llvm_type[&Type::Double].into_float_type().const_float(0.0), "double_to_bool")
                    .as_basic_value_enum(),
            }
        } else {
            value
        }
    }

    fn emit_instructions<'ast>(&self, ast: &'ast Spanned<Ast<'ast>>, function_context: &CodeGenFunctionContext<'ast, 'ctx>, codegen: &CodeGenLLVM<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match &ast.0 {
            Ast::Identifier(Identifier(name)) => {
                let variable = function_context.variables.get(name).expect("variable should exist");
                let value = function_context.builder.build_load(variable.ty, variable.ptr, name);
                Some(value)
            }
            Ast::UnaryOperation(UnaryOperation(operation, operand)) => {
                let operand_value = self.emit_instructions(operand, function_context, codegen).expect("operand should have a value");
                let operand_value = self.emit_implicit_cast_if_necessary(operand.0.as_handle(), operand_value, function_context, codegen);
                match operation {
                    UnaryOperationKind::Minus => {
                        if operand_value.is_int_value() {
                            // TODO: overflow?
                            let result = function_context.builder.build_int_neg(operand_value.into_int_value(), "neg");
                            Some(result.into())
                        } else {
                            let result = function_context.builder.build_float_neg(operand_value.into_float_value(), "neg");
                            Some(result.into())
                        }
                    }
                    UnaryOperationKind::Plus => Some(operand_value),
                }
            }
            Ast::StatementList(StatementList(statements)) => {
                for statement in statements {
                    self.emit_instructions(statement, function_context, codegen);
                }
                None
            }
            Ast::BinaryOperation(BinaryOperation(lhs, operation, rhs)) => {
                let lhs_value = self.emit_instructions(lhs, function_context, codegen).expect("lhs should have a value");
                let lhs_value = self.emit_implicit_cast_if_necessary(lhs.0.as_handle(), lhs_value, function_context, codegen);
                let rhs_value = self.emit_instructions(rhs, function_context, codegen).expect("rhs should have a value");
                let rhs_value = self.emit_implicit_cast_if_necessary(rhs.0.as_handle(), rhs_value, function_context, codegen);
                // TODO: overflow handling, check NaN handling, division by zero checking, power of zero checking

                if operation.is_comparison_op() {
                    if lhs_value.is_float_value() {
                        Some(
                            function_context
                                .builder
                                .build_float_compare(operation.to_llvm_float_comparison(), lhs_value.into_float_value(), rhs_value.into_float_value(), "eq")
                                .into(),
                        )
                    } else {
                        Some(
                            function_context
                                .builder
                                .build_int_compare(operation.to_llvm_int_comparison(), lhs_value.into_int_value(), rhs_value.into_int_value(), "eq")
                                .into(),
                        )
                    }
                } else {
                    match *operation {
                        BinaryOperationKind::Addition => {
                            if lhs_value.is_float_value() {
                                Some(function_context.builder.build_float_add(lhs_value.into_float_value(), rhs_value.into_float_value(), "add").into())
                            } else {
                                Some(function_context.builder.build_int_add(lhs_value.into_int_value(), rhs_value.into_int_value(), "add").into())
                            }
                        }
                        BinaryOperationKind::Subtraction => {
                            if lhs_value.is_float_value() {
                                Some(function_context.builder.build_float_sub(lhs_value.into_float_value(), rhs_value.into_float_value(), "sub").into())
                            } else {
                                Some(function_context.builder.build_int_sub(lhs_value.into_int_value(), rhs_value.into_int_value(), "sub").into())
                            }
                        }
                        BinaryOperationKind::Product => {
                            if lhs_value.is_float_value() {
                                Some(function_context.builder.build_float_mul(lhs_value.into_float_value(), rhs_value.into_float_value(), "mul").into())
                            } else {
                                Some(function_context.builder.build_int_mul(lhs_value.into_int_value(), rhs_value.into_int_value(), "mul").into())
                            }
                        }
                        _ => unimplemented!(),
                    }
                }
            }
            Ast::IfStatement(IfStatement { condition, statements }) => {
                let condition_value = self.emit_instructions(condition, function_context, codegen).expect("condition should have a value");
                let condition_value = self.emit_implicit_cast_if_necessary(condition.0.as_handle(), condition_value, function_context, codegen);

                let then_block = codegen.context.0.append_basic_block(function_context.function_value, "then");
                let else_block = codegen.context.0.append_basic_block(function_context.function_value, "after_if");

                function_context.builder.build_conditional_branch(condition_value.into_int_value(), then_block, else_block);

                function_context.builder.position_at_end(then_block);
                self.emit_instructions(statements, function_context, codegen);

                if function_context.is_bb_unterminated() {
                    function_context.builder.build_unconditional_branch(else_block);
                }

                function_context.builder.position_at_end(else_block);

                None
            }
            Ast::LiteralInt(LiteralInt(value)) => {
                let ty = codegen.type_to_llvm_type.get(&Type::Int).expect("int should exist").into_int_type();
                Some(ty.const_int(*value as u64, false).into())
            }
            Ast::LiteralBool(LiteralBool(bool)) => {
                let ty = codegen.type_to_llvm_type.get(&Type::Bool).expect("bool should exist").into_int_type();
                Some(ty.const_int(if *bool { 1 } else { 0 }, false).into())
            }
            Ast::LiteralDouble(LiteralDouble(value)) => {
                let ty = codegen.type_to_llvm_type.get(&Type::Double).expect("double should exist").into_float_type();
                Some(ty.const_float(*value).into())
            }
            Ast::Assignment(Assignment(name, expression)) => {
                let variable = function_context.variables.get(name).expect("variable should exist");
                let expression_value = self.emit_instructions(expression, function_context, codegen).expect("expression should have a value");
                function_context.builder.build_store(variable.ptr, expression_value);
                None
            }
            _ => None,
        }
    }

    pub fn dump_module(&self) {
        self.module.print_to_stderr();
        // TODO: init & output should be split, and also moved to a different method
        Target::initialize_x86(&InitializationConfig::default());
        let target_triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        let target = Target::from_triple(&target_triple).expect("target should exist");
        let target_machine = target.create_target_machine(&target_triple, "generic", "", OptimizationLevel::None, RelocMode::Default, CodeModel::Default).expect("target machine should exist");
        target_machine.write_to_file(&self.module, FileType::Assembly, Path::new("output.s")).expect("should write to file");
    }
}

impl<'f, 'ctx> CodeGenFunctionContext<'f, 'ctx> {
    pub fn is_bb_unterminated(&self) -> bool {
        self.builder.get_insert_block().expect("should have insert block").get_terminator().is_none()
    }
}

impl BinaryOperationKind {
    pub fn to_llvm_int_comparison(&self) -> inkwell::IntPredicate {
        match self {
            BinaryOperationKind::Equal => inkwell::IntPredicate::EQ,
            BinaryOperationKind::NotEqual => inkwell::IntPredicate::NE,
            BinaryOperationKind::LessThan => inkwell::IntPredicate::SLT,
            BinaryOperationKind::LessThanEqual => inkwell::IntPredicate::SLE,
            BinaryOperationKind::GreaterThan => inkwell::IntPredicate::SGT,
            BinaryOperationKind::GreaterThanEqual => inkwell::IntPredicate::SGE,
            _ => unreachable!(),
        }
    }

    pub fn to_llvm_float_comparison(&self) -> inkwell::FloatPredicate {
        match self {
            BinaryOperationKind::Equal => inkwell::FloatPredicate::UEQ,
            BinaryOperationKind::NotEqual => inkwell::FloatPredicate::UNE,
            BinaryOperationKind::LessThan => inkwell::FloatPredicate::ULT,
            BinaryOperationKind::LessThanEqual => inkwell::FloatPredicate::ULE,
            BinaryOperationKind::GreaterThan => inkwell::FloatPredicate::UGT,
            BinaryOperationKind::GreaterThanEqual => inkwell::FloatPredicate::UGE,
            _ => unreachable!(),
        }
    }
}
