use crate::analysis::semantic_analysis::SemanticAnalyser;
use crate::analysis::types::UniqueFunctionIdentifier;
use crate::ast::{
    Assignment, Ast, AstHandle, BinaryOperation, BinaryOperationKind, Identifier, IfStatement, LiteralBool, LiteralDouble, LiteralInt, ReturnStatement, StatementList, UnaryOperation,
    UnaryOperationKind,
};
use crate::function_info::FunctionInfo;
use crate::span::Spanned;
use crate::type_system::{ImplicitCast, Type};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType, VoidType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;

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
    optimization_level: u32,
    semantic_analyser: &'ctx SemanticAnalyser<'ctx>,
    modules: Vec<CodeGenInner<'ctx>>,
    type_to_llvm_type: HashMap<Type, BasicTypeEnum<'ctx>>,
    pass_managers: Vec<PassManager<Module<'ctx>>>,
    // Primitive types here for faster lookup
    void_type: VoidType<'ctx>,
    int_type: IntType<'ctx>,
    double_type: FloatType<'ctx>,
    bool_type: IntType<'ctx>,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn new(context: &'ctx CodeGenContext, semantic_analyser: &'ctx SemanticAnalyser, optimization_level: u32) -> Self {
        let mut type_to_llvm_type: HashMap<Type, BasicTypeEnum<'ctx>> = HashMap::new();

        // Store basic types
        let int_type = context.0.i64_type();
        let double_type = context.0.f64_type();
        let bool_type = context.0.bool_type();
        type_to_llvm_type.insert(Type::Int, BasicTypeEnum::IntType(int_type));
        type_to_llvm_type.insert(Type::Double, BasicTypeEnum::FloatType(double_type));
        type_to_llvm_type.insert(Type::Bool, BasicTypeEnum::IntType(bool_type));

        Self {
            context,
            optimization_level,
            semantic_analyser,
            modules: Vec::new(),
            type_to_llvm_type,
            void_type: context.0.void_type(),
            pass_managers: Vec::new(),
            int_type,
            double_type,
            bool_type,
        }
    }

    fn create_pass_manager(&self) -> PassManager<Module<'ctx>> {
        let pm = PassManager::create(());
        if self.optimization_level >= 1 {
            pm.add_type_based_alias_analysis_pass();
            pm.add_scoped_no_alias_aa_pass();
            pm.add_function_attrs_pass();
            pm.add_ipsccp_pass();
            pm.add_sccp_pass();
            pm.add_global_optimizer_pass(); // TODO: is this actually needed?
            pm.add_basic_alias_analysis_pass();
            pm.add_promote_memory_to_register_pass();
            pm.add_dead_arg_elimination_pass();
            pm.add_cfg_simplification_pass();
            pm.add_aggressive_dce_pass();
            pm.add_early_cse_mem_ssa_pass();
            pm.add_instruction_combining_pass();
            pm.add_instruction_simplify_pass();
            pm.add_partially_inline_lib_calls_pass();
            pm.add_licm_pass();
            pm.add_loop_deletion_pass();
            pm.add_loop_rotate_pass();
            pm.add_memcpy_optimize_pass();
            pm.add_reassociate_pass();
            pm.add_scalar_repl_aggregates_pass();
            pm.add_loop_idiom_pass();
        }
        if self.optimization_level >= 2 {
            pm.add_gvn_pass();
            pm.add_merged_load_store_motion_pass();
            pm.add_loop_vectorize_pass();
            pm.add_constant_merge_pass();
            pm.add_function_inlining_pass();
            pm.add_slp_vectorize_pass();
            pm.add_global_dce_pass();
            pm.add_tail_call_elimination_pass();
            pm.add_correlated_value_propagation_pass();
            pm.add_dead_store_elimination_pass();
            pm.add_jump_threading_pass();
        }
        if self.optimization_level >= 3 {
            pm.add_aggressive_inst_combiner_pass();
        }
        pm
    }

    pub fn add_module(&mut self, module_name: &'ctx str) {
        let module = self.context.0.create_module(module_name);
        let inner = CodeGenInner::new(module, self.semantic_analyser);
        self.modules.push(inner);
        self.pass_managers.push(self.create_pass_manager());
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

        let param_types: &[BasicMetadataTypeEnum<'ctx>] = &[];
        let function_type = if function_info.return_type() == Type::Void {
            codegen.void_type.fn_type(param_types, false)
        } else {
            codegen.type_to_llvm_type[&function_info.return_type()].fn_type(param_types, false)
        };

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

        if !function_value.verify(true) {
            function_value.print_to_stderr();
            panic!("Function '{}' is invalid", name.0);
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
                ImplicitCast::IntZext => function_context.builder.build_int_z_extend(value.into_int_value(), codegen.int_type, "zext").as_basic_value_enum(),
                ImplicitCast::UnsignedIntToDouble => function_context
                    .builder
                    .build_unsigned_int_to_float(value.into_int_value(), codegen.double_type, "double")
                    .as_basic_value_enum(),
                ImplicitCast::SignedIntToDouble => function_context
                    .builder
                    .build_signed_int_to_float(value.into_int_value(), codegen.double_type, "double")
                    .as_basic_value_enum(),
                ImplicitCast::IntToBool => function_context
                    .builder
                    .build_int_compare(inkwell::IntPredicate::NE, value.into_int_value(), codegen.int_type.const_int(0, false), "int_to_bool")
                    .as_basic_value_enum(),
                ImplicitCast::DoubleToBool => function_context
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::UNE, value.into_float_value(), codegen.double_type.const_float(0.0), "double_to_bool")
                    .as_basic_value_enum(),
            }
        } else {
            value
        }
    }

    fn emit_instructions_with_casts<'ast>(&self, ast: &'ast Spanned<Ast<'ast>>, function_context: &CodeGenFunctionContext<'ast, 'ctx>, codegen: &CodeGenLLVM<'ctx>) -> BasicValueEnum<'ctx> {
        let value = self.emit_instructions(ast, function_context, codegen).expect("ast should create a value");
        self.emit_implicit_cast_if_necessary(ast.0.as_handle(), value, function_context, codegen)
    }

    fn emit_instructions<'ast>(&self, ast: &'ast Spanned<Ast<'ast>>, function_context: &CodeGenFunctionContext<'ast, 'ctx>, codegen: &CodeGenLLVM<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match &ast.0 {
            Ast::Identifier(Identifier(name)) => {
                let variable = function_context.variables.get(name).expect("variable should exist");
                let value = function_context.builder.build_load(variable.ty, variable.ptr, name);
                Some(value)
            }
            Ast::UnaryOperation(UnaryOperation(operation, operand)) => {
                let operand_value = self.emit_instructions_with_casts(operand, function_context, codegen);
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
                let lhs_value = self.emit_instructions_with_casts(lhs, function_context, codegen);
                let rhs_value = self.emit_instructions_with_casts(rhs, function_context, codegen);
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
                        BinaryOperationKind::DoubleDivision => {
                            assert!(lhs_value.is_float_value() && rhs_value.is_float_value());
                            Some(function_context.builder.build_float_div(lhs_value.into_float_value(), rhs_value.into_float_value(), "div").into())
                        }
                        BinaryOperationKind::WholeDivision => {
                            if lhs_value.is_float_value() {
                                let div_result = function_context.builder.build_float_div(lhs_value.into_float_value(), rhs_value.into_float_value(), "div");
                                let floor_intrinsic = Intrinsic::find("llvm.floor").expect("floor intrinsic should exist");
                                let floor_function = floor_intrinsic.get_declaration(&self.module, &[lhs_value.get_type()]).expect("floor function should exist");
                                //let floor_result = function_context.builder.build_float_trunc(div_result, codegen.int_type, "trunc");
                                let floor_result = function_context
                                    .builder
                                    .build_call(floor_function, &[div_result.into()], "floor")
                                    .try_as_basic_value()
                                    .expect_left("value should exist");
                                Some(floor_result)
                            } else {
                                // TODO: -2//3 case etc
                                Some(function_context.builder.build_int_signed_div(lhs_value.into_int_value(), rhs_value.into_int_value(), "div").into())
                            }
                        }
                        BinaryOperationKind::Power => {
                            assert!(lhs_value.is_float_value() && rhs_value.is_float_value());
                            let pow_intrinsic = Intrinsic::find("llvm.pow").expect("pow intrinsic should exist");
                            let pow_function = pow_intrinsic.get_declaration(&self.module, &[lhs_value.get_type()]).expect("pow function should exist");
                            let pow_result = function_context
                                .builder
                                .build_call(pow_function, &[lhs_value.into(), rhs_value.into()], "pow")
                                .try_as_basic_value()
                                .expect_left("value should exist");
                            Some(pow_result)
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Ast::IfStatement(IfStatement { condition, statements }) => {
                let condition_value = self.emit_instructions_with_casts(condition, function_context, codegen);

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
            Ast::LiteralInt(LiteralInt(value)) => Some(codegen.int_type.const_int(*value as u64, false).into()),
            Ast::LiteralBool(LiteralBool(bool)) => Some(codegen.bool_type.const_int(if *bool { 1 } else { 0 }, false).into()),
            Ast::LiteralDouble(LiteralDouble(value)) => Some(codegen.double_type.const_float(*value).into()),
            Ast::Assignment(Assignment(name, expression)) => {
                let variable = function_context.variables.get(name).expect("variable should exist");
                let expression_value = self.emit_instructions(expression, function_context, codegen).expect("expression should have a value");
                function_context.builder.build_store(variable.ptr, expression_value);
                None
            }
            Ast::ReturnStatement(ReturnStatement { value }) => {
                if let Some(value) = value {
                    let expression_value = self.emit_instructions(value, function_context, codegen).expect("expression should have a value");
                    function_context.builder.build_return(Some(&expression_value));
                } else {
                    function_context.builder.build_return(None);
                }
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
        // TODO: set optimization level
        let target_machine = target
            .create_target_machine(&target_triple, "generic", "", OptimizationLevel::None, RelocMode::Default, CodeModel::Default)
            .expect("target machine should exist");
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
