use crate::analysis::semantic_analysis::SemanticAnalyser;
use crate::analysis::unique_function_identifier::UniqueFunctionIdentifier;
use crate::syntax::ast::{
    Assignment, Ast, BinaryOperation, BinaryOperationKind, Declaration, FunctionCall, Identifier, IfStatement, LiteralBool, LiteralFloat, LiteralInt, ReturnStatement, StatementList, UnaryOperation,
    UnaryOperationKind,
};
use crate::syntax::span::Spanned;
use crate::types::function_info::FunctionInfo;
use crate::types::type_system::{ImplicitCast, Type};
use crate::util::handle::Handle;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType, VoidType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{IntPredicate, OptimizationLevel};
use smallvec::SmallVec;
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

struct CodeGenFunctionContext<'ctx> {
    builder: Builder<'ctx>,
    function_value: FunctionValue<'ctx>,
    variables: HashMap<Handle, VariableInfo<'ctx>>,
}

struct CodeGenInner<'ctx> {
    module: Module<'ctx>,
    semantic_analyser: &'ctx SemanticAnalyser<'ctx>,
    function_declaration_handle_to_function_value: HashMap<Handle, FunctionValue<'ctx>>,
    optimization_level: u32,
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
        let inner = CodeGenInner::new(module, self.semantic_analyser, self.optimization_level);
        self.modules.push(inner);
        self.pass_managers.push(self.create_pass_manager());
    }

    pub fn declare_function(&mut self, name: &UniqueFunctionIdentifier, function_info: &FunctionInfo) {
        // TODO: hardcoded to first module right now
        let function_value = self.modules[0].declare_function(name, function_info, self);
        self.modules[0].function_declaration_handle_to_function_value.insert(function_info.as_handle(), function_value);
    }

    pub fn codegen_function(&mut self, name: &UniqueFunctionIdentifier, function_info: &FunctionInfo) {
        let builder = self.context.0.create_builder();

        // TODO: hardcoded to first module right now
        self.modules[0].codegen_function(name, function_info, builder, self);
    }

    pub fn optimize(&self) {
        self.pass_managers[0].run_on(&self.modules[0].module);
    }

    pub fn dump(&self) {
        for module in &self.modules {
            module.dump_module();
        }
    }
}

impl<'ctx> CodeGenInner<'ctx> {
    pub fn new(module: Module<'ctx>, semantic_analyser: &'ctx SemanticAnalyser<'ctx>, optimization_level: u32) -> Self {
        Self {
            module,
            semantic_analyser,
            function_declaration_handle_to_function_value: Default::default(),
            optimization_level,
        }
    }

    pub fn declare_function(&self, name: &UniqueFunctionIdentifier, function_info: &FunctionInfo, codegen: &CodeGenLLVM<'ctx>) -> FunctionValue<'ctx> {
        let arg_types = function_info
            .args()
            .iter()
            .map(|arg| {
                let arg = &arg.0;
                let ty = codegen.type_to_llvm_type[arg.ty()];
                BasicMetadataTypeEnum::from(ty)
            })
            .collect::<SmallVec<[BasicMetadataTypeEnum<'ctx>; 4]>>();

        let function_type = if *function_info.return_type() == Type::Void {
            codegen.void_type.fn_type(arg_types.as_slice(), false)
        } else {
            codegen.type_to_llvm_type[function_info.return_type()].fn_type(arg_types.as_slice(), false)
        };

        self.module.add_function(name.as_str(), function_type, None)
    }

    pub fn codegen_function(&self, name: &UniqueFunctionIdentifier, function_info: &FunctionInfo, builder: Builder<'ctx>, codegen: &CodeGenLLVM<'ctx>) {
        let context = &codegen.context.0;

        println!("codegen function: {}", name.0);

        let function_value = self.function_declaration_handle_to_function_value[&function_info.as_handle()];
        let basic_block = context.append_basic_block(function_value, "entry");
        builder.position_at_end(basic_block);

        let mut variables = HashMap::new();

        // Create memory locations for the local variables
        for (variable_name, variable_type) in function_info.variables() {
            // Can't have a declaration without an assignment, so a default value is not necessary
            let variable_type = codegen.type_to_llvm_type[variable_type];
            let variable_memory = builder.build_alloca(variable_type, "var");
            variables.insert(
                variable_name,
                VariableInfo {
                    ptr: variable_memory,
                    ty: variable_type,
                },
            );
        }

        // Copy arguments to the function's scope
        for (index, arg) in function_info.args().iter().enumerate() {
            let variable_memory = variables[&arg.0.as_handle()].ptr;
            let arg_value = function_value.get_nth_param(index as u32).expect("argument should exist");
            builder.build_store(variable_memory, arg_value);
        }

        // Emit instructions
        let body = function_info.body();
        let function_context = CodeGenFunctionContext { builder, function_value, variables };
        self.emit_instructions(body, &function_context, codegen);

        if function_context.is_bb_unterminated() {
            if *function_info.return_type() == Type::Void {
                function_context.builder.build_return(None);
            } else {
                function_context.builder.build_unreachable();
            }
        }

        if !function_value.verify(true) {
            function_value.print_to_stderr();
            panic!("Function '{}' is invalid", name.0);
        }
    }

    fn emit_implicit_cast_if_necessary(&self, handle: Handle, value: BasicValueEnum<'ctx>, function_context: &CodeGenFunctionContext<'ctx>, codegen: &CodeGenLLVM<'ctx>) -> BasicValueEnum<'ctx> {
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

    fn emit_instructions_with_casts<'ast>(&self, ast: &'ast Spanned<Ast<'ast>>, function_context: &CodeGenFunctionContext<'ctx>, codegen: &CodeGenLLVM<'ctx>) -> BasicValueEnum<'ctx> {
        let value = self.emit_instructions(ast, function_context, codegen).expect("ast should create a value");
        self.emit_implicit_cast_if_necessary(ast.0.as_handle(), value, function_context, codegen)
    }

    fn emit_instructions<'ast>(&self, ast: &'ast Spanned<Ast<'ast>>, function_context: &CodeGenFunctionContext<'ctx>, codegen: &CodeGenLLVM<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match &ast.0 {
            Ast::Identifier(Identifier(name)) => {
                let handle = self.semantic_analyser.identifier_to_declaration(ast.0.as_handle());
                let variable = function_context.variables.get(&handle).expect("variable should exist");
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
                                let floor_result = function_context
                                    .builder
                                    .build_call(floor_function, &[div_result.into()], "floor")
                                    .try_as_basic_value()
                                    .expect_left("value should exist");
                                Some(floor_result)
                            } else {
                                let lhs_value = lhs_value.into_int_value();
                                let rhs_value = rhs_value.into_int_value();
                                let sign_lhs = function_context.builder.build_and(lhs_value, codegen.int_type.const_int(1 << 63, false), "sign_lhs");
                                let sign_rhs = function_context.builder.build_and(rhs_value, codegen.int_type.const_int(1 << 63, false), "sign_rhs");
                                let division = function_context.builder.build_int_signed_div(lhs_value, rhs_value, "div");
                                let modulo = function_context.builder.build_int_signed_rem(lhs_value, rhs_value, "mod");
                                let comparison_sign = function_context.builder.build_int_compare(IntPredicate::EQ, sign_lhs, sign_rhs, "sign_cmp");
                                let comparison_mod = function_context.builder.build_int_compare(IntPredicate::EQ, modulo, codegen.int_type.const_int(0, false), "mod_cmp");
                                let both_conditions = function_context.builder.build_or(comparison_sign, comparison_mod, "both_cmp");
                                let different_sign_result = function_context.builder.build_int_sub(division, codegen.int_type.const_int(1, true), "diff_sign_div");
                                let result = function_context.builder.build_select(both_conditions, division, different_sign_result, "whole_div_int");
                                Some(result)
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
            Ast::IfStatement(IfStatement {
                condition,
                then_statements,
                else_statements,
            }) => {
                let condition_value = self.emit_instructions_with_casts(condition, function_context, codegen);

                let true_block = codegen.context.0.append_basic_block(function_context.function_value, "then");
                let after_if_block = codegen.context.0.append_basic_block(function_context.function_value, "after_if");
                let false_block = if else_statements.is_none() {
                    after_if_block
                } else {
                    codegen.context.0.append_basic_block(function_context.function_value, "else")
                };

                function_context.builder.build_conditional_branch(condition_value.into_int_value(), true_block, false_block);

                function_context.builder.position_at_end(true_block);
                self.emit_instructions(then_statements, function_context, codegen);
                if function_context.is_bb_unterminated() {
                    function_context.builder.build_unconditional_branch(after_if_block);
                }

                if let Some(else_statements) = else_statements {
                    function_context.builder.position_at_end(false_block);
                    self.emit_instructions(else_statements, function_context, codegen);
                    if function_context.is_bb_unterminated() {
                        function_context.builder.build_unconditional_branch(after_if_block);
                    }
                }

                if function_context.is_bb_unterminated() {
                    function_context.builder.build_unconditional_branch(after_if_block);
                }

                function_context.builder.position_at_end(after_if_block);

                None
            }
            Ast::LiteralInt(LiteralInt(value)) => Some(codegen.int_type.const_int(*value as u64, false).into()),
            Ast::LiteralBool(LiteralBool(bool)) => Some(codegen.bool_type.const_int(if *bool { 1 } else { 0 }, false).into()),
            Ast::LiteralFloat(LiteralFloat(value)) => Some(codegen.double_type.const_float(*value).into()),
            Ast::Assignment(Assignment(_, expression))
            | Ast::Declaration(Declaration {
                assignment: Assignment(_, expression),
                binding: _,
            }) => {
                let handle = self.semantic_analyser.identifier_to_declaration(ast.0.as_handle());
                let variable = function_context.variables.get(&handle).expect("variable should exist");
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
            Ast::FunctionCall(FunctionCall { callee, args }) => {
                let handle = self.semantic_analyser.identifier_to_declaration(callee.0.as_handle());
                let function_value = &self.function_declaration_handle_to_function_value[&handle];

                Some(
                    function_context
                        .builder
                        .build_call(
                            *function_value,
                            args.iter()
                                .map(|arg| self.emit_instructions(arg, function_context, codegen).expect("argument should have a value").into())
                                .collect::<Vec<_>>() // TODO: use smallvec
                                .as_slice(),
                            "call",
                        )
                        .try_as_basic_value()
                        .left()
                        .unwrap(), /* TODO */
                )
            }
            _ => {
                println!("Unhandled AST: {:?}", ast.0);
                None
            }
        }
    }

    pub fn dump_module(&self) {
        self.module.print_to_stderr();
        // TODO: init & output should be split, and also moved to a different method
        Target::initialize_x86(&InitializationConfig::default());
        let target_triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        let target = Target::from_triple(&target_triple).expect("target should exist");
        let optimization_level = match self.optimization_level {
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Less,
            2 => OptimizationLevel::Default,
            _ => OptimizationLevel::Aggressive,
        };
        let target_machine = target
            .create_target_machine(&target_triple, "generic", "", optimization_level, RelocMode::Default, CodeModel::Default)
            .expect("target machine should exist");
        target_machine.write_to_file(&self.module, FileType::Assembly, Path::new("output.s")).expect("should write to file");
    }
}

impl<'ctx> CodeGenFunctionContext<'ctx> {
    pub fn is_bb_unterminated(&self) -> bool {
        self.builder.get_insert_block().expect("should have insert block").get_terminator().is_none()
    }
}

impl BinaryOperationKind {
    pub fn to_llvm_int_comparison(&self) -> IntPredicate {
        match self {
            BinaryOperationKind::Equal => IntPredicate::EQ,
            BinaryOperationKind::NotEqual => IntPredicate::NE,
            BinaryOperationKind::LessThan => IntPredicate::SLT,
            BinaryOperationKind::LessThanEqual => IntPredicate::SLE,
            BinaryOperationKind::GreaterThan => IntPredicate::SGT,
            BinaryOperationKind::GreaterThanEqual => IntPredicate::SGE,
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
