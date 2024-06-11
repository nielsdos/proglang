use crate::analysis::semantic_analysis::ClassMap;
use crate::mid_ir::ir::{MidExpression, MidFunction, MidStatement, MidStatementList, MidTarget, MidVariableReference};
use crate::syntax::ast::BinaryOperationKind;
use crate::types::class_info::ClassInfo;
use crate::types::type_system::{FunctionType, Type};
use crate::util::handle::Handle;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType, VoidType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{types, AddressSpace, IntPredicate, OptimizationLevel};
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
    ty: AnyTypeEnum<'ctx>,
}

struct CodeGenFunctionContext<'ctx> {
    builder: Builder<'ctx>,
    function_value: FunctionValue<'ctx>,
    variables: Vec<VariableInfo<'ctx>>,
}

struct CodeGenInner<'ctx> {
    module: Module<'ctx>,
    function_declaration_handle_to_function_value: HashMap<Handle, FunctionValue<'ctx>>,
    optimization_level: u32,
    type_to_llvm_type: HashMap<Type<'ctx>, AnyTypeEnum<'ctx>>,
    // Primitive types here for faster lookup
    void_type: VoidType<'ctx>,
    int_type: IntType<'ctx>,
    double_type: FloatType<'ctx>,
    bool_type: IntType<'ctx>,
}

pub struct CodeGenLLVM<'ctx> {
    context: &'ctx CodeGenContext,
    optimization_level: u32,
    modules: Vec<CodeGenInner<'ctx>>,
    pass_managers: Vec<PassManager<Module<'ctx>>>,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn new(context: &'ctx CodeGenContext, optimization_level: u32) -> Self {
        Self {
            context,
            optimization_level,
            modules: Vec::new(),
            pass_managers: Vec::new(),
        }
    }

    fn create_pass_manager(&self) -> PassManager<Module<'ctx>> {
        let pm = PassManager::create(());
        if self.optimization_level >= 1 {
            pm.add_type_based_alias_analysis_pass();
            pm.add_scoped_no_alias_aa_pass();
            pm.add_function_attrs_pass();
            //pm.add_ipsccp_pass(); TODO: FIXME
            pm.add_sccp_pass();
            pm.add_global_optimizer_pass();
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
            pm.add_instruction_combining_pass();
        }
        pm
    }

    pub fn add_module(&mut self, module_name: &'ctx str) {
        let module = self.context.0.create_module(module_name);
        let inner = CodeGenInner::new(module, self.optimization_level, &self.context.0);
        self.modules.push(inner);
        self.pass_managers.push(self.create_pass_manager());
    }

    pub fn codegen_types(&mut self, class_map: &'ctx ClassMap<'ctx>) {
        // TODO: hardcoded to first module right now
        for class in class_map.values() {
            self.modules[0].declare_struct(class.name());
            println!("class: {:?}", class);
        }

        for class in class_map.values() {
            self.modules[0].codegen_struct(class);
        }
    }

    pub fn declare_function(&mut self, mid_function: &'ctx MidFunction<'ctx>) {
        // TODO: hardcoded to first module right now
        let function_value = self.modules[0].declare_function(mid_function);
        self.modules[0].function_declaration_handle_to_function_value.insert(mid_function.declaration_handle, function_value);
    }

    pub fn codegen_function(&mut self, mid_function: &'ctx MidFunction<'ctx>) {
        let builder = self.context.0.create_builder();

        // TODO: hardcoded to first module right now
        self.modules[0].codegen_function_prepare_types(mid_function);
        self.modules[0].codegen_function(mid_function, builder, self);
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
    pub fn new(module: Module<'ctx>, optimization_level: u32, context: &'ctx Context) -> Self {
        let mut type_to_llvm_type = HashMap::new();

        // Store basic types
        let int_type = context.i64_type();
        let double_type = context.f64_type();
        let bool_type = context.bool_type();
        type_to_llvm_type.insert(Type::Int, AnyTypeEnum::IntType(int_type));
        type_to_llvm_type.insert(Type::Double, AnyTypeEnum::FloatType(double_type));
        type_to_llvm_type.insert(Type::Bool, AnyTypeEnum::IntType(bool_type));

        Self {
            module,
            function_declaration_handle_to_function_value: Default::default(),
            optimization_level,
            type_to_llvm_type,
            void_type: context.void_type(),
            int_type,
            double_type,
            bool_type,
        }
    }

    fn declare_struct(&mut self, name: &'ctx str) {
        let structure = self.module.get_context().opaque_struct_type(name);
        self.type_to_llvm_type.insert(Type::UserType(name), AnyTypeEnum::StructType(structure));
    }

    fn codegen_struct(&mut self, class: &'ctx ClassInfo<'ctx>) {
        let structure = self.module.get_context().get_struct_type(class.name()).expect("struct should be declared");
        let field_types = class.fields_iter().map(|field| self.get_or_insert_llvm_type(field.ty())).collect::<Vec<_>>();
        structure.set_body(field_types.as_slice(), false);
    }

    fn construct_llvm_function_type(&mut self, function_type: &FunctionType<'ctx>) -> types::FunctionType<'ctx> {
        let return_type = self.get_or_insert_llvm_type(&function_type.return_type);
        let arg_types = function_type
            .arg_types
            .iter()
            .map(|arg| self.get_or_insert_llvm_type(arg).into())
            .collect::<SmallVec<[BasicMetadataTypeEnum<'ctx>; 4]>>();
        return_type.fn_type(arg_types.as_slice(), false)
    }

    fn convert_to_basic_type(&self, ty: &AnyTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
        match ty {
            AnyTypeEnum::FunctionType(ty) => BasicTypeEnum::PointerType(ty.ptr_type(AddressSpace::default())),
            AnyTypeEnum::IntType(ty) => BasicTypeEnum::IntType(*ty),
            AnyTypeEnum::FloatType(ty) => BasicTypeEnum::FloatType(*ty),
            AnyTypeEnum::PointerType(ty) => BasicTypeEnum::PointerType(*ty),
            AnyTypeEnum::StructType(ty) => BasicTypeEnum::StructType(*ty),
            AnyTypeEnum::ArrayType(ty) => BasicTypeEnum::ArrayType(*ty),
            AnyTypeEnum::VectorType(ty) => BasicTypeEnum::VectorType(*ty),
            AnyTypeEnum::VoidType(_) => unreachable!(),
        }
    }

    fn get_or_insert_llvm_type(&mut self, ty: &Type<'ctx>) -> BasicTypeEnum<'ctx> {
        if let Some(ty) = self.type_to_llvm_type.get(ty) {
            self.convert_to_basic_type(ty)
        } else {
            let llvm_ty = match ty {
                Type::Function(ty) => self.construct_llvm_function_type(ty).into(),
                Type::Reference(ty) => AnyTypeEnum::PointerType(self.get_or_insert_llvm_type(ty).ptr_type(AddressSpace::default())),
                _ => unimplemented!(),
            };
            self.type_to_llvm_type.insert(ty.clone(), llvm_ty);
            self.convert_to_basic_type(&llvm_ty)
        }
    }

    fn get_llvm_type_raw(&self, ty: &Type<'ctx>) -> &AnyTypeEnum<'ctx> {
        self.type_to_llvm_type.get(ty).expect("type should exist")
    }

    fn get_llvm_type(&self, ty: &Type<'ctx>) -> BasicTypeEnum<'ctx> {
        self.convert_to_basic_type(self.get_llvm_type_raw(ty))
    }

    pub fn declare_function(&mut self, mid_function: &'ctx MidFunction<'ctx>) -> FunctionValue<'ctx> {
        let arg_types = mid_function
            .function_type
            .arg_types
            .iter()
            .map(|arg| {
                let ty = self.get_or_insert_llvm_type(arg);
                BasicMetadataTypeEnum::from(ty)
            })
            .collect::<SmallVec<[BasicMetadataTypeEnum<'ctx>; 4]>>();

        // TODO: use function type construction helper (and fixup void in that place)
        let function_type = if *mid_function.return_type() == Type::Void {
            self.void_type.fn_type(arg_types.as_slice(), false)
        } else {
            self.get_or_insert_llvm_type(mid_function.return_type()).fn_type(arg_types.as_slice(), false)
        };

        self.module.add_function(mid_function.name, function_type, None)
    }

    // TODO: can this be more efficient than a 2-pass system?
    pub fn codegen_function_prepare_types(&mut self, mid_function: &'ctx MidFunction<'ctx>) {
        for variable_type in mid_function.variables.iter() {
            self.get_or_insert_llvm_type(variable_type);
        }
    }

    pub fn codegen_function(&self, mid_function: &'ctx MidFunction<'ctx>, builder: Builder<'ctx>, codegen: &CodeGenLLVM<'ctx>) {
        let context = &codegen.context.0;
        let function_value = self.function_declaration_handle_to_function_value[&mid_function.declaration_handle];

        if mid_function.always_inline {
            let always_inline = Attribute::get_named_enum_kind_id("alwaysinline");
            let attr = context.create_enum_attribute(always_inline, 0);
            function_value.add_attribute(AttributeLoc::Function, attr);
        }

        let basic_block = context.append_basic_block(function_value, "entry");
        builder.position_at_end(basic_block);

        let mut variables = Vec::new();

        // Create memory locations for the local variables
        for variable_type in &mid_function.variables {
            // Can't have a declaration without an assignment, so setting a default value is not necessary
            let variable_type = self.get_llvm_type_raw(variable_type);
            let variable_memory = builder.build_alloca(self.convert_to_basic_type(variable_type), "var");
            variables.push(VariableInfo {
                ptr: variable_memory,
                ty: *variable_type,
            });
        }

        // Copy arguments to the function's scope
        for (argument_index, &variable_index) in mid_function.arg_idx_to_var_idx.iter().enumerate() {
            let variable_memory = variables[variable_index].ptr;
            let arg_value = function_value.get_nth_param(argument_index as u32).expect("argument should exist");
            builder.build_store(variable_memory, arg_value);
        }

        // Emit instructions
        let body = &mid_function.statements;
        let function_context = CodeGenFunctionContext { builder, function_value, variables };
        self.emit_statement(body, &function_context, codegen);

        if function_context.is_bb_unterminated() {
            if *mid_function.return_type() == Type::Void {
                function_context.builder.build_return(None);
            } else {
                function_context.builder.build_unreachable();
            }
        }

        if !function_value.verify(true) {
            function_value.print_to_stderr();
            panic!("Function '{}' is invalid", mid_function.name);
        }
    }

    fn emit_statement_list(&self, statement_list: &'ctx MidStatementList, function_context: &CodeGenFunctionContext<'ctx>, codegen: &CodeGenLLVM<'ctx>) {
        for statement in &statement_list.list {
            self.emit_statement(statement, function_context, codegen);
        }
    }

    fn emit_statement(&self, statement: &'ctx MidStatement, function_context: &CodeGenFunctionContext<'ctx>, codegen: &CodeGenLLVM<'ctx>) {
        match statement {
            MidStatement::StatementList(statement_list) => {
                self.emit_statement_list(statement_list, function_context, codegen);
            }
            MidStatement::Assignment(assignment) => {
                let value = self.emit_expression(&assignment.value, function_context);
                let target = self.emit_target(&assignment.target, function_context);
                function_context.builder.build_store(target, value);
            }
            MidStatement::Return(return_statement) => match &return_statement.value {
                Some(value) => {
                    function_context.builder.build_return(Some(&self.emit_expression(value, function_context)));
                }
                None => {
                    function_context.builder.build_return(None);
                }
            },
            MidStatement::If(if_statement) => {
                let condition = self.emit_expression(&if_statement.condition, function_context);

                let true_block = codegen.context.0.append_basic_block(function_context.function_value, "then");
                let after_if_block = codegen.context.0.append_basic_block(function_context.function_value, "after_if");
                let false_block = if if_statement.else_statements.is_none() {
                    after_if_block
                } else {
                    codegen.context.0.append_basic_block(function_context.function_value, "else")
                };

                function_context.builder.build_conditional_branch(condition.into_int_value(), true_block, false_block);

                function_context.builder.position_at_end(true_block);
                self.emit_statement_list(&if_statement.then_statements, function_context, codegen);
                if function_context.is_bb_unterminated() {
                    function_context.builder.build_unconditional_branch(after_if_block);
                }

                if let Some(else_statements) = &if_statement.else_statements {
                    function_context.builder.position_at_end(false_block);
                    self.emit_statement_list(else_statements, function_context, codegen);
                    if function_context.is_bb_unterminated() {
                        function_context.builder.build_unconditional_branch(after_if_block);
                    }
                }

                if function_context.is_bb_unterminated() {
                    function_context.builder.build_unconditional_branch(after_if_block);
                }

                function_context.builder.position_at_end(after_if_block);
            }
        }
    }

    fn emit_variable_reference(&self, variable_reference: &'ctx MidVariableReference, function_context: &CodeGenFunctionContext<'ctx>) -> (PointerValue<'ctx>, AnyTypeEnum<'ctx>) {
        let data = &function_context.variables[variable_reference.variable_index];
        (data.ptr, data.ty)
    }

    fn emit_target(&self, target: &'ctx MidTarget, function_context: &CodeGenFunctionContext<'ctx>) -> PointerValue<'ctx> {
        match target {
            MidTarget::Variable(variable_reference) => self.emit_variable_reference(variable_reference, function_context).0,
        }
    }

    fn construct_argument_array(&self, args: &'ctx [MidExpression], function_context: &CodeGenFunctionContext<'ctx>) -> SmallVec<[BasicMetadataValueEnum<'ctx>; 4]> {
        args.iter().map(|arg| self.emit_expression(arg, function_context).into()).collect::<SmallVec<[_; 4]>>()
    }

    fn emit_expression(&self, expression: &'ctx MidExpression, function_context: &CodeGenFunctionContext<'ctx>) -> BasicValueEnum<'ctx> {
        match expression {
            MidExpression::VariableRead(variable_reference) => {
                let (pointer, ty) = self.emit_variable_reference(variable_reference, function_context);
                function_context.builder.build_load(self.convert_to_basic_type(&ty), pointer, "var")
            }
            MidExpression::VariableReference(variable_reference) => self.emit_variable_reference(variable_reference, function_context).0.as_basic_value_enum(),
            MidExpression::BinaryOperation(binary_operation) => {
                let lhs_value = self.emit_expression(&binary_operation.lhs, function_context);
                let rhs_value = self.emit_expression(&binary_operation.rhs, function_context);
                // TODO: overflow handling, check NaN handling, division by zero checking, power of zero checking

                if binary_operation.op.is_comparison_op() {
                    if lhs_value.is_float_value() {
                        function_context
                            .builder
                            .build_float_compare(binary_operation.op.to_llvm_float_comparison(), lhs_value.into_float_value(), rhs_value.into_float_value(), "eq")
                            .into()
                    } else {
                        function_context
                            .builder
                            .build_int_compare(binary_operation.op.to_llvm_int_comparison(), lhs_value.into_int_value(), rhs_value.into_int_value(), "eq")
                            .into()
                    }
                } else {
                    match binary_operation.op {
                        BinaryOperationKind::Addition => {
                            if lhs_value.is_float_value() {
                                function_context.builder.build_float_add(lhs_value.into_float_value(), rhs_value.into_float_value(), "add").into()
                            } else {
                                function_context.builder.build_int_add(lhs_value.into_int_value(), rhs_value.into_int_value(), "add").into()
                            }
                        }
                        BinaryOperationKind::Subtraction => {
                            if lhs_value.is_float_value() {
                                function_context.builder.build_float_sub(lhs_value.into_float_value(), rhs_value.into_float_value(), "sub").into()
                            } else {
                                function_context.builder.build_int_sub(lhs_value.into_int_value(), rhs_value.into_int_value(), "sub").into()
                            }
                        }
                        BinaryOperationKind::Product => {
                            if lhs_value.is_float_value() {
                                function_context.builder.build_float_mul(lhs_value.into_float_value(), rhs_value.into_float_value(), "mul").into()
                            } else {
                                function_context.builder.build_int_mul(lhs_value.into_int_value(), rhs_value.into_int_value(), "mul").into()
                            }
                        }
                        BinaryOperationKind::DoubleDivision => {
                            assert!(lhs_value.is_float_value() && rhs_value.is_float_value());
                            function_context.builder.build_float_div(lhs_value.into_float_value(), rhs_value.into_float_value(), "div").into()
                        }
                        BinaryOperationKind::WholeDivision => {
                            if lhs_value.is_float_value() {
                                let div_result = function_context.builder.build_float_div(lhs_value.into_float_value(), rhs_value.into_float_value(), "div");
                                let floor_intrinsic = Intrinsic::find("llvm.floor").expect("floor intrinsic should exist");
                                let floor_function = floor_intrinsic.get_declaration(&self.module, &[lhs_value.get_type()]).expect("floor function should exist");
                                function_context
                                    .builder
                                    .build_call(floor_function, &[div_result.into()], "floor")
                                    .try_as_basic_value()
                                    .expect_left("value should exist")
                            } else {
                                let lhs_value = lhs_value.into_int_value();
                                let rhs_value = rhs_value.into_int_value();
                                let sign_lhs = function_context.builder.build_and(lhs_value, self.int_type.const_int(1 << 63, false), "sign_lhs");
                                let sign_rhs = function_context.builder.build_and(rhs_value, self.int_type.const_int(1 << 63, false), "sign_rhs");
                                let division = function_context.builder.build_int_signed_div(lhs_value, rhs_value, "div");
                                let modulo = function_context.builder.build_int_signed_rem(lhs_value, rhs_value, "mod");
                                let comparison_sign = function_context.builder.build_int_compare(IntPredicate::EQ, sign_lhs, sign_rhs, "sign_cmp");
                                let comparison_mod = function_context.builder.build_int_compare(IntPredicate::EQ, modulo, self.int_type.const_int(0, false), "mod_cmp");
                                let both_conditions = function_context.builder.build_or(comparison_sign, comparison_mod, "both_cmp");
                                let different_sign_result = function_context.builder.build_int_sub(division, self.int_type.const_int(1, true), "diff_sign_div");
                                function_context.builder.build_select(both_conditions, division, different_sign_result, "whole_div_int")
                            }
                        }
                        BinaryOperationKind::Power => {
                            assert!(lhs_value.is_float_value() && rhs_value.is_float_value());
                            let pow_intrinsic = Intrinsic::find("llvm.pow").expect("pow intrinsic should exist");
                            let pow_function = pow_intrinsic.get_declaration(&self.module, &[lhs_value.get_type()]).expect("pow function should exist");
                            function_context
                                .builder
                                .build_call(pow_function, &[lhs_value.into(), rhs_value.into()], "pow")
                                .try_as_basic_value()
                                .expect_left("value should exist")
                        }
                        _ => unreachable!(),
                    }
                }
            }
            MidExpression::UnaryNegateOperation(expression) => {
                let operand_value = self.emit_expression(expression, function_context);
                if operand_value.is_int_value() {
                    // TODO: overflow?
                    let result = function_context.builder.build_int_neg(operand_value.into_int_value(), "neg");
                    result.into()
                } else {
                    let result = function_context.builder.build_float_neg(operand_value.into_float_value(), "neg");
                    result.into()
                }
            }
            MidExpression::LiteralInt(value) => self.int_type.const_int(*value as u64, false).into(),
            MidExpression::LiteralBool(bool) => self.bool_type.const_int(if *bool { 1 } else { 0 }, false).into(),
            MidExpression::LiteralFloat(value) => self.double_type.const_float(*value).into(),
            MidExpression::BuiltinSiToFp(expression) => {
                let value = self.emit_expression(expression, function_context);
                function_context
                    .builder
                    .build_signed_int_to_float(value.into_int_value(), self.double_type, "conv")
                    .as_basic_value_enum()
            }
            MidExpression::DirectCall(direct_call) => {
                // TODO: fail on void?
                let function_args = self.construct_argument_array(&direct_call.args, function_context);
                let function_value = self.function_declaration_handle_to_function_value[&direct_call.declaration_handle_of_target];

                function_context
                    .builder
                    .build_direct_call(function_value, function_args.as_slice(), "direct_call")
                    .try_as_basic_value()
                    .left()
                    .expect("valid expression")
            }
            MidExpression::IndirectCall(indirect_call) => {
                // TODO: fail on void?
                let function_args = self.construct_argument_array(&indirect_call.args, function_context);
                let llvm_callee_ty = self.get_llvm_type_raw(&Type::Function(indirect_call.ty.clone()));
                let function_value = self.emit_expression(&indirect_call.expression, function_context);

                function_context
                    .builder
                    .build_indirect_call(llvm_callee_ty.into_function_type(), function_value.into_pointer_value(), function_args.as_slice(), "indirect_call")
                    .try_as_basic_value()
                    .left()
                    .expect("valid expression")
            }
            MidExpression::FunctionReference(handle) => {
                let function_value = self.function_declaration_handle_to_function_value[handle];
                BasicValueEnum::PointerValue(function_value.as_global_value().as_pointer_value())
            }
            MidExpression::MemberReference(member_reference) => {
                let lhs = self.emit_expression(&member_reference.of, function_context);
                let of_type = self.get_llvm_type(member_reference.of_type.dereference());
                BasicValueEnum::PointerValue(
                    function_context
                        .builder
                        .build_struct_gep(of_type, lhs.into_pointer_value(), member_reference.index, "gep")
                        .expect("valid expression"),
                )
            }
            MidExpression::PointerLoad(pointer_load) => {
                let of = self.emit_expression(&pointer_load.of, function_context);
                let of_type = self.get_llvm_type(&pointer_load.of_type);
                function_context.builder.build_load(of_type, of.into_pointer_value(), "ptr_load").as_basic_value_enum()
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
