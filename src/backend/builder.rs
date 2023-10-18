use std::collections::HashMap;

use crate::backend::llvm::*;
use crate::frontend;
use crate::frontend::ast::CallingConvention;
use crate::frontend::ast::DivType;
use crate::frontend::ast::Expression;
use crate::frontend::ast::Namespace;
use crate::frontend::ast::Statement;
use crate::frontend::ast::StatementD;
use crate::frontend::checker;
use crate::frontend::checker::BuiltType;
use crate::frontend::checker::Checker;
use crate::frontend::tokens;
use crate::frontend::tokens::{Token, Type as TokenSTypes};
use anyhow::anyhow;
use anyhow::Result;
use llvm_sys::analysis::LLVMVerifierFailureAction;
use llvm_sys::analysis::LLVMVerifyModule;

pub type OptionalFrame<'a> = Option<&'a mut (HashMap<String, (Value, Type, Option<Value>)>, bool)>;
pub type SimulatedStack = Vec<(HashMap<String, (Value, Type, Option<Value>)>, bool)>;

pub struct ApolloBuilder<'b> {
    pub(crate) context: &'b Context,
    module: Module<'b>,
    builder: Builder<'b>,
    variable_stack: SimulatedStack,
    current_function: Value,
    unwinder: Value,
    catch_labels: Vec<BasicBlock>,
    personality_function: Value,
    exit_function: Value,
    class_types: HashMap<String, Type>,
    checker: &'b mut Checker<'b>,
    pub(crate) struct_types: HashMap<String, Type>,
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExpressionEvaluatorData {
    pub is_variable: bool,
    pub get_variable_ptr: bool,
}

#[macro_export]
macro_rules! untracked_mut {
    ($ref:expr, $t:ty) => {
        unsafe { ($ref as *const $t as *mut $t).as_mut().unwrap() }
    }
}

impl<'b> ApolloBuilder<'b> {
    pub fn evaluate_all(&mut self) -> Result<()> {
        for statement in untracked_mut!(self.checker.statements, Vec<(Token, Statement)>).iter_mut() {
            self.evaluate_statement(&statement.1)?;
        }
        Ok(())
    }

    pub fn start(&mut self) {
        // void @_Unwind_RaiseException(i8*)
        let unwind_raise_except_type = function_type(
            void_type_in_context(self.context),
            &[pointer_type(int8_type_in_context(self.context), 0)],
            false,
        );

        let unwinder = self
            .module
            .add_function("_Unwind_RaiseException", unwind_raise_except_type);

        self.unwinder = unsafe { Value::from_llvm_ref(unwinder.as_llvm_ref()) };

        let gxx_personality_v0_type = function_type(int32_type_in_context(self.context), &[], true);

        let gxx_personality_v0 = self
            .module
            .add_function("__gxx_personality_v0", gxx_personality_v0_type);

        self.personality_function = *gxx_personality_v0;

        let exit_type = function_type(
            void_type_in_context(self.context),
            &[int32_type_in_context(self.context)],
            false,
        );

        let exit_fn = self.module.add_function("exit", exit_type);

        self.exit_function = *exit_fn;
    }

    pub fn end(&mut self) -> Result<()> {


        Ok(())
    }

    pub fn new_with(
        context: &'b Context,
        checker: &'b mut Checker<'b>,
        module_name: &str,
    ) -> Result<Self> {
        let module = Module::create_in_context(module_name, context);
        let builder = Builder::new_in_context(context);
        Ok(Self {
            context,
            module,
            builder,
            variable_stack: vec![(HashMap::new(), true)],
            current_function: Value::null(),
            unwinder: Value::null(),
            catch_labels: vec![],
            personality_function: Value::null(),
            exit_function: Value::null(),
            class_types: HashMap::new(),
            checker,
            struct_types: HashMap::new(),
        })
    }

    pub fn dump(&self) {
        self.module.dump()
    }

    pub fn set_triple(&self, triple: &str) {
        self.module.set_target_triple(triple)
    }

    fn define_function(
        &mut self,
        function_name: &str,
        return_type: &tokens::Type,
        arguments_types: &[(String, tokens::Type)],
        function_body: Option<&[StatementD]>,
        may_throw: bool,
        is_var_args: bool,
        calling_convention: &CallingConvention,
    ) -> Result<()> {
        let return_type = return_type.as_type_in_builder(self);
        let mut llvm_arguments_types = vec![];
        for (_, argument_type) in arguments_types {
            llvm_arguments_types.push(argument_type.as_type_in_builder(self));
        }
        let function_type = function_type(return_type, &llvm_arguments_types, is_var_args);
        let llvm_function = self.module.add_function(function_name, function_type);
        let arguments = llvm_function.get_parameters();
        let mut start_vars = HashMap::new();
        for ((argument_name, argument_type), argument_value) in
            arguments_types.iter().zip(arguments)
        {
            start_vars.insert(
                argument_name.to_string(),
                (
                    argument_value,
                    argument_type.as_type_in_builder(self),
                    None,
                ),
            );
        }
        llvm_function.verify()?;
        llvm_function.set_calling_convention(*calling_convention);
        if let Some(function_body) = function_body {
            let old_function = self.current_function;
            self.current_function = *llvm_function;
            let start_block = llvm_function.append_basic_block_in_context("", self.context);
            self.variable_stack.push((start_vars, true));

            self.builder.position_at_end(&start_block);

            for statement in function_body {
                if self.evaluate_statement(&statement.1)? {
                    break;
                }
            }

            self.variable_stack.pop();
            self.current_function = old_function;
        }
        Ok(())
    }

    pub fn evaluate_statement(&mut self, statement: &Statement) -> Result<bool> {
        let mut returned = false;
        eprintln!("{:?}", statement);
        match statement {
            // this handles a struct field assignment
            Statement::StructFieldAssignment(var, _field, expr, index, struct_type) => {
                // value to store
                let value = self.evaluate_expression(expr, ExpressionEvaluatorData::default());
                // pointer to struct
                let (variable_value, _variable_type, _expression_value) = self.find_variable_valueref_unchecked(var);
                // pointer to age
                let field_ptr = self.builder.build_struct_get_element_ptr(struct_type.as_type_in_builder(self), variable_value, *index as u32, None);
                // store value in pointer to age
                self.builder.build_store(value, field_ptr);
            }
            Statement::ClassDeclaration(name, class) => {
                let native_type = self
                    .checker
                    .make_token_type(
                        &checker::Type::Class(name.to_string(), class.clone()),
                        &Token::default(),
                    )
                    .unwrap()
                    .as_type_in_builder(self);
                self.class_types.insert(name.to_string(), native_type);

                for (function_name, ((return_type, arguments, throws, is_var_args), block)) in &class.functions {
                    self.define_function(
                        &format!("{name}.{function_name}"),
                        return_type,
                        arguments,
                        Some(block),
                        *throws,
                        *is_var_args,
                        &CallingConvention::C,
                    )?;
                }

                for (function_name, (return_type, arguments, throws, is_var_args), block) in
                    &class.virtual_functions
                {
                    self.define_function(
                        &format!("virtual {name}.{function_name}"),
                        return_type,
                        arguments,
                        Some(block),
                        *throws,
                        *is_var_args,
                        &CallingConvention::C,
                    )?;
                }
            }
            Statement::ReturnVoid => {
                self.builder.build_return_void();
            }
            Statement::Block(b) => {
                self.variable_stack.push((HashMap::new(), false));
                let block = BasicBlock::create_in_context(self.context, "");
                self.builder.build_br(block);
                self.builder.position_at_end(&block);
                self.current_function.append_existing_basic_block(block);
                for statement in b {
                    let s = self.evaluate_statement(&statement.1)?;
                    if s {
                        returned = true;
                        break;
                    }
                }
                let block = BasicBlock::create_in_context(self.context, "");
                self.builder.build_br(block);
                self.current_function.append_existing_basic_block(block);
                self.builder.position_at_end(&block);
                self.variable_stack.pop();
            }
            Statement::NamespaceDeclaration(namespace_name, namespace) => {
                self.declare_namespace(namespace_name, namespace, "")?;
            }
            Statement::Return(expression) => {
                returned = true;
                self.make_return(expression);
            }
            Statement::VariableDeclaration(name, expression) => {
                self.declare_variable(expression, name);
            }
            Statement::FunctionDeclaration(
                function_name,
                (return_type, arguments_types, throws, is_var_args),
                calling_convention,
            ) => {
                self.define_function(function_name, return_type, arguments_types, None, *throws, *is_var_args, calling_convention)?;
            }
            Statement::FunctionDefinition(
                function_name,
                (return_type, arguments_types, throws, is_var_args),
                function_body,
                calling_convention,
            ) => {
                self.define_function(
                    function_name,
                    return_type,
                    arguments_types,
                    Some(function_body),
                    *throws,
                    *is_var_args,
                    calling_convention,
                )?;
            }
            Statement::IfElse(if_condition, if_block, else_block) => {
                self.if_else(if_condition, else_block, if_block)?;
            }
            Statement::While(while_condition, while_block) => {
                self.while_loop(while_condition, &while_block.1)?;
            }
            Statement::Call(function_signature, function_name, args, alltime_record_id) => {
                self.call(function_signature, args, function_name, *alltime_record_id);
            }
            Statement::DefineStruct(struct_name, struct_fields) => {
                let struct_type = self.context.create_named_struct(struct_name, &struct_fields.iter().map(|f| f.1.as_type_in_builder(self)).collect::<Vec<_>>(), false);
                self.struct_types.insert(struct_name.to_string(), struct_type);
            }
            Statement::DropGlue(_) => unreachable!(
                "DropGlue statement is a compiler intrinsic and cannot be manually built"
            ),
            Statement::Assignment(variable_name, expression) => {
                let variable = self.find_variable_valueref_unchecked(variable_name);
                let expression_value = self.evaluate_expression(expression, ExpressionEvaluatorData::default());
                let expression_type = expression_value.type_of();
                self.builder.build_store(expression_value, variable.0);
                self.set_variable_in_local_scope(
                    variable_name,
                    (variable.0, expression_type, Some(expression_value)),
                );
            }
            Statement::TryCatch(try_block, exception_name, catch_block) => {
                self.try_catch(try_block, exception_name, catch_block)?;
            }
            Statement::Throw(expression) => {
                returned = true;
                self.throw(expression);
            }
            Statement::SysDefReturn(expr) => {
                let value = self.evaluate_expression(expr, ExpressionEvaluatorData::default());
                self.builder.build_call(
                    function_type(
                        void_type_in_context(self.context),
                        &[int32_type_in_context(self.context)],
                        false,
                    ),
                    self.exit_function,
                    &[value],
                    None,
                );
                self.builder.build_unreachable();
                returned = true;
            }
        }
        Ok(returned)
    }

    fn throw(&mut self, expression: &Expression) {
        let exception = self.evaluate_expression(expression, ExpressionEvaluatorData::default());
        self.builder.build_call(
            function_type(
                void_type_in_context(self.context),
                &[pointer_type(int8_type_in_context(self.context), 0)],
                false,
            ),
            self.unwinder,
            &[exception],
            None,
        );
        self.builder.build_unreachable();
    }

    fn try_catch(
        &mut self,
        try_block: &(tokens::Token, Statement),
        exception_name: &str,
        catch_block: &(tokens::Token, Statement),
    ) -> Result<(), anyhow::Error> {
        let catch_basic_block = BasicBlock::create_in_context(self.context, "");
        let end_basic_block = BasicBlock::create_in_context(self.context, "");
        let try_basic_block = BasicBlock::create_in_context(self.context, "");
        self.builder.build_br(try_basic_block);
        self.current_function
            .append_existing_basic_block(try_basic_block);
        self.catch_labels.push(catch_basic_block);
        self.builder.position_at_end(&try_basic_block);
        let returned0 = self.evaluate_statement(&try_block.1)?;
        if !returned0 {
            self.builder.build_br(end_basic_block);
        }
        self.current_function
            .append_existing_basic_block(catch_basic_block);
        self.builder.position_at_end(&catch_basic_block);
        let landing_pad = self.builder.build_landing_pad(
            self.context.struct_type(
                &[pointer_type(int8_type_in_context(self.context), 0)],
                false,
            ),
            self.personality_function,
            0,
            None,
        );
        landing_pad.set_cleanup(true);
        let exception_message = self.builder.build_extract_value(landing_pad, 0, None);
        self.variable_stack.push((HashMap::new(), false));
        let exception_variable_ptr = self
            .builder
            .build_alloca(pointer_type(int8_type_in_context(self.context), 0), None);
        self.builder
            .build_store(exception_message, exception_variable_ptr);
        self.set_variable_in_local_scope(
            exception_name,
            (
                exception_variable_ptr,
                pointer_type(int8_type_in_context(self.context), 0),
                Some(exception_message),
            ),
        );
        let returned1 = self.evaluate_statement(&catch_block.1)?;
        self.variable_stack.pop();
        if !returned1 {
            self.builder.build_br(end_basic_block);
        }
        self.current_function
            .append_existing_basic_block(end_basic_block);
        self.builder.position_at_end(&end_basic_block);
        self.catch_labels.pop();
        Ok(())
    }

    fn get_last_catch(&mut self) -> BasicBlock {
        self.catch_labels.last().copied().unwrap()
    }

    fn declare_namespace(
        &mut self,
        namespace_name: &str,
        namespace: &Namespace,
        accumulated: &str,
    ) -> Result<()> {
        let accumulated_name = format!("{accumulated}{namespace_name}");
        for (function_name, ((return_type, arguments_types, may_throw, is_var_args), function_body)) in
            &namespace.static_functions
        {
            println!("{accumulated_name}::{function_name}");
            self.define_function(
                &format!("{accumulated_name}::{function_name}"),
                return_type,
                arguments_types,
                Some(function_body),
                *may_throw,
                *is_var_args,
                &CallingConvention::C,
            )?;
        }
        for (variable, variable_value) in &namespace.static_variables {
            self.declare_variable(variable_value, variable);
        }
        for (namespace_name, namespace) in &namespace.static_namespaces {
            self.declare_namespace(namespace_name, namespace, &format!("{accumulated_name}::"))?;
        }
        Ok(())
    }

    fn make_return(&mut self, expression: &Expression) {
        let expression_value = self.evaluate_expression(expression, ExpressionEvaluatorData::default());
        self.builder.build_return(expression_value);
    }

    fn if_else(
        &mut self,
        if_condition: &Expression,
        else_block: &Option<Box<StatementD>>,
        if_block: &StatementD,
    ) -> Result<(), anyhow::Error> {
        let condition = self.evaluate_expression(if_condition, ExpressionEvaluatorData::default());
        if let Some(else_block) = else_block {
            let true_block = BasicBlock::create_in_context(self.context, "");
            let false_block = BasicBlock::create_in_context(self.context, "");
            let end_block = BasicBlock::create_in_context(self.context, "");
            self.builder
                .build_conditional_br(condition, true_block, false_block);
            self.current_function
                .append_existing_basic_block(true_block);
            self.builder.position_at_end(&true_block);
            self.evaluate_statement(&if_block.1)?;
            self.builder.build_br(end_block);
            self.current_function
                .append_existing_basic_block(false_block);
            self.builder.position_at_end(&false_block);
            self.evaluate_statement(&else_block.1)?;
            self.builder.build_br(end_block);
            self.current_function.append_existing_basic_block(end_block);
            self.builder.position_at_end(&end_block);
        } else {
            let true_block = BasicBlock::create_in_context(self.context, "");
            let end_block = BasicBlock::create_in_context(self.context, "");
            self.builder
                .build_conditional_br(condition, true_block, end_block);
            self.current_function
                .append_existing_basic_block(true_block);
            self.builder.position_at_end(&true_block);
            self.evaluate_statement(&if_block.1)?;
            self.builder.build_br(end_block);
            self.current_function.append_existing_basic_block(end_block);
            self.builder.position_at_end(&end_block);
        }
        Ok(())
    }

    fn call(
        &mut self,
        function_signature: &(tokens::Type, Vec<(String, tokens::Type)>, bool, bool),
        args: &Vec<Expression>,
        function_name: &str,
        alltime_record_id: usize,
    ) -> Value {
        let mut arguments = Vec::new();

        let return_type = function_signature.0.as_type_in_builder(self);
        let mut parameter_types = Vec::new();

        for arg in args {
            arguments.push(self.evaluate_expression(arg, ExpressionEvaluatorData::default()));
        }

        for argument_type in &arguments {
            parameter_types.push(argument_type.type_of());
        }

        let function_type = function_type(return_type, &parameter_types, false);

        let function_value = self.find_function_unchecked(function_name);

        // checks if function throws
        if function_signature.2 {
            let after = BasicBlock::create_in_context(self.context, "");
            let last_catch = self.get_last_catch();

            let value = self.builder.build_invoke(
                function_type,
                function_value,
                &arguments,
                after,
                last_catch,
                None,
            );

            self.current_function.append_existing_basic_block(after);
            self.builder.position_at_end(&after);
            value
        } else {
            self.builder
                .build_call(function_type, function_value, &arguments, None)
        }
    }

    fn while_loop(
        &mut self,
        while_condition: &Expression,
        while_block: &Statement,
    ) -> Result<(), anyhow::Error> {
        let loop_block_condition = self
            .current_function
            .append_basic_block_in_context("", self.context);
        let while_boolean = self.evaluate_expression(while_condition, ExpressionEvaluatorData::default());
        let after_condition_block = BasicBlock::create_in_context(self.context, "");
        let loop_block_end = BasicBlock::create_in_context(self.context, "");
        self.builder
            .build_conditional_br(while_boolean, after_condition_block, loop_block_end);
        self.current_function
            .append_existing_basic_block(after_condition_block);
        self.evaluate_statement(while_block)?;
        self.builder.build_br(loop_block_condition);
        self.current_function
            .append_existing_basic_block(loop_block_end);
        Ok(())
    }

    fn declare_variable(&mut self, expression: &Expression, name: &str) {
        let expression_value = self.evaluate_expression(expression, ExpressionEvaluatorData { is_variable: true, get_variable_ptr: false });
        let expression_type = expression_value.type_of();
        let variable_ptr = self.builder.build_alloca(expression_type, None);
        self.builder.build_store(expression_value, variable_ptr);
        self.set_variable_in_local_scope(
            name,
            (variable_ptr, expression_type, Some(expression_value)),
        );
    }

    fn set_variable_in_local_scope(&mut self, name: &str, value: (Value, Type, Option<Value>)) {
        if let Some(frame) = self.find_variable_valueref_scope(name) {
            frame.0.insert(name.to_string(), value);
        } else {
            self.variable_stack
                .last_mut()
                .unwrap()
                .0
                .insert(name.to_string(), value);
        }
    }

    fn find_variable_valueref_scope(&mut self, name: &str) -> OptionalFrame {
        for frame in self.variable_stack.iter_mut().rev() {
            if frame.0.get(name).is_some() {
                return Some(frame);
            } else {
                continue;
            }
        }
        None
    }

    fn find_variable_valueref(&self, name: &str) -> Option<(Value, Type, Option<Value>)> {
        for frame in self.variable_stack.iter().rev() {
            if let Some(value) = frame.0.get(name) {
                return Some(*value);
            } else {
                continue;
            }
        }
        None
    }

    fn find_variable_valueref_unchecked(&self, name: &str) -> (Value, Type, Option<Value>) {
        self.find_variable_valueref(name).unwrap_or_else(|| {
            panic!("internal error: typechecking failed: attempt to search for an undefined variable {name} in Builder::find_variable_valueref_unchecked")
        })
    }

    fn find_function(&self, name: &str) -> Option<Value> {
        self.module.get_named_function(name).map(|v| *v)
    }

    fn find_function_unchecked(&self, name: &str) -> Value {
        self.find_function(name).unwrap_or_else(|| {
            panic!("internal error: typechecking failed: attempt to search for an undefined function: {name}")
        })
    }

    /// This evaluates expressions and returns them as an LLVMValue.
    pub fn evaluate_expression(&mut self, expression: &Expression, data: ExpressionEvaluatorData) -> Value {
        match expression {
            Expression::Eq(left, right, left_type, right_type) => {
                let left = self.evaluate_expression(left, data);
                let right = self.evaluate_expression(right, data);
                if left_type.is_signed_int() || left_type.is_unsigned_int() {
                    self.builder.build_integer_cmp(IntPredicate::Eq, left, right, None)
                } else if left_type.is_float() {
                    self.builder.build_floating_point_cmp(RealPredicate::Oeq, left, right, None)
                } else {
                    unreachable!()
                }
            }
            Expression::Ne(left, right, left_type, right_type) => {
                let left = self.evaluate_expression(left, data);
                let right = self.evaluate_expression(right, data);
                if left_type.is_signed_int() || left_type.is_unsigned_int() {
                    self.builder.build_integer_cmp(IntPredicate::Ne, left, right, None)
                } else if left_type.is_float() {
                    self.builder.build_floating_point_cmp(RealPredicate::One, left, right, None)
                } else {
                    unreachable!()
                }
            }
            Expression::Lt(left, right, left_type, right_type) => {
                let left = self.evaluate_expression(left, data);
                let right = self.evaluate_expression(right, data);
                if left_type.is_signed_int() {
                    self.builder.build_integer_cmp(IntPredicate::Slt, left, right, None)
                } else if left_type.is_unsigned_int() {
                    self.builder.build_integer_cmp(IntPredicate::Ult, left, right, None)
                } else if left_type.is_float() {
                    self.builder.build_floating_point_cmp(RealPredicate::Olt, left, right, None)
                } else {
                    unreachable!()
                }
            }
            Expression::Le(left, right, left_type, right_type) => {
                let left = self.evaluate_expression(left, data);
                let right = self.evaluate_expression(right, data);
                if left_type.is_signed_int() {
                    self.builder.build_integer_cmp(IntPredicate::Sle, left, right, None)
                } else if left_type.is_unsigned_int() {
                    self.builder.build_integer_cmp(IntPredicate::Ule, left, right, None)
                } else if left_type.is_float() {
                    self.builder.build_floating_point_cmp(RealPredicate::Ole, left, right, None)
                } else {
                    unreachable!()
                }
            }
            Expression::Gt(left, right, left_type, right_type) => {
                let left = self.evaluate_expression(left, data);
                let right = self.evaluate_expression(right, data);
                if left_type.is_signed_int() {
                    self.builder.build_integer_cmp(IntPredicate::Sgt, left, right, None)
                } else if left_type.is_unsigned_int() {
                    self.builder.build_integer_cmp(IntPredicate::Ugt, left, right, None)
                } else if left_type.is_float() {
                    self.builder.build_floating_point_cmp(RealPredicate::Ogt, left, right, None)
                } else {
                    unreachable!()
                }
            }
            Expression::Ge(left, right, left_type, right_type) => {
                let left = self.evaluate_expression(left, data);
                let right = self.evaluate_expression(right, data);
                if left_type.is_signed_int() {
                    self.builder.build_integer_cmp(IntPredicate::Sge, left, right, None)
                } else if left_type.is_unsigned_int() {
                    self.builder.build_integer_cmp(IntPredicate::Uge, left, right, None)
                } else if left_type.is_float() {
                    self.builder.build_floating_point_cmp(RealPredicate::Oge, left, right, None)
                } else {
                    unreachable!()
                }
            }
            Expression::Reference(b, variable) => {
                self.find_variable_valueref_unchecked(variable).0
            }
            Expression::StructFieldAccess(expr, field, field_index, target_type, struct_type) => {
                let struct_value = self.evaluate_expression(expr, ExpressionEvaluatorData { is_variable: false, get_variable_ptr: true });
                let field_ptr = self.builder.build_struct_get_element_ptr(struct_type.as_type_in_builder(self), struct_value, *field_index as u32, None);
                self.builder.build_load(target_type.as_type_in_builder(self), field_ptr, None)
            }
            Expression::StructInstantiate(name, fields) => {
                let local_ty = self.struct_types.get(name).copied().unwrap();

                let fields = fields.iter().map(|f| self.evaluate_expression(&f.1, ExpressionEvaluatorData::default())).collect::<Vec<_>>();

                let s = const_named_struct(local_ty, &fields);
                if data.is_variable {
                    s
                } else {
                    let ptr = self.builder.build_alloca(s.type_of(), None);
                    self.builder.build_store(s, ptr);
                    ptr
                }
            }
            Expression::TypeCast(expr, from, to) => {
                let value = self.evaluate_expression(expr, data);
                let from_type = from.as_type_in_builder(self);
                let destine_type = to.as_type_in_builder(self);
                if from.is_float() && to.is_float() {
                    self.builder.build_floating_point_cast(value, destine_type, None)
                } else if from.is_float() && to.is_signed_int() {
                    self.builder.build_floating_point_to_signed_integer(value, destine_type, None)
                } else if from.is_float() && to.is_unsigned_int() {
                    self.builder.build_floating_point_to_unsigned_integer(value, destine_type, None)
                } else if from.is_signed_int() && to.is_float() {
                    self.builder.build_signed_integer_to_floating_point(value, destine_type, None)
                } else if from.is_signed_int() && to.is_signed_int() {
                    self.builder.build_integer_cast2(value, destine_type, true, None)
                } else if from.is_signed_int() && to.is_unsigned_int() {
                    self.builder.build_integer_cast2(value, destine_type, false, None) // from here
                } else if from.is_unsigned_int() && to.is_float() {
                    self.builder.build_unsigned_integer_to_floating_point(value, destine_type, None)
                } else if from.is_unsigned_int() && to.is_signed_int() {
                    self.builder.build_integer_cast2(value, destine_type, true, None)
                } else if from.is_unsigned_int() && to.is_unsigned_int() {
                    self.builder.build_integer_cast2(value, destine_type, false, None)
                } else {
                    unimplemented!()
                }
            }
            Expression::New(name, args) => {
                unimplemented!("constructing classes is not implemented")
            }
            Expression::Deref(expression, ttarget_type) => {
                let target_type = ttarget_type.as_type_in_builder(self);
                if let tokens::Type::Struct { name, fields, packed } = ttarget_type {
                    self.evaluate_expression(expression, ExpressionEvaluatorData::default())
                } else {
                    let expr_ptr = self.evaluate_expression(expression, ExpressionEvaluatorData::default());
                    self.builder.build_load(target_type, expr_ptr, None)
                }
            }
            Expression::String(s) => self.builder.build_global_string(s, None),
            Expression::Variable(variable_name) => {
                let (variable_value, variable_type, variable_expr_value) =
                    self.find_variable_valueref_unchecked(variable_name);
                if data.get_variable_ptr {
                    variable_value
                } else {
                    variable_expr_value.unwrap_or(variable_value)
                }
            }
            Expression::NumberI64(n) => {
                let i64_type = int64_type_in_context(self.context);
                const_integer(i64_type, *n as u64, true)
            }
            Expression::NumberI32(n) => {
                let i32_type = int32_type_in_context(self.context);
                const_integer(i32_type, *n as u64, true)
            }
            Expression::NumberI16(n) => {
                let i16_type = int16_type_in_context(self.context);
                const_integer(i16_type, *n as u64, true)
            }
            Expression::NumberI8(n) => {
                let i8_type = int8_type_in_context(self.context);
                const_integer(i8_type, *n as u64, true)
            }
            Expression::NumberU64(n) => {
                let u64_type = int64_type_in_context(self.context);
                const_integer(u64_type, *n, false)
            }
            Expression::NumberU32(n) => {
                let u32_type = int32_type_in_context(self.context);
                const_integer(u32_type, *n as u64, false)
            }
            Expression::NumberU16(n) => {
                let u16_type = int16_type_in_context(self.context);
                const_integer(u16_type, *n as u64, false)
            }
            Expression::NumberU8(n) => {
                let u8_type = int8_type_in_context(self.context);
                const_integer(u8_type, *n as u64, false)
            }
            Expression::Addition(lhs, rhs) => {
                let lhs = self.evaluate_expression(lhs, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhs, ExpressionEvaluatorData::default());
                self.builder.build_add(lhs, rhs, None)
            }
            Expression::Subtraction(lhs, rhs) => {
                let lhs = self.evaluate_expression(lhs, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhs, ExpressionEvaluatorData::default());
                self.builder.build_sub(lhs, rhs, None)
            }
            Expression::Multiplication(lhs, rhs) => {
                let lhs = self.evaluate_expression(lhs, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhs, ExpressionEvaluatorData::default());
                self.builder.build_mul(lhs, rhs, None)
            }
            Expression::Division(lhse, rhse, divtype) => {
                let lhs = self.evaluate_expression(lhse, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhse, ExpressionEvaluatorData::default());
                if let DivType::SignedInt = divtype {
                    self.builder.build_signed_div(lhs, rhs, None)
                } else if let DivType::UnsignedInt = divtype {
                    self.builder.build_unsigned_div(lhs, rhs, None)
                } else {
                    self.builder.build_floating_point_div(lhs, rhs, None)
                }
            }
            Expression::Remainder(lhse, rhse, divtype) => {
                let lhs = self.evaluate_expression(lhse, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhse, ExpressionEvaluatorData::default());
                if let DivType::SignedInt = divtype {
                    self.builder.build_signed_rem(lhs, rhs, None)
                } else if let DivType::UnsignedInt = divtype {
                    self.builder.build_unsigned_rem(lhs, rhs, None)
                } else {
                    self.builder.build_floating_point_rem(lhs, rhs, None)
                }
            }
            Expression::Boolean(b) => {
                let bool_type = int1_type_in_context(self.context);
                const_integer(bool_type, *b as u64, false)
            }
            Expression::Call(function_signature, function_name, args, all_time_record_id) => {
                self.call(function_signature, args, function_name, *all_time_record_id)
            }
        }
    }

    /// This function gets the LLVM-generated bitcode of the module.
    pub fn get_bitcode(&self) -> Result<Vec<u8>> {
        unsafe {
            use llvm_sys::bit_writer::*;
            use llvm_sys::core::*;
            use std::ffi::CStr;

            self.dump();
            let mut out: *mut::libc::c_char = std::ptr::null_mut();
            let result = LLVMVerifyModule(
                self.module.as_llvm_ref(),
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
                &mut out
            );
            if result != 0 {
                let err_msg = CStr::from_ptr(out).to_string_lossy().into_owned();
                return Err(anyhow!("LLVM module verification failed: {}", err_msg))
            }

            let mem_buf = LLVMWriteBitcodeToMemoryBuffer(self.module.as_llvm_ref());

            if mem_buf.is_null() {
                return Err(anyhow!(
                    "Writing the LLVM bitcode to the memory buffer failed"
                ));
            }

            let bitcode_data = LLVMGetBufferStart(mem_buf) as *const u8;
            let bitcode_size = LLVMGetBufferSize(mem_buf);

            let bitcode_slice = std::slice::from_raw_parts(bitcode_data, bitcode_size);
            let bitcode_vec = bitcode_slice.to_vec();

            LLVMDisposeMemoryBuffer(mem_buf);

            Ok(bitcode_vec)
        }
    }
}
