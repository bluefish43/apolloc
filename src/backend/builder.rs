use std::collections::HashMap;

use crate::backend::llvm::*;
use crate::frontend::ast::CallingConvention;
use crate::frontend::ast::DivType;
use crate::frontend::ast::Expression;
use crate::frontend::ast::Namespace;
use crate::frontend::ast::Statement;
use crate::frontend::ast::StatementD;
use crate::frontend::checker;
use crate::frontend::checker::Checker;
use crate::frontend::tokens;
use crate::frontend::tokens::Token;
use crate::frontend::tokens::Type as TType;
use anyhow::anyhow;
use anyhow::Result;
use llvm_sys::analysis::LLVMVerifierFailureAction;
use llvm_sys::analysis::LLVMVerifyModule;

pub type OptionalFrame<'a> = Option<&'a mut (HashMap<String, (Value, Type, Option<Value>)>, bool, Vec<(CleanupAction, Vec<Value>)>)>;
pub type Frame = (HashMap<String, (Value, Type, Option<Value>)>, bool, Vec<(CleanupAction, Vec<Value>)>);
pub type SimulatedStack = Vec<Frame>;

#[derive(Clone, Copy)]
pub enum CleanupAction {
    SliceCleanup(Type),
}

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
    pub loc: Token,
    malloc: (Function, Type),
    free: (Function, Type),
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExpressionEvaluatorData {
    pub is_variable: bool,
    pub get_variable_ptr: bool,
    pub is_argument: bool,
}

#[macro_export]
macro_rules! untracked_mut {
    ($ref:expr, $t:ty) => {
        unsafe { ($ref as *const $t as *mut $t).as_mut().unwrap() }
    }
}

impl<'b> ApolloBuilder<'b> {
    /// Builds every statement to its equivalent LLVM bitcode
    pub fn evaluate_all(&mut self) -> Result<()> {
        for statement in untracked_mut!(self.checker.statements, Vec<(Token, Statement)>).iter_mut() {
            self.loc = statement.0.clone();
            self.evaluate_statement(&statement.1)?;
        }
        Ok(())
    }

    /// This function must be invoked before `evaluate_all`.
    /// This function sets up the required functions and intrinsics used by the compiler, such as `malloc` and `__cxa_throw`.
    pub fn start(&mut self) {
        // void @_Unwind_RaiseException(i8*)
        // let unwind_raise_except_type = function_type(
        //     void_type_in_context(self.context),
        //     &[pointer_type(int8_type_in_context(self.context), 0)],
        //     false,
        // );

        // let unwinder = self
        //     .module
        //     .add_function("_Unwind_RaiseException", unwind_raise_except_type);

        // self.unwinder = unsafe { Value::from_llvm_ref(unwinder.as_llvm_ref()) };

        // void @__cxa_throw(i8*, i8*, void*)
        let cxa_throw_type = function_type(
            void_type_in_context(self.context),
            &[pointer_type(int8_type_in_context(self.context), 0), pointer_type(int8_type_in_context(self.context), 0), pointer_type(int8_type_in_context(self.context), 0)],
            false
        );

        let unwinder = self
            .module
            .add_function("__cxa_throw", cxa_throw_type);
        
        self.unwinder = *unwinder;

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

        let malloc_type = function_type(
            pointer_type(
                void_type_in_context(self.context),
                0
            ),
            &[int64_type_in_context(self.context)],
            false
        );

        let malloc_function = self.module.add_function("malloc", malloc_type);

        let free_type = function_type(
            void_type_in_context(self.context),
            &[pointer_type(
                void_type_in_context(self.context),
                0
            ),],
            false
        );

        let free_function = self.module.add_function("free", free_type);

        self.malloc = (malloc_function, malloc_type);
        self.free = (free_function, free_type);

    }

    /// Creates a new builder with the specified `Context` and the specified `Checker`.
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
            variable_stack: vec![(HashMap::new(), true, vec![])],
            current_function: Value::null(),
            unwinder: Value::null(),
            catch_labels: vec![],
            personality_function: Value::null(),
            exit_function: Value::null(),
            class_types: HashMap::new(),
            checker,
            struct_types: HashMap::new(),
            loc: Token::default(),
            malloc: (Function(Value::null()), Type::null()),
            free: (Function(Value::null()), Type::null()),
        })
    }

    /// Dumps the inner module to the output stream.
    pub fn dump(&self) {
        self.module.dump()
    }

    /// Sets the module's target triple.
    pub fn set_triple(&self, triple: &str) {
        self.module.set_target_triple(triple)
    }

    /// Helper function to define a function.
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
        let return_type = return_type.as_type_in_builder(self, false);
        let mut llvm_arguments_types = vec![];
        let mut attributes: Vec<(usize, Vec<Attribute>)> = vec![];
        for (idx, (_, argument_type)) in arguments_types.iter().enumerate() {
            let (argument_type, attributes_at_index) = argument_type.arg_type(self);
            llvm_arguments_types.push(argument_type);
            attributes.push((idx, attributes_at_index));
        }
        let old_function = self.current_function;
        let function_type = function_type(return_type, &llvm_arguments_types, is_var_args);
        let llvm_function = self.module.add_function(function_name, function_type);
        for attribute_tuple in attributes {
            for attribute in attribute_tuple.1 {
                (*llvm_function).add_attribute_at_index(attribute_tuple.0 as u32 + 1, attribute);
            }
        }
        let arguments = llvm_function.get_parameters();
        let mut start_vars = HashMap::new();
        if function_body.is_some() {
            self.current_function = *llvm_function;
            let start_block = llvm_function.append_basic_block_in_context("", self.context);

            self.builder.position_at_end(&start_block);
        }
        for ((argument_name, argument_type), argument_value) in
            arguments_types.iter().zip(arguments)
        {
            if function_body.is_some() {
                if argument_type.is_struct() {
                    start_vars.insert(
                        argument_name.to_string(),
                        (
                            argument_value,
                            argument_type.as_type_in_builder(self, false),
                            None,
                        ),
                    );
                } else {
                    let argument_t = argument_type.as_type_in_builder(self, false);
                    let space = self.builder.build_alloca(argument_t, None);
                    self.builder.build_store(argument_value, space);
                    start_vars.insert(
                        argument_name.to_string(),
                        (
                            space,
                            argument_t,
                            None,
                        ),
                    );
                }
            } else {
                start_vars.insert(
                    argument_name.to_string(),
                    (
                        argument_value,
                        argument_type.as_type_in_builder(self, false),
                        None,
                    ),
                );
            }
        }
        llvm_function.set_calling_convention(*calling_convention);
        if let Some(function_body) = function_body {

            self.variable_stack.push((start_vars, true, vec![]));

            for statement in function_body {
                self.loc = statement.0.clone();
                if self.evaluate_statement(&statement.1)? {
                    break;
                }
            }

            self.variable_stack.pop();
            self.current_function = old_function;
        }
        //llvm_function.dump();
        llvm_function.verify()?;
        Ok(())
    }

    /// Matches on an individual Statement and evaluates it.
    pub fn evaluate_statement(&mut self, statement: &Statement) -> Result<bool> {
        let mut returned = false;
        //eprintln!("{:?}", statement);
        match statement {

            Statement::StructMethodCall(struct_value, fn_signature, struct_name, method_name, arguments, all_time_record_id) => {
                let mut args = vec![];
                args.push(*struct_value.clone());
                args.append(&mut arguments.clone());
                self.call(fn_signature, &args, &format!("{struct_name}::{method_name}"), *all_time_record_id);
            }
            Statement::StructMethodDefinition(struct_name, method_name, signature, function_body, this_type) => {
                let mut new_arguments = vec![("this".to_string(), this_type.clone())];
                new_arguments.append(&mut signature.1.clone());
                self.define_function(&format!("{struct_name}::{method_name}"), &signature.0, &new_arguments, Some(function_body), signature.2, signature.3, &CallingConvention::C)?;
            }
            // this handles a struct field assignment
            Statement::StructFieldAssignment(var, _field, expr, index, struct_type) => {
                // value to store
                let value = self.evaluate_expression(expr, ExpressionEvaluatorData::default());
                // pointer to struct
                let (variable_value, _variable_type, _expression_value) = self.find_variable_valueref_unchecked(var);
                // pointer to age
                let field_ptr = self.builder.build_struct_get_element_ptr(struct_type.as_type_in_builder(self, false), variable_value, *index as u32, None);
                // store value in pointer to age
                self.builder.build_store(value, field_ptr);
            }
            Statement::StructFieldIndirectAssignment(var, _field, expr, index, struct_type) => {
                let s_t = struct_type.clone().unwrap_ptr();
                // value to store
                let value = self.evaluate_expression(expr, ExpressionEvaluatorData { get_variable_ptr: false, is_argument: true, is_variable: false });
                // pointer to pointer to struct
                let (variable_value, _variable_type, _expression_value) = self.find_variable_valueref_unchecked(var);
                // // pointer to struct
                let struct_ptr = self.builder.build_load(struct_type.as_type_in_builder(self, false), variable_value, None);
                // pointer to age
                let field_ptr = self.builder.build_struct_get_element_ptr(s_t.as_type_in_builder(self, false), struct_ptr, *index as u32, None);
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
                    .as_type_in_builder(self, false);
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
                let scope = self.variable_stack.last().unwrap().clone();
                self.cleanup_action(&scope);
                self.builder.build_return_void();
            }
            Statement::Block(b) => {
                self.variable_stack.push((HashMap::new(), false, vec![]));
                for statement in b {
                    let s = self.evaluate_statement(&statement.1)?;
                    if s {
                        returned = true;
                        break;
                    }
                }
                self.variable_stack.pop();
            }
            Statement::NamespaceDeclaration(namespace_name, namespace) => {
                self.declare_namespace(namespace_name, namespace, "")?;
            }
            Statement::Return(expression) => {
                returned = true;
                self.make_return(expression);
            }
            Statement::VariableDeclaration(name, optional_specified_type, expression) => {
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
            Statement::DoWhile(do_block, while_cond) => {
                self.do_while_loop(&do_block.1,while_cond)?;
            }
            Statement::Call(function_signature, function_name, args, alltime_record_id) => {
                self.call(function_signature, args, function_name, *alltime_record_id);
            }
            Statement::DefineStruct(struct_name, struct_fields) => {
                let struct_type = self.context.create_named_struct(struct_name, &struct_fields.iter().map(|f| f.1.as_type_in_builder(self, false)).collect::<Vec<_>>(), false);
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

    /// Helper function to evaluate all of the cleanup actions set in the frame.
    fn cleanup_action(&mut self, frame: &Frame) {
        for (action, params) in &frame.2 {
            match action {
                CleanupAction::SliceCleanup(slice_t) => {
                    let malloced_slice = params[0];
                    self.builder.build_call(self.free.1, *self.free.0, &[malloced_slice], None);
                }
            }
        }
    }

    /// Helper function to throw an exception.
    fn throw(&mut self, expression: &Expression) {
        let exception = self.evaluate_expression(expression, ExpressionEvaluatorData::default());

        let string_type = pointer_type(int8_type_in_context(self.context), 0);

        let strlen_type = function_type(
            int64_type_in_context(self.context),
            &[string_type],
            false
        );

        let strlen = self
            .module
            .add_function("strlen", strlen_type);

        let exception_string_length = self.builder.build_call(strlen_type, *strlen, &[exception], None);
        let const_one = const_integer(int64_type_in_context(self.context), 1, false);
        let string_malloc_length = self.builder.build_add(exception_string_length, const_one, None);
        let exception_str = self.builder.build_call(self.malloc.1, *self.malloc.0, &[string_malloc_length], None);
        
        let strncpy_type = function_type(
            string_type, 
            &[string_type, string_type],
            false
        );

        let strncpy = self
            .module
            .add_function("strncpy", strncpy_type);

        let copied_str = self.builder.build_call(strncpy_type, *strncpy, &[exception_str, exception], None);

        self.builder.build_call(
            function_type(
                void_type_in_context(self.context),
                &[pointer_type(int8_type_in_context(self.context), 0), pointer_type(int8_type_in_context(self.context), 0), pointer_type(int8_type_in_context(self.context), 0)],
                false
            ),
            self.unwinder,
            &[copied_str, const_null(string_type), *self.free.0],
            None,
        );
        self.builder.build_unreachable();
    }

    /// Helper function to do a try-catch block.
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
        self.variable_stack.push((HashMap::new(), false, vec![]));
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

    /// Gets the last catch label set up.
    fn get_last_catch(&mut self) -> BasicBlock {
        self.catch_labels.last().copied().unwrap()
    }

    /// Helper function to declare a namespace.
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
            //println!("{accumulated_name}::{function_name}");
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

    /// Builds a return to the specified expression.
    fn make_return(&mut self, expression: &Expression) {
        let scope = self.variable_stack.last().unwrap().clone();
        self.cleanup_action(&scope);
        let expression_value = self.evaluate_expression(expression, ExpressionEvaluatorData::default());
        self.builder.build_return(expression_value);
    }

    /// Helper function to build an if-else conditional statement.
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

    /// Helper function to build a call to a function.
    fn call(
        &mut self,
        function_signature: &(tokens::Type, Vec<(String, tokens::Type)>, bool, bool),
        args: &Vec<Expression>,
        function_name: &str,
        alltime_record_id: usize,
    ) -> Value {
        let mut arguments = Vec::new();

        let return_type = function_signature.0.as_type_in_builder(self, false);
        let mut parameter_types = Vec::new();

        for arg in args {
            arguments.push(self.evaluate_expression(arg, ExpressionEvaluatorData { get_variable_ptr: true, is_variable: false, is_argument: true }));
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

    /// Helper function to do a while-loop.
    fn while_loop(
        &mut self,
        while_condition: &Expression,
        while_block: &Statement,
    ) -> Result<(), anyhow::Error> {
        let loop_block_condition = self
            .current_function
            .append_basic_block_in_context("", self.context);
        self.builder.build_br(loop_block_condition);
        self.builder.position_at_end(&loop_block_condition);

        let while_boolean = self.evaluate_expression(while_condition, ExpressionEvaluatorData::default());

        let after_condition_block = BasicBlock::create_in_context(self.context, "");
        let loop_block_end = BasicBlock::create_in_context(self.context, "");

        self.builder
            .build_conditional_br(while_boolean, after_condition_block, loop_block_end);

        self.current_function
            .append_existing_basic_block(after_condition_block);

        self.builder.position_at_end(&after_condition_block);

        self.evaluate_statement(while_block)?;

        self.builder.build_br(loop_block_condition);

        self.current_function
            .append_existing_basic_block(loop_block_end);

        self.builder.position_at_end(&loop_block_end);

        Ok(())
    }

    /// Helper function to do a do-while-loop.
    fn do_while_loop(
        &mut self,
        do_block: &Statement, // Changed the name to represent the do-while loop structure
        while_condition: &Expression,
    ) -> Result<(), anyhow::Error> {
        let loop_block_condition = self
            .current_function
            .append_basic_block_in_context("", self.context);
        let after_condition_block = BasicBlock::create_in_context(self.context, "");
        let loop_block_end = BasicBlock::create_in_context(self.context, "");
    
        // Always execute the loop body at least once
        self.builder.build_br(loop_block_condition);
    
        // Start of the loop body
        self.current_function.append_existing_basic_block(loop_block_condition);
        self.evaluate_statement(do_block)?;
    
        // Evaluate the loop condition
        let while_boolean = self.evaluate_expression(while_condition, ExpressionEvaluatorData::default());
        self.builder
            .build_conditional_br(while_boolean, loop_block_condition, after_condition_block);
    
        // Continue after the loop when the condition is false
        self.current_function.append_existing_basic_block(after_condition_block);
    
        Ok(())
    }    

    /// Helper function to declare a variable.
    fn declare_variable(&mut self, expression: &Expression, name: &str) {
        let expression_value = self.evaluate_expression(expression, ExpressionEvaluatorData { is_variable: true, get_variable_ptr: false, is_argument: true });
        let expression_type = expression_value.type_of();
        let variable_ptr = self.builder.build_alloca(expression_type, None);
        self.builder.build_store(expression_value, variable_ptr);
        self.set_variable_in_local_scope(
            name,
            (variable_ptr, expression_type, Some(expression_value)),
        );
    }

    /// Sets up a variable in the local scope.
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

    /// Searches for a variable. If it is found, it returns a Some with a mutable reference to the stack frame it is stored ta.
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

    /// Helper function to return the tuple that represents the variable in its scope if found.
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

    /// Unchecked variant.
    fn find_variable_valueref_unchecked(&self, name: &str) -> (Value, Type, Option<Value>) {
        self.find_variable_valueref(name).unwrap_or_else(|| {
            panic!("internal error: typechecking failed: attempt to search for an undefined variable {name} in Builder::find_variable_valueref_unchecked")
        })
    }

    /// Gets a named function in the module as a Value
    fn find_function(&self, name: &str) -> Option<Value> {
        self.module.get_named_function(name).map(|v| *v)
    }

    /// Gets a named function in the module as a Function
    fn find_function_fn(&self, name: &str) -> Option<Function> {
        self.module.get_named_function(name).map(|v| v)
    }

    /// Unchecked variant.
    fn find_function_unchecked(&self, name: &str) -> Value {
        self.find_function(name).unwrap_or_else(|| {
            panic!("internal error: typechecking failed: attempt to search for an undefined function: {name}")
        })
    }

    /// Unchecked variant.
    fn find_function_fn_unchecked(&self, name: &str) -> Function {
        self.find_function_fn(name).unwrap_or_else(|| {
            panic!("internal error: typechecking failed: attempt to search for an undefined function: {name}")
        })
    }

    /// This evaluates expressions and returns them as an LLVMValue.
    pub fn evaluate_expression(&mut self, expression: &Expression, data: ExpressionEvaluatorData) -> Value {
        match expression {
            Expression::StructMethodCall(struct_value, fn_signature, struct_name, method_name, arguments, all_time_record_id) => {
                let mut args = vec![];
                args.push(*struct_value.clone());
                args.append(&mut arguments.clone());
                self.call(fn_signature, &args, &format!("{struct_name}::{method_name}"), *all_time_record_id)
            }
            Expression::List(l, of) => {
                let mut values = vec![];
                for value in l {
                    values.push(self.evaluate_expression(value, data));
                }
                let slice_t = self.context.struct_type(&[int64_type_in_context(self.context), pointer_type(of.as_type_in_builder(self, false), 0)], false);
                let slice_struct = self.builder.build_alloca(slice_t, None);

                let malloc_size_argument = const_integer(int64_type_in_context(self.context), ((values.first().unwrap().type_of().size_of_in_module(&self.module) as usize) * values.len()) as u64, false);
                let space = self.builder.build_call(self.malloc.1, *self.malloc.0, &[malloc_size_argument], None);

                let ptr = self.builder.build_struct_get_element_ptr(slice_t, slice_struct, 1, None);
                self.builder.build_store(space, ptr);

                let len = self.builder.build_struct_get_element_ptr(slice_t, slice_struct, 0, None);
                self.builder.build_store(const_integer(int64_type_in_context(self.context), values.len() as u64, false), len);

                self.variable_stack.last_mut().unwrap().2.push((CleanupAction::SliceCleanup(slice_t), vec![space]));
                slice_struct
            }
            Expression::NullPointer => {
                const_null(pointer_type(void_type_in_context(self.context), 0))
            }
            Expression::And(lhs, rhs) => {
                let lhs = self.evaluate_expression(lhs, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhs, ExpressionEvaluatorData::default());
                self.builder.build_and(lhs, rhs, None)
            }
            Expression::Or(lhs, rhs) => {
                let lhs = self.evaluate_expression(lhs, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhs, ExpressionEvaluatorData::default());
                self.builder.build_or(lhs, rhs, None)
            }
            Expression::Xor(lhs, rhs) => {
                let lhs = self.evaluate_expression(lhs, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhs, ExpressionEvaluatorData::default());
                self.builder.build_xor(lhs, rhs, None)
            }
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
                let struct_value = self.evaluate_expression(expr, ExpressionEvaluatorData { is_variable: false, get_variable_ptr: true, is_argument: false });
                let field_ptr = self.builder.build_struct_get_element_ptr(struct_type.as_type_in_builder(self, false), struct_value, *field_index as u32, None);
                self.builder.build_load(target_type.as_type_in_builder(self, false), field_ptr, None)
            }
            Expression::IndirectStructAccess(expr, field, field_index, target_type, struct_type) => {
                let struct_value = self.evaluate_expression(expr, ExpressionEvaluatorData { is_variable: false, get_variable_ptr: true, is_argument: false });
                let struct_ptr = self.builder.build_load(pointer_type(struct_type.as_type_in_builder(self, false), 0), struct_value, None);
                let field_ptr = self.builder.build_struct_get_element_ptr(struct_type.as_type_in_builder(self, false), struct_ptr, *field_index as u32, None);
                self.builder.build_load(target_type.as_type_in_builder(self, false), field_ptr, None)
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
                let from_type = from.as_type_in_builder(self, true);
                let destine_type = to.as_type_in_builder(self, true);
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
                    unreachable!()
                }
            }
            Expression::New(name, args) => {
                unimplemented!("constructing classes is not implemented")
            }
            Expression::Deref(expression, ttarget_type) => {
                let target_type = ttarget_type.as_type_in_builder(self, true);
                let expr_ptr = self.evaluate_expression(expression, ExpressionEvaluatorData::default());
                self.builder.build_load(target_type, expr_ptr, None)
            }
            Expression::UncheckedCast(expr, t) => {
                let e = self.evaluate_expression(&expr, ExpressionEvaluatorData { is_variable: false, get_variable_ptr: true, is_argument: false });
                self.builder.build_bitcast(e, t.as_type_in_builder(self, true), None)
            }
            Expression::String(s) => self.builder.build_global_string(s, None),
            Expression::Variable(variable_name) => {
                let (variable_value, variable_type, variable_expr_value) =
                    self.find_variable_valueref_unchecked(variable_name);
                if data.is_argument && variable_type.kind() != TypeKind::Struct {
                    self.builder.build_load(variable_type, variable_value, None)
                } else {
                    variable_value
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
            Expression::Division(lhse, rhse, divtype, optype) => {
                let lhs = self.evaluate_expression(lhse, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhse, ExpressionEvaluatorData::default());
                if let DivType::SignedInt = divtype {
                    let int_type;
                    if optype == &TType::I8 {
                        int_type = int8_type_in_context(self.context)
                    } else if optype == &TType::I16 {
                        int_type = int16_type_in_context(self.context)
                    } else if optype == &TType::I32 {
                        int_type = int32_type_in_context(self.context)
                    } else if optype == &TType::I64 {
                        int_type = int64_type_in_context(self.context)
                    } else {
                        unreachable!()
                    }
                    let const_zero = const_integer(int_type, 0, true);

                    let check_part = BasicBlock::new_in_context(self.context, "");
                    let zero_part = BasicBlock::new_in_context(self.context, "");
                    let non_zero_part = BasicBlock::new_in_context(self.context, "");

                    self.builder.position_at_end(&check_part);

                    let is_zero = self.builder.build_integer_cmp(IntPredicate::Eq, rhs, const_zero, None);

                    self.builder.build_conditional_br(is_zero, zero_part, non_zero_part);

                    self.current_function.append_existing_basic_block(check_part);

                    self.builder.position_at_end(&zero_part);

                    self.throw(&Expression::String("Division by zero".to_string()));

                    self.current_function.append_existing_basic_block(zero_part);

                    self.builder.position_at_end(&non_zero_part);

                    let result = self.builder.build_signed_div(lhs, rhs, None);

                    self.current_function.append_existing_basic_block(non_zero_part);

                    result
                } else if let DivType::UnsignedInt = divtype {
                    let int_type;
                    if optype == &TType::U8 {
                        int_type = int8_type_in_context(self.context)
                    } else if optype == &TType::U16 {
                        int_type = int16_type_in_context(self.context)
                    } else if optype == &TType::U32 {
                        int_type = int32_type_in_context(self.context)
                    } else if optype == &TType::U64 {
                        int_type = int64_type_in_context(self.context)
                    } else {
                        unreachable!()
                    }
                    let const_zero = const_integer(int_type, 0, false);

                    let check_part = BasicBlock::new_in_context(self.context, "");
                    let zero_part = BasicBlock::new_in_context(self.context, "");
                    let non_zero_part = BasicBlock::new_in_context(self.context, "");

                    self.builder.position_at_end(&check_part);

                    let is_zero = self.builder.build_integer_cmp(IntPredicate::Eq, rhs, const_zero, None);

                    self.builder.build_conditional_br(is_zero, zero_part, non_zero_part);

                    self.current_function.append_existing_basic_block(check_part);

                    self.builder.position_at_end(&zero_part);

                    self.throw(&Expression::String("Division by zero".to_string()));

                    self.current_function.append_existing_basic_block(zero_part);

                    self.builder.position_at_end(&non_zero_part);

                    let result = self.builder.build_unsigned_div(lhs, rhs, None);

                    self.current_function.append_existing_basic_block(non_zero_part);

                    result
                } else {

                    let float_type;
                    if optype == &TType::F32 {
                        float_type = float32_type_in_context(self.context)
                    } else if optype == &TType::F64 {
                        float_type = float64_type_in_context(self.context)
                    } else {
                        unreachable!()
                    }
                    let const_zero = const_real(float_type, 0.0);

                    let check_part = BasicBlock::new_in_context(self.context, "");
                    let zero_part = BasicBlock::new_in_context(self.context, "");
                    let non_zero_part = BasicBlock::new_in_context(self.context, "");

                    self.builder.position_at_end(&check_part);

                    let is_zero = self.builder.build_integer_cmp(IntPredicate::Eq, rhs, const_zero, None);

                    self.builder.build_conditional_br(is_zero, zero_part, non_zero_part);

                    self.current_function.append_existing_basic_block(check_part);

                    self.builder.position_at_end(&zero_part);

                    self.throw(&Expression::String("Division by zero".to_string()));

                    self.current_function.append_existing_basic_block(zero_part);

                    self.builder.position_at_end(&non_zero_part);

                    let result = self.builder.build_floating_point_div(lhs, rhs, None);

                    self.current_function.append_existing_basic_block(non_zero_part);

                    result
                }
            }
            Expression::Remainder(lhse, rhse, divtype, optype) => {
                let lhs = self.evaluate_expression(lhse, ExpressionEvaluatorData::default());
                let rhs = self.evaluate_expression(rhse, ExpressionEvaluatorData::default());
                if let DivType::SignedInt = divtype {
                    let int_type;
                    if optype == &TType::I8 {
                        int_type = int8_type_in_context(self.context)
                    } else if optype == &TType::I16 {
                        int_type = int16_type_in_context(self.context)
                    } else if optype == &TType::I32 {
                        int_type = int32_type_in_context(self.context)
                    } else if optype == &TType::I64 {
                        int_type = int64_type_in_context(self.context)
                    } else {
                        unreachable!()
                    }
                    let const_zero = const_integer(int_type, 0, true);

                    let check_part = BasicBlock::new_in_context(self.context, "");
                    let zero_part = BasicBlock::new_in_context(self.context, "");
                    let non_zero_part = BasicBlock::new_in_context(self.context, "");

                    self.builder.position_at_end(&check_part);

                    let is_zero = self.builder.build_integer_cmp(IntPredicate::Eq, rhs, const_zero, None);

                    self.builder.build_conditional_br(is_zero, zero_part, non_zero_part);

                    self.current_function.append_existing_basic_block(check_part);

                    self.builder.position_at_end(&zero_part);

                    self.throw(&Expression::String("Division by zero".to_string()));

                    self.current_function.append_existing_basic_block(zero_part);

                    self.builder.position_at_end(&non_zero_part);

                    let result = self.builder.build_signed_rem(lhs, rhs, None);

                    self.current_function.append_existing_basic_block(non_zero_part);

                    result
                } else if let DivType::UnsignedInt = divtype {
                    let int_type;
                    if optype == &TType::U8 {
                        int_type = int8_type_in_context(self.context)
                    } else if optype == &TType::U16 {
                        int_type = int16_type_in_context(self.context)
                    } else if optype == &TType::U32 {
                        int_type = int32_type_in_context(self.context)
                    } else if optype == &TType::U64 {
                        int_type = int64_type_in_context(self.context)
                    } else {
                        unreachable!()
                    }
                    let const_zero = const_integer(int_type, 0, false);

                    let check_part = BasicBlock::new_in_context(self.context, "");
                    let zero_part = BasicBlock::new_in_context(self.context, "");
                    let non_zero_part = BasicBlock::new_in_context(self.context, "");

                    self.builder.position_at_end(&check_part);

                    let is_zero = self.builder.build_integer_cmp(IntPredicate::Eq, rhs, const_zero, None);

                    self.builder.build_conditional_br(is_zero, zero_part, non_zero_part);

                    self.current_function.append_existing_basic_block(check_part);

                    self.builder.position_at_end(&zero_part);

                    self.throw(&Expression::String("Division by zero".to_string()));

                    self.current_function.append_existing_basic_block(zero_part);

                    self.builder.position_at_end(&non_zero_part);

                    let result = self.builder.build_unsigned_rem(lhs, rhs, None);

                    self.current_function.append_existing_basic_block(non_zero_part);

                    result
                } else {

                    let float_type;
                    if optype == &TType::F32 {
                        float_type = float32_type_in_context(self.context)
                    } else if optype == &TType::F64 {
                        float_type = float64_type_in_context(self.context)
                    } else {
                        unreachable!()
                    }
                    let const_zero = const_real(float_type, 0.0);

                    let check_part = BasicBlock::new_in_context(self.context, "");
                    let zero_part = BasicBlock::new_in_context(self.context, "");
                    let non_zero_part = BasicBlock::new_in_context(self.context, "");

                    self.builder.position_at_end(&check_part);

                    let is_zero = self.builder.build_integer_cmp(IntPredicate::Eq, rhs, const_zero, None);

                    self.builder.build_conditional_br(is_zero, zero_part, non_zero_part);

                    self.current_function.append_existing_basic_block(check_part);

                    self.builder.position_at_end(&zero_part);

                    self.throw(&Expression::String("Division by zero".to_string()));

                    self.current_function.append_existing_basic_block(zero_part);

                    self.builder.position_at_end(&non_zero_part);

                    let result = self.builder.build_floating_point_rem(lhs, rhs, None);

                    self.current_function.append_existing_basic_block(non_zero_part);

                    result
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
    pub fn get_bitcode(&mut self, optimization_level: usize) -> Result<Vec<u8>> {
        assert!(&[0,1,2,3].contains(&optimization_level), "optimization level must be a number from 0 to 3");
        unsafe {
            use llvm_sys::bit_writer::*;
            use llvm_sys::core::*;
            use std::ffi::CStr;

            //self.set_up_libc()?;
            //self.dump();
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
