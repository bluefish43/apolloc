use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Display},
};

use crate::{frontend::ast::AccessModifier, untracked_mut};

use super::{
    ast::{Class, DivType, Expression, Statement, StatementD},
    tokens::{Token, Type as ForeignType},
};

#[derive(Debug, Clone)]
pub struct CheckerError(pub Token, pub String, pub Vec<String>);

impl CheckerError {
    pub fn new<T: ToString>(location: Token, message: T) -> Self {
        Self(location, message.to_string(), vec![])
    }

    pub fn add_note<T: ToString>(&mut self, note: T) {
        self.2.push(note.to_string());
    }
}

macro_rules! write_at {
    ($ptr:expr, $t:ty, $data:expr) => {
        unsafe { (($ptr) as *mut $ty).write($data) }
    }
}

impl Display for CheckerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut notes = String::new();
        for note in &self.2 {
            notes.push('\n');
            notes.push_str("Note:  ");
            notes.push_str(note);
        }
        write!(
            f,
            "{}:{}:{}: {}{}",
            self.0.defined_file_name, self.0.line, self.0.col, self.1, if !notes.is_empty() { notes.trim_end() } else { "" }
        )
    }
}

fn format_vector_as_list<T: Display>(vec: &[T]) -> String {
    let formatted_elements: Vec<String> = vec.iter().map(|item| item.to_string()).collect();
    format!("({})", formatted_elements.join(", ").trim_end_matches(", "))
}

impl Error for CheckerError {}

#[derive(Clone, PartialEq, Debug)]
pub enum BuiltType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F64,
    F32,
    String,
    Bool,
    PointerTo(Box<Type>),
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
        packed: bool,
    },
    None,
}

impl fmt::Display for BuiltType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltType::U8 => write!(f, "u8"),
            BuiltType::U16 => write!(f, "u16"),
            BuiltType::U32 => write!(f, "u32"),
            BuiltType::U64 => write!(f, "u64"),
            BuiltType::I8 => write!(f, "i8"),
            BuiltType::I16 => write!(f, "i16"),
            BuiltType::I32 => write!(f, "i32"),
            BuiltType::I64 => write!(f, "i64"),
            BuiltType::F32 => write!(f, "f32"),
            BuiltType::F64 => write!(f, "f64"),
            BuiltType::PointerTo(to) => write!(f, "*{to}"),
            BuiltType::String => write!(f, "string"),
            BuiltType::Struct { name, .. } => write!(f, "{name}"),
            BuiltType::Bool => write!(f, "bool"),
            BuiltType::None => write!(f, "none"),
        }
    }
}

impl BuiltType {
    pub fn is_numeric(&self) -> bool {
        matches!(self, BuiltType::U8)
            || matches!(self, BuiltType::U16)
            || matches!(self, BuiltType::U32)
            || matches!(self, BuiltType::U64)
            || matches!(self, BuiltType::I8)
            || matches!(self, BuiltType::I16)
            || matches!(self, BuiltType::I32)
            || matches!(self, BuiltType::I64)
            || matches!(self, BuiltType::F32)
            || matches!(self, BuiltType::F64)
    }

    pub fn is_signed_int(&self) -> bool {
        matches!(self, BuiltType::I8)
            || matches!(self, BuiltType::I16)
            || matches!(self, BuiltType::I32)
            || matches!(self, BuiltType::I64)
    }

    pub fn is_unsigned_int(&self) -> bool {
        matches!(self, BuiltType::U8)
            || matches!(self, BuiltType::U16)
            || matches!(self, BuiltType::U32)
            || matches!(self, BuiltType::U64)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Built(BuiltType),
    Class(String, Class),
    SSelf,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Built(b) => write!(f, "{b}"),
            Type::SSelf => write!(f, "Self"),
            Type::Class(c, _) => write!(f, "{c}"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum SelfMode {
    ByRef,
    ByMove,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TraitBounds {
    implements: Vec<String>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct VariableData {
    variable_type: Type,
    was_moved: bool,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionData {
    pub name: String,
    pub return_type: Type,
    pub generic_types: HashMap<String, Option<TraitBounds>>,
    pub arguments_types: Vec<(String, Type)>,
    pub takes_in_self: Option<SelfMode>,
    pub may_throw: bool,
    pub is_var_args: bool,
}

#[derive(Clone, PartialEq, Debug)]
pub struct StructData {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub generic_types: HashMap<String, Option<TraitBounds>>,
    pub implements: Vec<String>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Trait {
    name: String,
    required_functions: Vec<FunctionData>,
    generic_types: HashMap<String, TraitBounds>,
    requires: Vec<String>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Scope {
    local_variables: HashMap<String, VariableData>,
    local_functions: HashMap<String, (FunctionData, usize)>,
    local_structs: HashMap<String, StructData>,
    local_traits: HashMap<String, Trait>,
    generics: HashMap<String, TraitBounds>,
    self_type: Option<Type>,
    is_function_scope: bool,
    is_global: bool,
}

impl Scope {
    pub fn global_new() -> Scope {
        Self {
            local_variables: HashMap::new(),
            local_functions: HashMap::new(),
            local_structs: HashMap::new(),
            local_traits: HashMap::new(),
            generics: HashMap::new(),
            self_type: None,
            is_function_scope: false,
            is_global: true,
        }
    }

    pub fn function_new() -> Scope {
        Self {
            local_variables: HashMap::new(),
            local_functions: HashMap::new(),
            local_structs: HashMap::new(),
            local_traits: HashMap::new(),
            generics: HashMap::new(),
            is_function_scope: true,
            self_type: None,
            is_global: false,
        }
    }

    pub fn new() -> Scope {
        Self {
            local_variables: HashMap::new(),
            local_functions: HashMap::new(),
            local_structs: HashMap::new(),
            local_traits: HashMap::new(),
            generics: HashMap::new(),
            self_type: None,
            is_function_scope: false,
            is_global: false,
        }
    }
}

pub fn builtin_types(to: &mut HashMap<String, Trait>) {
    to.insert(
        "Add".to_string(),
        Trait {
            name: "Add".to_string(),
            required_functions: vec![FunctionData {
                name: "add".to_string(),
                return_type: Type::SSelf,
                generic_types: HashMap::new(),
                arguments_types: vec![(
                    "rhs".to_string(),
                    Type::Built(BuiltType::PointerTo(Box::new(Type::SSelf))),
                )],
                takes_in_self: Some(SelfMode::ByRef),
                may_throw: false,
                is_var_args: false,
            }],
            generic_types: HashMap::new(),
            requires: vec![],
        },
    );

    to.insert(
        "Sub".to_string(),
        Trait {
            name: "Sub".to_string(),
            required_functions: vec![FunctionData {
                name: "sub".to_string(),
                return_type: Type::SSelf,
                generic_types: HashMap::new(),
                arguments_types: vec![(
                    "rhs".to_string(),
                    Type::Built(BuiltType::PointerTo(Box::new(Type::SSelf))),
                )],
                takes_in_self: Some(SelfMode::ByRef),
                may_throw: false,
                is_var_args: false,
            }],
            generic_types: HashMap::new(),
            requires: vec![],
        },
    );

    to.insert(
        "Mul".to_string(),
        Trait {
            name: "Mul".to_string(),
            required_functions: vec![FunctionData {
                name: "mul".to_string(),
                return_type: Type::SSelf,
                generic_types: HashMap::new(),
                arguments_types: vec![(
                    "rhs".to_string(),
                    Type::Built(BuiltType::PointerTo(Box::new(Type::SSelf))),
                )],
                takes_in_self: Some(SelfMode::ByRef),
                may_throw: false,
                is_var_args: false,
            }],
            generic_types: HashMap::new(),
            requires: vec![],
        },
    );

    to.insert(
        "Div".to_string(),
        Trait {
            name: "Div".to_string(),
            required_functions: vec![FunctionData {
                name: "div".to_string(),
                return_type: Type::SSelf,
                generic_types: HashMap::new(),
                arguments_types: vec![(
                    "rhs".to_string(),
                    Type::Built(BuiltType::PointerTo(Box::new(Type::SSelf))),
                )],
                takes_in_self: Some(SelfMode::ByRef),
                may_throw: false,
                is_var_args: false,
            }],
            generic_types: HashMap::new(),
            requires: vec![],
        },
    );

    to.insert(
        "Rem".to_string(),
        Trait {
            name: "Rem".to_string(),
            required_functions: vec![FunctionData {
                name: "rem".to_string(),
                return_type: Type::SSelf,
                generic_types: HashMap::new(),
                arguments_types: vec![(
                    "rhs".to_string(),
                    Type::Built(BuiltType::PointerTo(Box::new(Type::SSelf))),
                )],
                takes_in_self: Some(SelfMode::ByRef),
                may_throw: false,
                is_var_args: false,
            }],
            generic_types: HashMap::new(),
            requires: vec![],
        },
    );

    to.insert(
        "Cloned".to_string(),
        Trait {
            name: "Cloned".to_string(),
            required_functions: vec![FunctionData {
                name: "cloned".to_string(),
                return_type: Type::SSelf,
                generic_types: HashMap::new(),
                arguments_types: vec![],
                takes_in_self: Some(SelfMode::ByRef),
                may_throw: false,
                is_var_args: false,
            }],
            generic_types: HashMap::new(),
            requires: vec![],
        },
    );

    to.insert(
        "Drop".to_string(),
        Trait {
            name: "drop".to_string(),
            required_functions: vec![FunctionData {
                name: "drop".to_string(),
                return_type: Type::Built(BuiltType::None),
                generic_types: HashMap::new(),
                arguments_types: vec![],
                takes_in_self: Some(SelfMode::ByRef),
                may_throw: false,
                is_var_args: false,
            }],
            generic_types: HashMap::new(),
            requires: vec![],
        },
    );
}

macro_rules! binary_operator_impl {
    ($self:expr, $location:expr, $x:expr, $y:expr, $ope:literal) => {{
        let (x, x_may_throw, _) = $self.check_expression(&mut *$x, $location)?;
        let (y, y_may_throw, _) = $self.check_expression(&mut *$y, $location)?;
        $self.assert_implements(&x, $ope, $location)?;
        $self.assert_implements(&y, $ope, $location)?;
        Ok((x, x_may_throw || y_may_throw, false))
    }};
}

pub struct Checker<'vec> {
    scopes: Vec<Scope>,
    classes: HashMap<String, Class>,
    pub statements: &'vec mut Vec<StatementD>,
    pub all_time_functions: HashMap<(usize, String), FunctionData>,
}

impl<'vec> Checker<'vec> {
    pub fn new(statements: &'vec mut Vec<StatementD>) -> Checker {
        Self {
            classes: HashMap::new(),
            scopes: vec![Scope::global_new()],
            statements,
            all_time_functions: HashMap::new(),
        }
    }

    pub fn make_token_type(&self, t: &Type, location: &Token) -> Result<ForeignType, CheckerError> {
        match t {
            Type::Built(b) => match b {
                BuiltType::Bool => Ok(ForeignType::Bool),
                BuiltType::F32 => Ok(ForeignType::F32),
                BuiltType::F64 => Ok(ForeignType::F64),
                BuiltType::I16 => Ok(ForeignType::I16),
                BuiltType::I32 => Ok(ForeignType::I32),
                BuiltType::I64 => Ok(ForeignType::I64),
                BuiltType::I8 => Ok(ForeignType::I8),
                BuiltType::None => Ok(ForeignType::None),
                BuiltType::PointerTo(to) => Ok(ForeignType::PointerTo(Box::new(
                    self.make_token_type(to, location)?,
                ))),
                BuiltType::String => Ok(ForeignType::String),
                BuiltType::U16 => Ok(ForeignType::U16),
                BuiltType::U32 => Ok(ForeignType::U32),
                BuiltType::U64 => Ok(ForeignType::U64),
                BuiltType::U8 => Ok(ForeignType::U8),
                BuiltType::Struct {
                    name,
                    fields,
                    packed,
                } => {
                    let mut new_fields = vec![];
                    for (field_name, field_type) in fields {
                        new_fields.push((
                            field_name.to_string(),
                            self.make_token_type(field_type, location)?,
                        ));
                    }
                    Ok(ForeignType::Struct {
                        name: name.to_string(),
                        fields: new_fields,
                        packed: *packed,
                    })
                }
            },
            Type::SSelf => match self.scopes.last().map(|s| s.self_type.clone()).unwrap() {
                Some(self_t) => self.make_token_type(&self_t, location),
                None => Err(CheckerError::new(
                    location.clone(),
                    "`Self` type is not defined at the current scope (not a method)".to_string(),
                )),
            },
            Type::Class(name, class) => {
                let mut fields = vec![(
                    "data_ptr".to_string(),
                    ForeignType::PointerTo(Box::new(ForeignType::None)),
                )];

                for (virtual_function_name, _function, _block) in &class.virtual_functions {
                    fields.push((
                        virtual_function_name.to_string(),
                        ForeignType::PointerTo(Box::new(ForeignType::None)),
                    ));
                }

                Ok(ForeignType::Struct {
                    name: name.to_string(),
                    fields,
                    packed: false,
                })
            }
        }
    }

    pub fn make_local_type(&mut self, t: &ForeignType) -> Result<Type, CheckerError> {
        match t {
            ForeignType::Struct {
                name,
                fields,
                packed,
            } => {
                let data = self.struct_data(name, &Token::default())?;
                let mut new_fields = Vec::new();
                for (field_name, field_type) in data.fields {
                    new_fields.push((field_name.to_string(), field_type));
                }
                Ok(Type::Built(BuiltType::Struct {
                    name: name.to_string(),
                    fields: new_fields,
                    packed: *packed,
                }))
            }
            ForeignType::I8 => Ok(Type::Built(BuiltType::I8)),
            ForeignType::I16 => Ok(Type::Built(BuiltType::I16)),
            ForeignType::I32 => Ok(Type::Built(BuiltType::I32)),
            ForeignType::I64 => Ok(Type::Built(BuiltType::I64)),
            ForeignType::U8 => Ok(Type::Built(BuiltType::U8)),
            ForeignType::U16 => Ok(Type::Built(BuiltType::U16)),
            ForeignType::U32 => Ok(Type::Built(BuiltType::U32)),
            ForeignType::U64 => Ok(Type::Built(BuiltType::U64)),
            ForeignType::F32 => Ok(Type::Built(BuiltType::F32)),
            ForeignType::F64 => Ok(Type::Built(BuiltType::F64)),
            ForeignType::Bool => Ok(Type::Built(BuiltType::Bool)),
            ForeignType::String => Ok(Type::Built(BuiltType::String)),
            ForeignType::PointerTo(to) => Ok(Type::Built(BuiltType::PointerTo(Box::new(
                self.make_local_type(to)?,
            )))),
            ForeignType::None => Ok(Type::Built(BuiltType::None)),
        }
    }

    pub fn current_scope(&self, f: impl FnOnce(&Scope) -> bool) -> bool {
        match self.scopes.last() {
            Some(scope) => f(scope),
            None => false,
        }
    }

    pub fn check(&mut self) -> Result<(), CheckerError> {
        let mut i = 0;
        let mut last_location = Token::default();
        while let Some((location, statement)) = untracked_mut!(&mut self.statements.get_mut(i), Option<&mut (Token, Statement)>) {
            last_location = location.clone();
            let (should_stop, _return_type, glue, throws) =
                self.check_statement(statement, location)?;
            let glue_len = glue.len();
            self.statements.splice(i..i, glue.into_iter());
            if should_stop {
                break;
            }
            i += glue_len;
            i += 1;
        }
        while self.scopes.len() > 1 {
            let closed_block = self.close_block(&last_location)?;
            self.statements.extend(closed_block.into_iter());
        }
        Ok(())
    }

    fn check_statement(
        &mut self,
        statemento: &mut Statement,
        location: &Token,
    ) -> Result<(bool, Option<Type>, Vec<StatementD>, bool), CheckerError> {
        let mut may_throw = false;
        match statemento {
            Statement::ClassDeclaration(name, class) => {
                for function in &class.functions {}
                self.classes.insert(name.to_string(), class.clone());
            }
            Statement::StructFieldAssignment(variable_name, struct_field, expression, index, struct_type) => {
                let variable_data = self.variable_data(variable_name, location)?.clone();
                *struct_type = self.make_token_type(&variable_data.variable_type, location)?;
                if let Type::Built(BuiltType::Struct { name, fields, packed }) = &variable_data.variable_type {
                    let field_index = fields.iter().position(|i| &i.0 == struct_field);
                    if field_index.is_none() {
                        return Err(CheckerError::new(location.clone(), format!("Struct `{name}` does not have a field named `{struct_field}`")))
                    } else {
                        let field_index = field_index.unwrap();
                        *index = field_index;
                    }
                } else {
                    return Err(CheckerError::new(location.clone(), format!("Cannot assign to a field of a non-struct type {}", variable_data.variable_type)))
                }
            }
            Statement::VariableDeclaration(variable_name, expression) => {
                let (expression_type, throws, can_be_referenced) = self.check_expression(expression, location)?;
                if self.current_scope(|scope| scope.is_global) {
                    return Err(CheckerError::new(
                        location.clone(),
                        "Cannot declare variables at the global scope".to_string(),
                    ));
                }
                may_throw |= throws;
                self.declare_val(variable_name, &expression_type);
            }
            Statement::Throw(expr) => {
                if self.current_scope(|scope| scope.is_global) {
                    return Err(CheckerError::new(
                        location.clone(),
                        "Cannot throw at the global scope".to_string(),
                    ));
                }
                match self.check_expression(expr, location)?.0 {
                    Type::Built(BuiltType::String) => {}
                    _ => {
                        return Err(CheckerError::new(
                            location.clone(),
                            "Throw expression must be a string".to_string(),
                        ))
                    }
                }
            }
            Statement::FunctionDeclaration(
                name,
                (return_type, arguments_types, throws, is_var_args),
                _calling_convention,
            ) => {
                self.function_definition(
                    name,
                    return_type,
                    location,
                    arguments_types,
                    None,
                    &mut may_throw,
                    throws,
                    *is_var_args,
                )?;
            }
            Statement::FunctionDefinition(
                name,
                (return_type, arguments_types, throws, is_var_args),
                statements,
                _calling_convention,
            ) => {
                self.function_definition(
                    name,
                    return_type,
                    location,
                    arguments_types,
                    Some(statements),
                    &mut may_throw,
                    throws,
                    *is_var_args,
                )?;
            }
            Statement::Return(r) => {
                if self.current_scope(|scope| scope.is_global) {
                    return Err(CheckerError::new(
                        location.clone(),
                        "Cannot return at the global scope".to_string(),
                    ));
                }
                let (return_type, may_throw, can_be_referenced) = self.check_expression(r, location)?;
                if let Type::Built(BuiltType::None) = return_type {
                    *statemento = Statement::ReturnVoid;
                    return Ok((true, Some(Type::Built(BuiltType::None)), vec![], may_throw));
                } else {
                    let drop_glue = self.close_block(location)?;
                    return Ok((true, Some(return_type), drop_glue, may_throw));
                }
            }
            Statement::ReturnVoid => {
                if self.current_scope(|scope| scope.is_global) {
                    return Err(CheckerError::new(
                        location.clone(),
                        "Cannot return at the global scope".to_string(),
                    ));
                }
                return Ok((true, Some(Type::Built(BuiltType::None)), vec![], false));
            }
            Statement::While(condition, block) => {
                if self.current_scope(|scope| scope.is_global) {
                    return Err(CheckerError::new(
                        location.clone(),
                        "Cannot use loops the global scope".to_string(),
                    ));
                }
                let cond_t = self.check_expression(condition, location)?;
                if Type::Built(BuiltType::Bool) != cond_t.0 {
                    return Err(CheckerError::new(
                        location.clone(),
                        "The while-loop condition must be a bool".to_string(),
                    ));
                }

                self.check_statement(&mut block.1, location)?;

                may_throw |= cond_t.1
            }
            Statement::IfElse(if_condition, if_block, else_block) => {
                if self.current_scope(|scope| scope.is_global) {
                    return Err(CheckerError::new(
                        location.clone(),
                        "Cannot use if-else at the global scope".to_string(),
                    ));
                }
                let cond_t = self.check_expression(if_condition, location)?;
                if Type::Built(BuiltType::Bool) != cond_t.0 {
                    return Err(CheckerError::new(
                        location.clone(),
                        "The while-loop condition must be a bool".to_string(),
                    ));
                }

                self.check_statement(&mut if_block.1, location)?;

                if let Some(else_block) = else_block {
                    self.check_statement(&mut else_block.1, location)?;
                }

                may_throw |= cond_t.1
            }
            Statement::Block(block) => {
                if self.current_scope(|scope| scope.is_global) {
                    return Err(CheckerError::new(
                        location.clone(),
                        "Cannot do a block at the global scope".to_string(),
                    ));
                }
                self.scopes.push(Scope::new());
                let mut has_ever_stopped = false;

                let mut i = 0;

                while let Some(statement) = block.get_mut(i) {
                    let (stopped, _optional_t, mut drop_glue, throws) =
                        self.check_statement(&mut statement.1, location)?;
                    may_throw |= throws;
                    if !has_ever_stopped {
                        has_ever_stopped = stopped;
                    }
                    block.append(&mut drop_glue);
                    i += 1;
                }
                if !has_ever_stopped {
                    self.close_block(location)?;
                }
                return Ok((false, None, vec![], may_throw));
            }
            Statement::DefineStruct(struct_name, struct_fields) => {
                let mut fields = Vec::new();
                for (field_name, struct_type) in struct_fields {
                    fields.push((field_name.to_string(), self.make_local_type(struct_type)?));
                }
                self.scopes.last_mut().unwrap().local_structs.insert(
                    struct_name.to_string(),
                    StructData {
                        name: struct_name.to_string(),
                        fields,
                        generic_types: HashMap::new(),
                        implements: vec![],
                    },
                );
            }
            Statement::Call(signature, function_name, arguments, all_time_record_id) => {
                let (function, record_id) = self.function_data(function_name, location)?;
                *all_time_record_id = record_id;
                // verifica se função é chamada com o número correto de argumentos
                // ou se ela toma como entrada um número variável de argumentos (...).
                if function.is_var_args && function.arguments_types.len() > arguments.len() {
                    return Err(CheckerError::new(location.clone(), format!("Function `{function_name}` has variant arguments, but from its required arguments perspective, {} are expected and {} were provided", function.arguments_types.len(), arguments.len())));
                }
                if function.arguments_types.len() != arguments.len() && !function.is_var_args {
                    return Err(CheckerError::new(location.clone(), format!("Function `{function_name}` expects {} arguments but {} arguments were provided", function.arguments_types.len(), arguments.len())));
                }
                signature.1 = function.arguments_types.iter().map(|arg| (arg.0.to_string(), self.make_token_type(&arg.1, location).unwrap())).collect();
                let mut may_throw = false;
                let mut args_types = function.arguments_types.iter();
                let mut args_exprs = arguments.iter_mut().map(|expression| self.check_expression(expression, location));
                loop {
                    let arg_type = args_types.next();
                    let arg_expr = args_exprs.next();
                    if let Some(arg_expr) = arg_expr {
                        let (expression_type, throws, can_be_referenced) = arg_expr?;
                        may_throw |= throws;
                        if let Some((argument_name, argument_type)) = arg_type {
                            if &expression_type != argument_type {
                                return Err(CheckerError::new(location.clone(), format!("Argument `{argument_name}` of function `{function_name}` expected a value of type {argument_type} but found a value of type {expression_type}")));
                            }
                        }
                    } else {
                        break;
                    }
                }
                let mut args = vec![];
                for argument in &function.arguments_types {
                    args.push((
                        argument.0.to_string(),
                        self.make_token_type(&argument.1, location)?,
                    ));
                }
                *signature = (
                    self.make_token_type(&function.return_type, location)?,
                    args,
                    function.may_throw || may_throw,
                    function.is_var_args
                );
            }
            Statement::Assignment(variable_name, expression) => {
                if self.current_scope(|scope| scope.is_global) {
                    return Err(CheckerError::new(
                        location.clone(),
                        "Cannot assign to a variable at the global scope".to_string(),
                    ));
                }
                let (expression_type, throws, can_be_referenced) = self.check_expression(expression, location)?;
                may_throw |= throws;
                let variable_data = self.variable_data(variable_name, location)?;
                if variable_data.variable_type != expression_type {
                    return Err(CheckerError::new(location.clone(), format!("Variable `{variable_name}` was declared with the type `{}` but it being assigned to a value of type `{}`", variable_data.variable_type, expression_type)));
                }
            }
            Statement::TryCatch(try_block, _, catch_block) => {
                self.check_statement(&mut try_block.1, location)?;
                self.check_statement(&mut catch_block.1, location)?;
            }
            _ => unimplemented!("{:?}", statemento),
        }
        Ok((false, None, vec![], may_throw))
    }

    fn function_definition(
        &mut self,
        name: &mut str,
        return_type: &mut ForeignType,
        location: &Token,
        arguments_types: &mut [(String, ForeignType)],
        statements: Option<&mut Vec<(Token, Statement)>>,
        may_throw: &mut bool,
        throws: &mut bool,
        is_var_args: bool,
    ) -> Result<(), CheckerError> {
        if let Some(value) = check_main_signature(name, return_type, location, arguments_types) {
            return value;
        }
        let mut variables = HashMap::new();
        for (argument_name, argument_type) in arguments_types.iter() {
            variables.insert(
                argument_name.to_string(),
                VariableData {
                    variable_type: self.make_local_type(argument_type)?,
                    was_moved: false,
                },
            );
        }
        let mut new_arguments = vec![];
        let return_typee = self.make_local_type(return_type)?;
        *return_type = self.make_token_type(&return_typee, location)?;
        for arg in arguments_types.iter_mut() {
            let arg_local = self.make_local_type(&arg.1)?;
            arg.1 = self.make_token_type(&arg_local, location)?;
            new_arguments.push((arg.0.clone(), arg_local));
        }
        let len = self.scopes.len();
        if let Some(scope) = self.scopes.last_mut() {
            scope.local_functions.insert(
                name.to_string(),
                (FunctionData {
                    name: name.to_string(),
                    return_type: return_typee.clone(),
                    generic_types: HashMap::new(),
                    arguments_types: new_arguments.clone(),
                    takes_in_self: None,
                    may_throw: false,
                    is_var_args,
                }, len),
            );
        }
        self.all_time_functions.insert((len, name.to_string()), FunctionData {
            name: name.to_string(),
            return_type: return_typee,
            generic_types: HashMap::new(),
            arguments_types: new_arguments,
            takes_in_self: None,
            may_throw: false,
            is_var_args,
        });
        if let Some(statements) = statements {
            self.scopes.push(Scope::function_new());
            if let Some(scope) = self.scopes.last_mut() {
                scope.local_variables = variables;
            }
            let mut has_ever_stopped = false;
            let mut i = 0;
            while let Some(statement) = statements.get_mut(i) {
                let (stopped, optional_t, drop_glue, throws) =
                    self.check_statement(&mut statement.1, location)?;
                *may_throw |= throws;
                if !has_ever_stopped {
                    has_ever_stopped = stopped;
                }
                statements.splice(i..i, drop_glue.into_iter());
                if let Some(t) = optional_t {
                    if t != self.make_local_type(return_type)? {
                        return Err(CheckerError::new(location.clone(), format!("The function `{name}` was expected to return {return_type} but it returned {t}")));
                    }
                }
                i += 1;
            }
            *throws = *may_throw;
            if !has_ever_stopped {
                if return_type != &ForeignType::None {
                    return Err(CheckerError::new(location.clone(), format!("Function `{name}` does not return anything but is declared with a return type of `{return_type}`")))
                } else {
                    statements.push((statements.last().map(|s| s.0.clone()).unwrap_or(location.clone()), Statement::ReturnVoid));
                }
                self.close_block(location)?;
            }
        }
        Ok(())
    }

    pub fn close_block(&mut self, location: &Token) -> Result<Vec<StatementD>, CheckerError> {
        let mut glue = vec![];
        match self.scopes.pop() {
            Some(scope) => {
                for (variable_name, variable_data) in scope.local_variables {
                    if !variable_data.was_moved
                        && self
                            .assert_implements(&variable_data.variable_type, "Drop", location)
                            .is_ok()
                    {
                        glue.push((location.clone(), Statement::DropGlue(variable_name)));
                    }
                }
                Ok(glue)
            }
            None => Err(CheckerError::new(
                location.clone(),
                "Failed to close block: no scope to close".to_string(),
            )),
        }
    }

    pub fn check_expression(
        &mut self,
        expression: &mut Expression,
        location: &Token,
    ) -> Result<(Type, bool, bool), CheckerError> {
        match expression {
            Expression::Eq(l, r, lt, rt) | Expression::Ne(l, r, lt, rt)
            | Expression::Lt(l, r, lt, rt) | Expression::Le(l, r, lt, rt)
            | Expression::Gt(l, r, lt, rt) | Expression::Ge(l, r, lt, rt) => {
                let left = self.check_expression(l, location)?;
                let right = self.check_expression(r, location)?;
                *lt = self.make_token_type(&left.0, location)?;
                if !lt.is_float() && !lt.is_signed_int() && !lt.is_unsigned_int() && !matches!(lt, ForeignType::PointerTo(_)) {
                    return Err(CheckerError::new(location.clone(), "Cannot compare non-scalar types".to_string()))
                }
                *rt = self.make_token_type(&right.0, location)?;
                if left.0 == right.0 {
                    if matches!(lt, ForeignType::PointerTo(_)) {
                        *lt = ForeignType::U64;
                        *lt = ForeignType::U64;
                    }
                    Ok((Type::Built(BuiltType::Bool), left.1 || right.1, false))
                } else {
                    Err(CheckerError::new(location.clone(), "Cannot compare different types".to_string()))
                }
            }
            Expression::Reference(expr, to) => {
                if let Expression::Variable(v) = &**expr {
                    *to = v.to_string();
                    let variable_data = self.variable_data(v, location)?;
                    Ok((Type::Built(BuiltType::PointerTo(Box::new(variable_data.variable_type.clone()))), false, true))
                } else {
                    Err(CheckerError::new(location.clone(), "Cannot take reference of non-lvalue expression"))
                }
            }
            Expression::StructFieldAccess(expr, field_name, index, tt, struct_type) => {
                let (expr_type, throws, can_be_referenced) = self.check_expression(expr, location)?;
                if let Type::Built(BuiltType::Struct { name, fields, packed }) = expr_type.clone() {
                    if let Some((index_found, (_, field_type))) = fields.iter().enumerate().find(|field| &field.1.0 == field_name) {
                        *index = index_found;
                        *tt = self.make_token_type(field_type, location)?;
                        *struct_type = self.make_token_type(&expr_type, location)?;
                        Ok((field_type.clone(), throws, false))
                    } else {
                        Err(CheckerError::new(location.clone(), format!("The struct `{name}` does not have a field named `{field_name}`")))
                    }
                } else {
                    Err(CheckerError::new(location.clone(), format!("Cannot access a field of a non-struct type: {expr_type}")))
                }
            }
            Expression::StructInstantiate(name, fields) => {
                let struct_data = self.struct_data(name, location)?;
                let mut ordered_fields = vec![];
                let mut throws = false;
                for (field_name, field_type) in &struct_data.fields {
                    let iterator = fields.iter_mut();
                    for item in iterator {
                        let (checked, may_throw, can_be_referenced) = self.check_expression(&mut item.1, location)?;
                        throws |= may_throw;
                        if field_name == &item.0 && field_type == &checked {
                            ordered_fields.push((item.0.clone(), item.1.clone()));
                            break;
                        } else if !struct_data.fields.contains(&(item.0.to_string(), checked.clone())) {
                            let mut error = CheckerError::new(location.clone(), format!("Struct `{name}` does not have a field {} of type {}", item.0, checked));
                            if let Some(pos) = struct_data.fields.iter().position(|i| i.0 == item.0) {
                                let (field_name, field_type) = struct_data.fields.get(pos).unwrap();
                                error.add_note(format!("But it does have a field {field_name} of type {field_type}"));
                            }
                            return Err(error)
                        }
                    }
                }
                *fields = ordered_fields;
                Ok((Type::Built(BuiltType::Struct { name: name.to_string(), fields: struct_data.fields.clone(), packed: false }), throws, false))
            }
            Expression::TypeCast(expr, ffrom, to) => {
                let (from, throws, can_be_referenced) = self.check_expression(expr, location)?;
                if matches!(from, Type::Built(BuiltType::PointerTo(_))) && matches!(to, crate::frontend::tokens::Type::PointerTo(_))
                    || &self.make_token_type(&from, location)? == to {
                        let local_to_type = self.make_local_type(to)?;
                        *expression = *expr.clone();
                        Ok((local_to_type, throws, false))
                } else {
                    *ffrom = self.make_token_type(&from, location)?;
                    let from_tokens_type = self.make_token_type(&from, location)?;
                    if from_tokens_type.is_primitive() && to.is_primitive() {
                        Ok((self.make_local_type(to)?, throws, false))
                    } else {
                        Err(CheckerError::new(location.clone(), format!("Cannot convert between non-primitive types: from {} to {}", from, to)))
                    }
                }
            }
            Expression::New(name, args) => {
                let class_data = self.classes.get(name).ok_or(CheckerError::new(
                    location.clone(),
                    format!("Class `{name}` does not exist"),
                ))?;
                if let Some(builder_fn) = class_data.functions.get("builder") {
                    Ok((
                        Type::Class(name.to_string(), class_data.clone()),
                        builder_fn.0 .2,
                        false,
                    ))
                } else {
                    Err(CheckerError::new(
                        location.clone(),
                        format!("Class `{name}` does not have a constructor"),
                    ))
                }
            }
            Expression::Deref(to, tt) => {
                let (to_t, can_throw, can_take_reference) = self.check_expression(to, location)?;
                if let Type::Built(BuiltType::PointerTo(to_type)) = to_t {
                    *tt = self.make_token_type(&to_type, location)?;
                    Ok((*to_type, can_throw, false))
                } else {
                    Err(CheckerError::new(
                        location.clone(),
                        format!("Cannot dereference a non-pointer type {to_t}"),
                    ))
                }
            }
            Expression::Variable(variable) => {
                let data = self.variable_data(variable, location)?;
                if data.was_moved {
                    Err(CheckerError::new(
                        location.clone(),
                        format!("Use of variable `{variable}` after a move"),
                    ))
                } else {
                    let t = data.variable_type.clone();
                    Ok((t, false, false))
                }
            }
            Expression::String(_) => Ok((Type::Built(BuiltType::String), false, false)),
            Expression::Boolean(_) => Ok((Type::Built(BuiltType::Bool), false, false)),
            Expression::NumberI64(_) => Ok((Type::Built(BuiltType::I64), false, false)),
            Expression::NumberI32(_) => Ok((Type::Built(BuiltType::I32), false, false)),
            Expression::NumberI16(_) => Ok((Type::Built(BuiltType::I16), false, false)),
            Expression::NumberI8(_) => Ok((Type::Built(BuiltType::I8), false, false)),
            Expression::NumberU64(_) => Ok((Type::Built(BuiltType::U64), false, false)),
            Expression::NumberU32(_) => Ok((Type::Built(BuiltType::U32), false, false)),
            Expression::NumberU16(_) => Ok((Type::Built(BuiltType::U16), false, false)),
            Expression::NumberU8(_) => Ok((Type::Built(BuiltType::U8), false, false)),
            Expression::Addition(x, y) => {
                binary_operator_impl!(self, location, x, y, "Add")
            }
            Expression::Subtraction(x, y) => {
                binary_operator_impl!(self, location, x, y, "Sub")
            }
            Expression::Multiplication(x, y) => {
                binary_operator_impl!(self, location, x, y, "Mul")
            }
            Expression::Division(x, y, other) => {
                let (x, x_may_throw, _) = self.check_expression(x, location)?;
                if let Type::Built(ref b) = x {
                    if b.is_signed_int() {
                        *other = DivType::SignedInt
                    } else if b.is_unsigned_int() {
                        *other = DivType::UnsignedInt
                    } else {
                        *other = DivType::Float
                    }
                }
                let (y, y_may_throw, _) = self.check_expression(y, location)?;
                self.assert_implements(&x, "Div", location)?;
                if let Type::Built(BuiltType::PointerTo(ref yinner)) = y {
                    if **yinner == x {
                        Ok((x, x_may_throw || y_may_throw, false))
                    } else {
                        Err(CheckerError::new(
                            location.clone(),
                            format!(
                                "Candidate types are not qualified for `{}`: {} and {}",
                                "Div", x, y
                            ),
                        ))
                    }
                } else if x == y {
                    if matches!(x, Type::Built(_)) {
                        Ok((x, x_may_throw || y_may_throw, false))
                    } else {
                        Err(CheckerError::new(location.clone(), format!("Non-builtin types like {} must have their right operand passed by reference to {}", x, "Div")))
                    }
                } else {
                    Err(CheckerError::new(
                        location.clone(),
                        format!(
                            "Candidate types are not qualified for `{}`: {} and {}",
                            "Div", x, y
                        ),
                    ))
                }
            }
            Expression::Remainder(x, y, _) => {
                binary_operator_impl!(self, location, x, y, "Rem")
            }
            Expression::Call(signature, function_name, arguments, all_time_record_id) => {
                let (function, record_id) = self.function_data(function_name, location)?;
                *all_time_record_id = record_id;
                // verifica se função é chamada com o número correto de argumentos
                // ou se ela toma como entrada um número variável de argumentos (...).
                if function.is_var_args && function.arguments_types.len() > arguments.len() {
                    return Err(CheckerError::new(location.clone(), format!("Function `{function_name}` has variant arguments, but from its required arguments perspective, {} are expected and {} were provided", function.arguments_types.len(), arguments.len())));
                }
                if function.arguments_types.len() != arguments.len() && !function.is_var_args {
                    return Err(CheckerError::new(location.clone(), format!("Function `{function_name}` expects {} arguments but {} arguments were provided", function.arguments_types.len(), arguments.len())));
                }
                signature.1 = function.arguments_types.iter().map(|arg| (arg.0.to_string(), self.make_token_type(&arg.1, location).unwrap())).collect();
                let mut may_throw = false;
                let mut args_types = function.arguments_types.iter();
                let mut args_exprs = arguments.iter_mut().map(|expression| self.check_expression(expression, location));
                loop {
                    let arg_type = args_types.next();
                    let arg_expr = args_exprs.next();
                    if let Some(arg_expr) = arg_expr {
                        let (expression_type, throws, can_be_referenced) = arg_expr?;
                        may_throw |= throws;
                        if let Some((argument_name, argument_type)) = arg_type {
                            if &expression_type != argument_type {
                                return Err(CheckerError::new(location.clone(), format!("Argument `{argument_name}` of function `{function_name}` expected a value of type {argument_type} but found a value of type {expression_type}")));
                            }
                        }
                    } else {
                        break;
                    }
                }
                let mut args = vec![];
                for argument in &function.arguments_types {
                    args.push((
                        argument.0.to_string(),
                        self.make_token_type(&argument.1, location)?,
                    ));
                }
                *signature = (
                    self.make_token_type(&function.return_type, location)?,
                    args,
                    function.may_throw || may_throw,
                    function.is_var_args,
                );
                Ok((function.return_type, function.may_throw, false))
            }
        }
    }

    pub fn assert_implements(
        &mut self,
        type_to: &Type,
        trait_: &str,
        location: &Token,
    ) -> Result<(), CheckerError> {
        match type_to {
            Type::Built(b) if b.is_numeric() => {
                if ["Add", "Sub", "Mul", "Div", "Rem"].contains(&trait_) {
                    Ok(())
                } else {
                    Err(CheckerError::new(
                        location.clone(),
                        format!("The type `{type_to}` does not implement the trait `{trait_}`"),
                    ))
                }
            }
            Type::Built(BuiltType::Struct { name, .. }) => {
                let struct_type = self.struct_data(name, location)?;
                if struct_type.implements.contains(&trait_.to_string()) {
                    Ok(())
                } else {
                    Err(CheckerError::new(
                        location.clone(),
                        format!("The struct `{type_to}` does not implement the trait `{trait_}`"),
                    ))
                }
            }
            Type::Built(b) if trait_ == "Copy" => {
                Ok(())
            }
            // Type::Generic(g) => {
            //     let bounds = self.generic_bounds(g, location)?;
            //     if bounds.implements.contains(&trait_.to_string()) {
            //         Ok(())
            //     } else {
            //         Err(CheckerError::new(
            //             location.clone(),
            //             format!("The generic `{g}` does not satisfy the trait bound `{trait_}`"),
            //         ))
            //     }
            // }
            _ => Err(CheckerError::new(
                location.clone(),
                format!("The type `{type_to}` does not implement the trait `{trait_}`"),
            )),
        }
    }

    pub fn variable_data(
        &mut self,
        variable_name: &str,
        location: &Token,
    ) -> Result<&mut VariableData, CheckerError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(data) = scope.local_variables.get_mut(variable_name) {
                return Ok(data);
            } else if scope.is_function_scope {
                return Err(CheckerError::new(
                    location.clone(),
                    format!("Variable `{variable_name}` not found at the current scope"),
                ));
            } else {
                continue;
            }
        }
        Err(CheckerError::new(
            location.clone(),
            format!("Variable `{variable_name}` not found at the current scope"),
        ))
    }

    pub fn declare_val(&mut self, variable_name: &str, type_to: &Type) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.local_variables.get(variable_name).is_some() {
                scope.local_variables.insert(
                    variable_name.to_string(),
                    VariableData {
                        variable_type: type_to.clone(),
                        was_moved: false,
                    },
                );
            } else if scope.is_function_scope {
                break;
            } else {
                continue;
            }
        }
        self.scopes.last_mut().unwrap().local_variables.insert(
            variable_name.to_string(),
            VariableData {
                variable_type: type_to.clone(),
                was_moved: false,
            },
        );
    }

    pub fn struct_data(
        &mut self,
        struct_name: &str,
        location: &Token,
    ) -> Result<StructData, CheckerError> {
        for scope in self.scopes.iter().rev() {
            if let Some(data) = scope.local_structs.get(struct_name).cloned() {
                return Ok(data);
            } else {
                continue;
            }
        }
        Err(CheckerError::new(
            location.clone(),
            format!("Struct `{struct_name}` not found at the current scope"),
        ))
    }

    pub fn function_data(
        &mut self,
        function_name: &str,
        location: &Token,
    ) -> Result<(FunctionData, usize), CheckerError> {
        for scope in self.scopes.iter().rev() {
            if let Some(data) = scope.local_functions.get(function_name).cloned() {
                return Ok(data);
            } else {
                continue;
            }
        }
        Err(CheckerError::new(
            location.clone(),
            format!("Function `{function_name}` not found at the current scope"),
        ))
    }

    pub fn generic_bounds(
        &mut self,
        generic_name: &str,
        location: &Token,
    ) -> Result<TraitBounds, CheckerError> {
        for scope in self.scopes.iter().rev() {
            if let Some(data) = scope.generics.get(generic_name).cloned() {
                return Ok(data);
            } else {
                continue;
            }
        }
        Err(CheckerError::new(
            location.clone(),
            format!("Generic `{generic_name}` not found at the current scope"),
        ))
    }
}

fn check_main_signature(
    name: &mut str,
    return_type: &mut ForeignType,
    location: &Token,
    arguments_types: &mut [(String, ForeignType)],
) -> Option<Result<(), CheckerError>> {
    if name == "main" && return_type != &mut ForeignType::I32 {
        return Some(Err(CheckerError::new(
            location.clone(), 
            format!("Wrong signature for `main` function: must return i32 but declared with a return type of {return_type}")
        )));
    } else if name == "main" && !arguments_types.is_empty() {
        return Some(Err(CheckerError::new(
            location.clone(),
            format!(
                "Wrong signature for `main` function: arguments must be () but it is {}",
                format_vector_as_list(
                    &arguments_types
                        .iter()
                        .map(|arg| arg.1.clone())
                        .collect::<Vec<_>>()
                )
            ),
        )));
    }
    None
}
