use anyhow::anyhow;

use super::{
    ast::{AccessModifier, Class, DivType, Expression, Statement, StatementD, CallingConvention},
    tokens::{Token, TokenKind, Type},
};
use std::{collections::HashMap, error::Error, fmt::Display, vec::IntoIter};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub location: Token,
    pub message: String,
}

macro_rules! op_parse_impl {
    ($self:expr, $expression:expr, $t:tt) => {
        $self.next_token();
        let rhs = $self.parse_expression()?;
        $expression = Expression::$t(Box::new($expression), Box::new(rhs));
    };
}

impl ParseError {
    pub fn new(location: Token, message: String) -> Self {
        Self { location, message }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}: {}\n{}", self.location.defined_file_name, self.location.line, self.location.length, self.message, self.location.defined_file.clone().unwrap_or("".to_owned()).lines().nth(self.location.line).unwrap_or(""))
    }
}

impl Error for ParseError {}

pub struct Parser {
    tokens: std::iter::Peekable<IntoIter<Token>>,
    last_token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            last_token: Token::default(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<StatementD>, ParseError> {
        let mut ss = vec![];

        while let Some(statement) = self.parse_statement()? {
            ss.push(statement);
        }

        Ok(ss)
    }

    pub fn parse_statement(&mut self) -> Result<Option<StatementD>, ParseError> {
        match self.next_token() {
            Some(token) => {
                match &token.kind {
                    TokenKind::Keyword(kw) => {
                        match kw.as_str() {
                            "var" => {
                                let variable_name = self.expect_identifier()?;
                                let optional_type;
                                if matches!(self.peek_token().map(|t| t.kind), Some(TokenKind::Colon)) {
                                    self.next_token();
                                    optional_type = Some(self.parse_type()?);
                                } else {
                                    optional_type = None;
                                }
                                self.expect_token(TokenKind::Equals)?;
                                let expression = self.parse_expression()?;
                                self.expect_token(TokenKind::Semicolon)?;
                                Ok(Some((
                                    token,
                                    Statement::VariableDeclaration(variable_name, optional_type, expression),
                                )))
                            }
                            "return" => {
                                if self
                                    .peek_token()
                                    .map(|t| matches!(t.kind, TokenKind::Semicolon))
                                    .unwrap_or(false)
                                {
                                    self.next_token();
                                    Ok(Some((token, Statement::ReturnVoid)))
                                } else {
                                    let expression = self.parse_expression()?;
                                    self.expect_token(TokenKind::Semicolon)?;
                                    Ok(Some((token, Statement::Return(expression))))
                                }
                            }
                            "function" => {
                                let function_name = self.expect_identifier()?;
                                let (arguments, is_var_args) = self.parse_function_arguments()?;
                                let return_type = self.parse_return_type()?;
                                let token = self.must_next_token()?;
                                match token.kind {
                                    TokenKind::Semicolon => {
                                        Ok(Some((
                                            token,
                                            Statement::FunctionDeclaration(
                                                function_name,
                                                (return_type, arguments, false, is_var_args),
                                                CallingConvention::C,
                                            ),
                                        )))
                                    }
                                    TokenKind::LeftKey => {
                                        let block = self.parse_block(false)?;
                                        Ok(Some((
                                            token,
                                            Statement::FunctionDefinition(
                                                function_name,
                                                (return_type, arguments, false, is_var_args),
                                                block,
                                                CallingConvention::C,
                                            ),
                                        )))
                                    }
                                    _ => {
                                        let token_type = token.kind.clone();
                                        Err(ParseError::new(token.clone(), format!("Unexpected token for a function definition: {token_type}")))
                                    }
                                }
                            }
                            "extern" => {
                                let calling_convention = self.expect_string()?;
                                match calling_convention.parse::<CallingConvention>() {
                                    Ok(convention) => {
                                        self.expect_token(TokenKind::Keyword("function".to_string()))?;
                                        let function_name = self.expect_identifier()?;
                                        let (arguments, is_var_args) = self.parse_function_arguments()?;
                                        let return_type = self.parse_return_type()?;
                                        let token = self.must_next_token()?;
                                        match token.kind {
                                            TokenKind::Semicolon => {
                                                Ok(Some((
                                                    token,
                                                    Statement::FunctionDeclaration(
                                                        function_name,
                                                        (return_type, arguments, false, is_var_args),
                                                        convention
                                                    ),
                                                )))
                                            }
                                            TokenKind::LeftKey => {
                                                let block = self.parse_block(false)?;
                                                Ok(Some((
                                                    token,
                                                    Statement::FunctionDefinition(
                                                        function_name,
                                                        (return_type, arguments, false, is_var_args),
                                                        block,
                                                        convention,
                                                    ),
                                                )))
                                            }
                                            _ => {
                                                let token_type = token.kind.clone();
                                                Err(ParseError::new(token.clone(), format!("Unexpected token for a function definition: {token_type}")))
                                            }
                                        }
                                    }
                                    Err(_) => {
                                        Err(ParseError::new(token, format!("Unknown calling convention: `{calling_convention}` (run `apolloc print calling-conventions` to see the available calling conventions)")))
                                    }
                                }
                            }
                            "while" => {
                                self.expect_token(TokenKind::LeftParen)?;
                                let condition = self.parse_expression()?;
                                self.expect_token(TokenKind::RightParen)?;
                                let statement = self.parse_statement()?;
                                match statement {
                                    Some(statement) => {
                                        Ok(Some((token, Statement::While(condition, Box::new(statement)))))
                                    }
                                    None => {
                                        Err(ParseError::new(token.clone(), "Expected a valid statement for the while loop but found `EOF`".to_string()))
                                    }
                                }
                            }
                            "do" => {
                                let statement = self.parse_statement()?;
                                self.expect_token(TokenKind::Keyword("while".to_string()))?;
                                self.expect_token(TokenKind::LeftParen)?;
                                let condition = self.parse_expression()?;
                                self.expect_token(TokenKind::RightParen)?;
                                match statement {
                                    Some(statement) => {
                                        Ok(Some((token, Statement::DoWhile(Box::new(statement), condition))))
                                    }
                                    None => {
                                        Err(ParseError::new(token.clone(), "Expected a valid statement for the do while loop but found `EOF`".to_string()))
                                    }
                                }
                            }
                            "if" => {
                                self.expect_token(TokenKind::LeftParen)?;
                                let condition = self.parse_expression()?;
                                self.expect_token(TokenKind::RightParen)?;
                                let if_block = self.parse_statement()?;
                                match if_block {
                            Some(if_statement) => {
                                if let Some(Token { kind: TokenKind::Keyword(kw), .. }) = self.peek_token() {
                                    if kw.as_str() == "else" {
                                        self.next_token();
                                        let else_block = self.parse_statement()?;
                                        match else_block {
                                            Some(else_block) => {
                                                Ok(Some((token, Statement::IfElse(condition, Box::new(if_statement), Some(Box::new(else_block))))))
                                            }
                                            None => {
                                                Err(ParseError::new(token.clone(), "Expected a valid statement for the else block but found `EOF`".to_string()))
                                            }
                                        }
                                    } else {
                                        Ok(Some((token, Statement::IfElse(condition, Box::new(if_statement), None))))
                                    }
                                } else {
                                    Ok(Some((token, Statement::IfElse(condition, Box::new(if_statement), None))))
                                }
                            }
                            None => {
                                Err(ParseError::new(token.clone(), "Expected a valid statement for the while loop but found `EOF`".to_string()))
                            }
                        }
                            }
                            "struct" => {
                                let struct_name = self.expect_identifier()?;
                                let struct_fields = self.parse_struct_fields()?;
                                Ok(Some((
                                    token,
                                    Statement::DefineStruct(struct_name, struct_fields),
                                )))
                            }
                            "throw" => {
                                let message = self.parse_expression()?;
                                self.expect_token(TokenKind::Semicolon)?;
                                Ok(Some((token, Statement::Throw(message))))
                            }
                            "try" => {
                                let try_statement = self.parse_statement()?;
                                self.expect_token(TokenKind::Keyword("catch".to_string()))?;
                                let try_statement = try_statement.unwrap();
                                self.expect_token(TokenKind::LeftParen)?;
                                let exception_name = self.expect_identifier()?;
                                self.expect_token(TokenKind::RightParen)?;
                                let catch_statement = self.parse_statement()?;
                                match catch_statement {
                                    Some(catch_statement) => Ok(Some((
                                        token,
                                        Statement::TryCatch(
                                            Box::new(try_statement),
                                            exception_name,
                                            Box::new(catch_statement),
                                        ),
                                    ))),
                                    None => Err(ParseError::new(
                                        token,
                                        "Expected statement after catch".to_string(),
                                    )),
                                }
                            }
                            "method" => {
                                let struct_name = self.expect_identifier()?;
                                self.expect_token(TokenKind::DoubleColon)?;
                                let method_name = self.expect_identifier()?;
                                let (arguments, is_var_args) = self.parse_method_function_arguments()?;
                                let return_type = self.parse_return_type()?;
                                let block = self.parse_block(true)?;
                                Ok(Some((
                                    token,
                                    Statement::StructMethodDefinition(struct_name, method_name, (return_type, arguments, false, is_var_args), block, Type::None)
                                )))
                            }
                            _ => unimplemented!(),
                        }
                    }
                    TokenKind::LeftKey => {
                        let block = self.parse_block(false)?;
                        Ok(Some((token, Statement::Block(block))))
                    }
                    TokenKind::Ident(i) => {
                        let token_symb = self.must_next_token()?;
                        match &token_symb.kind {
                            TokenKind::DoubleColon => {
                                let method_name = self.expect_identifier()?;
                                let arguments = self.parse_arguments()?;
                                self.expect_token(TokenKind::Semicolon)?;
                                // (struct_value, fn_signature, struct_name, method_name, args, all_time_record_id)
                                Ok(Some((
                                    token.clone(),
                                    Statement::StructMethodCall(
                                        Box::new(Expression::Variable(i.to_string())),
                                        (Type::None, vec![], false, false), String::new(),
                                        method_name,
                                        arguments,
                                        0
                                    )
                                )))
                            }
                            TokenKind::Equals => {
                                let expression = self.parse_expression()?;
                                self.expect_token(TokenKind::Semicolon)?;
                                let i = i.to_string();
                                Ok(Some((token, Statement::Assignment(i, expression))))
                            }
                            TokenKind::LeftParen => {
                                let call_parameters = self.parse_call_params(false)?;
                                let i = i.to_string();
                                self.expect_token(TokenKind::Semicolon)?;
                                Ok(Some((
                                    token,
                                    Statement::Call(
                                        (Type::None, vec![], false, false),
                                        i,
                                        call_parameters,
                                        1,
                                    ),
                                )))
                            }
                            TokenKind::Dot => {
                                let field = self.expect_identifier()?;
                                self.expect_token(TokenKind::Equals)?;
                                let expression = self.parse_expression()?;
                                self.expect_token(TokenKind::Semicolon)?;
                                let i = i.to_string();
                                Ok(Some((token, Statement::StructFieldAssignment(i, field, expression, 0, Type::None))))
                            }
                            TokenKind::PtrAccess => {
                                let field = self.expect_identifier()?;
                                self.expect_token(TokenKind::Equals)?;
                                let expression = self.parse_expression()?;
                                self.expect_token(TokenKind::Semicolon)?;
                                let i = i.to_string();
                                Ok(Some((token, Statement::StructFieldIndirectAssignment(i, field, expression, 0, Type::None))))
                            }
                            _ => Err(ParseError::new(
                                token_symb,
                                "This token was not expected after an identifier".to_string(),
                            )),
                        }
                    }
                    _ => Err(ParseError::new(token, "Unexpected token for a statement".to_string())),
                }
            }
            None => Ok(None),
        }
    }

    fn parse_class(&mut self) -> Result<Class, ParseError> {
        let mut extends: Vec<_> = Vec::new();
        let mut props = Vec::new();
        let mut functions = HashMap::new();
        let mut virtual_functions = Vec::new();

        let token = self.must_peek()?;
        match token.kind {
            TokenKind::Keyword(kw) if kw.as_str() == "extends" => {
                self.next_token();
                loop {
                    let name = self.expect_identifier()?;
                    extends.push(name);
                    if self.expect_token_peeked(TokenKind::Comma).is_ok() {
                        self.next_token();
                        continue;
                    } else {
                        break;
                    }
                }
            }
            TokenKind::LeftKey => {}
            _ => {
                return Err(ParseError::new(
                    token,
                    "Unexpected token for a class definition".to_string(),
                ))
            }
        }
        self.expect_token(TokenKind::LeftKey)?;

        loop {
            let token = self.must_next_token()?;
            match token.kind {
                TokenKind::Keyword(kw) if kw.as_str() == "builder" => {
                    let arguments = self.parse_function_arguments()?;
                    let return_type = self.parse_return_type()?;
                    let block = self.parse_block(true)?;
                    functions.insert(
                        "builder".to_string(),
                        ((return_type, arguments.0, false, arguments.1), block),
                    );
                }
                TokenKind::Keyword(kw) if kw.as_str() == "function" => {
                    let function_name = self.expect_identifier()?;
                    let arguments = self.parse_function_arguments()?;
                    let return_type = self.parse_return_type()?;
                    let block = self.parse_block(true)?;
                    functions.insert(function_name, ((return_type, arguments.0, false, arguments.1), block));
                }
                TokenKind::Keyword(kw) if kw.as_str() == "virtual" => {
                    self.expect_token(TokenKind::Keyword("function".to_string()))?;
                    let function_name = self.expect_identifier()?;
                    let arguments = self.parse_function_arguments()?;
                    let return_type = self.parse_return_type()?;
                    let block = self.parse_block(true)?;
                    virtual_functions.push((function_name, (return_type, arguments.0, false, arguments.1), block));
                }
                TokenKind::Keyword(kw) if kw.as_str() == "property" => {
                    let i = self.expect_identifier()?;
                    let expr_type = self.parse_return_type()?;
                    self.expect_token(TokenKind::Semicolon)?;
                    props.push((i, expr_type));
                }
                TokenKind::Ident(i) => {
                    let expr_type = self.parse_return_type()?;
                    props.push((i, expr_type));
                }
                TokenKind::RightKey => break,
                _ => {
                    return Err(ParseError::new(
                        token,
                        "Unexpected token for after an access modifier".to_string(),
                    ))
                }
            }
        }

        Ok(Class {
            fields: props,
            functions,
            virtual_functions,
            extends,
        })
    }

    pub fn parse_return_type(&mut self) -> Result<Type, ParseError> {
        self.expect_token(TokenKind::Colon)?;
        self.parse_type()
    }

    pub fn parse_function_arguments(&mut self) -> Result<(Vec<(String, Type)>, bool), ParseError> {
        let mut arguments = vec![];
        let mut va_args = false;

        self.expect_token(TokenKind::LeftParen)?;
        loop {
            if let Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            let token = self.must_next_token()?;
            match token.kind {
                TokenKind::Ident(argument_name) => {
                    self.expect_token(TokenKind::Colon)?;
                    let argument_type = self.parse_type()?;
                    arguments.push((argument_name, argument_type));
                }
                TokenKind::Ellipsis => {
                    va_args = true;
                    self.expect_token(TokenKind::RightParen)?;
                    break;
                }
                _ => {
                    let token_kind = token.kind.clone();
                    return Err(ParseError::new(token, format!("Unexpected token for an arguments definition: {token_kind}")))
                }
            }

            if let Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            self.expect_token(TokenKind::Comma)?;
        }

        Ok((arguments, va_args))
    }

    pub fn parse_method_function_arguments(&mut self) -> Result<(Vec<(String, Type)>, bool), ParseError> {
        let mut arguments = vec![];
        let mut va_args = false;

        self.expect_token(TokenKind::LeftParen)?;
        self.expect_token(TokenKind::Ident("this".to_string()))?;
        if self.peek_token().map(|t| t.kind == TokenKind::Comma).unwrap_or(false) {
            self.next_token();
        }
        loop {
            if let Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            let token = self.must_next_token()?;
            match token.kind {
                TokenKind::Ident(argument_name) => {
                    self.expect_token(TokenKind::Colon)?;
                    let argument_type = self.parse_type()?;
                    arguments.push((argument_name, argument_type));
                }
                TokenKind::Ellipsis => {
                    va_args = true;
                    self.expect_token(TokenKind::RightParen)?;
                    break;
                }
                _ => {
                    let token_kind = token.kind.clone();
                    return Err(ParseError::new(token, format!("Unexpected token for an arguments definition: {token_kind}")))
                }
            }

            if let Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            self.expect_token(TokenKind::Comma)?;
        }

        Ok((arguments, va_args))
    }

    pub fn parse_call_params(&mut self, expect: bool) -> Result<Vec<Expression>, ParseError> {
        let mut arguments = vec![];

        if expect {
            self.expect_token(TokenKind::LeftParen)?;
        }
        loop {
            if let Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            let argument = self.parse_expression()?;
            arguments.push(argument);

            if let Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            self.expect_token(TokenKind::Comma)?;
        }

        Ok(arguments)
    }

    pub fn parse_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut arguments = vec![];

        self.expect_token(TokenKind::LeftParen)?;
        loop {
            if let Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            let expr = self.parse_expression()?;
            arguments.push(expr);

            if let Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            self.expect_token(TokenKind::Comma)?;
        }

        Ok(arguments)
    }

    pub fn parse_struct_fields(&mut self) -> Result<Vec<(String, Type)>, ParseError> {
        let mut arguments = vec![];

        self.expect_token(TokenKind::LeftKey)?;
        loop {
            if let Some(Token {
                kind: TokenKind::RightKey,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            let argument_name = self.expect_identifier()?;
            self.expect_token(TokenKind::Colon)?;
            let argument_type = self.parse_type()?;
            arguments.push((argument_name, argument_type));

            if let Some(Token {
                kind: TokenKind::RightKey,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            self.expect_token(TokenKind::Comma)?;
        }

        Ok(arguments)
    }

    pub fn parse_struct_instance_fields(&mut self) -> Result<Vec<(String, Expression)>, ParseError> {
        let mut arguments = vec![];

        self.expect_token(TokenKind::LeftKey)?;
        loop {
            if let Some(Token {
                kind: TokenKind::RightKey,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            let argument_name = self.expect_identifier()?;
            self.expect_token(TokenKind::Colon)?;
            let argument_value = self.parse_expression()?;
            arguments.push((argument_name, argument_value));

            if let Some(Token {
                kind: TokenKind::RightKey,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            self.expect_token(TokenKind::Comma)?;
        }

        Ok(arguments)
    }

    pub fn parse_block(&mut self, expect: bool) -> Result<Vec<StatementD>, ParseError> {
        let mut statements = vec![];

        if expect {
            self.expect_token(TokenKind::LeftKey)?
        }
        loop {
            if let Some(Token {
                kind: TokenKind::RightKey,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }

            let statement = self.parse_statement()?;
            match statement {
                Some(statement) => {
                    statements.push(statement);
                }
                None => {
                    return Err(ParseError::new(
                        self.last_token.clone(),
                        "Block was not closed".to_string(),
                    ))
                }
            }

            if let Some(Token {
                kind: TokenKind::RightKey,
                ..
            }) = self.peek_token()
            {
                self.next_token();
                break;
            }
        }

        Ok(statements)
    }

    pub fn parse_term(&mut self) -> anyhow::Result<Expression, ParseError> {
        let token = self.must_next_token()?;
        match token.kind {
            TokenKind::LeftBrac => {
                let mut values = vec![];
                loop {
                    if matches!(self.peek_token(), Some(Token { kind: TokenKind::RightBrac, .. })) {
                        self.next_token();
                        break;
                    }
                    let expression = self.parse_expression()?;
                    values.push(expression);
                    if matches!(self.peek_token(), Some(Token { kind: TokenKind::Comma, .. })) {
                        self.next_token();
                    } else {
                        self.expect_token(TokenKind::RightBrac)?;
                        break;
                    }
                }
                Ok(Expression::List(values, Type::None))
            }
            TokenKind::Null => {
                Ok(Expression::NullPointer)
            }
            TokenKind::Int(i, ref t) => match t {
                Type::I8 => Ok(Expression::NumberI8(
                    <i64 as TryInto<i8>>::try_into(i)
                        .map_err(|e| ParseError::new(token, e.to_string()))?,
                )),
                Type::I16 => Ok(Expression::NumberI16(
                    <i64 as TryInto<i16>>::try_into(i)
                        .map_err(|e| ParseError::new(token, e.to_string()))?,
                )),
                Type::I32 => Ok(Expression::NumberI32(
                    <i64 as TryInto<i32>>::try_into(i)
                        .map_err(|e| ParseError::new(token, e.to_string()))?,
                )),
                Type::I64 => Ok(Expression::NumberI64(i)),
                Type::U8 => Ok(Expression::NumberU8(
                    <i64 as TryInto<u8>>::try_into(i)
                        .map_err(|e| ParseError::new(token, e.to_string()))?,
                )),
                Type::U16 => Ok(Expression::NumberU16(
                    <i64 as TryInto<u16>>::try_into(i)
                        .map_err(|e| ParseError::new(token, e.to_string()))?,
                )),
                Type::U32 => Ok(Expression::NumberU32(
                    <i64 as TryInto<u32>>::try_into(i)
                        .map_err(|e| ParseError::new(token, e.to_string()))?,
                )),
                Type::U64 => Ok(Expression::NumberU64(
                    <i64 as TryInto<u64>>::try_into(i)
                        .map_err(|e| ParseError::new(token, e.to_string()))?,
                )),
                _ => unreachable!(),
            },
            TokenKind::Char(c) => {
                Ok(Expression::NumberU32(c))
            }
            TokenKind::String(s) => Ok(Expression::String(s)),
            TokenKind::Ident(i) => {
                if self
                    .peek_token()
                    .map(|token| matches!(token.kind, TokenKind::LeftParen))
                    .unwrap_or(false)
                {
                    let arguments = self.parse_arguments()?;
                    Ok(Expression::Call((Type::None, vec![], false, false), i, arguments, 1))
                } else if self.peek_token().map(|token| matches!(token.kind, TokenKind::LeftKey)).unwrap_or(false) {
                    let struct_args = self.parse_struct_instance_fields()?;
                    Ok(Expression::StructInstantiate(i, struct_args))
                } else {
                    Ok(Expression::Variable(i))
                }
            }
            TokenKind::Times => {
                let other_term = self.parse_term()?;
                Ok(Expression::Deref(Box::new(other_term), Type::None))
            }
            TokenKind::Boolean(b) => Ok(Expression::Boolean(b)),
            TokenKind::MathAnd => {
                let other_term = self.parse_term()?;
                Ok(Expression::Reference(Box::new(other_term), String::new()))
            }
            TokenKind::LeftParen => {
                let expr = self.parse_expression()?;
                self.expect_token(TokenKind::RightParen)?;
                Ok(expr)
            }
            TokenKind::Keyword(kw) if kw.as_str() == "unchecked_cast" => {
                self.expect_token(TokenKind::LeftParen)?;
                let e = self.parse_expression()?;
                self.expect_token(TokenKind::Comma)?;
                let dst = self.parse_type()?;
                self.expect_token(TokenKind::RightParen)?;
                Ok(Expression::UncheckedCast(Box::new(e), dst))
            }
            _ => {
                let token_kind = token.kind.clone();
                Err(ParseError::new(
                    token,
                    format!("Not a valid token for an expression: `{}`", token_kind),
                ))
            }
        }
    }


    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expression = self.parse_term()?;
        while let Some(t) = self.peek_token() {
            match t.kind {
                TokenKind::DoubleColon => {
                    self.next_token();
                    let method_name = self.expect_identifier()?;
                    let arguments = self.parse_arguments()?;
                    // (struct_value, fn_signature, struct_name, method_name, args, all_time_record_id)
                    expression = Expression::StructMethodCall(Box::new(expression), (Type::None, vec![], false, false), String::new(), method_name, arguments, 0);
                }
                TokenKind::Plus => {
                    op_parse_impl!(self, expression, Addition);
                }
                TokenKind::Minus => {
                    op_parse_impl!(self, expression, Subtraction);
                }
                TokenKind::Times => {
                    op_parse_impl!(self, expression, Multiplication);
                }
                TokenKind::Divided => {
                    self.next_token();
                    let rhs = self.parse_expression()?;
                    expression = Expression::Division(
                        Box::new(expression),
                        Box::new(rhs),
                        DivType::SignedInt,
                        Type::None
                    );
                }
                TokenKind::Percent => {
                    self.next_token();
                    let rhs = self.parse_expression()?;
                    expression = Expression::Remainder(
                        Box::new(expression),
                        Box::new(rhs),
                        DivType::SignedInt,
                        Type::None
                    );
                }
                TokenKind::EqualTo => {
                    self.next_token();
                    let rhs = self.parse_expression()?;
                    expression = Expression::Eq(
                        Box::new(expression),
                        Box::new(rhs),
                        Type::None,
                        Type::None,
                    );
                }
                TokenKind::NotEqual => {
                    self.next_token();
                    let rhs = self.parse_expression()?;
                    expression = Expression::Ne(
                        Box::new(expression),
                        Box::new(rhs),
                        Type::None,
                        Type::None,
                    );
                }
                TokenKind::LessThan => {
                    self.next_token();
                    let rhs = self.parse_expression()?;
                    expression = Expression::Lt(
                        Box::new(expression),
                        Box::new(rhs),
                        Type::None,
                        Type::None,
                    );
                }
                TokenKind::LessThanE => {
                    self.next_token();
                    let rhs = self.parse_expression()?;
                    expression = Expression::Le(
                        Box::new(expression),
                        Box::new(rhs),
                        Type::None,
                        Type::None,
                    );
                }
                TokenKind::GreaterThan => {
                    self.next_token();
                    let rhs = self.parse_expression()?;
                    expression = Expression::Gt(
                        Box::new(expression),
                        Box::new(rhs),
                        Type::None,
                        Type::None,
                    );
                }
                TokenKind::GreaterThanE => {
                    self.next_token();
                    let rhs = self.parse_expression()?;
                    expression = Expression::Ge(
                        Box::new(expression),
                        Box::new(rhs),
                        Type::None,
                        Type::None,
                    );
                }
                TokenKind::Keyword(kw) if kw.as_str() == "as" => {
                    self.next_token();
                    let to_type = self.parse_type()?;
                    expression = Expression::TypeCast(
                        Box::new(expression),
                        Type::None,
                        to_type
                    );
                }
                TokenKind::Dot => {
                    self.next_token();
                    let field = self.expect_identifier()?;
                    expression = Expression::StructFieldAccess(
                        Box::new(expression),
                        field,
                        0,
                        Type::None,
                        Type::None,
                    );
                }
                TokenKind::PtrAccess => {
                    self.next_token();
                    let field = self.expect_identifier()?;
                    expression = Expression::IndirectStructAccess(
                        Box::new(expression),
                        field,
                        0,
                        Type::None,
                        Type::None,
                    );
                }
                TokenKind::MathAnd | TokenKind::LogicalAnd => {
                    op_parse_impl!(self, expression, And);
                }
                TokenKind::MathOr | TokenKind::LogicalOr => {
                    op_parse_impl!(self, expression, And);
                }
                TokenKind::LogicalXor => {
                    op_parse_impl!(self, expression, Xor);
                }
                _ => break,
            }
        }
        Ok(expression)
    }

    pub fn parse_type(&mut self) -> Result<Type, ParseError> {
        let token = self.must_next_token()?;
        let token_kind = token.kind.clone();
        match token.kind {
            TokenKind::Type(t) => Ok(t),
            TokenKind::Ident(i) => Ok(Type::Struct {
                name: i,
                fields: vec![],
                packed: false,
            }),
            TokenKind::Keyword(kw) if kw.as_str() == "slice" => {
                Ok(Type::Slice(Box::new(self.parse_type()?)))
            }
            TokenKind::None => Ok(Type::None),
            TokenKind::Times => Ok(Type::PointerTo(Box::new(self.parse_type()?))),
            _ => Err(ParseError::new(
                token,
                format!("Not a valid token for a type: `{}`", token_kind),
            )),
        }
    }

    pub fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.next_token() {
            Some(token) => match token.kind {
                TokenKind::Ident(i) => Ok(i),
                _ => {
                    let token_kind = token.kind.clone();
                    Err(ParseError::new(
                        token,
                        format!("Expected an identifier, but found `{token_kind}`"),
                    ))
                }
            },
            None => Err(ParseError::new(
                self.last_token.clone(),
                "Expected an identifier, but found `EOF`".to_string(),
            )),
        }
    }

    pub fn expect_string(&mut self) -> Result<String, ParseError> {
        match self.next_token() {
            Some(token) => match token.kind {
                TokenKind::String(i) => Ok(i),
                _ => {
                    let token_kind = token.kind.clone();
                    Err(ParseError::new(
                        token,
                        format!("Expected a string, but found `{token_kind}`"),
                    ))
                }
            },
            None => Err(ParseError::new(
                self.last_token.clone(),
                "Expected a string, but found `EOF`".to_string(),
            )),
        }
    }

    pub fn expect_token(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        match self.next_token() {
            Some(token) => {
                if token.kind != kind {
                    let token_kind = token.kind.clone();
                    Err(ParseError::new(
                        token,
                        format!("Expected token `{kind}` but found `{token_kind}`"),
                    ))
                } else {
                    Ok(())
                }
            }
            None => Err(ParseError::new(
                self.last_token.clone(),
                format!("Expected token `{kind}` but found `EOF`"),
            )),
        }
    }

    pub fn expect_token_peeked(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        match self.peek_token() {
            Some(token) => {
                if token.kind != kind {
                    let token_kind = token.kind.clone();
                    Err(ParseError::new(
                        token,
                        format!("Expected token `{kind}` but found `{token_kind}`"),
                    ))
                } else {
                    Ok(())
                }
            }
            None => Err(ParseError::new(
                self.last_token.clone(),
                format!("Expected token `{kind}` but found `EOF`"),
            )),
        }
    }

    pub fn must_next_token(&mut self) -> Result<Token, ParseError> {
        let tok = self.tokens.next();
        if let Some(tok) = tok {
            self.last_token = tok.clone();
            Ok(tok)
        } else {
            Err(ParseError::new(
                self.last_token.clone(),
                "Expected a token but found `EOF`".to_string(),
            ))
        }
    }

    pub fn must_peek(&mut self) -> Result<Token, ParseError> {
        let tok = self.tokens.peek().cloned();
        if let Some(tok) = tok {
            self.last_token = tok.clone();
            Ok(tok)
        } else {
            Err(ParseError::new(
                self.last_token.clone(),
                "Expected a token but found `EOF`".to_string(),
            ))
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        let tok = self.tokens.next();
        if let Some(tok) = tok {
            self.last_token = tok.clone();
            Some(tok)
        } else {
            None
        }
    }

    pub fn peek_token(&mut self) -> Option<Token> {
        let tok = self.tokens.peek();
        if let Some(tok) = tok {
            self.last_token = tok.clone();
            Some(tok.clone())
        } else {
            None
        }
    }
}
