use crate::backend::{llvm::Type as LType, builder::ApolloBuilder};
use crate::backend::llvm::*;
use std::fmt::{self, Write};

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
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
    OpaquePtr,
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
        packed: bool,
    },
    None,
    Slice(Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::PointerTo(t) => write!(f, "*{t}"),
            Type::Struct { name, .. } => write!(f, "{name}"),
            Type::None => write!(f, "none"),
            Type::OpaquePtr => write!(f, "opaqueptr"),
            Type::Slice(t) => write!(f, "[]{t}"),
        }
    }
}

macro_rules! string {
    ($e:expr) => {
        ($e.to_string())
    }
}

impl Type {
    pub fn mangled(&self) -> String {
        match self {
            Type::I8 => string!("00"),
            Type::I16 => string!("01"),
            Type::I32 => string!("02"),
            Type::I64 => string!("03"),
            Type::U8 => string!("04"),
            Type::U16 => string!("05"),
            Type::U32 => string!("06"),
            Type::U64 => string!("07"),
            Type::F32 => string!("08"),
            Type::F64 => string!("09"),
            Type::String => string!("0A"),
            Type::Bool => string!("0B"),
            Type::PointerTo(t) => format!("0C*{}", t.mangled()),
            Type::Struct { name, fields, packed } => {
                let mut output = if *packed {
                    string!("0D..")
                } else {
                    string!("0E..")
                };
                let _ = write!(output, "{name},{}$", fields.len());
                for field in fields {
                    let _ = write!(output, "#{}", field.1.mangled());
                }
                output
            },
            Type::None => string!("0E"),
            Type::OpaquePtr => string!("0F"),
            Type::Slice(t) => format!("10&{}", t.mangled()),
        }
    }

    pub fn unwrap_ptr(self) -> Type {
        match self {
            Type::PointerTo(t) => *t,
            _ => panic!("Called unwrap_ptr on non-ptr type"),
        }
    }

    pub fn is_unsigned_int(&self) -> bool {
        matches!(self, Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::PointerTo(_) | Type::Bool)
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct {..})
    }

    pub fn is_signed_int(&self) -> bool {
        matches!(self, Type::I8 | Type::I16 | Type::I32 | Type::I64)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }

    pub fn is_primitive(&self) -> bool {
        !matches!(self, Type::Struct { .. })
    }
}

impl Type {
    pub fn as_type_in_context(&self, ctx: &Context) -> LType {
        match self {
            Type::I8 | Type::U8 => int8_type_in_context(ctx),
            Type::I16 | Type::U16 => int16_type_in_context(ctx),
            Type::I32 | Type::U32 => int32_type_in_context(ctx),
            Type::I64 | Type::U64 => int64_type_in_context(ctx),
            Type::F64 => float64_type_in_context(ctx),
            Type::F32 => float32_type_in_context(ctx),
            Type::Bool => int1_type_in_context(ctx),
            Type::String => {
                let i8_type = int8_type_in_context(ctx);
                pointer_type(i8_type, 0)
            }
            Type::PointerTo(to) => pointer_type(to.as_type_in_context(ctx), 0),
            Type::Struct {
                name: _,
                fields,
                packed,
            } => pointer_type(ctx.struct_type(
                &fields
                    .iter()
                    .map(|t| t.1.as_type_in_context(ctx))
                    .collect::<Vec<_>>(),
                *packed,
            ), 0),
            Type::None => void_type_in_context(ctx),
            Type::OpaquePtr => pointer_type(void_type_in_context(ctx), 0),
            Type::Slice(t) => {
                ctx.struct_type(&[int32_type_in_context(ctx), pointer_type(t.as_type_in_context(ctx), 0)], false)
            }
        }
    }

    pub fn as_type_in_builder(&self, ctx: &ApolloBuilder, is_arg_t: bool) -> LType {
        match self {
            Type::I8 | Type::U8 => int8_type_in_context(ctx.context),
            Type::I16 | Type::U16 => int16_type_in_context(ctx.context),
            Type::I32 | Type::U32 => int32_type_in_context(ctx.context),
            Type::I64 | Type::U64 => int64_type_in_context(ctx.context),
            Type::F64 => float64_type_in_context(ctx.context),
            Type::F32 => float32_type_in_context(ctx.context),
            Type::Bool => int1_type_in_context(ctx.context),
            Type::String => {
                let i8_type = int8_type_in_context(ctx.context);
                pointer_type(i8_type, 0)
            }
            Type::PointerTo(to) => pointer_type(to.as_type_in_context(ctx.context), 0),
            Type::Struct {
                name,
                fields,
                packed,
            } => ctx.struct_types.get(name).copied().unwrap(),
            Type::None => void_type_in_context(ctx.context),
            Type::OpaquePtr => pointer_type(void_type_in_context(ctx.context), 0),
            Type::Slice(t) => {
                ctx.context.struct_type(&[int32_type_in_context(ctx.context), pointer_type(t.as_type_in_builder(ctx, false), 0)], false)
            }
        }
    }

    pub fn arg_type(&self, ctx: &ApolloBuilder) -> (LType, Vec<Attribute>) {
        match self {
            Type::Struct {
                name,
                fields,
                packed,
            } => {
                let struct_type = ctx.struct_types.get(name).copied().unwrap();
                let kind_id = get_enum_attribute_kind_for_name("byval");
                let by_val_attribute = ctx.context.create_type_attribute(kind_id as u32, struct_type);
                (pointer_type(struct_type, 0), vec![by_val_attribute])
            }
            _ => (self.as_type_in_builder(ctx, true), vec![]),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword(String),
    Ident(String),

    Type(Type),

    Config(String, Vec<String>),

    Int(i64, Type),
    Float(f64, Type),
    String(String),
    Char(u32),
    Boolean(bool),
    None,
    Null,

    DoubleColon,
    PtrAccess,
    Plus,
    Minus,
    Times,
    MathOr,
    MathAnd,
    Floor,
    Ceil,
    Divided,
    Percent,
    LogicalAnd,
    LogicalOr,
    LogicalXor,
    LeftShift,
    RightShift,
    EqualTo,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanE,
    GreaterThanE,
    Equals,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DividedEquals,
    PercentEquals,
    LeftParen,
    RightParen,
    LeftBrac,
    RightBrac,
    LeftKey,
    RightKey,
    Dot,
    Comma,
    Colon,
    Semicolon,
    Ellipsis,
    Not,
    DoubleAt,
    EndOfFile,
    Question,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::DoubleColon => write!(f, "::"),
            TokenKind::Char(c) => write!(f, "{} (char)", unsafe { if let Some(c) = char::from_u32(*c) {
                c.to_string()
            } else {
                "(bad char)".to_string()
            } } ),
            TokenKind::PtrAccess => write!(f, "->"),
            TokenKind::Keyword(i) => write!(f, "{i} (keyword)"),
            TokenKind::Ident(i) => write!(f, "{i} (identifier)"),
            TokenKind::Type(t) => write!(f, "{t} (type)"),
            TokenKind::Config(cfg, _) => write!(f, "#[{cfg}]"),
            TokenKind::Int(i, t) => write!(f, "{i}{t}"),
            TokenKind::Float(fl, t) => write!(f, "{fl}{t}"),
            TokenKind::None => write!(f, "none"),
            TokenKind::Null => write!(f, "null"),
            TokenKind::String(s) => write!(f, "{:?}", s),
            TokenKind::Boolean(b) => write!(f, "{b}"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Times => write!(f, "*"),
            TokenKind::MathOr => write!(f, "|"),
            TokenKind::MathAnd => write!(f, "&"),
            TokenKind::Divided => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::LogicalAnd => write!(f, "&&"),
            TokenKind::LogicalOr => write!(f, "||"),
            TokenKind::LogicalXor => write!(f, "^"),
            TokenKind::LeftShift => write!(f, "<<"),
            TokenKind::RightShift => write!(f, ">>"),
            TokenKind::EqualTo => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::LessThan => write!(f, "<"),
            TokenKind::LessThanE => write!(f, "<="),
            TokenKind::GreaterThan => write!(f, ">"),
            TokenKind::GreaterThanE => write!(f, ">="),
            TokenKind::Equals => write!(f, "="),
            TokenKind::PlusEquals => write!(f, "+="),
            TokenKind::MinusEquals => write!(f, "-="),
            TokenKind::TimesEquals => write!(f, "*="),
            TokenKind::DividedEquals => write!(f, "/="),
            TokenKind::PercentEquals => write!(f, "%="),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrac => write!(f, "["),
            TokenKind::RightBrac => write!(f, "]"),
            TokenKind::LeftKey => write!(f, "{{"),
            TokenKind::RightKey => write!(f, "}}"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Ellipsis => write!(f, "..."),
            TokenKind::Not => write!(f, "!"),
            TokenKind::DoubleAt => write!(f, "@@"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::EndOfFile => write!(f, "EOF"),
            TokenKind::Floor | TokenKind::Ceil => write!(f, "|="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub length: usize,
    pub line: usize,
    pub col: usize,
    pub defined_file: Option<String>,
    pub defined_file_name: String,
}

macro_rules! current_file {
    () => {
        {
            use memcmc::cell::CellInterface;
            $crate::CURRENT_FILE.get().clone()
        }
    }
}

macro_rules! current_file_name {
    () => {
        {
            use memcmc::cell::CellInterface;
            $crate::CURRENT_FILE_NAME.get().clone()
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::EndOfFile,
            length: 1,
            line: 1,
            col: 1,
            defined_file: Some(current_file!()),
            defined_file_name: current_file_name!(),
        }
    }
}

use crate::{backend::llvm::Context, CURRENT_FILE, CURRENT_FILE_NAME};

#[derive(Clone, Debug, PartialEq)]
pub struct Tokenizer {
    input: String,
    line: usize,
    column: usize,
    tokens: Vec<Token>,
}

impl Tokenizer {
    #[allow(dead_code)]
    pub fn new<T: ToString>(input: T) -> Tokenizer {
        Self {
            input: input.to_string().replace("\r\n", "\n"),
            line: 1,
            column: 1,
            tokens: Vec::new(),
        }
    }

    #[allow(dead_code)]
    pub fn tokenize(&mut self) -> Result<Vec<Token>, (String, usize, usize, usize)> {
        let mut iter = self.input.chars().peekable();
        loop {
            let next_char = iter.next();
            match next_char {
                Some(chr) => match chr {
                    '@' => {
                        if iter.peek() == Some(&'@') {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::DoubleAt,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2
                        } else {
                            return Err((
                                format!(
                                    "Unrecognized token '@{}'",
                                    iter.peek()
                                        .map(|c| c.to_string())
                                        .unwrap_or("eof".to_string())
                                ),
                                self.line,
                                self.column,
                                2,
                            ));
                        }
                    }
                    '#' if iter.peek() == Some(&'[') => {
                        iter.next();
                        let mut len = 2;

                        let mut name = String::new();
                        let mut attributes = Vec::new();

                        // Read the name
                        while let Some(&name_chr) = iter.peek() {
                            if name_chr != ']' && name_chr != '(' {
                                name.push(name_chr);
                                iter.next();
                                len += 1;
                            } else {
                                break;
                            }
                        }

                        // Check for attributes
                        if iter.peek() == Some(&'(') {
                            iter.next(); // Consume '('
                            len += 1;

                            // Read the attributes
                            let mut attr = String::new();
                            while let Some(&attr_chr) = iter.peek() {
                                if attr_chr != ')' {
                                    attr.push(attr_chr);
                                    iter.next();
                                    len += 1;
                                } else {
                                    break;
                                }
                            }

                            // Split attributes by commas
                            attributes = attr.split(',').map(|s| s.trim().to_string()).collect();

                            // Check for the closing parenthesis
                            if iter.peek() == Some(&')') {
                                iter.next(); // Consume ')'
                                len += 1;
                            }

                            // Check for the closing parenthesis
                            if iter.peek() == Some(&']') {
                                iter.next(); // Consume ']'
                                len += 1;
                            }
                        }

                        // Create a Config token and push it to the tokens vector
                        self.tokens.push(Token {
                            kind: TokenKind::Config(name, attributes),
                            length: len,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                    }
                    ':' => {
                        if iter.peek() == Some(&':') {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::DoubleColon,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2;
                        } else {
                            self.tokens.push(Token {
                                kind: TokenKind::Colon,
                                length: 1,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 1;
                        }
                    }
                    ';' => {
                        self.tokens.push(Token {
                            kind: TokenKind::Semicolon,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '0'..='9' => {
                        let mut number = String::new();
                        let mut underscores_length = 0;
                        number.push(chr);
                        while let Some(chr) = iter.peek() {
                            if chr.is_alphanumeric() || chr == &'.' {
                                number.push(*chr);
                                iter.next();
                            } else if chr == &'_' {
                                underscores_length += 1;
                                iter.next();
                                continue;
                            } else {
                                break;
                            }
                        }
                        if number.contains('.')
                            && (number.ends_with("f64") || !number.ends_with("f32"))
                        {
                            let number2 = number.trim_end_matches("f64");
                            let parsing_result = number2.parse::<f64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Float(parsed_number, Type::F64),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!("Error parsing float literal: {}", err),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else if number.contains('.')
                            && (number.ends_with("f32") || !number.ends_with("f64"))
                        {
                            let number2 = number.trim_end_matches("f32");
                            let parsing_result = number.parse::<f64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Float(parsed_number, Type::F32),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!("Error parsing float literal: {}", err),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else if number.ends_with("i64") {
                            let number2 = number.strip_suffix("i64").unwrap();
                            let parsing_result = number2.parse::<i64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Int(parsed_number, Type::I64),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!(
                                            "Error parsing integer literal: {} ({})",
                                            err, number2
                                        ),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else if number.ends_with("i32") {
                            let number2 = number.strip_suffix("i32").unwrap();
                            let parsing_result = number2.parse::<i64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Int(parsed_number, Type::I32),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!(
                                            "Error parsing integer literal: {} ({})",
                                            err, number2
                                        ),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else if number.ends_with("i16") {
                            let number2 = number.strip_suffix("i16").unwrap();
                            let parsing_result = number2.parse::<i64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Int(parsed_number, Type::I16),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!(
                                            "Error parsing integer literal: {} ({})",
                                            err, number2
                                        ),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else if number.ends_with("i8") {
                            let number2 = number.strip_suffix("i8").unwrap();
                            let parsing_result = number2.parse::<i64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Int(parsed_number, Type::I8),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!(
                                            "Error parsing integer literal: {} ({})",
                                            err, number2
                                        ),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else if number.ends_with("u64") {
                            let number2 = number.strip_suffix("u64").unwrap();
                            let parsing_result = number2.parse::<i64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Int(parsed_number, Type::U64),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!(
                                            "Error parsing integer literal: {} ({})",
                                            err, number2
                                        ),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else if number.ends_with("u32") {
                            let number2 = number.strip_suffix("u32").unwrap();
                            let parsing_result = number2.parse::<i64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Int(parsed_number, Type::U32),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!(
                                            "Error parsing integer literal: {} ({})",
                                            err, number2
                                        ),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else if number.ends_with("u16") {
                            let number2 = number.strip_suffix("u16").unwrap();
                            let parsing_result = number2.parse::<i64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Int(parsed_number, Type::U16),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!(
                                            "Error parsing integer literal: {} ({})",
                                            err, number2
                                        ),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else if number.ends_with("u8") {
                            let number2 = number.strip_suffix("u8").unwrap();
                            let parsing_result = number2.parse::<i64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Int(parsed_number, Type::U8),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!(
                                            "Error parsing integer literal: {} ({})",
                                            err, number2
                                        ),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        } else {
                            let parsing_result = number.parse::<i64>();
                            match parsing_result {
                                Ok(parsed_number) => {
                                    self.tokens.push(Token {
                                        kind: TokenKind::Int(parsed_number, Type::I32),
                                        length: number.len() + underscores_length,
                                        line: self.line,
                                        col: self.column,
                                        defined_file: Some(current_file!()),
                                        defined_file_name: current_file_name!(),
                                    });
                                    self.column += number.len() + underscores_length;
                                }
                                Err(err) => {
                                    return Err((
                                        format!(
                                            "Error parsing integer literal: {} ({})",
                                            err, number
                                        ),
                                        self.line,
                                        self.column,
                                        number.len(),
                                    ))
                                }
                            }
                        }
                    }
                    '\"' => {
                        let mut reached = false;
                        let mut string = String::new();
                        while let Some(ch) = iter.next() {
                            match ch {
                                '\"' => {
                                    reached = true;
                                    break;
                                }
                                '\n' => {
                                    self.column = 1;
                                    self.line += 1;

                                    string.push('\n');
                                }
                                '\r' => {
                                    if Some(&'\n') == iter.peek() {
                                        self.line += 1;
                                        self.column = 1;
                                        string.push_str("\r\n");
                                        iter.next();
                                    } else {
                                        string.push('\r');
                                    }
                                }
                                '\\' => match iter.next() {
                                    Some(c) => match c {
                                        'n' => {
                                            string.push('\n');
                                        }
                                        'r' => {
                                            string.push('\r');
                                        }
                                        't' => {
                                            string.push('\t');
                                        }
                                        '\\' => {
                                            string.push('\\');
                                        }
                                        '0' => {
                                            string.push('\0');
                                        }
                                        'u' => {
                                            let mut digits = String::new();
                                            for _ in 0..4 {
                                                match iter.next() {
                                                                Some(digit) => {
                                                                    digits.push(digit);
                                                                }
                                                                None => {
                                                                    return Err((
                                                                        "A unicode escape sequence must have 4 hexadecimal digits in the sense of \\u{{7FFF}}".to_string(),
                                                                    self.line, self.column, digits.len()))
                                                                }
                                                            }
                                            }
                                            if let Ok(num) = digits.parse::<u32>() {
                                                if let Some(ch) = char::from_u32(num) {
                                                    string.push(ch);
                                                }
                                            } else if let Err(err) = digits.parse::<u32>() {
                                                return Err((format!(
                                                                "Error during unicode escape sequence '\\u{}' parsing: {}",
                                                                digits, err
                                                            ), self.line, self.column, digits.len()));
                                            }
                                        }
                                        '"' => {
                                            string.push('"');
                                        }
                                        _ => {
                                            return Err((
                                                format!("Unknown escape sequence '\\{}'", c),
                                                self.line,
                                                self.column,
                                                string.len(),
                                            ))
                                        }
                                    },
                                    None => {
                                        return Err((
                                            "Unclosed string literal".to_string(),
                                            self.line,
                                            self.column,
                                            string.len(),
                                        ));
                                    }
                                },
                                _ => {
                                    string.push(ch);
                                }
                            }
                        }
                        if !reached {
                            return Err((
                                "Unclosed string literal".to_string(),
                                self.line,
                                self.column,
                                string.len(),
                            ));
                        }
                        let strlen = string.len();
                        self.tokens.push(Token {
                            kind: TokenKind::String(string),
                            length: strlen + 2,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += strlen + 2;
                    }
                    '\'' => {
                        let c = iter.next();
                        match c {
                            Some(ch) => {
                                let c: u32 = match ch {
                                    '\"' => {
                                        '"' as u32
                                    }
                                    '\n' => {
                                        '\n' as u32
                                    }
                                    '\r' => {
                                        '\r' as u32
                                    }
                                    '\\' => match iter.next() {
                                        Some(c) => match c {
                                            'n' => {
                                                '\n' as u32
                                            }
                                            'r' => {
                                                '\r' as u32
                                            }
                                            't' => {
                                                '\t' as u32
                                            }
                                            '\\' => {
                                                '\\' as u32
                                            }
                                            '0' => {
                                                '\0' as u32
                                            }
                                            'u' => {
                                                let mut digits = String::new();
                                                for _ in 0..4 {
                                                    match iter.next() {
                                                        Some(digit) => {
                                                            digits.push(digit);
                                                        }
                                                        None => {
                                                            return Err((
                                                                "A unicode escape sequence must have 4 hexadecimal digits in the sense of \\u{{7FFF}}".to_string(),
                                                            self.line, self.column, digits.len()))
                                                        }
                                                    }
                                                }
                                                if let Ok(num) = digits.parse::<u32>() {
                                                    num
                                                } else if let Err(err) = digits.parse::<u32>() {
                                                    return Err((format!(
                                                        "Error during unicode escape sequence '\\u{}' parsing: {}",
                                                        digits, err
                                                    ), self.line, self.column, digits.len()));
                                                } else {
                                                    unreachable!()
                                                }
                                            }
                                            '"' => {
                                                '"' as u32
                                            }
                                            _ => {
                                                return Err((
                                                    format!("Unknown escape sequence '\\{}'", c),
                                                    self.line,
                                                    self.column,
                                                    1
                                                ))
                                            }
                                        },
                                        None => {
                                            return Err((
                                                "Unclosed char literal".to_string(),
                                                self.line,
                                                self.column,
                                                2
                                            ));
                                        }
                                    },
                                    _ => {
                                        ch as u32
                                    }
                                };
                                match iter.next() {
                                    Some('\'') => {
                                        self.tokens.push(Token {
                                            kind: TokenKind::Char(c),
                                            length: 3,
                                            line: self.line,
                                            col: self.column,
                                            defined_file: Some(current_file!()),
                                            defined_file_name: current_file_name!(),
                                        });
                                    }
                                    _ => {
                                        return Err((
                                            "Unclosed char literal".to_string(),
                                            self.line,
                                            self.column,
                                            1
                                        ));
                                    }
                                }
                            }
                            _ => {
                                return Err((
                                    "Unclosed char literal".to_string(),
                                    self.line,
                                    self.column,
                                    1
                                ));
                            }
                        }
                    }
                    '!' if iter.peek() == Some(&'=') => {
                        iter.next();
                        self.tokens.push(Token {
                            kind: TokenKind::NotEqual,
                            length: 2,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 2;
                    }
                    '!' => {
                        self.tokens.push(Token {
                            kind: TokenKind::Not,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '+' => {
                        if let Some(&'=') = iter.peek() {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::PlusEquals,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2;
                        } else {
                            self.tokens.push(Token {
                                kind: TokenKind::Plus,
                                length: 1,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 1;
                        }
                    }
                    '-' => {
                        if let Some(&'=') = iter.peek() {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::MinusEquals,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2;
                            continue;
                        } else if let Some(&'>') = iter.peek() {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::PtrAccess,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2;
                            continue;
                        }
                        if let Some(c) = iter.peek() {
                            if c.is_numeric() {
                                let mut number = String::new();
                                let mut underscores_length = 0;
                                number.push(chr);
                                while let Some(chr) = iter.peek() {
                                    if chr.is_numeric() || chr == &'.' {
                                        number.push(*chr);
                                        iter.next();
                                    } else if chr == &'_' {
                                        underscores_length += 1;
                                        iter.next();
                                        continue;
                                    } else {
                                        break;
                                    }
                                }
                                if number.contains('.')
                                    && (number.ends_with("f64") || !number.ends_with("f32"))
                                {
                                    let number2 = number.trim_end_matches("f64");
                                    let parsing_result = number2.parse::<f64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Float(parsed_number, Type::F64),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!("Error parsing float literal: {}", err),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else if number.contains('.')
                                    && (number.ends_with("f32") || !number.ends_with("f64"))
                                {
                                    let number2 = number.trim_end_matches("f32");
                                    let parsing_result = number.parse::<f64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Float(parsed_number, Type::F32),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!("Error parsing float literal: {}", err),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else if number.ends_with("i64") {
                                    let number2 = number.strip_suffix("i64").unwrap();
                                    let parsing_result = number2.parse::<i64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Int(parsed_number, Type::I64),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!(
                                                    "Error parsing integer literal: {} ({})",
                                                    err, number2
                                                ),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else if number.ends_with("i32") {
                                    let number2 = number.strip_suffix("i32").unwrap();
                                    let parsing_result = number2.parse::<i64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Int(parsed_number, Type::I32),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!(
                                                    "Error parsing integer literal: {} ({})",
                                                    err, number2
                                                ),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else if number.ends_with("i16") {
                                    let number2 = number.strip_suffix("i16").unwrap();
                                    let parsing_result = number2.parse::<i64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Int(parsed_number, Type::I16),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!(
                                                    "Error parsing integer literal: {} ({})",
                                                    err, number2
                                                ),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else if number.ends_with("i8") {
                                    let number2 = number.strip_suffix("i8").unwrap();
                                    let parsing_result = number2.parse::<i64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Int(parsed_number, Type::I8),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!(
                                                    "Error parsing integer literal: {} ({})",
                                                    err, number2
                                                ),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else if number.ends_with("u64") {
                                    let number2 = number.strip_suffix("u64").unwrap();
                                    let parsing_result = number2.parse::<i64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Int(parsed_number, Type::U64),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!(
                                                    "Error parsing integer literal: {} ({})",
                                                    err, number2
                                                ),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else if number.ends_with("u32") {
                                    let number2 = number.strip_suffix("u32").unwrap();
                                    let parsing_result = number2.parse::<i64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Int(parsed_number, Type::U32),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!(
                                                    "Error parsing integer literal: {} ({})",
                                                    err, number2
                                                ),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else if number.ends_with("u16") {
                                    let number2 = number.strip_suffix("u16").unwrap();
                                    let parsing_result = number2.parse::<i64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Int(parsed_number, Type::U16),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!(
                                                    "Error parsing integer literal: {} ({})",
                                                    err, number2
                                                ),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else if number.ends_with("u8") {
                                    let number2 = number.strip_suffix("u8").unwrap();
                                    let parsing_result = number2.parse::<i64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Int(parsed_number, Type::U8),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!(
                                                    "Error parsing integer literal: {} ({})",
                                                    err, number2
                                                ),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                } else {
                                    let parsing_result = number.parse::<i64>();
                                    match parsing_result {
                                        Ok(parsed_number) => {
                                            self.tokens.push(Token {
                                                kind: TokenKind::Int(parsed_number, Type::I32),
                                                length: number.len() + underscores_length,
                                                line: self.line,
                                                col: self.column,
                                                defined_file: Some(
                                                    current_file!(),
                                                ),
                                                defined_file_name: current_file_name!(),
                                            });
                                            self.column += number.len() + underscores_length;
                                        }
                                        Err(err) => {
                                            return Err((
                                                format!(
                                                    "Error parsing integer literal: {} ({})",
                                                    err, number
                                                ),
                                                self.line,
                                                self.column,
                                                number.len(),
                                            ))
                                        }
                                    }
                                }
                            } else {
                                self.tokens.push(Token {
                                    kind: TokenKind::Minus,
                                    length: 1,
                                    line: self.line,
                                    col: self.column,
                                    defined_file: Some(current_file!()),
                                    defined_file_name: current_file_name!(),
                                });
                                self.column += 1;
                            }
                        } else {
                            self.tokens.push(Token {
                                kind: TokenKind::Minus,
                                length: 1,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 1;
                        }
                    }
                    '*' => {
                        if let Some(&'=') = iter.peek() {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::TimesEquals,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2;
                        } else {
                            self.tokens.push(Token {
                                kind: TokenKind::Times,
                                length: 1,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 1;
                        }
                    }
                    '/' => {
                        if let Some(&'=') = iter.peek() {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::DividedEquals,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2;
                        } else if let Some(&'/') = iter.peek() {
                            iter.next();
                            for c in iter.by_ref() {
                                if c == '\n' {
                                    self.line += 1;
                                    self.column = 1;
                                    break;
                                } else {
                                    self.column += 1;
                                }
                            }
                        } else {
                            self.tokens.push(Token {
                                kind: TokenKind::Divided,
                                length: 1,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 1;
                        }
                    }
                    '?' => {
                        self.tokens.push(Token {
                            kind: TokenKind::Question,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '%' => {
                        if let Some(&'=') = iter.peek() {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::PercentEquals,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2;
                        } else {
                            self.tokens.push(Token {
                                kind: TokenKind::Percent,
                                length: 1,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 1;
                        }
                    }
                    '&' => {
                        if iter.peek() == Some(&'&') {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::LogicalAnd,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2;
                        } else {
                            self.tokens.push(Token {
                                kind: TokenKind::MathAnd,
                                length: 1,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 1;
                        }
                    }
                    '|' => {
                        if iter.peek() == Some(&'|') {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::LogicalOr,
                                length: 2,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 2;
                        } else {
                            self.tokens.push(Token {
                                kind: TokenKind::MathOr,
                                length: 1,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 1;
                        }
                    }
                    '^' => {
                        self.tokens.push(Token {
                            kind: TokenKind::LogicalXor,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '<' if iter.peek() == Some(&'<') => {
                        iter.next();
                        self.tokens.push(Token {
                            kind: TokenKind::LeftShift,
                            length: 2,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 2;
                    }
                    '>' if iter.peek() == Some(&'>') => {
                        iter.next();
                        self.tokens.push(Token {
                            kind: TokenKind::RightShift,
                            length: 2,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 2;
                    }
                    '<' if iter.peek() == Some(&'=') => {
                        iter.next();
                        self.tokens.push(Token {
                            kind: TokenKind::LessThanE,
                            length: 2,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 2;
                    }
                    '>' if iter.peek() == Some(&'=') => {
                        iter.next();
                        self.tokens.push(Token {
                            kind: TokenKind::GreaterThanE,
                            length: 2,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 2;
                    }
                    '<' => {
                        self.tokens.push(Token {
                            kind: TokenKind::LessThan,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '>' => {
                        self.tokens.push(Token {
                            kind: TokenKind::GreaterThan,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '(' => {
                        self.tokens.push(Token {
                            kind: TokenKind::LeftParen,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    ')' => {
                        self.tokens.push(Token {
                            kind: TokenKind::RightParen,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '[' => {
                        self.tokens.push(Token {
                            kind: TokenKind::LeftBrac,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    ']' => {
                        self.tokens.push(Token {
                            kind: TokenKind::RightBrac,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '{' => {
                        self.tokens.push(Token {
                            kind: TokenKind::LeftKey,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '}' => {
                        self.tokens.push(Token {
                            kind: TokenKind::RightKey,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '.' => {
                        if let Some(&'.') = iter.peek() {
                            iter.next();
                            if let Some(&'.') = iter.peek() {
                                iter.next();
                                self.tokens.push(Token {
                                    kind: TokenKind::Ellipsis,
                                    length: 3,
                                    line: self.line,
                                    col: self.column,
                                    defined_file: Some(current_file!()),
                                    defined_file_name: current_file_name!(),
                                });
                                self.column += 3;
                                continue;
                            } else {
                                return Err((
                                    "Unrecognized token: '..'".to_string(),
                                    self.line,
                                    self.column,
                                    2,
                                ));
                            }
                        }
                        self.tokens.push(Token {
                            kind: TokenKind::Dot,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    ',' => {
                        self.tokens.push(Token {
                            kind: TokenKind::Comma,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '=' if iter.peek() == Some(&'=') => {
                        iter.next();
                        self.tokens.push(Token {
                            kind: TokenKind::EqualTo,
                            length: 2,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 2;
                    }
                    '=' => {
                        self.tokens.push(Token {
                            kind: TokenKind::Equals,
                            length: 1,
                            line: self.line,
                            col: self.column,
                            defined_file: Some(current_file!()),
                            defined_file_name: current_file_name!(),
                        });
                        self.column += 1;
                    }
                    '\n' => {
                        self.line += 1;
                        self.column = 1;
                    }
                    c if !c.is_whitespace()
                        && (!c.is_ascii_punctuation() || c == '_' && c != ';')
                        && !c.is_numeric() =>
                    {
                        let mut identifier = String::new();
                        identifier.push(chr);
                        while let Some(c) = iter.peek() {
                            if !c.is_whitespace()
                                && (!c.is_ascii_punctuation() || c == &'_' && c != &';')
                            {
                                identifier.push(*c);
                                iter.next();
                            } else {
                                break;
                            }
                        }
                        if [
                            "var", "return", "function", "while", "if", "else", "struct", "throw",
                            "try", "catch", "class", "property", "builder", "extern", "as", "do",
                            "unchecked_cast", "slice", "method", "static"
                        ]
                        .contains(&identifier.as_str())
                        {
                            let length = identifier.len();
                            self.tokens.push(Token {
                                kind: TokenKind::Keyword(identifier),
                                length,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += length;
                        } else if ["true", "false"].contains(&identifier.as_str()) {
                            let length = identifier.len();
                            self.tokens.push(Token {
                                kind: TokenKind::Boolean(&identifier != "false"),
                                length,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += length;
                        } else if &identifier == "none" {
                            self.tokens.push(Token {
                                kind: TokenKind::None,
                                length: 4,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 4;
                        } else if &identifier == "null" {
                            self.tokens.push(Token {
                                kind: TokenKind::Null,
                                length: 4,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += 4;
                        } else if [
                            "i64", "i32", "i16", "i8", "u64", "u32", "u16", "u8", "f64", "f32",
                            "string", "bool", "opaqueptr"
                        ]
                        .contains(&identifier.as_str())
                        {
                            let length = identifier.len();
                            let output_type = match identifier.as_str() {
                                "i64" => Type::I64,
                                "i32" => Type::I32,
                                "i16" => Type::I16,
                                "i8" => Type::I8,
                                "u64" => Type::U64,
                                "u32" => Type::U32,
                                "u16" => Type::U16,
                                "u8" => Type::U8,
                                "f64" => Type::F64,
                                "f32" => Type::F32,
                                "string" => Type::String,
                                "bool" => Type::Bool,
                                "opaqueptr" => Type::OpaquePtr,
                                _ => unreachable!(),
                            };
                            self.tokens.push(Token {
                                kind: TokenKind::Type(output_type),
                                length,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += length;
                        } else {
                            let length = identifier.len();
                            self.tokens.push(Token {
                                kind: TokenKind::Ident(identifier),
                                length,
                                line: self.line,
                                col: self.column,
                                defined_file: Some(current_file!()),
                                defined_file_name: current_file_name!(),
                            });
                            self.column += length;
                        }
                    }
                    chr if chr.is_whitespace() => {
                        self.column += 1;
                        continue;
                    }
                    _ => {
                        return Err((
                            format!("Unknown token: {:?}", chr),
                            self.line,
                            self.column,
                            1,
                        ))
                    }
                },
                None => {
                    break;
                }
            }
        }
        Ok(self.tokens.clone())
    }
}
