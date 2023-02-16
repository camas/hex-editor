use std::{collections::HashMap, str::FromStr};

use lazy_static::lazy_static;
use num_traits::Num;
use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::PrattParser,
    Parser,
};

#[cfg(test)]
mod tests;

// TODO: Bitfields, Preprocessor, more. Just go through the docs

// Here there be dragons

#[derive(Parser)]
#[grammar = "bt.pest"]
struct BtParser;

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ParseError {
    #[error(transparent)]
    Pest(Box<pest::error::Error<Rule>>),

    #[error(transparent)]
    ParseInt(#[from] std::num::ParseIntError),
    #[error(transparent)]
    ParseFloat(#[from] std::num::ParseFloatError),
}

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(value: pest::error::Error<Rule>) -> Self {
        ParseError::Pest(Box::new(value))
    }
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Declare {
        local: bool,
        object_type: TypeDeclaration,
        name: String,
        args: Vec<Expression>,
        attributes: Vec<Attribute>,
    },
    DeclareAndAssign {
        local: bool,
        object_type: TypeDeclaration,
        name: String,
        attributes: Vec<Attribute>,
        value: Expression,
    },
    // TODO: bit of a hack
    DeclareMultiple(Vec<Statement>),
    Typedef {
        original: String,
        alias: String,
    },
    DeclareFunction {
        object_ref: ObjectRef,
        return_type: TypeDeclaration,
        args: Vec<FunctionArg>,
        statements: Vec<Statement>,
    },
    DeclareEnum {
        object_ref: ObjectRef,
        instance_name: Option<TypeDeclaration>,
        enum_type: Option<String>,
        variants: Vec<(String, Option<Expression>)>,
        attributes: Vec<Attribute>,
    },
    // TODO: bit of a hack. should be ignored later
    DeclareForwardStruct,
    DeclareStruct {
        object_ref: ObjectRef,
        instance_name: Option<TypeDeclaration>,
        args: Vec<FunctionArg>,
        statements: Vec<Statement>,
        attributes: Vec<Attribute>,
    },
    DeclareUnion {
        object_ref: ObjectRef,
        instance_name: Option<TypeDeclaration>,
        args: Vec<FunctionArg>,
        statements: Vec<Statement>,
        attributes: Vec<Attribute>,
    },
    If {
        condition: Expression,
        statements: Vec<Statement>,
        else_ifs: Vec<(Expression, Vec<Statement>)>,
        final_else: Option<Vec<Statement>>,
    },
    While {
        condition: Expression,
        statements: Vec<Statement>,
    },
    DoWhile {
        condition: Expression,
        statements: Vec<Statement>,
    },
    For {
        initialization: Option<Expression>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        statements: Vec<Statement>,
    },
    Switch {
        value: Expression,
        switches: Vec<(SwitchType, Vec<Statement>)>,
    },
    Break,
    Continue,
    Return(Expression),
    ReturnVoid,
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SwitchType {
    Default,
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub(crate) name: String,
    pub(crate) value: AttributeValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttributeValue {
    String(Vec<u8>),
    Character(i8),
    Number(Number),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArg {
    pub object_type: TypeDeclaration,
    pub name: String,
    pub reference: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDeclaration {
    Normal(String),
    Array { name: String, size: Expression },
    UnsizedArray { name: String },
}

impl TypeDeclaration {
    pub fn is_void(&self) -> bool {
        matches!(self, TypeDeclaration::Normal(name) if name == "void")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Unary(UnaryOperator, Box<Expression>),
    Binary {
        operator: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Ternary {
        condition: Box<Expression>,
        true_expression: Box<Expression>,
        false_expression: Box<Expression>,
    },
    Cast {
        target: Box<Expression>,
        cast_type: String,
    },
    CallFunction {
        target: Box<Expression>,
        arguments: Vec<Expression>,
    },
    GetArrayIndex {
        target: Box<Expression>,
        index: Box<Expression>,
    },
    GetMember {
        target: Box<Expression>,
        name: String,
    },
    Identifier(String),
    String(Vec<u8>),
    WideString(Vec<u8>),
    Char(i8),
    Number(Number),
    DeclareArrayValues(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    SuffixIncrement,
    SuffixDecrement,
    PrefixIncrement,
    PrefixDecrement,
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Multiply,
    Divide,
    Modulus,
    Add,
    Subtract,
    LeftShift,
    RightShift,
    LessThan,
    LessThanOrEqual,
    MoreThan,
    MoreThanOrEqual,
    Equal,
    NotEqual,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    LogicalAnd,
    LogicalOr,
    Assign,
    AssignAdd,
    AssignSubtract,
    AssignMultiply,
    AssignDivide,
    AssignModulus,
    AssignLeftShift,
    AssignRightShift,
    AssignBitwiseAnd,
    AssignBitwiseXor,
    AssignBitwiseOr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Bool(bool),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    F32(f32),
    F64(f64),
}

lazy_static! {
    static ref EXPRESSION_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // https://www.sweetscape.com/010editor/manual/Expressions.htm
        // https://en.cppreference.com/w/c/language/operator_precedence
        PrattParser::new()
            .op(Op::postfix(Increment)
                | Op::postfix(Decrement)
                | Op::postfix(CallFunction)
                | Op::postfix(GetArrayIndex)
                | Op::postfix(GetMember))
            .op(Op::prefix(PreIncrement)
                | Op::prefix(PreDecrement)
                | Op::prefix(PreAdd)
                | Op::prefix(PreSubtract)
                | Op::prefix(LogicalNot)
                | Op::prefix(BitwiseNot)
                | Op::prefix(Cast))
            .op(Op::infix(Multiply, Left) | Op::infix(Divide, Left) | Op::infix(Modulus, Left))
            .op(Op::infix(Add, Left) | Op::infix(Subtract, Left))
            .op(Op::infix(LeftShift, Left) | Op::infix(RightShift, Left))
            .op(Op::infix(LessThan, Left)
                | Op::infix(LessThanOrEqual, Left)
                | Op::infix(MoreThan, Left)
                | Op::infix(MoreThanOrEqual, Left))
            .op(Op::infix(Equal, Left) | Op::infix(NotEqual, Left))
            .op(Op::infix(BitwiseAnd, Left))
            .op(Op::infix(BitwiseXor, Left))
            .op(Op::infix(BitwiseOr, Left))
            .op(Op::infix(LogicalAnd, Left))
            .op(Op::infix(LogicalOr, Left))
            .op(Op::infix(Ternary, Right))
            .op(Op::infix(Assign, Right)
                | Op::infix(AssignAdd, Right)
                | Op::infix(AssignSubtract, Right)
                | Op::infix(AssignMultiply, Right)
                | Op::infix(AssignDivide, Right)
                | Op::infix(AssignModulus, Right)
                | Op::infix(AssignLeftShift, Right)
                | Op::infix(AssignRightShift, Right)
                | Op::infix(AssignBitwiseAnd, Right)
                | Op::infix(AssignBitwiseXor, Right)
                | Op::infix(AssignBitwiseOr, Right))
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjectRef {
    Void,
    Basic(BasicObject),
    BasicFunction(BasicFunction),
    Struct(u64),
    Enum(u64),
    Union(u64),
    Function(u64),
}

impl ObjectRef {
    pub fn index(&self) -> u64 {
        match self {
            ObjectRef::Struct(i)
            | ObjectRef::Enum(i)
            | ObjectRef::Union(i)
            | ObjectRef::Function(i) => *i,
            ObjectRef::Void | ObjectRef::Basic(_) | ObjectRef::BasicFunction(_) => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BasicObject {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    F32,
    F64,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BasicFunction {
    Printf,
    Warning,
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
struct ParseContext {
    object_refs: HashMap<String, ObjectRef>,
    struct_ref_counter: u64,
    enum_ref_counter: u64,
    union_ref_counter: u64,
    function_ref_counter: u64,
}

impl Default for ParseContext {
    fn default() -> Self {
        let mut object_refs = HashMap::new();

        macro_rules! insert_basic {
            ($btype:ident, $names:expr) => {
                let object_ref = ObjectRef::Basic(BasicObject::$btype);
                for name in $names {
                    object_refs.insert(name.to_string(), object_ref);
                }
            };
        }

        macro_rules! insert_basic_fn {
            ($fn_name:expr, $fn_type:ident) => {
                object_refs.insert(
                    $fn_name.to_string(),
                    ObjectRef::BasicFunction(BasicFunction::$fn_type),
                );
            };
        }

        // https://www.sweetscape.com/010editor/manual/DataTypes.htm
        // Also rust types because better
        // TODO: Should probably be moved to transform
        insert_basic!(U8, &["u8", "uchar", "ubyte", "UCHAR", "UBYTE"]);
        insert_basic!(I8, &["i8", "char", "byte", "CHAR", "BYTE"]);
        insert_basic!(
            U16,
            &["u16", "ushort", "uint16", "USHORT", "UINT16", "WORD"]
        );
        insert_basic!(I16, &["i16", "short", "int16", "SHORT", "INT16"]);
        insert_basic!(
            U32,
            &["u32", "uint", "uint32", "ulong", "UINT", "UINT32", "ULONG", "DWORD"]
        );
        insert_basic!(
            I32,
            &["i32", "int", "int32", "long", "INT", "INT32", "LONG"]
        );
        insert_basic!(
            U64,
            &["u64", "uint64", "uquad", "UINT64", "UQUAD", "QWORD", "__uint64"]
        );
        insert_basic!(I64, &["i64", "int64", "quad", "INT64", "QUAD", "__int64"]);
        insert_basic!(F32, &["f32", "float", "FLOAT"]);
        insert_basic!(F64, &["f64", "double", "DOUBLE"]);
        // insert_basic!(&["hfloat", "HFLOAT"]);
        // insert_basic!(&["DOSDATE", "DOSTIME", "FILETIME", "OLETIME", "time_t", "time64_t"]);
        insert_basic!(String, &["string"]);
        // insert_basic!(&["wchar_t", "wstring"]);
        // insert_basic!(&["GUID"]);
        // insert_basic!(&["Opcode"]);

        insert_basic_fn!("Printf", Printf);
        insert_basic_fn!("LittleEndian", LittleEndian);
        insert_basic_fn!("BigEndian", BigEndian);
        insert_basic_fn!("Warning", Warning);

        Self {
            object_refs,
            struct_ref_counter: 0,
            enum_ref_counter: 0,
            union_ref_counter: 0,
            function_ref_counter: 1, // Start at 1 for root function
        }
    }
}

impl ParseContext {
    fn create_struct_ref(&mut self, names: Vec<String>) -> ObjectRef {
        let struct_ref = ObjectRef::Struct(self.struct_ref_counter);
        self.struct_ref_counter += 1;

        self.insert_ref_names(names, struct_ref);

        struct_ref
    }

    fn create_enum_ref(&mut self, names: Vec<String>) -> ObjectRef {
        let enum_ref = ObjectRef::Enum(self.enum_ref_counter);
        self.enum_ref_counter += 1;

        self.insert_ref_names(names, enum_ref);

        enum_ref
    }

    fn create_union_ref(&mut self, names: Vec<String>) -> ObjectRef {
        let union_ref = ObjectRef::Union(self.union_ref_counter);
        self.union_ref_counter += 1;

        self.insert_ref_names(names, union_ref);

        union_ref
    }

    fn create_function_ref(&mut self, names: Vec<String>) -> ObjectRef {
        let function_ref = ObjectRef::Function(self.function_ref_counter);
        self.function_ref_counter += 1;

        self.insert_ref_names(names, function_ref);

        function_ref
    }

    fn insert_ref_names(&mut self, names: Vec<String>, object_ref: ObjectRef) {
        for name in names {
            if let Some(existing) = self.object_refs.insert(name.clone(), object_ref) {
                panic!(
                    "Object already declared with name '{}': {:?}",
                    name, existing
                );
            }
        }
    }
}

#[derive(Debug)]
pub struct ParsedData {
    pub statements: Vec<Statement>,
    pub object_refs: HashMap<String, ObjectRef>,
    pub struct_ref_counter: u64,
    pub enum_ref_counter: u64,
    pub union_ref_counter: u64,
    pub function_ref_counter: u64,
}

pub fn parse_bt<S: AsRef<str>>(data: S) -> ParseResult<ParsedData> {
    let pairs = BtParser::parse(Rule::Program, data.as_ref())?;

    let mut context = ParseContext::default();

    let statements = pairs
        .take_while(|p| !matches!(p.as_rule(), Rule::EOI))
        .map(|p| parse_statement(&mut context, p))
        .collect();

    Ok(ParsedData {
        statements,
        object_refs: context.object_refs,
        struct_ref_counter: context.struct_ref_counter,
        enum_ref_counter: context.enum_ref_counter,
        union_ref_counter: context.union_ref_counter,
        function_ref_counter: context.function_ref_counter,
    })
}

fn parse_statement(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    match pair.as_rule() {
        Rule::Statement => parse_statement(context, pair.into_inner().next().unwrap()),
        Rule::Expression => Statement::Expression(parse_expression(pair.into_inner())),
        Rule::Declare => parse_declare(pair),
        Rule::DeclareFunction => parse_declare_function(context, pair),
        Rule::DeclareEnum => parse_declare_enum(context, pair),
        Rule::DeclareForwardStruct => Statement::DeclareForwardStruct,
        Rule::DeclareStruct => parse_declare_struct(context, pair),
        Rule::Return => Statement::Return(parse_expression(pair.into_inner())),
        Rule::ReturnVoid => Statement::ReturnVoid,
        Rule::If => parse_if(context, pair),
        Rule::While => parse_while(context, pair),
        Rule::DoWhile => parse_do_while(context, pair),
        Rule::For => parse_for(context, pair),
        Rule::Break => Statement::Break,
        Rule::Continue => Statement::Continue,
        Rule::Switch => parse_switch(context, pair),
        Rule::BasicTypedef => parse_typedef(pair),
        Rule::DeclareUnion => parse_union(context, pair),
        _ => unreachable!("Unknown statement {:?} {:?}", pair.as_rule(), pair.as_str()),
    }
}

fn parse_union(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    let mut pairs = pair.into_inner();
    let mut next = pairs.next().unwrap();

    let typedef = if matches!(next.as_rule(), Rule::Typedef) {
        next = pairs.next().unwrap();
        true
    } else {
        false
    };

    let name = if matches!(next.as_rule(), Rule::Identifier) {
        let name = next.as_str().to_string();
        next = pairs.next().unwrap();
        Some(name)
    } else {
        None
    };

    let args = if matches!(next.as_rule(), Rule::StructArgs) {
        let args = next
            .into_inner()
            .map(|pair| {
                let mut pairs = pair.into_inner();
                let object_type = parse_type_declaration(pairs.next().unwrap());
                let mut next = pairs.next().unwrap();
                let reference = if matches!(next.as_rule(), Rule::FunctionArgRef) {
                    next = pairs.next().unwrap();
                    true
                } else {
                    false
                };
                let name = next.as_str().to_string();
                FunctionArg {
                    object_type,
                    name,
                    reference,
                }
            })
            .collect();
        next = pairs.next().unwrap();
        args
    } else {
        Vec::new()
    };

    let statements = next
        .into_inner()
        .map(|p| parse_statement(context, p))
        .collect();

    let mut next = pairs.next();

    let instance_name = match next {
        Some(pair) if !matches!(pair.as_rule(), Rule::Attributes) => {
            next = pairs.next();
            Some(parse_type_declaration(pair))
        }
        _ => None,
    };

    let attributes = match next {
        Some(pair) if matches!(pair.as_rule(), Rule::Attributes) => parse_attributes(pair),
        _ => Vec::new(),
    };

    let (names, instance_name) = calculate_ref_and_name(context, typedef, name, instance_name);
    let object_ref = context.create_enum_ref(names);

    Statement::DeclareUnion {
        instance_name,
        object_ref,
        args,
        statements,
        attributes,
    }
}

fn parse_typedef(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();
    let original = inner.next().unwrap().as_str().to_string();
    let alias = inner.next().unwrap().as_str().to_string();

    Statement::Typedef { original, alias }
}

fn parse_switch(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let value = parse_expression(inner.next().unwrap().into_inner());

    let switches = inner
        .map(|pair| {
            let mut inner = pair.into_inner();

            let type_pair = inner.next().unwrap();
            let switch_type = match type_pair.as_rule() {
                Rule::Default => SwitchType::Default,
                Rule::Expression => {
                    SwitchType::Expression(parse_expression(type_pair.into_inner()))
                }
                _ => unreachable!(),
            };

            let statements = inner
                .next()
                .unwrap()
                .into_inner()
                .map(|p| parse_statement(context, p))
                .collect();

            (switch_type, statements)
        })
        .collect();

    Statement::Switch { value, switches }
}

fn parse_for(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let initialization = inner
        .next()
        .unwrap()
        .into_inner()
        .next()
        .map(|p| parse_expression(p.into_inner()));
    let condition = inner
        .next()
        .unwrap()
        .into_inner()
        .next()
        .map(|p| parse_expression(p.into_inner()));
    let increment = inner
        .next()
        .unwrap()
        .into_inner()
        .next()
        .map(|p| parse_expression(p.into_inner()));
    let statements = inner
        .next()
        .unwrap()
        .into_inner()
        .map(|p| parse_statement(context, p))
        .collect();

    Statement::For {
        initialization,
        condition,
        increment,
        statements,
    }
}

fn parse_while(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let condition = parse_expression(inner.next().unwrap().into_inner());
    let statements = inner
        .next()
        .unwrap()
        .into_inner()
        .map(|p| parse_statement(context, p))
        .collect();

    Statement::While {
        condition,
        statements,
    }
}

fn parse_do_while(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let statements = inner
        .next()
        .unwrap()
        .into_inner()
        .map(|p| parse_statement(context, p))
        .collect();
    let condition = parse_expression(inner.next().unwrap().into_inner());

    Statement::DoWhile {
        condition,
        statements,
    }
}

fn parse_if(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let condition = parse_expression(inner.next().unwrap().into_inner());
    let statements = inner
        .next()
        .unwrap()
        .into_inner()
        .map(|p| parse_statement(context, p))
        .collect();

    let mut else_ifs = Vec::new();
    while matches!(inner.peek().map(|p| p.as_rule()), Some(Rule::ElseIf)) {
        let mut inner = inner.next().unwrap().into_inner();
        let condition = parse_expression(inner.next().unwrap().into_inner());
        let statements = inner
            .next()
            .unwrap()
            .into_inner()
            .map(|p| parse_statement(context, p))
            .collect();
        else_ifs.push((condition, statements))
    }

    let final_else = inner.next().map(|pair| {
        pair.into_inner()
            .next()
            .unwrap()
            .into_inner()
            .map(|p| parse_statement(context, p))
            .collect()
    });

    Statement::If {
        condition,
        statements,
        else_ifs,
        final_else,
    }
}

fn parse_declare(pair: Pair<Rule>) -> Statement {
    let mut pairs = pair.into_inner();
    let first_pair = pairs.next().unwrap();
    let local = matches!(first_pair.as_rule(), Rule::Local);
    let partial_type = if local {
        pairs.next().unwrap().as_str().to_string()
    } else {
        first_pair.as_str().to_string()
    };

    let statements = pairs
        .map(|p| {
            let mut pairs = p.into_inner();
            let identifier = pairs.next().unwrap().into_inner().next().unwrap();

            let partial_name = parse_type_declaration(identifier);

            let mut next_pair = pairs.next();

            let args = match next_pair {
                Some(pair) if matches!(pair.as_rule(), Rule::DeclareInnerArgs) => {
                    next_pair = pairs.next();
                    pair.into_inner()
                        .map(|p| parse_expression(p.into_inner()))
                        .collect()
                }
                _ => Vec::new(),
            };

            let attributes = match next_pair {
                Some(pair) if matches!(pair.as_rule(), Rule::Attributes) => {
                    next_pair = pairs.next();
                    parse_attributes(pair)
                }
                _ => Vec::new(),
            };

            let value = match next_pair {
                Some(pair) => {
                    let pair = pair.into_inner().next().unwrap();
                    Some(match pair.as_rule() {
                        Rule::Expression => parse_expression(pair.into_inner()),
                        Rule::ArrayExpression => {
                            let values = pair
                                .into_inner()
                                .map(|p| parse_expression(p.into_inner()))
                                .collect();
                            Expression::DeclareArrayValues(values)
                        }
                        _ => unreachable!(),
                    })
                }
                None => None,
            };

            // Need to swap name and type as for some reason (c sucking) declaration is
            //   int a[1] = ...
            // but used elsewhere like
            //   int[1] a() {}
            let partial_type = partial_type.clone();
            let (name, object_type) = match partial_name {
                TypeDeclaration::Normal(name) => (name, TypeDeclaration::Normal(partial_type)),
                TypeDeclaration::Array { name, size } => (
                    name,
                    TypeDeclaration::Array {
                        name: partial_type,
                        size,
                    },
                ),
                TypeDeclaration::UnsizedArray { name } => {
                    (name, TypeDeclaration::UnsizedArray { name: partial_type })
                }
            };

            match value {
                Some(value) => {
                    assert!(args.is_empty());
                    Statement::DeclareAndAssign {
                        local,
                        object_type,
                        name,
                        attributes,
                        value,
                    }
                }
                None => Statement::Declare {
                    local,
                    object_type,
                    name,
                    args,
                    attributes,
                },
            }
        })
        .collect::<Vec<_>>();

    if statements.len() == 1 {
        statements.into_iter().next().unwrap()
    } else {
        Statement::DeclareMultiple(statements)
    }
}

fn parse_attributes(pair: Pair<Rule>) -> Vec<Attribute> {
    pair.into_inner()
        .map(|pair| {
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str().to_string();
            let value_pair = pairs.next().unwrap();
            let value = match value_pair.as_rule() {
                Rule::String => AttributeValue::String(parse_string(value_pair)),
                Rule::Identifier => AttributeValue::Identifier(value_pair.as_str().to_string()),
                Rule::Character => AttributeValue::Character(parse_character(value_pair)),
                Rule::Number => AttributeValue::Number(parse_number(value_pair).unwrap()),
                _ => unreachable!(),
            };
            Attribute { name, value }
        })
        .collect()
}

fn parse_declare_function(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    let mut pairs = pair.into_inner();

    let return_type = parse_type_declaration(pairs.next().unwrap());
    let name = pairs.next().unwrap().as_str().to_string();

    let args_pair = pairs.next().unwrap();
    let args = args_pair
        .into_inner()
        .map(|pair| {
            let mut pairs = pair.into_inner();
            let object_type = parse_type_declaration(pairs.next().unwrap());
            let mut next = pairs.next().unwrap();
            let reference = if matches!(next.as_rule(), Rule::FunctionArgRef) {
                next = pairs.next().unwrap();
                true
            } else {
                false
            };
            let name = next.as_str().to_string();
            FunctionArg {
                object_type,
                name,
                reference,
            }
        })
        .collect();

    let statements = pairs
        .next()
        .unwrap()
        .into_inner()
        .map(|p| parse_statement(context, p))
        .collect();

    let object_ref = context.create_function_ref(vec![name]);

    Statement::DeclareFunction {
        object_ref,
        return_type,
        args,
        statements,
    }
}

fn parse_declare_enum(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    let mut pairs = pair.into_inner();
    let mut next = pairs.next().unwrap();

    let typedef = if matches!(next.as_rule(), Rule::Typedef) {
        next = pairs.next().unwrap();
        true
    } else {
        false
    };

    let enum_type = if matches!(next.as_rule(), Rule::EnumType) {
        let enum_type = next.into_inner().next().unwrap().as_str().to_string();
        next = pairs.next().unwrap();
        Some(enum_type)
    } else {
        None
    };

    let name = if matches!(next.as_rule(), Rule::Identifier) {
        let name = next.as_str().to_string();
        next = pairs.next().unwrap();
        Some(name)
    } else {
        None
    };

    let variants = next
        .into_inner()
        .map(|pair| {
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str().to_string();
            let value = pairs.next().map(|p| parse_expression(p.into_inner()));
            (name, value)
        })
        .collect();
    let mut next = pairs.next();

    let instance_name = match next {
        Some(pair) if !matches!(pair.as_rule(), Rule::Attributes) => {
            next = pairs.next();
            Some(parse_type_declaration(pair))
        }
        _ => None,
    };

    let attributes = match next {
        Some(pair) if matches!(pair.as_rule(), Rule::Attributes) => parse_attributes(pair),
        _ => Vec::new(),
    };

    let (names, instance_name) = calculate_ref_and_name(context, typedef, name, instance_name);
    let object_ref = context.create_enum_ref(names);

    Statement::DeclareEnum {
        instance_name,
        object_ref,
        enum_type,
        variants,
        attributes,
    }
}

fn parse_declare_struct(context: &mut ParseContext, pair: Pair<Rule>) -> Statement {
    let mut pairs = pair.into_inner();
    let mut next = pairs.next().unwrap();

    let typedef = if matches!(next.as_rule(), Rule::Typedef) {
        next = pairs.next().unwrap();
        true
    } else {
        false
    };

    let name = if matches!(next.as_rule(), Rule::Identifier) {
        let name = next.as_str().to_string();
        next = pairs.next().unwrap();
        Some(name)
    } else {
        None
    };

    let args = if matches!(next.as_rule(), Rule::StructArgs) {
        let args = next
            .into_inner()
            .map(|pair| {
                let mut pairs = pair.into_inner();
                let object_type = parse_type_declaration(pairs.next().unwrap());
                let mut next = pairs.next().unwrap();
                let reference = if matches!(next.as_rule(), Rule::FunctionArgRef) {
                    next = pairs.next().unwrap();
                    true
                } else {
                    false
                };
                let name = next.as_str().to_string();
                FunctionArg {
                    object_type,
                    name,
                    reference,
                }
            })
            .collect();
        next = pairs.next().unwrap();
        args
    } else {
        Vec::new()
    };

    let statements = next
        .into_inner()
        .map(|p| parse_statement(context, p))
        .collect();

    let mut next = pairs.next();

    let instance_name = match next {
        Some(pair) if !matches!(pair.as_rule(), Rule::Attributes) => {
            next = pairs.next();
            Some(parse_type_declaration(pair))
        }
        _ => None,
    };

    let attributes = match next {
        Some(pair) if matches!(pair.as_rule(), Rule::Attributes) => parse_attributes(pair),
        _ => Vec::new(),
    };

    let (names, instance_name) = calculate_ref_and_name(context, typedef, name, instance_name);
    let object_ref = context.create_struct_ref(names);

    Statement::DeclareStruct {
        instance_name,
        object_ref,
        args,
        statements,
        attributes,
    }
}

fn calculate_ref_and_name(
    context: &mut ParseContext,
    typedef: bool,
    name: Option<String>,
    instance_name: Option<TypeDeclaration>,
) -> (Vec<String>, Option<TypeDeclaration>) {
    match (typedef, name, instance_name) {
        (_, None, None) => panic!("Struct with no name or instance name"),
        (true, Some(name), Some(name2)) => (vec![name, name2.name().to_string()], None),
        (true, Some(name), None) => (vec![name], None),
        (true, None, Some(name)) => (vec![name.name().to_string()], None),
        (false, Some(name), instance_name) => (vec![name], instance_name),
        (false, None, instance_name) => (vec![], instance_name),
    }
}

fn parse_type_declaration(pair: Pair<Rule>) -> TypeDeclaration {
    match pair.as_rule() {
        Rule::TypeDeclaration => parse_type_declaration(pair.into_inner().next().unwrap()),
        Rule::ArrayIdentifier => {
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str().to_string();
            let size = parse_expression(pairs.next().unwrap().into_inner());
            TypeDeclaration::Array { name, size }
        }
        Rule::UnsizedArrayIdentifier => {
            let name = pair.into_inner().next().unwrap().as_str().to_string();
            TypeDeclaration::UnsizedArray { name }
        }
        Rule::Identifier => TypeDeclaration::Normal(pair.as_str().to_string()),
        _ => unreachable!(),
    }
}

fn parse_expression(pairs: Pairs<Rule>) -> Expression {
    EXPRESSION_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::Identifier => Expression::Identifier(primary.as_str().to_string()),
            Rule::WideString => {
                Expression::WideString(parse_string(primary.into_inner().next().unwrap()))
            }
            Rule::String => Expression::String(parse_string(primary)),
            Rule::Character => Expression::Char(parse_character(primary)),
            // TODO: handle result instead of unwrapping (Can invalid numbers even be parsed?)
            Rule::Number => Expression::Number(parse_number(primary).unwrap()),
            Rule::Expression => parse_expression(primary.into_inner()),
            _ => unreachable!("Unexpected expression: {:?}", primary),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::Cast => Expression::Cast {
                target: Box::new(rhs),
                cast_type: op.into_inner().next().unwrap().as_str().to_string(),
            },
            other => {
                let operator = match other {
                    Rule::PreIncrement => UnaryOperator::PrefixIncrement,
                    Rule::PreDecrement => UnaryOperator::PrefixDecrement,
                    Rule::PreAdd => UnaryOperator::Plus,
                    Rule::PreSubtract => UnaryOperator::Minus,
                    Rule::LogicalNot => UnaryOperator::LogicalNot,
                    Rule::BitwiseNot => UnaryOperator::BitwiseNot,
                    _ => unreachable!(),
                };
                Expression::Unary(operator, Box::new(rhs))
            }
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::CallFunction => {
                let arguments = op
                    .into_inner()
                    .map(|p| parse_expression(p.into_inner()))
                    .collect();
                Expression::CallFunction {
                    target: Box::new(lhs),
                    arguments,
                }
            }
            Rule::GetArrayIndex => {
                let index = parse_expression(op.into_inner().next().unwrap().into_inner());
                Expression::GetArrayIndex {
                    target: Box::new(lhs),
                    index: Box::new(index),
                }
            }
            Rule::GetMember => {
                let name = op.into_inner().next().unwrap().as_str().to_string();
                Expression::GetMember {
                    target: Box::new(lhs),
                    name,
                }
            }
            other => {
                let operator = match other {
                    Rule::Increment => UnaryOperator::SuffixIncrement,
                    Rule::Decrement => UnaryOperator::SuffixDecrement,
                    _ => unreachable!(),
                };
                Expression::Unary(operator, Box::new(lhs))
            }
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::Ternary => {
                let middle = parse_expression(op.into_inner().next().unwrap().into_inner());
                Expression::Ternary {
                    condition: Box::new(lhs),
                    true_expression: Box::new(middle),
                    false_expression: Box::new(rhs),
                }
            }
            other => {
                let operator = match other {
                    Rule::Multiply => BinaryOperator::Multiply,
                    Rule::Divide => BinaryOperator::Divide,
                    Rule::Modulus => BinaryOperator::Modulus,
                    Rule::Add => BinaryOperator::Add,
                    Rule::Subtract => BinaryOperator::Subtract,
                    Rule::LeftShift => BinaryOperator::LeftShift,
                    Rule::RightShift => BinaryOperator::RightShift,
                    Rule::LessThan => BinaryOperator::LessThan,
                    Rule::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
                    Rule::MoreThan => BinaryOperator::MoreThan,
                    Rule::MoreThanOrEqual => BinaryOperator::MoreThanOrEqual,
                    Rule::Equal => BinaryOperator::Equal,
                    Rule::NotEqual => BinaryOperator::NotEqual,
                    Rule::BitwiseAnd => BinaryOperator::BitwiseAnd,
                    Rule::BitwiseXor => BinaryOperator::BitwiseXor,
                    Rule::BitwiseOr => BinaryOperator::BitwiseOr,
                    Rule::LogicalAnd => BinaryOperator::LogicalAnd,
                    Rule::LogicalOr => BinaryOperator::LogicalOr,
                    Rule::Assign => BinaryOperator::Assign,
                    Rule::AssignAdd => BinaryOperator::AssignAdd,
                    Rule::AssignSubtract => BinaryOperator::AssignSubtract,
                    Rule::AssignMultiply => BinaryOperator::AssignMultiply,
                    Rule::AssignDivide => BinaryOperator::AssignDivide,
                    Rule::AssignModulus => BinaryOperator::AssignModulus,
                    Rule::AssignLeftShift => BinaryOperator::AssignLeftShift,
                    Rule::AssignRightShift => BinaryOperator::AssignRightShift,
                    Rule::AssignBitwiseAnd => BinaryOperator::AssignBitwiseAnd,
                    Rule::AssignBitwiseXor => BinaryOperator::AssignBitwiseXor,
                    Rule::AssignBitwiseOr => BinaryOperator::AssignBitwiseOr,
                    _ => unreachable!(),
                };
                Expression::Binary {
                    operator,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
        })
        .parse(pairs)
}

fn parse_character(pair: Pair<Rule>) -> i8 {
    let pair_str = pair.as_str();
    let mut pair_str_inner = pair_str.chars().skip(1).take(pair_str.len() - 2);
    let first = pair_str_inner.next().unwrap();
    if first == '\\' {
        parse_escaped_char(pair_str_inner) as i8
    } else {
        first as i8
    }
}

fn parse_string(string_pair: Pair<Rule>) -> Vec<u8> {
    let string_pair_str = string_pair.as_str();
    let mut result = Vec::with_capacity(string_pair_str.len());
    let mut chars = string_pair_str[1..(string_pair_str.len() - 1)].chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                result.push(parse_escaped_char(chars.by_ref()));
            }
            _ => result.push(c as u8),
        }
    }
    result
}

fn parse_escaped_char<I: Iterator<Item = char>>(mut chars: I) -> u8 {
    // https://en.wikipedia.org/wiki/Escape_sequences_in_C
    // https://www.sweetscape.com/010editor/manual/ArraysStrings.htm
    let c = chars.next().unwrap();
    match c {
        'a' => 0x7,
        'b' => 0x8,
        'e' => 0x1b,
        'f' => 0xc,
        'n' => b'\n',
        'r' => b'\r',
        't' => b'\t',
        'v' => 0xb,
        '\\' => b'\\',
        '\'' => b'\'',
        '"' => b'"',
        '?' => b'?',
        'x' => {
            let digits = chars.by_ref().take(2).collect::<String>();
            u8::from_str_radix(&digits, 16).unwrap()
        }
        a => {
            // Assume octal
            let digits = std::iter::once(a)
                .chain(chars.by_ref().take(2))
                .collect::<String>();
            u8::from_str_radix(&digits, 8).unwrap()
        }
    }
}

fn parse_number(number_pair: Pair<Rule>) -> ParseResult<Number> {
    let type_pair = number_pair.into_inner().next().unwrap();
    Ok(match type_pair.as_rule() {
        Rule::UnsignedLongIntNumber => {
            Number::U64(parse_int_number(inner_pair(inner_pair(type_pair)))?)
        }
        Rule::LongIntNumber => Number::I64(parse_int_number(inner_pair(inner_pair(type_pair)))?),
        Rule::UnsignedIntNumber => {
            Number::U32(parse_int_number(inner_pair(inner_pair(type_pair)))?)
        }
        Rule::FloatNumber => Number::F64(parse_float_number(type_pair)?),
        Rule::SmallFloatNumber => Number::F32(parse_float_number(inner_pair(type_pair))?),
        Rule::IntNumber => Number::I32(parse_int_number(inner_pair(type_pair))?),
        Rule::Boolean => Number::Bool(match type_pair.into_inner().next().unwrap().as_rule() {
            Rule::True => true,
            Rule::False => false,
            _ => unreachable!(),
        }),
        _ => panic!(
            "Unexpected number type {:?} {:?}",
            type_pair.as_rule(),
            type_pair.as_str()
        ),
    })
}

fn parse_int_number<T>(inner: Pair<Rule>) -> ParseResult<T>
where
    T: Num<FromStrRadixErr = std::num::ParseIntError> + FromStr<Err = std::num::ParseIntError>,
{
    let inner_str = inner.as_str();
    Ok(match inner.as_rule() {
        Rule::DecimalNumber => inner_str.parse::<T>()?,
        Rule::BinaryNumber => T::from_str_radix(&inner_str[2..], 2)?,
        Rule::HexNumber => {
            if let Some(inner_str) = inner_str.strip_prefix("0x") {
                T::from_str_radix(inner_str, 16)?
            } else {
                // Ends in 'h'. 010 quirk
                T::from_str_radix(&inner_str[..(inner_str.len() - 1)], 16)?
            }
        }
        Rule::OctalNumber => {
            if let Some(inner_str) = inner_str.strip_prefix("0o") {
                T::from_str_radix(inner_str, 8)?
            } else {
                // Starts with just '0'. 010 quirk
                T::from_str_radix(&inner_str[1..], 8)?
            }
        }
        _ => panic!(
            "Unexpected integer number: {:?} {:?}",
            inner.as_rule(),
            inner_str,
        ),
    })
}

fn parse_float_number<T>(inner: Pair<Rule>) -> ParseResult<T>
where
    T: FromStr<Err = std::num::ParseFloatError>,
{
    let inner_str = inner.as_str();
    Ok(match inner.as_rule() {
        Rule::FloatNumber => inner.as_str().parse::<T>()?,
        _ => panic!(
            "Unexpected float number: {:?} {:?}",
            inner.as_rule(),
            inner_str,
        ),
    })
}

fn inner_pair(pair: Pair<Rule>) -> Pair<Rule> {
    pair.into_inner().next().unwrap()
}

impl TypeDeclaration {
    pub fn name(&self) -> &str {
        match self {
            Self::Normal(name) => name,
            Self::Array { name, .. } => name,
            Self::UnsizedArray { name } => name,
        }
    }
}
