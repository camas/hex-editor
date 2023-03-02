use crate::object::{NumberType, ObjectType};

#[derive(Debug)]
pub enum Instruction {
    /// Pop single item from stack
    Pop,
    /// Pop to a variable
    PopVariable(VariableRef),
    // Push basic objects to stack
    PushVariable(VariableRef),
    PushString(Vec<u8>),
    PushWideString(Vec<u8>),
    PushBool(bool),
    PushChar(i8),
    PushU8(u8),
    PushU32(u32),
    PushI32(i32),
    PushU64(u64),
    PushI64(i64),
    PushF32(f32),
    PushF64(f64),
    /// Object declaration
    DeclareObject {
        variable_ref: VariableRef,
        object_type: ObjectType,
    },
    DeclareArray {
        variable_ref: VariableRef,
        number_type: NumberType,
    },
    /// Object declaration by reading from data
    ReadObject {
        name: String,
        variable_ref: VariableRef,
        object_type: ObjectType,
        arg_count: usize,
        attributes: Vec<Attribute>,
    },
    ReadArray {
        name: String,
        variable_ref: VariableRef,
        number_type: NumberType,
        attributes: Vec<Attribute>,
    },
    /// Cast top of stack to target object (Replaces)
    Cast(ObjectType),
    /// Call a function. Args are on stack
    CallFunction {
        function_ref: FunctionRef,
        arg_count: usize,
    },
    CallBasicFunction {
        basic_function: BasicFunction,
        arg_count: usize,
    },
    Return,
    ReturnVoid,
    /// Gets item from an array
    /// Top of stack is the array, second top the index
    GetArrayIndex,
    /// Gets the member of the item on top of the stack
    GetMember(String),
    /// Array declaration values. Top n stack objects are the values
    DeclareArrayValues(usize),
    // Unary instructions. Acts on top stack value
    SuffixIncrement,
    SuffixDecrement,
    PrefixIncrement,
    PrefixDecrement,
    Positive,
    Negate,
    UnaryLogicalNot,
    UnaryBitwiseNot,
    // Binary instructions. Acts on top two stack values
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
    Label(LabelRef),
    Jump(LabelRef),
    JumpTrue(LabelRef),
    JumpFalse(LabelRef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableRef(pub(crate) u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LabelRef(pub(crate) u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionRef(pub(crate) u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructRef(pub(crate) u64);

#[derive(Debug, Clone)]
pub enum Attribute {
    Color(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BasicFunction {
    Printf,
    Warning,
    LittleEndian,
    BigEndian,
}
