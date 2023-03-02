use std::num::Wrapping;

use crate::instruction::VariableRef;

use self::number::Number;

pub(crate) mod number;

#[derive(thiserror::Error, Debug)]
pub enum ObjectError {
    // TODO: Replace with actual errors instead of being lazy
    #[error("{0}")]
    GenericError(&'static str),
    #[error("Int operation on float")]
    IntOperationOnFloat,
    #[error("Can't {0} on {1:?}")]
    InvalidUnaryOperation(&'static str, Object),
    #[error("Can't {0} on {1:?} and {2:?}")]
    InvalidBinaryOperation(&'static str, Object, Object),
    #[error("Can't add {0:?} to {1:?}")]
    InvalidAddType(Object, Object),
    #[error("Invalid cast target {0:?}")]
    InvalidCastTarget(ObjectType),
}

type ObjectResult<T> = Result<T, ObjectError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Void,
    Number(Number),
    Array(NumberArray),
    ArrayRef {
        number_type: Number,
        start: u64,
        size: u64,
    },
    VariableRef(VariableRef),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ObjectType {
    Void,
    Number(NumberType),
    Array(NumberType),
    ArrayRef(NumberType),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumberType {
    Char,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberArray {
    Char(Vec<u8>),
    U8(Vec<u8>),
    I8(Vec<i8>),
    U16(Vec<u16>),
    I16(Vec<i16>),
    U32(Vec<u32>),
    I32(Vec<i32>),
    U64(Vec<u64>),
    I64(Vec<i64>),
    F32(Vec<f32>),
    F64(Vec<f64>),
}

macro_rules! unary_number_operation {
    ($operation_name:ident) => {
        pub(crate) fn $operation_name(self) -> ObjectResult<Object> {
            Ok(match self {
                Object::Number(a) => Object::Number(Number::$operation_name(a)),
                s => {
                    return Err(ObjectError::InvalidUnaryOperation(
                        stringify!($operation_name),
                        s,
                    ))
                }
            })
        }
    };
}

macro_rules! unary_number_result_operation {
    ($operation_name:ident) => {
        pub(crate) fn $operation_name(self) -> ObjectResult<Object> {
            Ok(match self {
                Object::Number(a) => Object::Number(Number::$operation_name(a)?),
                s => {
                    return Err(ObjectError::InvalidUnaryOperation(
                        stringify!($operation_name),
                        s,
                    ))
                }
            })
        }
    };
}

macro_rules! binary_number_operation {
    ($operation_name:ident) => {
        pub(crate) fn $operation_name(self, other: Object) -> ObjectResult<Object> {
            Ok(match (self, other) {
                (Object::Number(a), Object::Number(b)) => Object::Number(a.$operation_name(b)),
                (s, o) => {
                    return Err(ObjectError::InvalidBinaryOperation(
                        stringify!($operation_name),
                        s,
                        o,
                    ))
                }
            })
        }
    };
}

macro_rules! binary_number_result_operation {
    ($operation_name:ident) => {
        pub(crate) fn $operation_name(self, other: Object) -> ObjectResult<Object> {
            Ok(match (self, other) {
                (Object::Number(a), Object::Number(b)) => Object::Number(a.$operation_name(b)?),
                (s, o) => {
                    return Err(ObjectError::InvalidBinaryOperation(
                        stringify!($operation_name),
                        s,
                        o,
                    ))
                }
            })
        }
    };
}

macro_rules! object_num_init {
    ($name:ident, $type:ty, $num_type:ident) => {
        pub(crate) fn $name(value: $type) -> Object {
            Object::Number(Number::$num_type(Wrapping(value)))
        }
    };
}

macro_rules! get_num {
    ($fn_name:ident, $return_type:ty, $num_type:ident) => {
        pub(crate) fn $fn_name(&self) -> $return_type {
            match self {
                Object::Number(Number::$num_type(v)) => v.0,
                _ => unreachable!(),
            }
        }
    };
}

macro_rules! get_numf {
    ($fn_name:ident, $return_type:ty, $num_type:ident) => {
        pub(crate) fn $fn_name(&self) -> $return_type {
            match self {
                Object::Number(Number::$num_type(v)) => *v,
                _ => unreachable!(),
            }
        }
    };
}

impl Object {
    object_num_init!(new_char, u8, Char);
    object_num_init!(new_u8, u8, U8);
    object_num_init!(new_i8, i8, I8);
    object_num_init!(new_u16, u16, U16);
    object_num_init!(new_i16, i16, I16);
    object_num_init!(new_u32, u32, U32);
    object_num_init!(new_i32, i32, I32);
    object_num_init!(new_u64, u64, U64);
    object_num_init!(new_i64, i64, I64);

    pub(crate) fn new_f32(value: f32) -> Object {
        Object::Number(Number::F32(value))
    }

    pub(crate) fn new_f64(value: f64) -> Object {
        Object::Number(Number::F64(value))
    }

    pub(crate) fn as_type(&self) -> ObjectType {
        match self {
            Object::Void => ObjectType::Void,
            Object::Number(number) => ObjectType::Number(match number {
                Number::Char(_) => NumberType::Char,
                Number::U8(_) => NumberType::U8,
                Number::I8(_) => NumberType::I8,
                Number::U16(_) => NumberType::U16,
                Number::I16(_) => NumberType::I16,
                Number::U32(_) => NumberType::U32,
                Number::I32(_) => NumberType::I32,
                Number::U64(_) => NumberType::U64,
                Number::I64(_) => NumberType::I64,
                Number::F32(_) => NumberType::F32,
                Number::F64(_) => NumberType::F64,
            }),
            Object::Array(number_array) => ObjectType::Array(match number_array {
                NumberArray::Char(_) => NumberType::Char,
                NumberArray::U8(_) => NumberType::U8,
                NumberArray::I8(_) => NumberType::I8,
                NumberArray::U16(_) => NumberType::U16,
                NumberArray::I16(_) => NumberType::I16,
                NumberArray::U32(_) => NumberType::U32,
                NumberArray::I32(_) => NumberType::I32,
                NumberArray::U64(_) => NumberType::U64,
                NumberArray::I64(_) => NumberType::I64,
                NumberArray::F32(_) => NumberType::F32,
                NumberArray::F64(_) => NumberType::F64,
            }),
            Object::ArrayRef { number_type, .. } => ObjectType::Array(match number_type {
                Number::Char(_) => NumberType::Char,
                Number::U8(_) => NumberType::U8,
                Number::I8(_) => NumberType::I8,
                Number::U16(_) => NumberType::U16,
                Number::I16(_) => NumberType::I16,
                Number::U32(_) => NumberType::U32,
                Number::I32(_) => NumberType::I32,
                Number::U64(_) => NumberType::U64,
                Number::I64(_) => NumberType::I64,
                Number::F32(_) => NumberType::F32,
                Number::F64(_) => NumberType::F64,
            }),
            Object::VariableRef(_) => todo!(),
        }
    }

    pub(crate) fn cast(self, target_type: ObjectType) -> ObjectResult<Object> {
        Ok(match (self, target_type) {
            (Object::Number(number), ObjectType::Number(target_number_type)) => {
                Object::Number(number.cast(target_number_type))
            }
            _ => return Err(ObjectError::InvalidCastTarget(target_type)),
        })
    }

    pub(crate) fn as_string(&self) -> String {
        match self {
            Object::Number(number) => match number {
                Number::U8(v) => v.0.to_string(),
                Number::I8(v) => v.0.to_string(),
                Number::U16(v) => v.0.to_string(),
                Number::I16(v) => v.0.to_string(),
                Number::U32(v) => v.0.to_string(),
                Number::I32(v) => v.0.to_string(),
                Number::U64(v) => v.0.to_string(),
                Number::I64(v) => v.0.to_string(),
                Number::F32(v) => v.to_string(),
                Number::F64(v) => v.to_string(),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    pub(crate) fn as_hex_string(&self) -> String {
        match self {
            Object::Number(number) => match number {
                Number::Char(v) => format!("{:x}", v.0),
                Number::U8(v) => format!("{:x}", v.0),
                Number::I8(v) => format!("{:x}", v.0),
                Number::U16(v) => format!("{:x}", v.0),
                Number::I16(v) => format!("{:x}", v.0),
                Number::U32(v) => format!("{:x}", v.0),
                Number::I32(v) => format!("{:x}", v.0),
                Number::U64(v) => format!("{:x}", v.0),
                Number::I64(v) => format!("{:x}", v.0),
                Number::F32(_) => todo!(),
                Number::F64(_) => todo!(),
            },
            _ => todo!(),
        }
    }

    pub(crate) fn as_upper_hex_string(&self) -> String {
        match self {
            Object::Number(number) => match number {
                Number::Char(v) => format!("{:X}", v.0),
                Number::U8(v) => format!("{:X}", v.0),
                Number::I8(v) => format!("{:X}", v.0),
                Number::U16(v) => format!("{:X}", v.0),
                Number::I16(v) => format!("{:X}", v.0),
                Number::U32(v) => format!("{:X}", v.0),
                Number::I32(v) => format!("{:X}", v.0),
                Number::U64(v) => format!("{:X}", v.0),
                Number::I64(v) => format!("{:X}", v.0),
                Number::F32(_) => todo!(),
                Number::F64(_) => todo!(),
            },
            _ => todo!(),
        }
    }

    pub(crate) fn as_octal_string(&self) -> String {
        match self {
            Object::Number(number) => match number {
                Number::Char(v) => format!("{:o}", v.0),
                Number::U8(v) => format!("{:o}", v.0),
                Number::I8(v) => format!("{:o}", v.0),
                Number::U16(v) => format!("{:o}", v.0),
                Number::I16(v) => format!("{:o}", v.0),
                Number::U32(v) => format!("{:o}", v.0),
                Number::I32(v) => format!("{:o}", v.0),
                Number::U64(v) => format!("{:o}", v.0),
                Number::I64(v) => format!("{:o}", v.0),
                Number::F32(_) => todo!(),
                Number::F64(_) => todo!(),
            },
            _ => todo!(),
        }
    }

    get_num!(get_u8, u8, U8);
    get_num!(get_i8, i8, I8);
    get_num!(get_u16, u16, U16);
    get_num!(get_i16, i16, I16);
    get_num!(get_u32, u32, U32);
    get_num!(get_i32, i32, I32);
    get_num!(get_u64, u64, U64);
    get_num!(get_i64, i64, I64);
    get_numf!(get_f32, f32, F32);
    get_numf!(get_f64, f64, F64);

    pub(crate) fn add(self, other: Object) -> ObjectResult<Object> {
        Ok(match (self, other) {
            (Object::Number(a), Object::Number(b)) => Object::Number(a.add(b)),
            (Object::Array(NumberArray::Char(mut a)), Object::Array(NumberArray::Char(b))) => {
                a.extend(b);
                Object::Array(NumberArray::Char(a))
            }
            (s, o) => return Err(ObjectError::InvalidAddType(s, o)),
        })
    }

    unary_number_operation!(negate);
    unary_number_operation!(logical_not);
    unary_number_result_operation!(bitwise_not);
    binary_number_operation!(multiply);
    binary_number_operation!(divide);
    binary_number_operation!(modulus);
    binary_number_operation!(subtract);
    binary_number_result_operation!(left_shift);
    binary_number_result_operation!(right_shift);
    binary_number_operation!(less_than);
    binary_number_operation!(less_than_or_equal);
    binary_number_operation!(more_than);
    binary_number_operation!(more_than_or_equal);
    binary_number_operation!(equal);
    binary_number_operation!(not_equal);
    binary_number_result_operation!(bitwise_and);
    binary_number_result_operation!(bitwise_xor);
    binary_number_result_operation!(bitwise_or);
    binary_number_operation!(logical_and);
    binary_number_operation!(logical_or);
}

impl ObjectType {
    pub(crate) fn create_default(&self) -> ObjectResult<Object> {
        Ok(match self {
            ObjectType::Void => Object::Void,
            ObjectType::Number(number_type) => Object::Number(match number_type {
                NumberType::Char => Number::Char(Wrapping(0)),
                NumberType::U8 => Number::U8(Wrapping(0)),
                NumberType::I8 => Number::I8(Wrapping(0)),
                NumberType::U16 => Number::U16(Wrapping(0)),
                NumberType::I16 => Number::I16(Wrapping(0)),
                NumberType::U32 => Number::U32(Wrapping(0)),
                NumberType::I32 => Number::I32(Wrapping(0)),
                NumberType::U64 => Number::U64(Wrapping(0)),
                NumberType::I64 => Number::I64(Wrapping(0)),
                NumberType::F32 => Number::F32(0.),
                NumberType::F64 => Number::F64(0.),
            }),
            ObjectType::Array(NumberType::Char) => Object::Array(NumberArray::Char(Vec::new())),
            _ => return Err(ObjectError::GenericError("can't create default type")),
        })
    }
}
