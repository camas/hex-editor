use std::num::Wrapping;

use super::{ObjectError, ObjectResult};

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    // All integers are Wrapping to mimic C
    Char(Wrapping<u8>),
    U8(Wrapping<u8>),
    I8(Wrapping<i8>),
    U16(Wrapping<u16>),
    I16(Wrapping<i16>),
    U32(Wrapping<u32>),
    I32(Wrapping<i32>),
    U64(Wrapping<u64>),
    I64(Wrapping<i64>),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

macro_rules! unary_operation {
    ($func_name:ident, $operation:expr) => {
        pub(crate) fn $func_name(self) -> Number {
            match self {
                Number::Char(v) => Number::Char($operation(v)),
                Number::U8(v) => Number::U8($operation(v)),
                Number::I8(v) => Number::I8($operation(v)),
                Number::U16(v) => Number::U16($operation(v)),
                Number::I16(v) => Number::I16($operation(v)),
                Number::U32(v) => Number::U32($operation(v)),
                Number::I32(v) => Number::I32($operation(v)),
                Number::U64(v) => Number::U64($operation(v)),
                Number::I64(v) => Number::I64($operation(v)),
                Number::F32(v) => Number::F32($operation(v)),
                Number::F64(v) => Number::F64($operation(v)),
            }
        }
    };
}

macro_rules! binary_operation {
    ($func_name:ident, $operation:tt) => {
        pub(crate)  fn $func_name(self, other: Number) -> Number {
            // For integers use the highest bit count, preferring unsigned values
            // When adding float to int prefer float
            match (self, other) {
                (Number::F64(a), b) | (b, Number::F64(a)) => Number::F64(a $operation b.as_f64()),
                (Number::F32(a), b) | (b, Number::F32(a)) => Number::F32(a $operation b.as_f64() as f32),
                (Number::U64(a), b) | (b, Number::U64(a)) => Number::U64(a $operation Wrapping(b.as_u64())),
                (Number::I64(a), b) | (b, Number::I64(a)) => {
                    Number::I64(a $operation Wrapping(b.as_u64() as i64))
                }
                (Number::U32(a), b) | (b, Number::U32(a)) => {
                    Number::U32(a $operation Wrapping(b.as_u64() as u32))
                }
                (Number::I32(a), b) | (b, Number::I32(a)) => {
                    Number::I32(a $operation Wrapping(b.as_u64() as i32))
                }
                (Number::U16(a), b) | (b, Number::U16(a)) => {
                    Number::U16(a $operation Wrapping(b.as_u64() as u16))
                }
                (Number::I16(a), b) | (b, Number::I16(a)) => {
                    Number::I16(a $operation Wrapping(b.as_u64() as i16))
                }
                (Number::U8(a), b) | (b, Number::U8(a)) => {
                    Number::U8(a $operation Wrapping(b.as_u64() as u8))
                }
                (Number::I8(a), b) | (b, Number::I8(a)) => {
                    Number::I8(a $operation Wrapping(b.as_u64() as i8))
                }
                (Number::Char(a), Number::Char(b)) => { Number::Char(a $operation b) }
            }
        }
    };
}

macro_rules! binary_shift_operation {
    ($operation_name:ident, $operation:tt) => {
        pub(crate)  fn $operation_name(self, other: Number) -> ObjectResult<Number> {
            Ok(match (self, other) {
                (Number::F64(_) | Number::F32(_), _) => {
                    return Err(ObjectError::IntOperationOnFloat);
                }
                (Number::U64(a), b) | (b, Number::U64(a)) => Number::U64(a $operation b.as_u64() as usize),
                (Number::I64(a), b) | (b, Number::I64(a)) => Number::I64(a $operation b.as_u64() as usize),
                (Number::U32(a), b) | (b, Number::U32(a)) => Number::U32(a $operation b.as_u64() as usize),
                (Number::I32(a), b) | (b, Number::I32(a)) => Number::I32(a $operation b.as_u64() as usize),
                (Number::U16(a), b) | (b, Number::U16(a)) => Number::U16(a $operation b.as_u64() as usize),
                (Number::I16(a), b) | (b, Number::I16(a)) => Number::I16(a $operation b.as_u64() as usize),
                (Number::U8(a), b) | (b, Number::U8(a)) => Number::U8(a $operation b.as_u64() as usize),
                (Number::I8(a), b) | (b, Number::I8(a)) => Number::I8(a $operation b.as_u64() as usize),
                (Number::Char(a), b) => Number::Char(a $operation b.as_u64() as usize),
            })
        }
    };
}

macro_rules! binary_int_operation {
    ($operation_name:ident, $operation:tt) => {
        pub(crate)  fn $operation_name(self, other: Number) -> ObjectResult<Number> {
            Ok(match (self, other) {
                (Number::F64(_) | Number::F32(_), _) => {
                    return Err(ObjectError::IntOperationOnFloat);
                }
                (Number::U64(a), b) | (b, Number::U64(a)) => Number::U64(a $operation Wrapping(b.as_u64())),
                (Number::I64(a), b) | (b, Number::I64(a)) => Number::I64(a $operation Wrapping(b.as_u64() as i64)),
                (Number::U32(a), b) | (b, Number::U32(a)) => Number::U32(a $operation  Wrapping(b.as_u64() as u32)),
                (Number::I32(a), b) | (b, Number::I32(a)) => Number::I32(a $operation  Wrapping(b.as_u64() as i32)),
                (Number::U16(a), b) | (b, Number::U16(a)) => Number::U16(a $operation Wrapping(b.as_u64() as u16)),
                (Number::I16(a), b) | (b, Number::I16(a)) => Number::I16(a $operation  Wrapping(b.as_u64() as i16)),
                (Number::U8(a), b) | (b, Number::U8(a)) => Number::U8(a $operation  Wrapping(b.as_u64() as u8)),
                (Number::I8(a), b) | (b, Number::I8(a)) => Number::I8(a $operation  Wrapping(b.as_u64() as i8)),
                (Number::Char(a), b) => Number::Char(a $operation  Wrapping(b.as_u64() as u8)),
            })
        }
    };
}

macro_rules! equality_operation {
    ($operation_name:ident, $operation:tt) => {
        pub(crate)  fn $operation_name(self, other: Number) -> Number {
            let result = match (&self, &other) {
                (Number::F64(_) | Number::F32(_), _) | (_, Number::F64(_) | Number::F32(_)) => {
                    self.as_f64() $operation other.as_f64()
                }
                (_, _) => self.as_u64() $operation other.as_u64(),
            };
            Number::U8(Wrapping(result as u8))
        }
    };
}

macro_rules! get_type {
    ($fn_name:ident, $return_type:ty, $num_type:ident) => {
        pub(crate) fn $fn_name(&self) -> $return_type {
            match self {
                Number::$num_type(v) => *v,
                _ => unreachable!(),
            }
        }
    };
}

impl Number {
    pub(crate) fn as_u64(&self) -> u64 {
        match self {
            Number::Char(v) => v.0 as u64,
            Number::U8(v) => v.0 as u64,
            Number::I8(v) => v.0 as u64,
            Number::U16(v) => v.0 as u64,
            Number::I16(v) => v.0 as u64,
            Number::U32(v) => v.0 as u64,
            Number::I32(v) => v.0 as u64,
            Number::U64(v) => v.0,
            Number::I64(v) => v.0 as u64,
            Number::F32(v) => *v as u64,
            Number::F64(v) => *v as u64,
        }
    }

    pub(crate) fn as_f64(&self) -> f64 {
        match self {
            Number::Char(v) => v.0 as f64,
            Number::U8(v) => v.0 as f64,
            Number::I8(v) => v.0 as f64,
            Number::U16(v) => v.0 as f64,
            Number::I16(v) => v.0 as f64,
            Number::U32(v) => v.0 as f64,
            Number::I32(v) => v.0 as f64,
            Number::U64(v) => v.0 as f64,
            Number::I64(v) => v.0 as f64,
            Number::F32(v) => *v as f64,
            Number::F64(v) => *v,
        }
    }

    pub(crate) fn as_bool(&self) -> bool {
        match self {
            Number::Char(v) => v.0 != 0,
            Number::U8(v) => v.0 != 0,
            Number::I8(v) => v.0 != 0,
            Number::U16(v) => v.0 != 0,
            Number::I16(v) => v.0 != 0,
            Number::U32(v) => v.0 != 0,
            Number::I32(v) => v.0 != 0,
            Number::U64(v) => v.0 != 0,
            Number::I64(v) => v.0 != 0,
            Number::F32(v) => *v != 0.,
            Number::F64(v) => *v != 0.,
        }
    }

    pub(crate) fn cast(&self, target_type: NumberType) -> Number {
        match target_type {
            NumberType::Char => Number::Char(Wrapping(self.as_u64() as u8)),
            NumberType::U8 => Number::U8(Wrapping(self.as_u64() as u8)),
            NumberType::I8 => Number::I8(Wrapping(self.as_u64() as i8)),
            NumberType::U16 => Number::U16(Wrapping(self.as_u64() as u16)),
            NumberType::I16 => Number::I16(Wrapping(self.as_u64() as i16)),
            NumberType::U32 => Number::U32(Wrapping(self.as_u64() as u32)),
            NumberType::I32 => Number::I32(Wrapping(self.as_u64() as i32)),
            NumberType::U64 => Number::U64(Wrapping(self.as_u64())),
            NumberType::I64 => Number::I64(Wrapping(self.as_u64() as i64)),
            NumberType::F32 => Number::F32(self.as_f64() as f32),
            NumberType::F64 => Number::F64(self.as_f64()),
        }
    }

    unary_operation!(negate, std::ops::Neg::neg);
    binary_operation!(multiply, *);
    binary_operation!(divide, /);
    binary_operation!(modulus, %);
    binary_operation!(add, +);
    binary_operation!(subtract, -);
    binary_shift_operation!(left_shift, <<);
    binary_shift_operation!(right_shift, >>);
    equality_operation!(less_than, <);
    equality_operation!(less_than_or_equal, <=);
    equality_operation!(more_than, >);
    equality_operation!(more_than_or_equal, >=);
    equality_operation!(equal, ==);
    equality_operation!(not_equal, !=);
    binary_int_operation!(bitwise_and, &);
    binary_int_operation!(bitwise_xor, ^);
    binary_int_operation!(bitwise_or, |);

    pub(crate) fn logical_not(self) -> Number {
        Number::U8(Wrapping((!self.as_bool()) as u8))
    }

    pub(crate) fn bitwise_not(self) -> ObjectResult<Number> {
        Ok(match self {
            Number::Char(v) => Number::Char(!v),
            Number::U8(v) => Number::U8(!v),
            Number::I8(v) => Number::I8(!v),
            Number::U16(v) => Number::U16(!v),
            Number::I16(v) => Number::I16(!v),
            Number::U32(v) => Number::U32(!v),
            Number::I32(v) => Number::I32(!v),
            Number::U64(v) => Number::U64(!v),
            Number::I64(v) => Number::I64(!v),
            Number::F32(_) | Number::F64(_) => {
                return Err(ObjectError::GenericError("can't bitwise not a float"))
            }
        })
    }

    pub(crate) fn logical_and(self, other: Number) -> Number {
        Number::U8(Wrapping((self.as_bool() && other.as_bool()) as u8))
    }

    pub(crate) fn logical_or(self, other: Number) -> Number {
        Number::U8(Wrapping((self.as_bool() && other.as_bool()) as u8))
    }
}

impl NumberType {
    pub(crate) fn bitsize(&self) -> u8 {
        match self {
            NumberType::Char => 8,
            NumberType::U8 => 8,
            NumberType::I8 => 8,
            NumberType::U16 => 16,
            NumberType::I16 => 16,
            NumberType::U32 => 32,
            NumberType::I32 => 32,
            NumberType::U64 => 64,
            NumberType::I64 => 64,
            NumberType::F32 => 32,
            NumberType::F64 => 64,
        }
    }

    pub(crate) fn signed(&self) -> bool {
        match self {
            NumberType::Char
            | NumberType::U8
            | NumberType::U16
            | NumberType::U32
            | NumberType::U64 => false,
            NumberType::I8
            | NumberType::I16
            | NumberType::I32
            | NumberType::I64
            | NumberType::F32
            | NumberType::F64 => true,
        }
    }

    pub(crate) fn float(&self) -> bool {
        matches!(self, NumberType::F32 | NumberType::F64)
    }
}
