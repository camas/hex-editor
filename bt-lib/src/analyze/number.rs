use std::num::Wrapping;

use super::{AnalyzeError, AnalyzeResult};

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    // All integers are Wrapping to mimic C
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

macro_rules! unary_operation {
    ($func_name:ident, $operation:expr) => {
        pub fn $func_name(self) -> Number {
            match self {
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
        pub fn $func_name(self, other: Number) -> Number {
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
                (Number::I8(a), Number::I8(b)) => { Number::I8(a $operation b) }
            }
        }
    };
}

macro_rules! binary_shift_operation {
    ($operation_name:ident, $operation:tt) => {
        pub fn $operation_name(self, other: Number) -> AnalyzeResult<Number> {
            Ok(match (self, other) {
                (Number::F64(_) | Number::F32(_), _) => {
                    return Err(AnalyzeError::IntOperationOnFloat);
                }
                (Number::U64(a), b) | (b, Number::U64(a)) => Number::U64(a $operation b.as_u64() as usize),
                (Number::I64(a), b) | (b, Number::I64(a)) => Number::I64(a $operation b.as_u64() as usize),
                (Number::U32(a), b) | (b, Number::U32(a)) => Number::U32(a $operation b.as_u64() as usize),
                (Number::I32(a), b) | (b, Number::I32(a)) => Number::I32(a $operation b.as_u64() as usize),
                (Number::U16(a), b) | (b, Number::U16(a)) => Number::U16(a $operation b.as_u64() as usize),
                (Number::I16(a), b) | (b, Number::I16(a)) => Number::I16(a $operation b.as_u64() as usize),
                (Number::U8(a), b) | (b, Number::U8(a)) => Number::U8(a $operation b.as_u64() as usize),
                (Number::I8(a), b) => Number::I8(a $operation b.as_u64() as usize),
            })
        }
    };
}

macro_rules! binary_int_operation {
    ($operation_name:ident, $operation:tt) => {
        pub fn $operation_name(self, other: Number) -> AnalyzeResult<Number> {
            Ok(match (self, other) {
                (Number::F64(_) | Number::F32(_), _) => {
                    return Err(AnalyzeError::IntOperationOnFloat);
                }
                (Number::U64(a), b) | (b, Number::U64(a)) => Number::U64(a $operation Wrapping(b.as_u64())),
                (Number::I64(a), b) | (b, Number::I64(a)) => Number::I64(a $operation Wrapping(b.as_u64() as i64)),
                (Number::U32(a), b) | (b, Number::U32(a)) => Number::U32(a $operation  Wrapping(b.as_u64() as u32)),
                (Number::I32(a), b) | (b, Number::I32(a)) => Number::I32(a $operation  Wrapping(b.as_u64() as i32)),
                (Number::U16(a), b) | (b, Number::U16(a)) => Number::U16(a $operation Wrapping(b.as_u64() as u16)),
                (Number::I16(a), b) | (b, Number::I16(a)) => Number::I16(a $operation  Wrapping(b.as_u64() as i16)),
                (Number::U8(a), b) | (b, Number::U8(a)) => Number::U8(a $operation  Wrapping(b.as_u64() as u8)),
                (Number::I8(a), b) => Number::I8(a $operation  Wrapping(b.as_u64() as i8)),
            })
        }
    };
}

macro_rules! equality_operation {
    ($operation_name:ident, $operation:tt) => {
        pub fn $operation_name(self, other: Number) -> Number {
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

impl Number {
    pub fn bitsize(&self) -> u8 {
        match self {
            Number::U8(_) => 8,
            Number::I8(_) => 8,
            Number::U16(_) => 16,
            Number::I16(_) => 16,
            Number::U32(_) => 32,
            Number::I32(_) => 32,
            Number::U64(_) => 64,
            Number::I64(_) => 64,
            Number::F32(_) => 32,
            Number::F64(_) => 64,
        }
    }

    pub fn signed(&self) -> bool {
        match self {
            Number::U8(_) | Number::U16(_) | Number::U32(_) | Number::U64(_) => false,
            Number::I8(_)
            | Number::I16(_)
            | Number::I32(_)
            | Number::I64(_)
            | Number::F32(_)
            | Number::F64(_) => true,
        }
    }

    pub fn float(&self) -> bool {
        matches!(self, Number::F32(_) | Number::F64(_))
    }

    pub fn as_u64(&self) -> u64 {
        match self {
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

    pub fn as_f64(&self) -> f64 {
        match self {
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

    pub fn as_bool(&self) -> bool {
        match self {
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

    pub fn logical_not(self) -> Number {
        Number::U8(Wrapping((!self.as_bool()) as u8))
    }

    pub fn bitwise_not(self) -> AnalyzeResult<Number> {
        Ok(match self {
            Number::U8(v) => Number::U8(!v),
            Number::I8(v) => Number::I8(!v),
            Number::U16(v) => Number::U16(!v),
            Number::I16(v) => Number::I16(!v),
            Number::U32(v) => Number::U32(!v),
            Number::I32(v) => Number::I32(!v),
            Number::U64(v) => Number::U64(!v),
            Number::I64(v) => Number::I64(!v),
            Number::F32(_) | Number::F64(_) => {
                return Err(AnalyzeError::GenericError("can't bitwise not a float"))
            }
        })
    }

    pub fn logical_and(self, other: Number) -> Number {
        Number::U8(Wrapping((self.as_bool() && other.as_bool()) as u8))
    }

    pub fn logical_or(self, other: Number) -> Number {
        Number::U8(Wrapping((self.as_bool() && other.as_bool()) as u8))
    }
}
