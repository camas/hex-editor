#![feature(drain_filter)]
#![feature(let_chains)]

#[macro_use]
extern crate pest_derive;

use std::io::SeekFrom;

use common::{DataSourceTraits, Reader};
use parse::ParseError;
use transform::{BtProgram, TransformError};

pub mod analyze;
mod instruction;
pub mod object;
pub mod parse;
pub mod transform;

pub use analyze::analyze_data;
pub use analyze::parsed::ParsedObject;
pub use object::number::Number;
pub use object::Object;

use crate::object::{number::NumberType, NumberArray};

#[derive(thiserror::Error, Debug)]
pub enum CompileError {
    #[error("Error during parsing: {0}")]
    Parse(#[from] ParseError),
    #[error("Error during transform: {0}")]
    Transform(#[from] TransformError),
}

pub fn compile(input: &str) -> Result<BtProgram, CompileError> {
    Ok(transform::transform(parse::parse_bt(input)?)?)
}

pub fn resolve(
    number_type: NumberType,
    start: u64,
    size: u64,
    data: &mut Box<dyn DataSourceTraits>,
) -> Object {
    // TODO: Bit of a hack

    let mut reader = Reader::new(data);
    reader.seek(SeekFrom::Start(start)).unwrap();
    macro_rules! read_array {
        ($func:ident) => {
            (0..size)
                .map(|_| reader.$func())
                .collect::<std::io::Result<Vec<_>>>()
                .unwrap()
        };
    }
    // TODO: Handle endianness somehow
    Object::Array(match number_type {
        NumberType::Char => NumberArray::Char(read_array!(read_u8)),
        NumberType::U8 => NumberArray::U8(read_array!(read_u8)),
        NumberType::I8 => NumberArray::I8(read_array!(read_i8)),
        NumberType::U16 => NumberArray::U16(read_array!(read_u16)),
        NumberType::I16 => NumberArray::I16(read_array!(read_i16)),
        NumberType::U32 => NumberArray::U32(read_array!(read_u32)),
        NumberType::I32 => NumberArray::I32(read_array!(read_i32)),
        NumberType::U64 => NumberArray::U64(read_array!(read_u64)),
        NumberType::I64 => NumberArray::I64(read_array!(read_i64)),
        NumberType::F32 => NumberArray::F32(read_array!(read_f32)),
        NumberType::F64 => NumberArray::F64(read_array!(read_f64)),
    })
}
