#![feature(drain_filter)]
#![feature(let_chains)]

#[macro_use]
extern crate pest_derive;

use parse::ParseError;
use transform::{BtProgram, TransformError};

pub mod analyze;
pub mod parse;
pub mod transform;

pub use analyze::analyze_data;
pub use analyze::number::Number;
pub use analyze::parsed::ParsedObject;

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
