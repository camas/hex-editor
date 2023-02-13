#![feature(seek_stream_len)]

mod datasource;
mod reader;

pub use datasource::{DataSource, DataSourceTraits};
pub use reader::Reader;
