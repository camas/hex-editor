use crate::analyzer::{AnalyzerResult, ReadContext};

use super::Object;

pub struct ObjectDefinition {
    pub name: String,
    pub read_function: Box<dyn Fn(&mut ReadContext) -> AnalyzerResult<()>>,
    pub display_function: Option<Box<dyn Fn(&Object) -> String>>,
}
