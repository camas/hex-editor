use std::collections::HashMap;

use super::Object;

#[derive(Debug)]
pub struct ParsedObjects {
    pub(crate) objects: Vec<ParsedObject>,
    current_indent: u8,
    seen_names_stack: Vec<HashMap<String, u64>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedObject {
    pub name: String,
    pub value: Object,
    pub indent: u8,
    pub start: u64,
    pub end: u64,
    pub background_color: Option<u32>,
}

impl Default for ParsedObjects {
    fn default() -> Self {
        let seen_names_stack = vec![HashMap::new()];

        Self {
            objects: Default::default(),
            current_indent: Default::default(),
            seen_names_stack,
        }
    }
}

impl ParsedObjects {
    pub fn add(
        &mut self,
        variable_name: String,
        value: Object,
        start: u64,
        end: u64,
        background_color: Option<u32>,
    ) {
        // TODO: Colors
        // TODO: Store type (either in name or extra field)
        // TODO: Store array info
        self.objects.push(ParsedObject {
            name: variable_name,
            value,
            indent: self.current_indent,
            start,
            end,
            background_color,
        })
    }

    pub fn indent(&mut self) {
        self.current_indent += 1;
        self.seen_names_stack.push(HashMap::new());
    }

    pub fn unindent(&mut self) {
        self.current_indent -= 1;
        self.seen_names_stack.pop();
    }
}
