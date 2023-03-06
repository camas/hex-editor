use std::collections::HashMap;

use crate::{
    instruction::Color,
    object::{number::NumberType, NumberArray, ObjectType},
};

use super::Object;

#[derive(Debug)]
pub struct ParsedObjects {
    pub(crate) objects: Vec<ParsedObject>,
    current_indent: u8,
    seen_names_stack: Vec<HashMap<String, u64>>,
}

#[derive(Debug, Clone)]
pub struct ParsedObject {
    pub name: String,
    pub value: Object,
    pub indent: u8,
    pub start: u64,
    pub end: u64,
    pub color: Option<Color>,
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
        color: Option<Color>,
    ) {
        // Consolidates sequential basic types into arrays
        if let Some(last) = self.objects.last_mut() {
            if last.name == variable_name && last.end + 1 == start {
                match (last.value.as_type(), value.as_type()) {
                    (ObjectType::Number(number_type), ObjectType::Number(other))
                        if number_type == other =>
                    {
                        let new_value = Object::Array(match number_type {
                            NumberType::Char => {
                                NumberArray::Char(vec![last.value.get_u8(), value.get_u8()])
                            }
                            NumberType::U8 => {
                                NumberArray::U8(vec![last.value.get_u8(), value.get_u8()])
                            }
                            NumberType::I8 => {
                                NumberArray::I8(vec![last.value.get_i8(), value.get_i8()])
                            }
                            NumberType::U16 => {
                                NumberArray::U16(vec![last.value.get_u16(), value.get_u16()])
                            }
                            NumberType::I16 => {
                                NumberArray::I16(vec![last.value.get_i16(), value.get_i16()])
                            }
                            NumberType::U32 => {
                                NumberArray::U32(vec![last.value.get_u32(), value.get_u32()])
                            }
                            NumberType::I32 => {
                                NumberArray::I32(vec![last.value.get_i32(), value.get_i32()])
                            }
                            NumberType::U64 => {
                                NumberArray::U64(vec![last.value.get_u64(), value.get_u64()])
                            }
                            NumberType::I64 => {
                                NumberArray::I64(vec![last.value.get_i64(), value.get_i64()])
                            }
                            NumberType::F32 => {
                                NumberArray::F32(vec![last.value.get_f32(), value.get_f32()])
                            }
                            NumberType::F64 => {
                                NumberArray::F64(vec![last.value.get_f64(), value.get_f64()])
                            }
                        });
                        last.value = new_value;
                        last.end = end;
                        return;
                    }
                    (ObjectType::Array(number_type), ObjectType::Number(other))
                        if number_type == other =>
                    {
                        let last_array = match &mut last.value {
                            Object::Array(v) => v,
                            _ => unreachable!(),
                        };
                        match last_array {
                            NumberArray::Char(v) => v.push(value.get_u8()),
                            NumberArray::U8(v) => v.push(value.get_u8()),
                            NumberArray::I8(v) => v.push(value.get_i8()),
                            NumberArray::U16(v) => v.push(value.get_u16()),
                            NumberArray::I16(v) => v.push(value.get_i16()),
                            NumberArray::U32(v) => v.push(value.get_u32()),
                            NumberArray::I32(v) => v.push(value.get_i32()),
                            NumberArray::U64(v) => v.push(value.get_u64()),
                            NumberArray::I64(v) => v.push(value.get_i64()),
                            NumberArray::F32(v) => v.push(value.get_f32()),
                            NumberArray::F64(v) => v.push(value.get_f64()),
                        }
                        last.end = end;
                        return;
                    }
                    _ => {}
                }
            }
        }

        self.objects.push(ParsedObject {
            name: variable_name,
            value,
            indent: self.current_indent,
            start,
            end,
            color,
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
