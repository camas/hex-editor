use std::{collections::HashMap, rc::Rc};

use crate::parse::{BasicObject, ObjectRef};

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
        // TODO: This is really ugly. ObjectRef needs a rework
        if let Some(last) = self.objects.last_mut() {
            if last.name == variable_name && last.end + 1 == start {
                let last_object_ref = last.value.as_object_ref();
                if last_object_ref == value.as_object_ref()
                    && matches!(last_object_ref, ObjectRef::Basic(_))
                {
                    let new_object = match last_object_ref {
                        ObjectRef::Basic(basic) => match basic {
                            BasicObject::U8 => {
                                Object::U8Array(Rc::new(vec![last.value.get_u8(), value.get_u8()]))
                            }
                            BasicObject::I8 => {
                                Object::I8Array(Rc::new(vec![last.value.get_i8(), value.get_i8()]))
                            }
                            BasicObject::U16 => Object::U16Array(Rc::new(vec![
                                last.value.get_u16(),
                                value.get_u16(),
                            ])),
                            BasicObject::I16 => Object::I16Array(Rc::new(vec![
                                last.value.get_i16(),
                                value.get_i16(),
                            ])),
                            BasicObject::U32 => Object::U32Array(Rc::new(vec![
                                last.value.get_u32(),
                                value.get_u32(),
                            ])),
                            BasicObject::I32 => Object::I32Array(Rc::new(vec![
                                last.value.get_i32(),
                                value.get_i32(),
                            ])),
                            BasicObject::U64 => Object::U64Array(Rc::new(vec![
                                last.value.get_u64(),
                                value.get_u64(),
                            ])),
                            BasicObject::I64 => Object::I64Array(Rc::new(vec![
                                last.value.get_i64(),
                                value.get_i64(),
                            ])),
                            BasicObject::F32 => Object::F32Array(Rc::new(vec![
                                last.value.get_f32(),
                                value.get_f32(),
                            ])),
                            BasicObject::F64 => Object::F64Array(Rc::new(vec![
                                last.value.get_f64(),
                                value.get_f64(),
                            ])),
                            _ => todo!(),
                        },
                        _ => unreachable!(),
                    };
                    last.value = new_object;
                    last.end = end;
                    return;
                }
                if let ObjectRef::Array(array_type) = last_object_ref {
                    if let ObjectRef::Basic(value_basic_type) = value.as_object_ref() {
                        if array_type == value_basic_type {
                            // TODO: don't clone an entire vec every time we want to add a value
                            let new_value = match &last.value {
                                Object::U8Array(v) => Object::U8Array(Rc::new(
                                    [(**v).clone(), vec![value.get_u8()]].concat(),
                                )),
                                Object::I8Array(v) => Object::I8Array(Rc::new(
                                    [(**v).clone(), vec![value.get_i8()]].concat(),
                                )),
                                Object::U16Array(v) => Object::U16Array(Rc::new(
                                    [(**v).clone(), vec![value.get_u16()]].concat(),
                                )),
                                Object::I16Array(v) => Object::I16Array(Rc::new(
                                    [(**v).clone(), vec![value.get_i16()]].concat(),
                                )),
                                Object::U32Array(v) => Object::U32Array(Rc::new(
                                    [(**v).clone(), vec![value.get_u32()]].concat(),
                                )),
                                Object::I32Array(v) => Object::I32Array(Rc::new(
                                    [(**v).clone(), vec![value.get_i32()]].concat(),
                                )),
                                Object::U64Array(v) => Object::U64Array(Rc::new(
                                    [(**v).clone(), vec![value.get_u64()]].concat(),
                                )),
                                Object::I64Array(v) => Object::I64Array(Rc::new(
                                    [(**v).clone(), vec![value.get_i64()]].concat(),
                                )),
                                Object::F32Array(v) => Object::F32Array(Rc::new(
                                    [(**v).clone(), vec![value.get_f32()]].concat(),
                                )),
                                Object::F64Array(v) => Object::F64Array(Rc::new(
                                    [(**v).clone(), vec![value.get_f64()]].concat(),
                                )),
                                _ => todo!(),
                            };
                            last.value = new_value;
                            last.end = end;
                            return;
                        }
                    }
                }
            }
        }

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
