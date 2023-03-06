use std::borrow::Borrow;

use bt_lib::{
    analyze::AnalyzedData,
    object::{NumberArray, Object},
    ParsedObject,
};
use sycamore::prelude::*;
use uuid::Uuid;

use crate::{DataInfo, Invalidate, RunRequested};

const ARRAY_DATA_LIMIT: u64 = 20;

#[component]
pub fn ParsedView<G: Html>(cx: Scope) -> View<G> {
    let data_info_signal = use_context::<Signal<DataInfo>>(cx);
    let analyzed_data_signal = use_context::<Signal<AnalyzedData>>(cx);
    let invalidate_signal = use_context::<Signal<Invalidate>>(cx);
    let run_requested_signal = use_context::<Signal<RunRequested>>(cx);

    let row_data_signal = create_signal(cx, Vec::new());

    let run_clicked = |_| {
        run_requested_signal.trigger_subscribers();
    };

    create_effect(cx, || {
        let data_info = data_info_signal.get_untracked();
        let results = analyzed_data_signal.get();

        let uuid = Uuid::new_v4();
        row_data_signal.set(
            results
                .parsed_objects
                .iter()
                .map(|v| RowData::from_parsed_object(v, uuid, data_info.borrow()))
                .collect(),
        );
        invalidate_signal.modify().invalidate();
    });

    view! {cx,
        div(class="parsed") {
            div(class="parsed-title"){
                div(class="parsed-title-text"){("Template Results")}
                div(
                    class="parsed-title-run-button",
                    on:click=run_clicked,
                ) {("Run")}
            }
            div(class="parsed-grid") {
                div(class="parsed-header") {
                    div(){("Name")}
                    div(){("Value")}
                    div(){("Type")}
                    div(){("Offset")}
                    div(){("Size")}
                    div(){("Color")}
                }
                Keyed(
                    iterable=row_data_signal,
                    view=|cx, r| parsed_row(cx, r),
                    key=|x| format!("{}-{}-{}",x.result_uuid, x.start, x.name),
                )
            }
        }
    }
}

fn parsed_row<G: Html>(cx: Scope, row_info: RowData) -> View<G> {
    let name = row_info.name.clone();
    let type_string = row_info.type_string.clone();
    let value = row_info.value.clone();
    let bg_color = match row_info.bg_color {
        Some(v) => format!("background-color: #{:06X}", v),
        None => "".to_string(),
    };
    view! {cx,
        div(){(name)}
        div(class="parsed-row-value"){(value)}
        div(){(type_string)}
        div(){(format!("{:#x} - {:#x}", row_info.start, row_info.end))}
        div(){(row_info.size().to_string())}
        div(class="parsed-row-color") {
            div(style=bg_color)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct RowData {
    result_uuid: Uuid,
    start: u64,
    end: u64,
    name: String,
    type_string: String,
    value: String,
    bg_color: Option<u32>,
}

impl RowData {
    fn from_parsed_object(
        parsed_object: &ParsedObject,
        result_uuid: Uuid,
        data_info: &DataInfo,
    ) -> RowData {
        let (type_string, value) = object_to_string_data(&parsed_object.value, data_info);
        RowData {
            result_uuid,
            start: parsed_object.start,
            end: parsed_object.end,
            name: parsed_object.name.clone(),
            type_string,
            value,
            bg_color: parsed_object.color.map(|v| v.0),
        }
    }

    fn size(&self) -> u64 {
        self.end - self.start + 1
    }
}

fn object_to_string_data(object: &Object, data_info: &DataInfo) -> (String, String) {
    match object {
        Object::Number(number) => match number {
            bt_lib::Number::Char(v) => ("char".to_string(), (v.0 as char).to_string()),
            bt_lib::Number::U8(v) => ("u8".to_string(), v.to_string()),
            bt_lib::Number::I8(v) => ("i8".to_string(), v.to_string()),
            bt_lib::Number::U16(v) => ("u16".to_string(), v.to_string()),
            bt_lib::Number::I16(v) => ("i16".to_string(), v.to_string()),
            bt_lib::Number::U32(v) => ("u32".to_string(), v.to_string()),
            bt_lib::Number::I32(v) => ("i32".to_string(), v.to_string()),
            bt_lib::Number::U64(v) => ("u64".to_string(), v.to_string()),
            bt_lib::Number::I64(v) => ("i64".to_string(), v.to_string()),
            bt_lib::Number::F32(v) => ("f32".to_string(), format!("{:.8}", v)),
            bt_lib::Number::F64(v) => ("f64".to_string(), format!("{:.8}", v)),
        },
        bt_lib::Object::Array(NumberArray::Char(v)) => (
            format!("char[{}]", v.len()),
            String::from_utf8_lossy(&v.iter().cloned().take(10).collect::<Vec<_>>()).into_owned(),
        ),
        bt_lib::Object::Array(NumberArray::U8(v)) => {
            (format!("u8[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::Array(NumberArray::I8(v)) => {
            (format!("i8[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::Array(NumberArray::U16(v)) => {
            (format!("u16[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::Array(NumberArray::I16(v)) => {
            (format!("i16[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::Array(NumberArray::U32(v)) => {
            (format!("u32[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::Array(NumberArray::I32(v)) => {
            (format!("i32[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::Array(NumberArray::U64(v)) => {
            (format!("u64[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::Array(NumberArray::I64(v)) => {
            (format!("i64[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::Array(NumberArray::F32(v)) => {
            (format!("f32[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::Array(NumberArray::F64(v)) => {
            (format!("f64[{}]", v.len()), array_value_str(v))
        }
        bt_lib::Object::ArrayRef {
            number_type,
            start,
            size,
        } => {
            let object = bt_lib::resolve(
                *number_type,
                *start,
                (*size).min(ARRAY_DATA_LIMIT),
                &mut data_info.provider.get_reader_mut(),
            );
            (
                format!("{}[{}]", number_type.as_str(), size),
                object_to_string_data(&object, data_info).1,
            )
        }
        bt_lib::Object::Struct(_) => todo!(),
        bt_lib::Object::VariableRef(_)
        | bt_lib::Object::Void
        | bt_lib::Object::ArrayEntryRef { .. }
        | bt_lib::Object::TempArray(_) => unreachable!(),
    }
}

fn array_value_str<T>(values: &[T]) -> String
where
    T: ToString,
{
    values
        .iter()
        .take(ARRAY_DATA_LIMIT as usize)
        .map(|v| v.to_string())
        .intersperse(", ".to_string())
        .collect()
}
