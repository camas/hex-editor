use bt_lib::{
    analyze::{self, AnalyzedData},
    ParsedObject,
};
use sycamore::prelude::*;
use uuid::Uuid;

use crate::{Invalidate, RunRequested};

#[component]
pub fn ParsedView<G: Html>(cx: Scope) -> View<G> {
    let analyzed_data_signal = use_context::<Signal<AnalyzedData>>(cx);
    let invalidate_signal = use_context::<Signal<Invalidate>>(cx);
    let run_requested_signal = use_context::<Signal<RunRequested>>(cx);

    let row_data_signal = create_signal(cx, Vec::new());

    let run_clicked = |_| {
        run_requested_signal.trigger_subscribers();
    };

    create_effect(cx, || {
        let results = analyzed_data_signal.get();

        let uuid = Uuid::new_v4();
        row_data_signal.set(
            results
                .parsed_objects
                .iter()
                .map(|v| RowData::from_parsed_object(v, uuid))
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
    fn from_parsed_object(parsed_object: &ParsedObject, result_uuid: Uuid) -> RowData {
        let (type_string, value) = match &parsed_object.value {
            analyze::Object::Number(number) => match number {
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
            analyze::Object::CharArray(v) => (
                format!("char[{}]", v.len()),
                String::from_utf8_lossy(&v.iter().cloned().take(10).collect::<Vec<_>>())
                    .into_owned(),
            ),
            analyze::Object::U8Array(v) => (format!("u8[{}]", v.len()), array_value_str(v)),
            analyze::Object::I8Array(v) => (format!("i8[{}]", v.len()), array_value_str(v)),
            analyze::Object::U16Array(v) => (format!("u16[{}]", v.len()), array_value_str(v)),
            analyze::Object::I16Array(v) => (format!("i16[{}]", v.len()), array_value_str(v)),
            analyze::Object::U32Array(v) => (format!("u32[{}]", v.len()), array_value_str(v)),
            analyze::Object::I32Array(v) => (format!("i32[{}]", v.len()), array_value_str(v)),
            analyze::Object::U64Array(v) => (format!("u64[{}]", v.len()), array_value_str(v)),
            analyze::Object::I64Array(v) => (format!("i64[{}]", v.len()), array_value_str(v)),
            analyze::Object::F32Array(v) => (format!("f32[{}]", v.len()), array_value_str(v)),
            analyze::Object::F64Array(v) => (format!("f64[{}]", v.len()), array_value_str(v)),
            analyze::Object::VariableRef(_) | analyze::Object::Void => unreachable!(),
        };
        RowData {
            result_uuid,
            start: parsed_object.start,
            end: parsed_object.end,
            name: parsed_object.name.clone(),
            type_string,
            value,
            bg_color: parsed_object.background_color,
        }
    }

    fn size(&self) -> u64 {
        self.end - self.start + 1
    }
}

fn array_value_str<T>(values: &[T]) -> String
where
    T: ToString,
{
    const TAKE_COUNT: usize = 10;
    if values.len() < TAKE_COUNT {
        values
            .iter()
            .map(|v| v.to_string())
            .intersperse(", ".to_string())
            .collect()
    } else {
        values
            .iter()
            .take(TAKE_COUNT)
            .map(|v| v.to_string())
            .intersperse(", ".to_string())
            .chain(std::iter::once("...".to_string()))
            .collect()
    }
}
