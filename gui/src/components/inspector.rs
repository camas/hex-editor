use sycamore::prelude::*;

use crate::{DataInfo, Selection};

#[component]
pub fn Inspector<G: Html>(cx: Scope) -> View<G> {
    let data_info_signal = use_context::<Signal<DataInfo>>(cx);
    let selection_signal = use_context::<Signal<Option<Selection>>>(cx);

    let inspector_data = create_memo(cx, || {
        let data_info = data_info_signal.get();
        let selection = selection_signal.get();

        if selection.is_none() {
            return InspectorData::default();
        }

        let selection = selection.as_ref().as_ref().unwrap();
        let fetch_end = (selection.start + 0x40).min(data_info.provider.len());
        let data = data_info.provider.get_range(selection.start, fetch_end);

        macro_rules! get_primitive {
            ($type:ty, $byte_count:expr) => {
                if data.len() >= $byte_count {
                    Some(
                        <$type>::from_le_bytes(data[0..$byte_count].try_into().unwrap())
                            .to_string(),
                    )
                } else {
                    None
                }
            };
        }

        let bits = if !data.is_empty() {
            Some(format!("{:08b}", data[0]))
        } else {
            None
        };
        let u8 = if !data.is_empty() {
            Some(data[0].to_string())
        } else {
            None
        };
        let i8 = if !data.is_empty() {
            Some((data[0] as i8).to_string())
        } else {
            None
        };
        let u16 = get_primitive!(u16, 2);
        let i16 = get_primitive!(i16, 2);
        let u32 = get_primitive!(u32, 4);
        let i32 = get_primitive!(i32, 4);
        let u64 = get_primitive!(u64, 8);
        let i64 = get_primitive!(i64, 8);
        let u128 = get_primitive!(u128, 16);
        let i128 = get_primitive!(i128, 16);
        let f32 = get_primitive!(f32, 4);
        let f64 = get_primitive!(f64, 8);

        let cstring = String::from_utf8_lossy(
            &data
                .iter()
                .take_while(|&&v| v != 0)
                .copied()
                .collect::<Vec<_>>(),
        )
        .to_string();

        InspectorData {
            bits,
            u8,
            i8,
            u16,
            i16,
            u32,
            i32,
            u64,
            i64,
            u128,
            i128,
            f32,
            f64,
            cstring,
        }
    });

    view! {cx,
        div(class="inspector") {
            div(class="inspector-header") {"Inspector"}
            div(class="inspector-content") {
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"Binary"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().bits.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"u8"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().u8.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"i8"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().i8.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"u16"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().u16.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"i16"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().i16.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"u32"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().u32.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"i32"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().i32.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"u64"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().u64.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"i64"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().i64.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"u128"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().u128.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"i128"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().i128.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"f32"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().f32.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"f64"}
                    div(class="inspector-content-row-data"){(match inspector_data.get().f64.as_ref() {
                        Some(v) => v.to_string(),
                        None => "".to_string(),
                    })}
                }
                div(class="inspector-content-row") {
                    div(class="inspector-content-row-header"){"cstring"}
                    div(class="inspector-content-row-data"){(inspector_data.get().cstring)}
                }
            }
        }
    }
}

#[derive(Default)]
struct InspectorData {
    bits: Option<String>,
    u8: Option<String>,
    i8: Option<String>,
    u16: Option<String>,
    i16: Option<String>,
    u32: Option<String>,
    i32: Option<String>,
    u64: Option<String>,
    i64: Option<String>,
    u128: Option<String>,
    i128: Option<String>,
    f32: Option<String>,
    f64: Option<String>,
    cstring: String,
}
