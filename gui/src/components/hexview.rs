use std::rc::Rc;

use bt_lib::analyze::AnalyzedData;
use sycamore::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{Element, Event, HtmlElement, HtmlInputElement, WheelEvent};

use crate::{
    lookups::{HEX_LOOKUP_LOWER, HEX_LOOKUP_UPPER, VISIBLE_ASCII_LOOKUP},
    use_data_from_js_file, DataInfo, Invalidate, Selection, BYTES_PER_ROW,
};

const ROW_HEIGHT_PX: u64 = 18;
const HEX_FORMAT: u128 = lexical::NumberFormatBuilder::new().radix(16).build();

#[component]
pub fn HexView<G: Html>(cx: Scope) -> View<G> {
    view! {cx,
        div(class="hexview") {
            Header{}
            Rows{}
        }
    }
}

#[component]
fn Header<G: Html>(cx: Scope) -> View<G> {
    let data_info_signal = use_context::<Signal<DataInfo>>(cx);
    let invalidate_signal = use_context::<Signal<Invalidate>>(cx);

    let name_signal = create_memo(cx, || {
        invalidate_signal.track();
        let data_info = data_info_signal.get();
        data_info.name.clone()
    });

    let file_input_ref = create_node_ref(cx);
    let filename_click = |_| {
        if let Some(file_input_node) = file_input_ref.try_get::<DomNode>() {
            if let Some(file_input_element) =
                file_input_node.to_web_sys().dyn_ref::<HtmlInputElement>()
            {
                file_input_element.click();
            }
        }
    };
    let file_input_changed = move |_| {
        // TODO: Use FileAPI

        let Some(file_input_node) = file_input_ref.try_get::<DomNode>()
            else { return };

        // Get single file from input
        let Some(file) = file_input_node
            .to_web_sys()
            .dyn_ref::<HtmlInputElement>()
            .and_then(|e| e.files())
            .filter(|fl| fl.length() >= 1)
            .and_then(|fl| fl.item(0))
            else { return };

        use_data_from_js_file(cx, file);
    };

    let offsets = View::new_fragment(
        (0..0x10)
            .map(|i| {
                let text = HEX_LOOKUP_LOWER[i];
                view! {cx, div(class="hexview-header-offset"){(text)}}
            })
            .collect(),
    );
    let text_offsets = View::new_fragment(
        "0123456789abcdef"
            .chars()
            .map(|i| {
                view! {cx, div(class="hexview-header-textoffset"){(i)}}
            })
            .collect(),
    );
    view! {cx,
        div(class="hexview-header-filename-row") {
            div(
                class="hexview-header-filename",
                on:click=filename_click,
            ) {(name_signal.get())}
            input(
                ref=file_input_ref,
                type="file",
                class="hexview-header-fileinput",
                on:change=file_input_changed,
            )
        }
        div(class="hexview-header-offsets-container") {
            div(class="hexview-header-offsets") {(offsets)}
            div(class="hexview-header-text-offsets") {(text_offsets)}
        }
    }
}

#[derive(Debug, PartialEq)]
struct RowDisplayInfo {
    hash: u64,
    offset: u64,
    offset_text: String,
    bytes: Rc<Vec<ByteDisplayInfo>>,
}

#[derive(Debug, PartialEq)]
struct ByteDisplayInfo {
    value: Option<u8>,
    color: Option<u32>,
}

impl RowDisplayInfo {
    fn new(
        row_offset: u64,
        data: &[u8],
        data_full_len: u64,
        data_offset: u64,
        invalidate: Invalidate,
        colors: &[Option<u32>],
    ) -> Self {
        // Should have benchmarked but this is probably faster than the `format!(stuff)`
        // key being used before
        // Also means we get to use this sick new hashing alg
        let hash = xxhash_rust::xxh3::xxh3_64(
            &[invalidate.value().to_le_bytes(), row_offset.to_le_bytes()].concat(),
        );

        if row_offset >= data_full_len {
            let bytes = Rc::new(
                (0..BYTES_PER_ROW)
                    .map(|_| ByteDisplayInfo {
                        value: None,
                        color: None,
                    })
                    .collect(),
            );
            return RowDisplayInfo {
                hash,
                offset: row_offset,
                offset_text: "".to_string(),
                bytes,
            };
        }
        let bytes = Rc::new(
            (row_offset..(row_offset + BYTES_PER_ROW))
                .map(|byte_offset| {
                    let (value, color) = if byte_offset >= data_full_len {
                        (None, &None)
                    } else {
                        (
                            Some(data[(byte_offset - data_offset) as usize]),
                            &colors[(byte_offset - data_offset) as usize],
                        )
                    };
                    ByteDisplayInfo {
                        value,
                        color: *color,
                    }
                })
                .collect(),
        );

        let offset_text = if row_offset >= data_full_len {
            "".to_string()
        } else {
            // TODO: Lowercase?
            lexical::to_string_with_options::<_, HEX_FORMAT>(
                row_offset,
                &lexical::WriteIntegerOptions::default(),
            )
        };
        RowDisplayInfo {
            hash,
            offset: row_offset,
            offset_text,
            bytes,
        }
    }
}

struct MouseDown(bool);

#[component]
fn Rows<G: Html>(cx: Scope) -> View<G> {
    let data_info_signal = use_context::<Signal<DataInfo>>(cx);
    let invalidate_signal = use_context::<Signal<Invalidate>>(cx);
    let analyzed_data_signal = use_context::<Signal<AnalyzedData>>(cx);

    let mouse_down_signal = create_signal(cx, MouseDown(false));
    provide_context_ref(cx, mouse_down_signal);
    // Set to something large so that enought rows are created to fill screen on first render
    let display_height_signal = create_signal(cx, 2000);
    let scroll_offset_signal = create_signal(cx, 0_f64);
    let row_data_signal = create_signal(cx, Vec::<Rc<RowDisplayInfo>>::new());

    // Update height on mount
    let rows_ref = create_node_ref(cx);
    on_mount(cx, || {
        if let Some(rows_ref) = rows_ref.try_get::<DomNode>() {
            if let Some(element) = rows_ref.to_web_sys().dyn_ref::<Element>() {
                if *display_height_signal.get() != element.client_height() {
                    display_height_signal.set(element.client_height());
                }
            }
        }
    });

    // Update row data on data, scroll or display height change
    create_effect(cx, || {
        // TODO: Reuse row data

        // Get values from signals
        let data_info = data_info_signal.get();
        let scroll_offset = *scroll_offset_signal.get();
        let display_height = *display_height_signal.get();
        let invalidate = *invalidate_signal.get();

        let data_full_len = data_info.provider.len();

        // Update scroll offset if necessary
        let data_row_count = (data_full_len + BYTES_PER_ROW - 1) / BYTES_PER_ROW;
        let max_scroll_offset = (data_row_count - 1) * ROW_HEIGHT_PX;
        let scroll_offset = scroll_offset.clamp(0., max_scroll_offset as f64);

        // Calculate rows to display
        let first_row = (scroll_offset / ROW_HEIGHT_PX as f64).floor() as u64;
        let rows_to_display = (display_height as f64 / ROW_HEIGHT_PX as f64).ceil() as u64;

        let data_start_index = first_row * BYTES_PER_ROW;
        let data_end_index = data_start_index + (rows_to_display * BYTES_PER_ROW);
        let data_end_index = data_end_index.min(data_full_len);
        let data = data_info
            .provider
            .get_range(data_start_index, data_end_index);

        // Calculate colors
        let analyzed_data = analyzed_data_signal.get_untracked();
        let color_objects_in_range = analyzed_data
            .parsed_objects
            .iter()
            .filter(|o| {
                o.background_color.is_some()
                    && !(o.end < data_start_index || o.start > data_end_index)
            })
            // Last items take precedence
            .rev()
            .collect::<Vec<_>>();
        let colors = (data_start_index..=data_end_index)
            .map(|i| {
                color_objects_in_range
                    .iter()
                    .find(|o| o.start <= i && o.end >= i)
                    .map(|o| o.background_color.unwrap())
            })
            .collect::<Vec<_>>();

        let new_row_data = (first_row..(first_row + rows_to_display))
            .map(|i| {
                let row_offset = i * BYTES_PER_ROW;
                Rc::new(RowDisplayInfo::new(
                    row_offset,
                    &data,
                    data_full_len,
                    data_start_index,
                    invalidate,
                    &colors,
                ))
            })
            .collect::<Vec<_>>();
        row_data_signal.set(new_row_data);
    });

    let handle_wheel = move |e: Event| {
        let e = e.unchecked_into::<WheelEvent>();

        let data_info_signal = use_context::<Signal<DataInfo>>(cx);
        let data_info = data_info_signal.get_untracked();

        let data_row_count = (data_info.provider.len() + BYTES_PER_ROW - 1) / BYTES_PER_ROW;
        let max_scroll_offset = (data_row_count - 1) * ROW_HEIGHT_PX;
        scroll_offset_signal
            .set((*scroll_offset_signal.get() + e.delta_y()).clamp(0., max_scroll_offset as f64));

        e.prevent_default();
    };

    view! {cx,
        div(
            ref=rows_ref,
            class="hexview-rows",
            on:wheel=handle_wheel,
        ){
            Keyed(
                iterable=row_data_signal,
                view=|cx, r| hex_row(cx, r), // view=hex_row doesn't work. Lifetimes :)
                key=|r| r.hash,
            )
        }
    }
}

fn hex_row<G: Html>(cx: Scope, row_info: Rc<RowDisplayInfo>) -> View<G> {
    let row_info_offset = row_info.offset;
    let selected_signal = create_memo(cx, move || {
        let selection = &*use_context::<Signal<Option<Selection>>>(cx).get();
        if let Some(selection) = selection {
            selection.contains_row(row_info_offset)
        } else {
            false
        }
    });

    let even_offset = (row_info_offset / BYTES_PER_ROW) % 2 == 0;
    let (bytes_hex_view, bytes_str_view) = row_info
        .bytes
        .iter()
        .enumerate()
        .map(|(i, byte_info)| {
            let offset = row_info.offset + i as u64;
            (
                hex_view(cx, byte_info, offset),
                str_view(cx, byte_info, offset),
            )
        })
        .unzip();

    let bytes_hex_view = View::new_fragment(bytes_hex_view);
    let bytes_str_view = View::new_fragment(bytes_str_view);

    let hexview_row_class = if even_offset {
        "hexview-row hexview-row-even"
    } else {
        "hexview-row hexview-row-odd"
    };
    let selected_hexview_row_class = if even_offset {
        "hexview-row hexview-row-even hexview-row-selected"
    } else {
        "hexview-row hexview-row-odd hexview-row-selected"
    };

    view! {cx,
        div(
            class=(if *selected_signal.get() {selected_hexview_row_class} else {hexview_row_class}),
            on:mousedown=move |e| on_mouse_down(cx,e),
            on:mouseup=move |e| on_mouse_up(cx, e),
            on:mousemove=move |e| on_mouse_move(cx, e),
        ) {
            div(class="hexview-row-header") {(row_info.offset_text)}
            div(class="hexview-row-bytes"){(bytes_hex_view)}
            div(class="hexview-row-chars"){(bytes_str_view)}
        }
    }
}

fn hex_view<G: Html>(cx: BoundedScope, byte_info: &ByteDisplayInfo, offset: u64) -> View<G> {
    let selected_signal = create_memo(cx, move || {
        let selection = &*use_context::<Signal<Option<Selection>>>(cx).get();
        if let Some(selection) = selection {
            selection.contains(offset)
        } else {
            false
        }
    });

    match byte_info.value {
        Some(value) => {
            let class = if value == 0 {
                "hexview-row-byte hexview-row-byte-zero"
            } else {
                "hexview-row-byte"
            };
            let selected_class = if value == 0 {
                "hexview-row-byte hexview-row-byte-zero hexview-row-byte-selected"
            } else {
                "hexview-row-byte hexview-row-byte-selected"
            };

            let background_color_class = match byte_info.color {
                Some(v) => format!("background-color: #{v:06x}bb"),
                None => "".to_string(),
            };

            view! {cx,
                div(
                    class="hexview-row-byte-container",
                    data-index=offset,
                ) {
                    div(style=background_color_class)
                    div(
                        class=(if *selected_signal.get() { selected_class } else { class }),
                    ){
                        (HEX_LOOKUP_UPPER[value as usize])
                    }
                }
            }
        }
        None => {
            view! {cx,
                div(class=("hexview-row-byte-container"))
            }
        }
    }
}

fn str_view<G: Html>(cx: BoundedScope, byte_info: &ByteDisplayInfo, offset: u64) -> View<G> {
    let selected_signal = create_memo(cx, move || {
        let selection = &*use_context::<Signal<Option<Selection>>>(cx).get();
        if let Some(selection) = selection {
            selection.contains(offset)
        } else {
            false
        }
    });

    match byte_info.value {
        Some(value) => {
            let class = if value == 0 {
                "hexview-row-char hexview-row-char-zero"
            } else {
                "hexview-row-char"
            };
            let selected_class = if value == 0 {
                "hexview-row-char hexview-row-char-zero hexview-row-char-selected"
            } else {
                "hexview-row-char hexview-row-char-selected"
            };

            let background_color_class = match byte_info.color {
                Some(v) => format!("background-color: #{v:06x}bb"),
                None => "".to_string(),
            };

            view! {cx,
                div(
                    class="hexview-row-char-container",
                    data-index=offset,
                ) {
                    div(style=background_color_class)
                    div(
                        class=(if *selected_signal.get() { selected_class } else { class }),
                    ){(VISIBLE_ASCII_LOOKUP[value as usize])}
                }
            }
        }
        None => {
            view! {cx,
                div(
                    class=("hexview-row-char-container")
                )
            }
        }
    }
}

fn on_mouse_down(cx: BoundedScope, event: Event) {
    let index = match get_data_index_from_event(&event) {
        Some(v) => v,
        None => return,
    };

    // TODO: Shift click
    let selection_signal = use_context::<Signal<Option<Selection>>>(cx);
    let mouse_down_signal = use_context::<Signal<MouseDown>>(cx);

    selection_signal.set(Some(Selection {
        start: index,
        end: index,
    }));
    mouse_down_signal.set(MouseDown(true));
}

fn on_mouse_up(cx: BoundedScope, event: Event) {
    let index = match get_data_index_from_event(&event) {
        Some(v) => v,
        None => return,
    };

    let selection_signal = use_context::<Signal<Option<Selection>>>(cx);
    let mouse_down_signal = use_context::<Signal<MouseDown>>(cx);

    if let Some(s) = selection_signal.get().as_ref() {
        selection_signal.set(Some(Selection {
            start: s.start,
            end: index,
        }));
    }
    mouse_down_signal.set(MouseDown(false));
}

fn on_mouse_move(cx: BoundedScope, event: Event) {
    let index = match get_data_index_from_event(&event) {
        Some(v) => v,
        None => return,
    };

    let selection_signal = use_context::<Signal<Option<Selection>>>(cx);
    let mouse_down_signal = use_context::<Signal<MouseDown>>(cx);

    if mouse_down_signal.get().0 {
        if let Some(s) = selection_signal.get().as_ref() {
            selection_signal.set(Some(Selection {
                start: s.start,
                end: index,
            }));
        }
    }
}

fn get_data_index_from_event(event: &Event) -> Option<u64> {
    event
        .target()?
        .dyn_ref::<HtmlElement>()?
        .get_attribute("data-index")?
        .parse::<u64>()
        .ok()
}
