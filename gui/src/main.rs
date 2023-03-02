#![feature(int_roundings)]
#![feature(iter_intersperse)]

use std::{io::Cursor, num::Wrapping};

use bt_lib::analyze::AnalyzedData;
use common::DataSource;
use components::{
    footer::Footer, hexview::HexView, inspector::Inspector, parsed::ParsedView,
    scripteditor::ScriptEditor,
};
use js_sys::Uint8Array;
use sycamore::prelude::*;
use sycamore_futures::spawn_local_scoped;
use uuid::Uuid;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;
#[cfg(target_family = "wasm")]
use web_sys::ClipboardEvent;
use web_sys::{DataTransfer, DragEvent, Event};

mod components;
mod lookups;

const BYTES_PER_ROW: u64 = 0x10;

pub fn main() {
    // Setup logging
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();
    wasm_logger::init(wasm_logger::Config::default());

    // Run app
    sycamore::render(|cx| {
        view! { cx, App{} }
    });

    // Remove loading screen
    web_sys::window()
        .unwrap()
        .document()
        .unwrap()
        .get_element_by_id("loading-screen")
        .unwrap()
        .remove();
}

struct DataInfo {
    uuid: Uuid,
    name: String,
    provider: DataSource,
}

#[derive(Debug, Clone, Copy)]
struct Selection {
    // Deliberately allows end to be smaller than start for a backwards selection
    // TODO: Might be a better way of doing this? Get rid of the ifs in every function
    start: u64,
    end: u64,
}

impl Selection {
    pub fn contains(&self, offset: u64) -> bool {
        if self.end >= self.start {
            (self.start..=self.end).contains(&offset)
        } else {
            (self.end..=self.start).contains(&offset)
        }
    }

    pub fn contains_row(&self, offset: u64) -> bool {
        let (start, end) = if self.start > self.end {
            (self.end, self.start)
        } else {
            (self.start, self.end)
        };
        let row_start = start - (start % BYTES_PER_ROW);
        let row_end = end - (end % BYTES_PER_ROW) + BYTES_PER_ROW;
        (row_start..row_end).contains(&offset)
    }
}

#[derive(Debug, Clone, Copy)]
struct Invalidate(Wrapping<u64>);

impl Invalidate {
    fn value(&self) -> u64 {
        self.0 .0
    }

    fn invalidate(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug)]
struct RunRequested;

#[component]
fn App<G: Html>(cx: Scope) -> View<G> {
    let mut data = br"               
               
               
     Camas'    
   Hex Editor  
               
               
Open file by:  
 Click top left
 Drag & drop   
 Copy & paste  
               
               
"
    .to_vec();

    for _ in 0..200 {
        data.extend(b"TESTDATA\x00\x00\x00");
    }

    let provider = DataSource::new(Box::new(Cursor::new(data)));
    let data_info = create_signal(
        cx,
        DataInfo {
            uuid: Uuid::new_v4(),
            name: "[in-memory data]".to_string(),
            provider,
        },
    );
    provide_context_ref(cx, data_info);

    // TODO: Use a generic container so that more than bt can be used
    let analyzed_data: AnalyzedData = AnalyzedData {
        parsed_objects: Vec::new(),
        stdout: Vec::new(),
    };
    let analyzed_data_signal = create_signal(cx, analyzed_data);
    provide_context_ref(cx, analyzed_data_signal);

    let selection: Option<Selection> = None;
    let selection_signal = create_signal(cx, selection);
    provide_context_ref(cx, selection_signal);

    let invalidate = Invalidate(Wrapping(0));
    let invalidate_signal = create_signal(cx, invalidate);
    provide_context_ref(cx, invalidate_signal);

    // TODO: Better way to pass messages?
    let run_requested_signal = create_signal(cx, RunRequested {});
    provide_context_ref(cx, run_requested_signal);

    view! {cx,
        div(
            class="app",
            on:paste=move |e| on_paste(cx, e),
            // Prevent the browser opening the file/object as a page
            on:dragover=|e: Event| e.prevent_default(),
            on:drop=move |e| on_drop(cx, e),
        ) {
            Inspector{}
            HexView{}
            ParsedView{}
            ScriptEditor{}
            Footer{}
        }
    }
}

#[cfg(target_family = "wasm")]
fn on_paste(cx: BoundedScope, event: Event) {
    let event = event.dyn_into::<ClipboardEvent>().unwrap();
    let Some(clipboard_data) = event.clipboard_data()
        else { return };
    use_data_from_data_transfer(cx, clipboard_data);
}

#[cfg(not(target_family = "wasm"))]
fn on_paste(_cx: BoundedScope, _event: Event) {
    unreachable!()
}

fn on_drop(cx: BoundedScope, event: Event) {
    event.prevent_default();
    let event = event.dyn_into::<DragEvent>().unwrap();
    let Some(data_transfer) = event.data_transfer()
        else { return };
    use_data_from_data_transfer(cx, data_transfer);
}

fn use_data_from_data_transfer(cx: BoundedScope, data_transfer: DataTransfer) {
    for i in 0..data_transfer.items().length() {
        let data_item = data_transfer.items().get(i).unwrap();
        if data_item.kind().as_str() == "file" {
            use_data_from_js_file(cx, data_item.get_as_file().unwrap().unwrap());
            return;
        }
    }
}

pub fn use_data_from_js_file(cx: BoundedScope, file: web_sys::File) {
    // Fetch file data in new thread
    spawn_local_scoped(cx, async move {
        let Ok(buffer) = JsFuture::from(file.array_buffer()).await
            else { return };

        let selection_signal = use_context::<Signal<Option<Selection>>>(cx);
        let invalidate_signal = use_context::<Signal<Invalidate>>(cx);
        let data_info_signal = use_context::<Signal<DataInfo>>(cx);
        selection_signal.set_silent(None);
        data_info_signal.set_silent(DataInfo {
            uuid: Uuid::new_v4(),
            name: file.name(),
            provider: DataSource::new(Box::new(Cursor::new(Uint8Array::new(&buffer).to_vec()))),
        });
        invalidate_signal.modify().invalidate();
    });
}
