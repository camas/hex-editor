use bt_lib::{analyze::AnalyzedData, analyze_data, compile};
use log::error;
use monaco::api::{CodeEditor, CodeEditorOptions};
use sycamore::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::HtmlElement;

use crate::{DataInfo, RunRequested};

#[component]
pub fn ScriptEditor<G: Html>(cx: Scope) -> View<G> {
    let data_info_signal = use_context::<Signal<DataInfo>>(cx);
    let analyzed_data_signal = use_context::<Signal<AnalyzedData>>(cx);
    let run_requested_signal = use_context::<Signal<RunRequested>>(cx);

    let editor_signal = create_signal(cx, None);

    let monaco_ref = create_node_ref(cx);

    on_mount(cx, || {
        if let Some(monaco_ref) = monaco_ref.try_get::<DomNode>() {
            if let Some(element) = monaco_ref.to_web_sys().dyn_ref::<HtmlElement>() {
                let editor = CodeEditor::create(element, Some(get_options()));
                editor.get_model().unwrap().set_value(
                    "// Example script
                    
u8 a[53]<color=0x000000>;
u8 b[6]<color=0xff0000>;
u8 a[8]<color=0x000000>;
u8 a[10]<color=0xff0000>;
u8 a[35]<color=0x000000>;
",
                );
                editor_signal.set(Some(editor));
                run_requested_signal.set(RunRequested);
            }
        }
    });

    create_effect(cx, || {
        run_requested_signal.track();

        let editor = editor_signal.get_untracked();
        let editor = match editor.as_ref() {
            Some(v) => v,
            None => return,
        };
        let text_model = match editor.get_model() {
            Some(v) => v,
            None => return,
        };
        let program = text_model.get_value();

        let program = match compile(&program) {
            Ok(v) => v,
            Err(e) => {
                error!("Error compiling program: {}", e);
                return;
            }
        };

        let data_info = data_info_signal.get_untracked();

        let results = match analyze_data(&program, &mut data_info.provider.get_reader_mut()) {
            Ok(v) => v,
            Err(e) => {
                error!("Error running program: {}", e);
                return;
            }
        };

        analyzed_data_signal.set(results);
    });

    view! {cx,
        div(class="scripteditor") {
            div(class="scripteditor-monaco", ref=monaco_ref)
        }
    }
}

fn get_options() -> CodeEditorOptions {
    CodeEditorOptions::default()
        .with_language("cpp".to_owned())
        // .with_value(CONTENT.to_owned())
        .with_builtin_theme(monaco::sys::editor::BuiltinTheme::VsDark)
        .with_automatic_layout(true)
}
