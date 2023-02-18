use git_version::git_version;
use sycamore::prelude::*;

#[component]
pub fn Footer<G: Html>(cx: Scope) -> View<G> {
    let git_commit_hash = git_version!(args = ["--always", "--abbrev=40"]);
    let github_commit_url = format!("https://github.com/camas/hex-editor/commit/{git_commit_hash}");
    let git_version_string = git_version!(args = ["--always", "--abbrev=8", "--dirty=-dirty"]);
    view! {cx,
        div(class="footer") {
            div(){"hex-editor-" a(href=github_commit_url, target="_blank", rel="noopener"){(git_version_string)}}
            div()
            div(){"View on " a(href="https://github.com/camas/hex-editor", target="_blank", rel="noopener"){"GitHub"}}
        }
    }
}
