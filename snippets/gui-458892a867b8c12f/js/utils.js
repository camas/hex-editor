export function define_css(name, value) {
    var sheet = document.getElementById(name);
    if (sheet == null) {
        sheet = document.createElement("style");
        sheet.id = name;
        document.body.appendChild(sheet);
    }
    sheet.innerHTML = value;
}
