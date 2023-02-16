use crate::parse::parse_bt;

use super::transform;

#[test]
fn test_expressions() {
    let input = r#"
        local int a = 1, b = 2, c = 3;
        a + b + c;

        void d(int a) {
            Printf("%d\n", a);
        }

        d(1,2);

        int e;
        int e;

        local int a = 0.1 ? 3 : 4;

        if(1) {
            i32 a;
        } else if (2) {
            i32 b;
        } else if (3) {
            i32 c;
        } else {
            i32 d;
        }

        i32 a[10];
        "#;

    let parsed = parse_bt(input).unwrap();
    println!("{:#?}", parsed);

    let result = transform(parsed).unwrap();
    println!("{:#?}", result);
}

#[test]
fn transform_bt() {
    let input = std::fs::read_to_string("../bt-lib/test-resources/PNG.bt").unwrap();
    let parsed = parse_bt(input).unwrap();
    println!("{:#?}", parsed);

    let result = transform(parsed).unwrap();
    println!("{:#?}", result);
}
