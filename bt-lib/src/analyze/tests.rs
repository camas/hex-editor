use std::io::Cursor;

use common::DataSource;

use crate::{parse::parse_bt, transform::transform};

use super::*;

#[test]
fn analyze_basic_data() {
    let data = vec![0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x07];

    let bt_data = "
        local int a = 1.2;
        local int a;
        float a;
        int a<bgcolor=0xaa00aa>;
    ";

    let program = transform(parse_bt(bt_data).unwrap()).unwrap();
    println!("{:#?}", program);

    let result = analyze_data(
        &program,
        &mut DataSource::new(Box::new(Cursor::new(data))).get_reader_mut(),
    )
    .unwrap();
    println!("{:#?}", result);
}

#[test]
fn if_statement() {
    let data = vec![0x01, 0x00, 0x00, 0x00];
    let program_str = "
        if (false) {
            i32 a;
        } else {
            i32 b;
        }
    ";
    test_program_output(program_str, data, vec![("b", Object::new_i32(1))], "");
}

#[test]
fn while_statement() {
    let data = vec![0x01, 0x02, 0x03, 0x04];
    let program_str = "
        local i32 a = 2;
        while (a) {
            u8 b;
            a -= 1;
        }
    ";
    test_program_output(
        program_str,
        data,
        vec![("b", Object::Array(NumberArray::U8(vec![1, 2])))],
        "",
    );
}

#[test]
fn for_statement() {
    let data = vec![1_u8; 10];
    let program_str = "
        local u8 a;
        for (a = 0; a < 3; a++) {
            u8 b;
        }
    ";
    test_program_output(
        program_str,
        data,
        vec![("b", Object::Array(NumberArray::U8(vec![1, 1, 1])))],
        "",
    );
}

#[test]
fn increment_decrement() {
    let program_str = r#"
        local u8 a = 5;
        local u8 b = 5;
        local u8 c = 5;
        local u8 d = 5;
        Printf("%d %d %d %d\n", --a, ++b, c++, d++);
    "#;

    test_program_output(program_str, vec![], vec![], "4 6 5 5\n");
}

#[test]
fn variable_scope() {
    let program_str = r#"
    local int a = 0;

    void aaa() {
        a += 10;
    }

    aaa();
    aaa();

    Printf("%d\n", a);
"#;

    test_program_output(program_str, vec![], vec![], "20\n");
}

#[test]
fn arrays() {
    let data = vec![0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a];
    let program_str = r#"
        u8 a[10];
        local u8 b[3] = {2-1,2,2+1};
        b[0] += 10;
        Printf("%d %d\n", a[1+3], b[0]);
    "#;

    test_program_output(
        program_str,
        data,
        vec![(
            "a",
            Object::ArrayRef {
                number_type: NumberType::U8,
                start: 0,
                size: 10,
            },
        )],
        "5 11\n",
    );
}

#[test]
fn function() {
    let data = vec![];
    let program_str = r#"
        void a(i32 a, f32 b) {
            Printf("Hello %d %f\n", a, b);
        }

        a(4, 5.3);
    "#;

    test_program_output(program_str, data, Vec::new(), "Hello 4 5.3\n");
}

#[test]
fn structs() {
    let data = vec![0x00, 0x00, 0x00, 0x10, 0x00, 0x12, 0x00, 0x03];
    let program_str = r#"
        struct A {
            u32 b1;
            u16 b2;
            u8 b3;
            u8 b4;
        };

        A a;

        Printf("%d\n", a.b3);
    "#;

    test_program_output(program_str, data, Vec::new(), "");
}

#[test]
fn analyze_lenna() {
    let data = std::fs::read("../bt-lib/test-resources/Lenna.png").unwrap();
    let program = std::fs::read_to_string("../bt-lib/test-resources/PNG.bt").unwrap();

    test_program_output(&program, data, Vec::new(), "");
}

fn test_program_output(
    program_str: &str,
    data: Vec<u8>,
    expected_objects: Vec<(&str, Object)>,
    expected_stdout: &str,
) {
    let program = transform(parse_bt(program_str).unwrap()).unwrap();
    println!("{:#?}", program);

    let result = analyze_data(
        &program,
        &mut DataSource::new(Box::new(Cursor::new(data))).get_reader_mut(),
    )
    .unwrap();
    println!("{:#?}", result);

    assert_eq!(result.parsed_objects.len(), expected_objects.len());
    for (actual, expected) in result
        .parsed_objects
        .into_iter()
        .zip(expected_objects.into_iter())
    {
        assert_eq!(actual.name, expected.0);
        assert_eq!(actual.value, expected.1);
    }

    assert_eq!(String::from_utf8(result.stdout).unwrap(), expected_stdout);
}
