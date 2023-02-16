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
fn test_if_statement() {
    let data = vec![0x01, 0x00, 0x00, 0x00];
    let program_str = "
        if (false) {
            i32 a;
        } else {
            i32 b;
        }
    ";
    test_program_output(program_str, data, vec![("b", Object::new_i32(1))], b"");
}

#[test]
fn test_while_statement() {
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
        vec![("b", Object::new_u8(1)), ("b", Object::new_u8(2))],
        b"",
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

    test_program_output(program_str, data, Vec::new(), b"Hello 4 5.3\n");
}

#[test]
fn analyze_lenna() {
    let data = std::fs::read("../bt-lib/test-resources/Lenna.png").unwrap();
    let program = std::fs::read_to_string("../bt-lib/test-resources/PNG.bt").unwrap();

    test_program_output(&program, data, Vec::new(), b"");
}

fn test_program_output(
    program_str: &str,
    data: Vec<u8>,
    expected_objects: Vec<(&str, Object)>,
    expected_stdout: &[u8],
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

    assert_eq!(result.stdout, expected_stdout);
}
