use std::io::Cursor;

use common::DataSource;

use crate::{parse::parse_bt, transform::transform};

use super::*;

#[test]
fn analyze_basic_data() {
    println!("{:#?}", Wrapping(255_u8) << 3_usize);
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
fn analyze_lenna() {
    // let data = std::fs::read("../bt-lib/test-resources/Lenna.png").unwrap();

    // let program = transform(
    //     parse_bt(std::fs::read_to_string("../bt-lib/test-resources/PNG.bt").unwrap()).unwrap(),
    // )
    // .unwrap();

    // println!("{:#?}", analyze(&program, &data));
}
