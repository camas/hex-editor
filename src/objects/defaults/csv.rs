use crate::{analyzer::Analyzer, objects::ObjectData};

pub fn register(analyzer: &mut Analyzer) {
    analyzer.register_definition(
        "field",
        |r| {
            while !r.eof()? {
                let next = r.peek_u8()?;
                match next {
                    b'\n' | b',' => break,
                    _ => {}
                };
                r.read_u8("data")?;
            }
            Ok(())
        },
        None,
    );
    analyzer.register_definition(
        "record",
        |r| {
            while !r.eof()? {
                r.read_object("field", "field")?;
                if r.eof()? {
                    break;
                }
                let next = r.peek_u8()?;
                match next {
                    b'\n' => {
                        r.read_u8("newline")?;
                        break;
                    }
                    b',' => {
                        r.read_u8("separator")?;
                    }
                    other => {
                        return Err(format!("Unexpected char {}", other).into());
                    }
                }
            }
            Ok(())
        },
        None,
    );
    analyzer.register_definition(
        "csv",
        |r| {
            while !r.eof()? {
                r.read_object("record", "record")?;
            }
            Ok(())
        },
        Some(Box::new(|o| {
            let record_count = match o.data {
                ObjectData::Object(ref data) => data["record"].len(),
                _ => 0,
            };
            format!("CSV {} records", record_count)
        })),
    );
}
