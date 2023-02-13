use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufReader, Cursor, Read, Seek, SeekFrom},
    path::Path,
};

use linked_hash_map::LinkedHashMap;

use crate::{
    objects::{definition::ObjectDefinition, IntoObjectData, Object, ObjectData, RootObject},
    reader::Reader,
};

pub type AnalyzerResult<T> = Result<T, Box<dyn std::error::Error>>;

pub struct Analyzer {
    reader: Reader,
    definitions: HashMap<String, ObjectDefinition>,
}

impl Analyzer {
    pub fn from_file(path: &Path) -> io::Result<Self> {
        let reader = Reader::new(BufReader::new(File::open(path)?));
        Ok(Analyzer {
            reader,
            definitions: HashMap::new(),
        })
    }

    pub fn from_memory(data: Vec<u8>) -> Self {
        let reader = Reader::new(BufReader::new(Cursor::new(data)));
        Analyzer {
            reader,
            definitions: HashMap::new(),
        }
    }

    pub fn register_definition<F, S: ToString>(
        &mut self,
        name: S,
        read_function: F,
        display_function: Option<Box<dyn Fn(&Object) -> String>>,
    ) -> &ObjectDefinition
    where
        F: Fn(&mut ReadContext) -> AnalyzerResult<()> + 'static,
    {
        let name = name.to_string();
        let definition = ObjectDefinition {
            name: name.clone(),
            read_function: Box::new(read_function),
            display_function,
        };
        if self.definitions.contains_key(&name) {
            panic!("Definition already registered for {}", name);
        }
        self.definitions.entry(name).or_insert(definition)
    }

    pub fn register_default_definitions(&mut self) {
        crate::objects::defaults::csv::register(self);
    }

    pub fn read_object(
        &mut self,
        name: String,
        definition_name: &str,
    ) -> AnalyzerResult<RootObject> {
        let position = self.reader.position()?;
        let mut context = ReadContext {
            reader: &mut self.reader,
            definitions: &self.definitions,
            data: LinkedHashMap::new(),
        };
        let definition = self.definitions.get(definition_name).unwrap();
        (definition.read_function)(&mut context)?;
        Ok(RootObject {
            name,
            object: Object {
                position,
                length: 0,
                data: ObjectData::Object(context.data),
                definition_name: Some(definition_name.to_string()),
            },
        })
    }

    pub fn display(&self, object: &Object) -> String {
        let definition = self
            .definitions
            .get(object.definition_name.as_ref().unwrap())
            .unwrap();
        (definition.display_function.as_ref().unwrap())(object)
    }
}

pub trait Readable: Sized {
    fn read(analyzer: &mut Analyzer) -> Result<Self, Box<dyn std::error::Error>>;
}

pub struct ReadContext<'a> {
    reader: &'a mut Reader,
    definitions: &'a HashMap<String, ObjectDefinition>,
    pub data: LinkedHashMap<String, Vec<Object>>,
}

macro_rules! peek_primitive {
    ($func_name:ident, $read_func:ident, $primitive:ident, $seek_offset:expr) => {
        pub fn $func_name(&mut self) -> AnalyzerResult<$primitive> {
            let result = self.reader.$read_func()?;
            self.reader.seek(SeekFrom::Current($seek_offset))?;
            Ok(result)
        }
    };
}

macro_rules! read_primitive {
    ($func_name:ident, $primitive:ident, $primitive_mapping:ident, $size:expr) => {
        pub fn $func_name<S: ToString>(&mut self, name: S) -> AnalyzerResult<$primitive> {
            let name = name.to_string();
            let position = self.reader.position()?;
            let value = self.reader.$func_name()?;
            self.data.entry(name).or_default().push(Object {
                position,
                length: $size,
                data: ObjectData::$primitive_mapping(value),
                definition_name: None,
            });
            Ok(value)
        }
    };
}

impl<'a> ReadContext<'a> {
    pub fn eof(&mut self) -> AnalyzerResult<bool> {
        self.reader.eof()
    }

    peek_primitive!(peek_u8, read_u8, u8, -1);
    peek_primitive!(peek_i8, read_i8, i8, -1);
    peek_primitive!(peek_u16, read_u16, u16, -2);
    peek_primitive!(peek_i16, read_i16, i16, -2);
    peek_primitive!(peek_u32, read_u32, u32, -4);
    peek_primitive!(peek_i32, read_i32, i32, -4);
    peek_primitive!(peek_u64, read_u64, u64, -8);
    peek_primitive!(peek_i64, read_i64, i64, -8);

    read_primitive!(read_u8, u8, U8, 1);
    read_primitive!(read_i8, i8, I8, 1);
    read_primitive!(read_u16, u16, U16, 2);
    read_primitive!(read_i16, i16, I16, 2);
    read_primitive!(read_u32, u32, U32, 4);
    read_primitive!(read_i32, i32, I32, 4);
    read_primitive!(read_u64, u64, U64, 8);
    read_primitive!(read_i64, i64, I64, 8);

    pub fn read_object<S: ToString>(
        &mut self,
        name: S,
        definition_name: &str,
    ) -> AnalyzerResult<&Object> {
        let name = name.to_string();
        let definition = self.definitions.get(definition_name).unwrap();
        let position = self.reader.position()?;
        let mut context = ReadContext {
            reader: &mut self.reader,
            definitions: self.definitions,
            data: LinkedHashMap::new(),
        };
        (definition.read_function)(&mut context)?;
        let object = Object {
            position,
            length: 0,
            data: ObjectData::Object(context.data),
            definition_name: Some(definition_name.to_string()),
        };
        let entry = self.data.entry(name).or_default();
        entry.push(object);
        Ok(entry.last().unwrap())
    }

    pub fn read_bytes<S: ToString>(&mut self, name: S, count: u64) -> AnalyzerResult<&[u8]> {
        let name = name.to_string();
        let position = self.reader.position()?;
        let bytes = self.reader.read_bytes(count)?;
        let entry = self.data.entry(name).or_default();
        entry.push(Object {
            position,
            length: count,
            data: ObjectData::Bytes(bytes),
            definition_name: None,
        });
        if let ObjectData::Bytes(data) = &entry.last().unwrap().data {
            Ok(data)
        } else {
            unreachable!();
        }
    }

    pub fn read_until<S: ToString, F>(&mut self, name: S, predicate: F) -> AnalyzerResult<&[u8]>
    where
        F: Fn(u8) -> bool,
    {
        let name = name.to_string();
        let position = self.reader.position()?;
        let bytes = self.reader.read_until(predicate)?;
        let entry = self.data.entry(name).or_default();
        entry.push(Object {
            position,
            length: bytes.len() as u64,
            data: ObjectData::Bytes(bytes),
            definition_name: None,
        });
        if let ObjectData::Bytes(data) = &entry.last().unwrap().data {
            Ok(data)
        } else {
            unreachable!();
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn parse_csv() {
        let mut analyzer = Analyzer::from_file(Path::new("test-data/data.csv")).unwrap();
        crate::objects::defaults::csv::register(&mut analyzer);
        let csv = analyzer.read_object("csv".to_string(), "csv").unwrap();
        println!("{}", analyzer.display(&csv.object));
        println!("{:?}", csv);
    }
}
