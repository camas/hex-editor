use std::{
    cell::RefCell,
    io::{Read, Seek, SeekFrom},
    rc::Rc,
};

pub trait DataSourceTraits: Read + Seek {}
impl<T: Read + Seek> DataSourceTraits for T {}

pub struct DataSource {
    len: u64,
    reader: Rc<RefCell<Box<dyn DataSourceTraits>>>,
}

impl DataSource {
    pub fn new(mut reader: Box<dyn DataSourceTraits>) -> DataSource {
        let len = reader.stream_len().unwrap();
        DataSource {
            len,
            reader: Rc::new(RefCell::new(reader)),
        }
    }

    pub fn len(&self) -> u64 {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn get_range(&self, start: u64, end: u64) -> Vec<u8> {
        let size = end - start;
        let mut reader = self.reader.borrow_mut();
        reader.seek(SeekFrom::Start(start)).unwrap();
        let mut buffer = vec![0; size as usize];
        reader.read_exact(&mut buffer).unwrap();
        buffer
    }

    pub fn get_reader_mut(&self) -> std::cell::RefMut<Box<dyn DataSourceTraits>> {
        self.reader.borrow_mut()
    }
}
