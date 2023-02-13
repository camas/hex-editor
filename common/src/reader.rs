use std::{
    fmt::Debug,
    io::{self, SeekFrom},
};

use crate::DataSourceTraits;

pub struct Reader<R: DataSourceTraits> {
    inner: R,
    bits_left: u8,
}

macro_rules! read_primitive {
    ($func_name:ident, $primitive:ident, $byte_count:expr) => {
        pub fn $func_name(&mut self) -> io::Result<$primitive> {
            Ok($primitive::from_le_bytes(
                self.read_bytes($byte_count)?.try_into().unwrap(),
            ))
        }
    };
}

impl<R: DataSourceTraits> Reader<R> {
    pub fn new(inner: R) -> Self {
        Reader {
            inner,
            bits_left: 0,
            // cur_byte: 0,
        }
    }

    pub fn position(&mut self) -> io::Result<u64> {
        self.inner.stream_position()
    }

    pub fn eof(&mut self) -> io::Result<bool> {
        Ok(self.inner.stream_len()? == self.inner.stream_position()?)
    }

    pub fn byte_aligned(&self) -> bool {
        self.bits_left == 0
    }

    // fn read_bit(&mut self) -> io::Result<bool> {
    //     if self.bits_left == 0 {
    //         let mut buffer = vec![0; 1];
    //         self.inner.read_exact(&mut buffer)?;
    //         self.cur_byte = buffer[0];
    //         self.bits_left = 8;
    //     }

    //     let bit = (self.cur_byte & 0b1000_0000) != 0;
    //     self.cur_byte <<= 1;
    //     self.bits_left -= 1;

    //     Ok(bit)
    // }

    // fn read_bits(&mut self, count: u8) -> io::Result<u8> {
    //     assert!(count <= 8);
    //     // Could probably be optimized
    //     let mut value = 0;
    //     for _ in 0..count {
    //         value <<= 1;
    //         value |= self.read_bit()? as u8;
    //     }
    //     Ok(value)
    // }

    pub fn read_bytes(&mut self, count: u64) -> io::Result<Vec<u8>> {
        if self.byte_aligned() {
            let mut bytes = vec![0; count.try_into().unwrap()];
            self.inner.read_exact(&mut bytes)?;
            Ok(bytes)
        } else {
            todo!()
        }
    }

    pub fn read_until<F>(&mut self, predicate: F) -> io::Result<Vec<u8>>
    where
        F: Fn(u8) -> bool,
    {
        let mut bytes = Vec::new();
        loop {
            let v = self.read_u8()?;
            if predicate(v) {
                self.seek(SeekFrom::Current(-1))?;
                break;
            }
            bytes.push(v);
        }
        Ok(bytes)
    }

    read_primitive!(read_u8, u8, 1);
    read_primitive!(read_i8, i8, 1);
    read_primitive!(read_u16, u16, 2);
    read_primitive!(read_i16, i16, 2);
    read_primitive!(read_u32, u32, 4);
    read_primitive!(read_i32, i32, 4);
    read_primitive!(read_u64, u64, 8);
    read_primitive!(read_i64, i64, 8);
    read_primitive!(read_f32, f32, 4);
    read_primitive!(read_f64, f64, 8);

    pub fn seek(&mut self, seek_from: SeekFrom) -> io::Result<()> {
        self.inner.seek(seek_from)?;
        Ok(())
    }
}

impl<R: DataSourceTraits> Debug for Reader<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Reader")
            // .field("inner", &self.inner)
            .field("bits_left", &self.bits_left)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    #[test]
    fn read_primitive() {
        fn reader() -> Reader<impl DataSourceTraits> {
            let data = &[0xff, 0xfe, 0x1c, 0xfe, 0x22, 0xb1, 0x99, 0xfb][..];
            Reader::new(Cursor::new(data))
        }

        assert_eq!(reader().read_u8().unwrap(), 0xff);
        assert_eq!(reader().read_i8().unwrap(), -1);
        assert_eq!(reader().read_u16().unwrap(), 0xfeff);
        assert_eq!(reader().read_i16().unwrap(), -257);
        assert_eq!(reader().read_u32().unwrap(), 0xfe1cfeff);
        assert_eq!(reader().read_i32().unwrap(), -31654145);
        assert_eq!(reader().read_u64().unwrap(), 0xfb99b122fe1cfeff);
        assert_eq!(reader().read_i64().unwrap(), -317027534902591745);
        assert_eq!(reader().read_f32().unwrap(), -5.2170896e37);
        assert_eq!(reader().read_f64().unwrap(), -2.4450783668352983e287);
    }

    // #[test]
    // fn reads_bits_correctly() {
    //     let mut r = Reader::new(Cursor::new(vec![0b11110000, 0b01101110]));
    //     assert_eq!(r.read_bits(4).unwrap(), 0b1111);
    //     assert_eq!(r.read_bits(6).unwrap(), 0b000001);
    //     assert_eq!(r.read_bits(6).unwrap(), 0b101110);
    // }
}
