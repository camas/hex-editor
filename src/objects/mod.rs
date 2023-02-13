use linked_hash_map::LinkedHashMap;

pub mod defaults;
pub mod definition;

#[derive(Debug)]
pub struct RootObject {
    pub name: String,
    pub object: Object,
}

#[derive(Debug)]
pub struct Object {
    pub position: u64,
    pub length: u64,
    pub data: ObjectData,
    pub definition_name: Option<String>,
}

#[derive(Debug)]
pub enum ObjectData {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    Bytes(Vec<u8>),
    Object(LinkedHashMap<String, Vec<Object>>),
}

pub trait IntoObjectData {
    fn into_object_data(self) -> ObjectData;
}

macro_rules! into_obj_data_primitive {
    ($primitive:ident, $object_data:ident) => {
        impl IntoObjectData for $primitive {
            fn into_object_data(self) -> ObjectData {
                ObjectData::$object_data(self)
            }
        }
    };
}

into_obj_data_primitive!(u8, U8);
into_obj_data_primitive!(i8, I8);
into_obj_data_primitive!(u16, U16);
into_obj_data_primitive!(i16, I16);
into_obj_data_primitive!(u32, U32);
into_obj_data_primitive!(i32, I32);
into_obj_data_primitive!(u64, U64);
into_obj_data_primitive!(i64, I64);
