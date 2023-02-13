pub mod vec;

pub trait DataProvider {
    fn len(&self) -> usize;

    fn get(&self, index: usize) -> u8;

    fn get_range(&self, index: usize, count: usize) -> Vec<u8>;

    fn set(&mut self, index: usize, value: u8);
}
