use super::DataProvider;

pub struct VecDataProvider {
    data: Vec<u8>,
}

impl VecDataProvider {
    pub fn new(data: Vec<u8>) -> Self {
        VecDataProvider { data }
    }
}

impl DataProvider for VecDataProvider {
    fn len(&self) -> usize {
        self.data.len()
    }

    fn get(&self, index: usize) -> u8 {
        self.data[index]
    }

    fn get_range(&self, start: usize, end: usize) -> Vec<u8> {
        self.data[start..end].to_vec()
    }

    fn set(&mut self, index: usize, value: u8) {
        self.data[index] = value;
    }
}
