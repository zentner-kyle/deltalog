use std::collections::hash_map::{HashMap};

#[derive(Debug)]
pub struct NameTable {
    name_to_index: HashMap<String, usize>,
    index_to_name: HashMap<usize, String>,
    next_index: usize,
}

impl NameTable {
    pub fn new() -> Self {
        NameTable {
            name_to_index: HashMap::new(),
            index_to_name: HashMap::new(),
            next_index: 0,
        }
    }

    pub fn get(&mut self, name: &str) -> usize {
        if let Some(index) = self.name_to_index.get(name) {
            return *index;
        }
        let this_index = self.next_index;
        self.next_index += 1;
        self.name_to_index.insert(name.to_owned(), this_index);
        self.index_to_name.insert(this_index, name.to_owned());
        return this_index;
    }

    pub fn get_name(&self, index: usize) -> Option<&str> {
        self.index_to_name.get(&index).map(|s| &s[..])
    }

    pub fn to_reverse(self) -> HashMap<usize, String> {
        let mut result = HashMap::new();
        for (name, index) in self.name_to_index.into_iter() {
            result.insert(index, name);
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::{NameTable};

    #[test]
    fn get_names() {
        let mut names = NameTable::new();
        assert_eq!(names.get("X"), 0);
        assert_eq!(names.get("Y"), 1);
        assert_eq!(names.get("X"), 0);
        assert_eq!(names.get("Y"), 1);
        assert_eq!(names.get("Z"), 2);
    }
}
