use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum KagcConst {
    Str(String)
}

#[derive(Debug, Default)]
pub struct ConstPool {
    pub entries: Vec<KagcConst>,
    index_by_value: HashMap<KagcConst, usize>
}

impl ConstPool {
    pub fn insert(&mut self, constant: KagcConst) -> usize {
        if let Some(&idx) = self.index_by_value.get(&constant) {
            idx
        } else {
            let idx = self.entries.len();
            self.entries.push(constant.clone());
            self.index_by_value.insert(constant, idx);
            idx
        }
    }

    pub fn get(&self, index: usize) -> Option<&KagcConst> {
        self.entries.get(index)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &KagcConst> {
        self.entries.iter()
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (usize, &KagcConst)> {
        self.entries.iter().enumerate()
    }
}