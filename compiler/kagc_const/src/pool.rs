use std::collections::{BTreeMap, HashMap};

pub type PoolIdx = usize;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RecordConst {
    pub type_name: String,
    pub alias: String,
    pub fields: BTreeMap<String, PoolIdx>
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum KagcConst {
    Str(String),
    Record(RecordConst),
    Int(i64),
    Bool(bool)
}

#[derive(Debug, Clone)]
pub struct ConstEntry {
    pub value: KagcConst,
    pub origin_func: Option<usize>
}

#[derive(Debug, Default)]
pub struct ConstPool {
    pub entries: Vec<ConstEntry>,
    index_by_value: HashMap<KagcConst, usize>
}

impl ConstPool {
    pub fn insert(&mut self, constant: KagcConst, origin_func: Option<usize>) -> PoolIdx {
        if let Some(&idx) = self.index_by_value.get(&constant) {
            idx
        } else {
            let idx = self.entries.len();
            let entry = ConstEntry {
                value: constant.clone(),
                origin_func,
            };
            self.entries.push(entry);
            self.index_by_value.insert(constant, idx);
            idx
        }
    }

    pub fn get(&self, index: PoolIdx) -> Option<&ConstEntry> {
        self.entries.get(index)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &ConstEntry> {
        self.entries.iter()
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (PoolIdx, &ConstEntry)> {
        self.entries.iter().enumerate()
    }
}