use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use indexmap::IndexMap;
use kagc_types::builtins::obj::KObjType;

/// Pool index is used to index items in the const pool.
pub type PoolIdx = usize;

/// Size of a const pool item
pub type ConstSize = usize;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OrderedMap<K: Hash + Eq, V>(pub IndexMap<K, V>);

impl<K: Hash + Eq, V: Hash> Hash for OrderedMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (k, v) in &self.0 {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl<K: Hash + Eq, V> OrderedMap<K, V> {
    pub fn iter(&self) -> indexmap::map::Iter<K, V> {
        self.0.iter()
    }
}

impl<'a, K: Hash + Eq, V> IntoIterator for &'a OrderedMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = indexmap::map::Iter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RecordConst {
    /// Record's name
    pub type_name: String,

    /// Variable's name
    pub alias: String,

    /// Name and the pool index of fields
    pub fields: OrderedMap<String, PoolIdx>,

    /// How to align the record: platform dependent
    pub alignment: usize
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
    pub ob_type: KObjType,
    pub origin_func: Option<usize>
}

#[derive(Debug, Default)]
pub struct ConstPool {
    pub entries: Vec<ConstEntry>,
    index_by_value: HashMap<KagcConst, usize>
}

impl ConstPool {
    pub fn insert(
        &mut self, 
        constant: KagcConst, 
        ob_type: KObjType, 
        origin_func: Option<usize>
    ) -> PoolIdx {
        if let Some(&idx) = self.index_by_value.get(&constant) {
            idx
        } else {
            let idx = self.entries.len();
            let entry = ConstEntry {
                value: constant.clone(),
                origin_func,
                ob_type
            };
            self.entries.push(entry);
            self.index_by_value.insert(constant, idx);
            idx
        }
    }

    pub fn size(&self, idx: PoolIdx) -> Option<ConstSize> {
        if let Some(item) = self.get(idx) {
            return match &item.value {
                KagcConst::Str(value) => Some(value.len() + 1 /* +1 for null byte */),
                KagcConst::Int(_) => Some(4),
                KagcConst::Bool(_) => Some(1),

                KagcConst::Record(record_const) => {
                    let mut computed = 0;
                    for (_, item_idx) in record_const.fields.iter() {
                        if let Some(field_size) = self.size(*item_idx) {
                            computed += field_size;
                        }
                        else {
                            return None;
                        }
                    }
                    Some(computed)
                }
            };
        }
        None
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