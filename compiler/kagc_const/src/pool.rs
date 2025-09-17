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
                KagcConst::Int(_) => Some(8),
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

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;
    use kagc_types::builtins::obj::KObjType;

    #[test]
    fn test_insert_and_deduplication() {
        let mut pool = ConstPool::default();

        let idx1 = pool.insert(KagcConst::Int(42), KObjType::KInt, None);
        let idx2 = pool.insert(KagcConst::Int(42), KObjType::KInt, None);

        assert_eq!(idx1, idx2, "Inserting the same constant should reuse index");
        assert_eq!(pool.len(), 1);
    }

    #[test]
    fn test_insert_different_constants() {
        let mut pool = ConstPool::default();

        let idx1 = pool.insert(KagcConst::Int(10), KObjType::KInt, None);
        let idx2 = pool.insert(KagcConst::Bool(true), KObjType::KInt, None);

        assert_ne!(idx1, idx2);
        assert_eq!(pool.len(), 2);
    }

    #[test]
    fn test_size_primitives() {
        let mut pool = ConstPool::default();

        let idx_str = pool.insert(KagcConst::Str("hello".into()), KObjType::KStr, None);
        let idx_int = pool.insert(KagcConst::Int(123), KObjType::KInt, None);
        let idx_bool = pool.insert(KagcConst::Bool(true), KObjType::KInt, None);

        assert_eq!(pool.size(idx_str), Some(6)); // 5 chars + null terminator
        assert_eq!(pool.size(idx_int), Some(8));
        assert_eq!(pool.size(idx_bool), Some(1));
    }

    #[test]
    fn test_size_record() {
        let mut pool = ConstPool::default();

        let idx_str = pool.insert(KagcConst::Str("abc".into()), KObjType::KStr, None);
        let idx_bool = pool.insert(KagcConst::Bool(true), KObjType::KInt, None);

        let mut fields = IndexMap::new();
        fields.insert("field1".to_string(), idx_str);
        fields.insert("field2".to_string(), idx_bool);

        let record = RecordConst {
            type_name: "MyRecord".to_string(),
            alias: "r".to_string(),
            fields: OrderedMap(fields),
            alignment: 4,
        };

        let idx_record = pool.insert(KagcConst::Record(record),KObjType::KRec, None);

        // Size = len("abc")+1 + 1 = 4+1 = 5
        assert_eq!(pool.size(idx_record), Some(5));
    }

    #[test]
    fn test_size_record_with_invalid_field() {
        let mut pool = ConstPool::default();

        let mut fields = IndexMap::new();
        fields.insert("broken".to_string(), 99); // invalid index

        let record = RecordConst {
            type_name: "Broken".to_string(),
            alias: "b".to_string(),
            fields: OrderedMap(fields),
            alignment: 8,
        };

        let idx = pool.insert(KagcConst::Record(record), KObjType::KRec, None);

        assert_eq!(pool.size(idx), None, "Should return None if field index is invalid");
    }

    #[test]
    fn test_pool_state_methods() {
        let mut pool = ConstPool::default();
        assert!(pool.is_empty());

        let idx = pool.insert(KagcConst::Int(1), KObjType::KInt, Some(123));
        assert!(!pool.is_empty());
        assert_eq!(pool.len(), 1);

        let entry = pool.get(idx).unwrap();
        assert_eq!(entry.origin_func, Some(123));

        let collected: Vec<_> = pool.iter().collect();
        assert_eq!(collected.len(), 1);

        let enumerated: Vec<_> = pool.iter_enumerated().collect();
        assert_eq!(enumerated[0].0, idx);
    }

    #[test]
    fn test_ordered_map_hash_and_equality() {
        use std::collections::hash_map::DefaultHasher;

        let mut m1 = IndexMap::new();
        m1.insert("a".to_string(), 1);
        m1.insert("b".to_string(), 2);

        let mut m2 = IndexMap::new();
        m2.insert("a".to_string(), 1);
        m2.insert("b".to_string(), 2);

        let ordered1 = OrderedMap(m1);
        let ordered2 = OrderedMap(m2);

        assert_eq!(ordered1, ordered2);

        let mut hasher1 = DefaultHasher::new();
        ordered1.hash(&mut hasher1);

        let mut hasher2 = DefaultHasher::new();
        ordered2.hash(&mut hasher2);

        assert_eq!(hasher1.finish(), hasher2.finish());
    }
}