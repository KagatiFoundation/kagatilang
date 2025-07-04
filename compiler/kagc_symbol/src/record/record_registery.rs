use std::collections::HashMap;

use kagc_types::record::RecordType;

use crate::registery::*;

impl RegisteryEntry for RecordType {
    fn name() -> String {
        todo!()
    }
}

#[derive(Debug, Default)]
pub struct RecordRegistery {
    registery: HashMap<String, RecordType>,
}

impl Registry<RecordType> for RecordRegistery {
    fn lookup<Key: __RegisLK>(&self, key: &Key) -> Option<&RecordType> {
        if let Some(name) = key.key_as_str() {
            return self.registery.get(name);
        }
        else if key.key_as_int().is_some() {
            panic!("Lookup using integer is not supported by RecordRegistery");
        }
        None
    }

    fn lookup_mut<Key: __RegisLK>(&mut self, key: &Key) -> Option<&mut RecordType> {
        if let Some(name) = key.key_as_str() {
            return self.registery.get_mut(name);
        }
        else if key.key_as_int().is_some() {
            panic!("Lookup using integer is not supported by RecordRegistery");
        }
        None
    }

    fn declare(&mut self, entry: RecordType) -> Option<usize> {
        if self.registery.contains_key(&entry.name) {
            None
        }
        else {
            self.registery.insert(entry.name.clone(), entry);
            Some(0)
        }
    }

    fn count(&self) -> usize {
        self.registery.len()
    }
}