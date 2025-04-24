type __RegisIdx = usize;

/// Registery lookup key
pub trait __RegisLK {
    fn key_as_str(&self) -> Option<&str> { None }
    fn key_as_int(&self) -> Option<__RegisIdx> { None }
}

pub type RegisteryLookupKey = dyn __RegisLK;

impl __RegisLK for &str {
    fn key_as_str(&self) -> Option<&str> {
        Some(self)
    }
}

impl __RegisLK for __RegisIdx {
    fn key_as_int(&self) -> Option<__RegisIdx> {
        Some(*self)
    }
}

pub trait RegisteryEntry {
    fn name() -> String;
}

pub trait Registry<T: RegisteryEntry> {
    fn lookup<Key: __RegisLK>(&self, key: &Key) -> Option<&T>;

    fn lookup_mut<Key: __RegisLK>(&mut self, key: &Key) -> Option<&mut T>;
    
    fn declare(&mut self, entry: T) -> Option<usize>;
    
    fn count(&self) -> usize;

    fn contains<Key: __RegisLK>(&self, key: &Key) -> bool {
        self.lookup(key).is_some()
    }    
}