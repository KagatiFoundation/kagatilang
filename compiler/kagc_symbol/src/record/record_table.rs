// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::collections::HashMap;

use kagc_types::record::RecordType;

pub struct RecordTable<'tcx> {
    arena: &'tcx typed_arena::Arena<RecordType<'tcx>>,
    records: RefCell<HashMap<&'tcx str, &'tcx RecordType<'tcx>>>,
}

impl<'tcx> RecordTable<'tcx> {
    pub fn new(arena: &'tcx typed_arena::Arena<RecordType<'tcx>>) -> Self {
        Self {
            arena,
            records: RefCell::new(HashMap::new())
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&'tcx RecordType<'tcx>> {
        let records = self.records.borrow();
        records.get(name).copied()
    }

    pub fn declare(&self, name: &'tcx str, entry: RecordType<'tcx>) -> Result<&'tcx RecordType<'tcx>, &'tcx RecordType<'tcx>> {
        let mut records = self.records.borrow_mut();
        if let Some(existing) = records.get(name) {
            return Err(*existing);
        }
        let allocated = self.arena.alloc(entry);
        records.insert(name, allocated);
        Ok(allocated)
    }

    pub fn count(&self) -> usize {
        self.records.borrow().len()
    }
}