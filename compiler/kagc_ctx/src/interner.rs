// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;
use std::cell::RefCell;

pub struct StringInterner<'tcx> {
    pub arena: &'tcx typed_arena::Arena<String>,
    cache: RefCell<HashMap<&'tcx str, &'tcx str>>,
}

impl<'tcx> StringInterner<'tcx> {
    pub fn new(arena: &'tcx typed_arena::Arena<String>) -> Self {
        Self {
            arena,
            cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn intern(&self, s: &str) -> &'tcx str {
        let mut cache = self.cache.borrow_mut();
        if let Some(existing) = cache.get(s) {
            return existing;
        }

        let allocated = self.arena.alloc(s.to_string());
        let permanent_ref: &'tcx str = allocated.as_str();

        cache.insert(permanent_ref, permanent_ref);
        permanent_ref
    }
}