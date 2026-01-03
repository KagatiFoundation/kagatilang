// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::{RefCell, Cell};
use std::collections::HashMap;

use crate::source::SourceFile;

#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub struct FileId(pub usize);

pub struct SourceMap<'tcx> {
    arena: &'tcx typed_arena::Arena<SourceFile<'tcx>>,
    files: RefCell<HashMap<FileId, &'tcx SourceFile<'tcx>>>,
    current: Cell<FileId>, 
}

impl<'tcx> SourceMap<'tcx> {
    pub fn new(arena: &'tcx typed_arena::Arena<SourceFile<'tcx>>) -> Self {
        Self {
            arena,
            files: RefCell::default(),
            current: Cell::default()
        }
    }

    pub fn get(&self, idx: FileId) -> Option<&'tcx SourceFile<'tcx>> {
        self.files.borrow().get(&idx).copied()
    }

    pub fn insert(&self, file: SourceFile<'tcx>) -> FileId {
        let idx = self.current.get();
        self.current.set(FileId(idx.0 + 1));
        let file_ref = self.arena.alloc(file);
        self.files.borrow_mut().insert(idx, file_ref);
        idx
    }
}