// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;

use crate::SourceFile;

#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub struct FileId(pub usize);

pub struct SourceMap<'tcx> {
    arena: &'tcx typed_arena::Arena<SourceFile<'tcx>>,
    files: RefCell<HashMap<FileId, &'tcx SourceFile<'tcx>>>,
    current: FileId, 
}

impl Debug for SourceMap<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f
			.debug_struct("SourceMap")
			.field("files", &self.files)
			.field("current", &self.current)
			.finish()
	}
}

impl<'tcx> SourceMap<'tcx> {
    pub fn new(arena: &'tcx typed_arena::Arena<SourceFile<'tcx>>) -> Self {
        Self {
            arena,
            files: RefCell::default(),
            current: FileId(0)
        }
    }

    pub fn get(&self, idx: FileId) -> Option<&'tcx SourceFile<'tcx>> {
        self.files.borrow().get(&idx).copied()
    }

    pub fn insert(&mut self, file: SourceFile<'tcx>) -> FileId {
        let idx = self.current;

		self.current = FileId(idx.0 + 1);

        let file_ref = self.arena.alloc(file);
        self.files.borrow_mut().insert(idx, file_ref);
        idx
    }
}