// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use crate::source::SourceFile;

#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub struct FileId(pub usize);

#[derive(Debug, Default)]
pub struct SourceMap {
    pub current: FileId,
    files: HashMap<FileId, SourceFile>,
}

impl SourceMap {
    pub fn get(&self, idx: FileId) -> Option<&SourceFile> {
        self.files.get(&idx)
    }

    pub fn insert(&mut self, file: SourceFile) -> Option<FileId> {
        let idx = self.current;
        self.current = FileId(idx.0 + 1);
        self.files.insert(idx, file);
        Some(idx)
    }
}