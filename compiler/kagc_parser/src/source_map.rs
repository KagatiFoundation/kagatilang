// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_comp_unit::source::SourceFile;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct FileId(pub usize);

#[derive(Debug, Default, Clone)]
pub struct SourceMap {
    pub files: HashMap<FileId, SourceFile>,
    current: usize
}

impl SourceMap {
    pub fn add_virtual_file(&mut self, name: &str, content: &str) -> FileId {
        let idx = self.current;
        self.current += 1;
        let file = SourceFile::from_string(name, content);
        self.files.insert(FileId(idx), file);
        FileId(idx)
    }

    pub fn add_file(&mut self, file: SourceFile) -> FileId {
        let idx = self.current;
        self.current += 1;
        self.files.insert(FileId(idx), file);
        FileId(idx)
    }
}