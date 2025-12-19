use std::collections::HashMap;

use crate::source::SourceFile;

pub type FilePoolIdx = usize;

pub const DUMMY_FILE_INDEX: usize = 0xFFFFFFFF;

#[derive(Debug, Default, Clone)]
pub struct FileMeta {
    pub name: String,

    pub abs_path: String
}

#[derive(Debug, Default)]
pub struct FilePool {
    idx: FilePoolIdx,
    files: HashMap<FilePoolIdx, FileMeta>
}

impl FilePool {
    pub fn get(&self, idx: FilePoolIdx) -> Option<&FileMeta> {
        self.files.get(&idx)
    }

    pub fn insert(&mut self, file: FileMeta) -> Option<FilePoolIdx> {
        let idx = self.idx;
        self.idx += 1;
        self.files.insert(idx, file);
        Some(idx)
    }
}

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