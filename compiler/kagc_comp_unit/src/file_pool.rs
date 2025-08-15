use std::collections::HashMap;

pub type FilePoolIdx = usize;

#[derive(Debug, Default)]
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