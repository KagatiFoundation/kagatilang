use crate::file_pool::{FileMeta, FilePool, FilePoolIdx};

#[derive(Debug, Default)]
pub struct FileCtx {
    pool: FilePool,
    pub current: FilePoolIdx
}

impl FileCtx {
    pub fn get(&self, idx: FilePoolIdx) -> Option<&FileMeta> {
        self.pool.get(idx)
    }

    pub fn insert(&mut self, file: FileMeta) -> Option<FilePoolIdx> {
        self.pool.insert(file)
    }
}