// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::fs;
use std::path::PathBuf;

use crate::source::FileMeta;

use super::SourceFile;

pub struct SourceFileLoader;

impl SourceFileLoader {
    /// Loads a file from disk and returns a SourceFile.
    pub fn load<'tcx>(path: &PathBuf, arena: &'tcx typed_arena::Arena<String>) -> std::io::Result<SourceFile<'tcx>> {
        let content = fs::read_to_string(path)?;
        let alloced_str = arena.alloc(content);
        Ok(SourceFile {
            content: alloced_str,
            meta: FileMeta {
                name: path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default(),
                abs_path: PathBuf::from(path)
            }
        })
    }
}