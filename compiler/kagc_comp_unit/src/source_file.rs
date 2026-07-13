// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub const DUMMY_FILE_INDEX: usize = 0xFFFFFFFF;
use std::path::PathBuf;
use typed_arena::Arena;

#[derive(Debug, Default, Clone)]
pub struct FileMeta {
    pub name: String,
    pub abs_path: PathBuf,
}

#[derive(Clone, Debug)]
pub struct SourceFile<'tcx> {
    pub content: &'tcx str,
    pub meta: FileMeta,
}

impl<'tcx> SourceFile<'tcx> {
    /// Loads a file from disk and keeps its content in the arena.
    pub fn from_file(
        path: PathBuf, 
        arena: &'tcx Arena<String>
    ) -> std::io::Result<Self> {
        let raw_content = std::fs::read_to_string(&path)?;
        
        let allocated_str: &'tcx String = arena.alloc(raw_content);

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| "unknown.kag".to_string());

        Ok(Self {
            content: allocated_str.as_str(),
            meta: FileMeta { 
                abs_path: path, 
                name 
            }
        })
    }

    pub fn from_string(
        name: &str, 
        content: &str, 
        arena: &'tcx Arena<String>
    ) -> Self {
        let allocated_str = arena.alloc(content.to_string());

        Self {
            content: allocated_str.as_str(),
            meta: FileMeta { 
                name: name.to_string(), 
                abs_path: PathBuf::from(format!("<{}>", name)) 
            }
        }
    }
}