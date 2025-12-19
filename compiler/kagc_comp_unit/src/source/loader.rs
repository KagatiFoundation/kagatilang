// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

use crate::source::FileMeta;

use super::SourceFile;

pub struct SourceFileLoader;

impl SourceFileLoader {
    /// Loads a file from disk and returns a SourceFile.
    pub fn load(path: &PathBuf) -> std::io::Result<SourceFile> {
        let content = fs::read_to_string(path)?;

        Ok(SourceFile {
            content: Rc::new(content),
            meta: FileMeta {
                name: path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default(),
                abs_path: path.to_string_lossy().to_string()
            }
        })
    }
}