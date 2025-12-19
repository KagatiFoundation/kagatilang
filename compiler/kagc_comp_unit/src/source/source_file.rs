// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#![allow(unused_assignments)]

use std::path::Path;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ParsingStageError {
    FileNotFound,
    TokenizationError,
    ParsingError,
    FileReadingError,
    None,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ParsingStage {
    /// Nothing has been done till now. Even the file hasn't
    /// been loaded into memory.
    Unprocessed,

    /// File has been loaded into memory.
    Loaded,

    /// Tokens has been generated for this file.
    Tokenized,

    Parsed,

    /// Some kind of error has occured during reading this file
    /// or during token generation.
    Error(ParsingStageError),
}

pub const DUMMY_FILE_INDEX: usize = 0xFFFFFFFF;

#[derive(Debug, Default, Clone)]
pub struct FileMeta {
    pub name: String,

    pub abs_path: String
}

#[derive(Clone, Debug)]
pub struct SourceFile {
    pub content: Rc<String>,
    pub meta: FileMeta
}

impl SourceFile {
    pub fn from_file(path: &str) -> std::io::Result<Self> {
        let file_path: &Path = Path::new(path);
        let content: String = std::fs::read_to_string(file_path)?;
        let name = file_path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();

        Ok(Self {
            content: Rc::new(content),
            meta: FileMeta { abs_path: path.to_string(), name }
        })
    }

    pub fn from_string(name: &str, content: &str) -> Self {
        Self {
            content: Rc::new(content.to_string()),
            meta: FileMeta { name: name.to_string(), abs_path: format!("<{name}>") }
        }
    }
}