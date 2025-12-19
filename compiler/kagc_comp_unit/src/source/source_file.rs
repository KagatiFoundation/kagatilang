/*
MIT License

Copyright (c) 2023 Kagati Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#![allow(unused_assignments)]

use std::path::Path;
use std::rc::Rc;

use crate::source_map::FileMeta;

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