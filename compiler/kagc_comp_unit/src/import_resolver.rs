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

use std::{
    env, 
    path::{
        Path, 
        PathBuf
    }
};

use crate::source::{
    loader::SourceFileLoader, 
    SourceFile
};

pub struct ImportResolver;

impl ImportResolver {
    pub fn resolve(import_path_str: &str) -> std::io::Result<SourceFile> {
        let import_path = Path::new(import_path_str).with_extension("kag");

        if import_path.exists() {
            return SourceFile::from_file(import_path_str);
        }

        if let Ok(kagc_path) = env::var("KAGC_PATH") {
            let alt_path = PathBuf::from(kagc_path).join(import_path);
            if alt_path.exists() {
                return SourceFileLoader::load(&alt_path);
            }
        }
        else {
            eprintln!("KAGC_PATH is not set.");
        }

        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("No module named '{}' found", import_path_str),
        ))
    }
}