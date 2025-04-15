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
    cell::RefCell,
    fs::File,
    io::{Error, Read},
    path::Path,
    rc::Rc,
    str::FromStr,
};

use kagc_lexer::Tokenizer;
use kagc_token::Token;

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

/// Meta information of the source files.
#[derive(Debug, Clone)]
pub struct SourceFileMeta {
    /// Path of the source file.
    pub path: String,

    /// Name of the source file.
    pub name: String,
}

impl SourceFileMeta {
    pub fn new(path: String, name: String) -> Self {
        Self {
            path,
            name
        }
    }
}

#[derive(Clone, Debug)]
pub struct SourceFile {
    /// Meta information
    pub meta_info: SourceFileMeta,

    /// This contains the tokens after they have been
    /// generated
    pub tokens: Option<Vec<Token>>,

    /// Tracks different stages of this file.
    pub stage: ParsingStage,

    /// Contains the source file's content in string format after
    /// the file has been read.
    pub source: Rc<String>,
}

impl SourceFile {
    pub fn new(path: &str) -> Self {
        let file_path: &Path = Path::new(&path);
        let mut stage: ParsingStage = ParsingStage::Unprocessed;

        if !file_path.exists() {
            stage = ParsingStage::Error(ParsingStageError::FileNotFound);
            panic!("File not found: {:?}", file_path);
        }

        if !file_path.is_file() {
            panic!("{} is not a file!", path);
        }

        Self {
            meta_info: SourceFileMeta::new(
                String::from_str(path).ok().unwrap(),
                file_path
                    .file_name()
                    .map(|name| name.to_string_lossy().into_owned())
                    .unwrap_or(String::from(""))
            ),
            tokens: None,
            stage,
            source: Rc::new("".to_string()),
        }
    }

    /// Read file into the internal ```source``` field.
    ///
    /// Returns ```Result<i32, Error>```.
    pub fn read(&mut self) -> Result<i32, Error> {
        let mut file_res: Result<File, std::io::Error> = File::open(Path::new(&self.meta_info.path));
        let mut file_size: i32 = -1;

        if let Ok(ref mut file) = file_res {
            let mut input_holder: String = String::from("");
            let file_read_res: Result<usize, std::io::Error> = file.read_to_string(&mut input_holder);

            // read as string
            self.source = Rc::new(input_holder.clone());

            if let Ok(fs) = file_read_res {
                self.stage = ParsingStage::Loaded;
                file_size = fs as i32;
            } 
            else {
                self.stage = ParsingStage::Error(ParsingStageError::FileReadingError);
                return Err(file_read_res.err().unwrap());
            }
        }
        Ok(file_size)
    }

    /// Converts the source content of the ```SourceFile``` into a stream
    /// of tokens.
    pub fn tokenize(&mut self, tokener: Rc<RefCell<Tokenizer>>) {
        let mut tokener_borrow = tokener.borrow_mut();
        self.tokens = Some(tokener_borrow.tokenize(Rc::clone(&self.source)));
    }
}
