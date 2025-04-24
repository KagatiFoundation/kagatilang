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

#![allow(clippy::new_without_default)]

use std::collections::HashMap;

use super::FunctionInfo;

#[derive(Debug)]
pub struct FunctionInfoTable {
    functions: HashMap<String, FunctionInfo>,
}

impl FunctionInfoTable {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new()
        }
    }
    
    pub fn add(&mut self, func_info: FunctionInfo) {
        self.functions.insert(func_info.name.clone(), func_info);
    }

    pub fn get(&self, name: &str) -> Option<&FunctionInfo> {
        self.functions.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut FunctionInfo> {
        self.functions.get_mut(name)
    }

    pub fn get_by_id(&self, id: usize) -> Option<&FunctionInfo> {
        let func_info = self.functions.iter().find(|func| func.1.func_id == id);
        if let Some(func) = func_info {
            Some(func.1)
        }
        else {
            None
        }
    }
}