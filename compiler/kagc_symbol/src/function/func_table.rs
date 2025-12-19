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

use crate::registery::Registry;

use super::FunctionInfo;

#[derive(Debug, Default, Clone)]
pub struct FunctionInfoTable {
    functions: HashMap<String, FunctionInfo>,
}

impl Registry<FunctionInfo> for FunctionInfoTable {
    fn lookup<Key: crate::registery::__RegisLK>(&self, key: &Key) -> Option<&FunctionInfo> {
        if let Some(name) = key.key_as_str() {
            return self.functions.get(name);
        }
        else if key.key_as_int().is_some() {
            panic!("Lookup using integer is not supported by FunctionInfoTable");
        }
        None
    }

    fn lookup_mut<Key: crate::registery::__RegisLK>(&mut self, key: &Key) -> Option<&mut FunctionInfo> {
        if let Some(name) = key.key_as_str() {
            return self.functions.get_mut(name);
        }
        else if key.key_as_int().is_some() {
            panic!("Lookup using integer is not supported by RecordRegistery");
        }
        None
    }

    fn declare(&mut self, entry: FunctionInfo) -> Option<usize> {
        if self.functions.contains_key(&entry.name) {
            None
        }
        else {
            self.functions.insert(entry.name.clone(), entry);
            Some(0)
        }
    }

    fn count(&self) -> usize {
        self.functions.len()
    }
}

impl FunctionInfoTable {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new()
        }
    }

    pub fn lookup_by_id(&self, id: usize) -> Option<&FunctionInfo> {
        let func_info = self.functions.iter().find(|func| func.1.func_id == id);
        if let Some(func) = func_info {
            Some(func.1)
        }
        else {
            None
        }
    }
}