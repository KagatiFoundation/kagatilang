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

use itertools::Itertools;
use kagc_types::LitTypeVariant;

use crate::{sym::{StorageClass, Symbol}, symbol_table::Symtable};

/// Inavlid function ID.
pub const INVALID_FUNC_ID: usize = 0xFFFFFFFF;

/// Function ID
pub type FunctionId = usize;

/// Represents a function parameter in the symbol table.
#[derive(Clone, Debug)]
pub struct FuncParam {
    /// The type of the function parameter.
    pub lit_type: LitTypeVariant,

    /// The name of the function parameter.
    pub name: String,

    pub offset: i32
}

#[derive(Clone, Debug)]
pub struct FunctionInfo {
    /// Name of the function.
    pub name: String,

    /// ID of the function.
    pub func_id: FunctionId,

    /// The amount of stack space the function takes.
    pub stack_size: i32,

    /// The return type of the function.
    pub return_type: LitTypeVariant,

    /// Contains information about the variables defined locally in 'this' function
    pub local_syms: Symtable<Symbol>,

    pub param_types: Vec<LitTypeVariant>,
    
    /// Storage class of the function.
    pub storage_class: StorageClass
}

impl FunctionInfo {
    pub fn new(
        name: String, 
        func_id: usize, 
        stack_size: i32, 
        return_type: LitTypeVariant,
        storage_class: StorageClass,
        locals: Symtable<Symbol>,
        param_types: Vec<LitTypeVariant>
    ) -> Self {
        Self {
            name, 
            func_id,
            stack_size, 
            return_type, 
            local_syms: locals,
            storage_class,
            param_types
        }
    }

    pub fn get_param(&self, param_name: &str) -> Option<&Symbol> {
        self.collect_params().into_iter().find(|&sym| sym.name == param_name)
    }

    pub fn has_param(&self, param_name: &str) -> bool {
        self.collect_params().into_iter().any(|sym| sym.name == param_name)
    }

    pub fn has_local_sym(&self, local_sym_name: &str) -> bool {
        self.collect_locals().into_iter().any(|sym| sym.name == local_sym_name)
    }

    pub fn collect_locals(&self) -> Vec<&Symbol> {
        self.local_syms.iter().filter(|&sym| sym.class == StorageClass::LOCAL).collect_vec()
    }

    pub fn collect_params(&self) -> Vec<&Symbol> {
        self.local_syms.iter().filter(|&sym| sym.class == StorageClass::PARAM).collect_vec()
    }
}

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