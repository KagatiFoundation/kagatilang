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

use kagc_types::LitTypeVariant;

use crate::{
    registery::RegisteryEntry, 
    sym::StorageClass, 
    SymbolPos, 
};

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

    /// The return type of the function.
    pub return_type: LitTypeVariant,

    /// Contains information about the variables defined locally in 'this' function
    pub local_syms: Vec<SymbolPos>,

    pub param_types: Vec<LitTypeVariant>,
    
    /// Storage class of the function.
    pub storage_class: StorageClass
}

impl FunctionInfo {
    pub fn new(
        name: String, 
        func_id: usize, 
        return_type: LitTypeVariant,
        storage_class: StorageClass,
        locals: Vec<SymbolPos>,
        param_types: Vec<LitTypeVariant>
    ) -> Self {
        Self {
            name, 
            func_id,
            return_type, 
            local_syms: locals,
            storage_class,
            param_types
        }
    }
}

impl RegisteryEntry for FunctionInfo {
    fn name(&self) -> String {
        self.name.clone()
    }
}