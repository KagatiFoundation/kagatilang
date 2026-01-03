// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#![allow(clippy::new_without_default)]

use std::cell::RefCell;
use std::collections::HashMap;

use super::FunctionInfo;

#[derive(Default)]
pub struct FunctionInfoTable<'tcx> {
    arena: typed_arena::Arena<FunctionInfo<'tcx>>,
    functions: RefCell<HashMap<&'tcx str, &'tcx FunctionInfo<'tcx>>>
}

impl<'tcx> FunctionInfoTable<'tcx> {
    pub fn new() -> Self {
        Self {
            arena: typed_arena::Arena::new(),
            functions: RefCell::new(HashMap::new())
        }
    }

    pub fn lookup(&'tcx self, name: &str) -> Option<&'tcx FunctionInfo<'tcx>> {
        let functions = self.functions.borrow();
        functions.get(name).copied()
    }

    pub fn declare(&'tcx self, name: &'tcx str, entry: FunctionInfo<'tcx>) -> Option<&'tcx FunctionInfo<'tcx>> {
        let mut functions = self.functions.borrow_mut();
        let allocated = self.arena.alloc(entry);
        functions.insert(name, allocated)
    }

    pub fn lookup_by_id(&self, id: usize) -> Option<&'tcx FunctionInfo<'tcx>> {
        let functions = self.functions.borrow();
        let function = functions.iter().find(|&(_, &func)| func.func_id == id)?;
        Some(function.1)
    }

    pub fn count(&self) -> usize {
        self.functions.borrow().len()
    }
}