// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#![allow(clippy::new_without_default)]

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use crate::function::FuncId;

use super::Func;

pub struct FuncTable<'tcx> {
    arena: &'tcx typed_arena::Arena<Func<'tcx>>,
    functions: RefCell<HashMap<&'tcx str, &'tcx Func<'tcx>>>,
    next_id: Cell<usize>,
}

impl<'tcx> FuncTable<'tcx> {
    pub fn new(arena: &'tcx typed_arena::Arena<Func<'tcx>>) -> Self {
        Self {
            arena,
            functions: RefCell::new(HashMap::new()),
            next_id: Cell::new(0) // begin the counter
        }
    }

    pub fn get(&self, name: &str) -> Option<&'tcx Func<'tcx>> {
        let functions = self.functions.borrow();
        functions.get(name).copied()
    }

    pub fn add(&self, name: &'tcx str, func: Func<'tcx>) -> Result<&'tcx Func<'tcx>, &'tcx Func<'tcx>>  {
        let mut functions = self.functions.borrow_mut();
        if let Some(existing) = functions.get(name) {
            return Err(*existing);
        }

        let func_id = self.next_id.replace(self.next_id.get() + 1);
        func.id.replace(FuncId(func_id));

        let alloced = self.arena.alloc(func);
        functions.insert(name, alloced);
        Ok(alloced)
    }

    pub fn get_by_id(&self, id: FuncId) -> Option<&'tcx Func<'tcx>> {
        let functions = self.functions.borrow();
        let function = functions.iter().find(|&(_, &func)| func.id.get() == id)?;
        Some(function.1)
    }

    pub fn count(&self) -> usize {
        self.functions.borrow().len()
    }
}