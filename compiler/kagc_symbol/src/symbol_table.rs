// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt::Debug;

use crate::{Sym, SymId};

/// Maximum number of symbols that can be stored in the symbol table.
pub const NSYMBOLS: usize = 1024;

pub struct SymTable<'tcx> {
    arena: &'tcx typed_arena::Arena<Sym<'tcx>>,
    pub symbols: RefCell<HashMap<&'tcx str, &'tcx Sym<'tcx>>>,
    pub next_id: Cell<usize>,
}

impl Debug for SymTable<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SymTable").field("symbols", &self.symbols).field("next_id", &self.next_id).finish()
    }
}

impl<'tcx> SymTable<'tcx> {
    pub fn new(arena: &'tcx typed_arena::Arena<Sym<'tcx>>) -> Self {
        Self {
            arena,
            symbols: RefCell::new(HashMap::new()),
            next_id: Cell::new(0) // begin the count
        }
    }

    pub fn add(&self, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        let mut syms = self.symbols.borrow_mut();
        if let Some(existing) = syms.get(sym.name) {
            return Err(*existing);
        }
        
        let sym_id = self.next_id.replace(self.next_id.get() + 1);
        sym.id.replace(SymId(sym_id));

        let alloced = self.arena.alloc(sym);
        syms.insert(alloced.name, alloced);
        Ok(alloced)
    }

    pub fn get(&'tcx self, name: &str) -> Option<&'tcx Sym<'tcx>> {
        let syms = self.symbols.borrow();
        let sym = syms.get(name)?;
        Some(sym)
    }
}

#[cfg(test)]
mod tests {}