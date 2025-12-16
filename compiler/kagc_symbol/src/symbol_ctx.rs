// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::*;

#[derive(Debug, Default, Clone)]
pub struct SymbolContext {
    /// Symbol table for this context
    pub table: Symtable<Symbol>,
}

impl SymbolContext {
    pub fn lookup<Key>(&self, key: &Key) -> Option<&Symbol> 
    where Key: STableLookupKey {
        if let Some(sym) = self.table.lookup(key) {
            return Some(sym);
        }
        None
    }

    pub fn lookup_mut<Key>(&mut self, key: &Key) -> Option<&mut Symbol> 
    where Key: STableLookupKey {
        if let Some(sym) = self.table.lookup_mut(key) {
            return Some(sym);
        }
        None
    }

    pub fn declare(&mut self, sym: Symbol) -> Option<usize> {
        self.table.declare(sym)
    }
}