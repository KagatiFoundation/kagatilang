// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_symbol::{Symbol, Symtable};

/// Scope ID
pub(crate) type ScopeId = usize;

#[derive(Debug, Default, Eq, PartialEq)]
pub enum ScopeType {
    #[default] Function,
    Loop,
    If,

    /// Default scope
    Root
}

#[derive(Debug, Default)]
pub struct Scope {
    pub table: Symtable<Symbol>,

    pub parent_scope: Option<ScopeId>,

    pub typ: ScopeType
}

impl Scope {
    pub fn new(parent_scope: ScopeId, typ: ScopeType) -> Self {
        Self {
            table: Symtable::default(),
            parent_scope: Some(parent_scope),
            typ
        }
    }

    pub fn declare(&mut self, sym: Symbol) -> Option<usize> {
        self.table.declare(sym)
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.table.lookup(&name)
    }

    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.table.lookup_mut(&name)
    }
}