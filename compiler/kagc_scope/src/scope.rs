// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::Cell;

use kagc_symbol::{Sym, SymTable, Symbol, Symtable};

/// Scope ID
#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ScopeId(pub usize);

pub const INVALID_SCOPE_ID: usize = 0xFFFFFFFC;

#[derive(Debug, Default, Eq, PartialEq, Clone, Copy)]
pub enum ScopeType {
    Function,
    Loop,
    If,

    /// Default scope
    #[default] 
    Root
}

#[derive(Debug, Default, Clone)]
pub struct Scope<'tcx> {
    pub table: Symtable<Symbol<'tcx>>,

    pub parent_scope: Option<ScopeId>,

    pub typ: ScopeType
}

impl<'tcx> Scope<'tcx> {
    pub fn new(parent_scope: ScopeId, typ: ScopeType) -> Self {
        Self {
            table: Symtable::default(),
            parent_scope: Some(parent_scope),
            typ
        }
    }

    pub fn declare(&mut self, sym: Symbol<'tcx>) -> Option<usize> {
        self.table.declare(sym)
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol<'tcx>> {
        self.table.lookup(&name)
    }

    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol<'tcx>> {
        self.table.lookup_mut(&name)
    }
}

pub struct _Scope<'tcx> {
    symt: SymTable<'tcx>,
    pub(crate) parent: Option<ScopeId>,
    pub(crate) ty: ScopeType,
    pub(crate) id: Cell<ScopeId>
}

impl<'tcx> _Scope<'tcx> {
    pub fn new(
        arena: &'tcx typed_arena::Arena<Sym<'tcx>>, 
        ty: ScopeType,
        parent: Option<ScopeId>
    ) -> Self {
        Self {
            symt: SymTable::new(arena),
            parent,
            ty,
            id: Cell::new(ScopeId(INVALID_SCOPE_ID))
        }
    }

    pub fn add_sym(&self, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        self.symt.add(sym)
    }

    pub fn get_sym(&'tcx self, name: &str) -> Option<&'tcx Sym<'tcx>> {
        self.symt.get(name)
    }
}