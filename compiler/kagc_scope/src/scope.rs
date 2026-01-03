// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_symbol::{Sym, SymTable, Symbol, Symtable};

/// Scope ID
#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ScopeId(pub usize);

#[derive(Debug, Default, Eq, PartialEq, Clone, Copy)]
pub enum ScopeType {
    #[default] Function,
    Loop,
    If,

    /// Default scope
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
    pub symt: SymTable<'tcx>,
    pub parent_scope: Option<ScopeId>,
    pub ty: ScopeType
}

impl<'tcx> _Scope<'tcx> {
    pub fn new(arena: &'tcx typed_arena::Arena<Sym<'tcx>>, ty: ScopeType) -> Self {
        Self {
            symt: SymTable::new(arena),
            parent_scope: None,
            ty
        }
    }

    pub fn add_sym(&self, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        self.symt.add(sym)
    }

    pub fn get_sym(&'tcx self, name: &str) -> Option<&'tcx Sym<'tcx>> {
        self.symt.get(name)
    }
}