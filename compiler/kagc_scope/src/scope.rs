// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::{cell::Cell, fmt::Debug};

use kagc_symbol::{Sym, SymTable};

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

pub struct Scope<'tcx> {
    pub(crate) symt: SymTable<'tcx>,
    pub(crate) parent: Option<ScopeId>,
    pub(crate) ty: ScopeType,
    pub id: Cell<ScopeId>
}

impl Debug for Scope<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Scope").field("symt", &self.symt).field("parent", &self.parent).field("ty", &self.ty).field("id", &self.id).finish()
    }
}

impl<'tcx> Scope<'tcx> {
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