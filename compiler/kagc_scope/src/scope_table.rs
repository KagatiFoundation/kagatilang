// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use crate::scope::{Scope, ScopeId};

pub struct ScopeTable<'tcx> {
    arena: &'tcx typed_arena::Arena<Scope<'tcx>>,
    pub(crate) scopes: RefCell<HashMap<ScopeId, &'tcx Scope<'tcx>>>,
    next_id: Cell<usize>
}

impl<'tcx> ScopeTable<'tcx> {
    pub fn new(arena: &'tcx typed_arena::Arena<Scope<'tcx>>) -> Self {
        Self {
            arena,
            scopes: RefCell::default(),
            next_id: Cell::new(0) // start counting from 0
        }
    }

    pub fn contains(&self, id: ScopeId) -> bool {
        self.scopes.borrow().contains_key(&id)
    }

    pub fn get(&self, id: ScopeId) -> Option<&'tcx Scope<'tcx>> {
        let scopes = self.scopes.borrow();
        scopes.get(&id).copied()
    }

    pub fn add(&self, scope: Scope<'tcx>) -> Option<&'tcx Scope<'tcx>>  {
        let mut scopes = self.scopes.borrow_mut();

        let scope_id = self.next_id.replace(self.next_id.get() + 1);
        scope.id.replace(ScopeId(scope_id));

        let alloced = self.arena.alloc(scope);
        scopes.insert(ScopeId(scope_id), alloced);
        Some(alloced)
    }

    pub fn iter(&self) -> impl Iterator<Item = &'tcx Scope<'tcx>> {
        let scopes = self.scopes.borrow();
        scopes.values().copied().collect::<Vec<_>>().into_iter()
    }
}