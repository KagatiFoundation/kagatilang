// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::{cell::{Cell, RefCell}, fmt::Debug};
use std::collections::HashMap;

use kagc_ast::NodeId;
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
	Block,

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

pub struct ScopeDatabase<'tcx> {
    /// Holds the permanent ownership of all scope structures via an arena allocation
    pub scope_arena: &'tcx typed_arena::Arena<Scope<'tcx>>,
    
    /// Maps a unique ScopeId back to its arena-allocated scope structural asset
    pub scopes: RefCell<HashMap<ScopeId, &'tcx Scope<'tcx>>>,
    
    /// Maps an individual AST node ID directly to its pre-calculated scope block boundary
    pub node_to_scope_map: RefCell<HashMap<NodeId, ScopeId>>,
    
    /// An internal counter to safely mint unique, incremental ScopeId boundaries
    next_scope_id: Cell<usize>,
}

impl<'tcx> ScopeDatabase<'tcx> {
    pub fn new(scope_arena: &'tcx typed_arena::Arena<Scope<'tcx>>) -> Self {
        Self {
            scope_arena,
            scopes: RefCell::default(),
            node_to_scope_map: RefCell::default(),
            next_scope_id: Cell::new(0),
        }
    }

	pub fn create_scope(
        &self, 
        sym_arena: &'tcx typed_arena::Arena<Sym<'tcx>>, 
        ty: ScopeType, 
        parent: Option<ScopeId>
    ) -> ScopeId {
        let current_id = ScopeId(self.next_scope_id.get());
        self.next_scope_id.set(current_id.0 + 1);

        let new_scope = Scope {
            symt: SymTable::new(sym_arena),
            parent,
            ty,
            id: Cell::new(current_id),
        };

        let allocated_ref = self.scope_arena.alloc(new_scope);
        
        self.scopes.borrow_mut().insert(current_id, allocated_ref);

        current_id
    }

	pub fn get_scope(&self, id: ScopeId) -> Option<&'tcx Scope<'tcx>> {
        let scopes = self.scopes.borrow();
        scopes.get(&id).copied()
    }

	pub fn lookup_sym(&self, starting_scope: ScopeId, name: &str) -> Option<&'tcx Sym<'tcx>> {
        let scopes = self.scopes.borrow();
        let current_scope = scopes.get(&starting_scope)?;
        
        if let Some(sym_ref) = current_scope.symt.symbols.borrow().get(name) {
            return Some(*sym_ref);
        }
        
        if let Some(parent_id) = current_scope.parent {
            drop(scopes); 
            self.lookup_sym(parent_id, name)
        } else {
            None
        }
    }

	pub fn iter(&self) -> impl Iterator<Item = &'tcx Scope<'tcx>> {
        let scopes = self.scopes.borrow();
        scopes.values().copied().collect::<Vec<_>>().into_iter()
    }
}