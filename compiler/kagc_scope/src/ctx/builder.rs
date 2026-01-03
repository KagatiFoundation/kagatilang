// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};

use kagc_symbol::Sym;
use kagc_symbol::function::FunctionInfoTable;
use kagc_symbol::record::RecordTable;

use crate::ctx::{CurrentScopeMeta, ScopeCtx};
use crate::scope::{_Scope, ScopeId, ScopeType};

pub struct ScopeCtxBuilder<'tcx> {
    functions:              Option<FunctionInfoTable<'tcx>>,
    current_function:       Option<usize>,
    scope_mgr:              Option<HashMap<ScopeId, _Scope<'tcx>>>,
    current_scope:          Option<CurrentScopeMeta>,
    scope_id_counter:       Option<ScopeId>,
    prev_scope:             Option<ScopeId>,
    records:                Option<RecordTable<'tcx>>,
    user_types:             Option<HashSet<String>>,
    scope_stack:            Option<HashSet<ScopeId>>,
    sym_arena:              Option<&'tcx typed_arena::Arena<Sym<'tcx>>>,
}

impl<'tcx> ScopeCtxBuilder<'tcx> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            functions:          None,
            current_function:   None,
            scope_mgr:          None,
            current_scope:      None,
            scope_id_counter:   None,
            prev_scope:         None,
            records:            None,
            user_types:         None,
            scope_stack:        None,
            sym_arena:          None,
        }
    }

    pub fn functions(mut self, functions: FunctionInfoTable<'tcx>) -> Self {
        self.functions = Some(functions);
        self
    }

    pub fn current_function(mut self, current_function: usize) -> Self {
        self.current_function = Some(current_function);
        self
    }

    pub fn scope_manager(mut self, scope_mgr: HashMap<ScopeId, _Scope<'tcx>>) -> Self {
        self.scope_mgr = Some(scope_mgr);
        self
    }

    pub fn current_scope(mut self, current_scope: CurrentScopeMeta) -> Self {
        self.current_scope = Some(current_scope);
        self
    }

    pub fn scope_id_counter(mut self, scope_id_counter: ScopeId) -> Self {
        self.scope_id_counter = Some(scope_id_counter);
        self
    }

    pub fn previous_scope(mut self, prev_scope: ScopeId) -> Self {
        self.prev_scope = Some(prev_scope);
        self
    }

    pub fn records(mut self, records: RecordTable<'tcx>) -> Self {
        self.records = Some(records);
        self
    }

    pub fn user_types(mut self, user_types: HashSet<String>) -> Self {
        self.user_types = Some(user_types);
        self
    }

    pub fn sym_arena(mut self, arena: &'tcx typed_arena::Arena<Sym<'tcx>>) -> Self {
        self.sym_arena = Some(arena);
        self
    }

    pub fn build(self) -> ScopeCtx<'tcx> {
        ScopeCtx {
            functions: self.functions.unwrap_or_default(),
            current_function: Cell::new(self.current_function.unwrap_or_default()),
            scopes: self.scope_mgr.unwrap_or_default(),
            current_scope: Cell::new(self.current_scope.unwrap_or(CurrentScopeMeta {
                id: ScopeId::default(),
                typ: ScopeType::default(),
            })),
            scope_id_counter: Cell::new(self.scope_id_counter.unwrap_or(ScopeId(0))),
            prev_scope: Cell::new(self.prev_scope.unwrap_or(ScopeId(0))),
            records: self.records.unwrap(), // unsafe unwrap call
            user_types: RefCell::new(self.user_types.unwrap_or_default()),
            sym_arena: self.sym_arena.unwrap(), // unsafe unwrap call
            scope_stack: RefCell::new(self.scope_stack.unwrap_or_default())
        }
    }
}