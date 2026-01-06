// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};

use kagc_ast::NodeId;
use kagc_symbol::Sym;
use kagc_symbol::function::{FuncId, FuncTable};
use kagc_symbol::record::RecordTable;

use crate::ctx::{_ScopeMeta, ScopeCtx};
use crate::scope::{ScopeId, ScopeType};
use crate::scope_table::ScopeTable;

pub struct ScopeCtxBuilder<'tcx> {
    functions:              Option<FuncTable<'tcx>>,
    current_function:       Option<FuncId>,
    scope_table:            Option<ScopeTable<'tcx>>,
    current_scope:          Option<_ScopeMeta>,
    previous_scope:         Option<_ScopeMeta>,
    scope_id_counter:       Option<ScopeId>,
    records:                Option<RecordTable<'tcx>>,
    user_types:             Option<HashSet<String>>,
    scope_stack:            Option<Vec<ScopeId>>,
    sym_arena:              Option<&'tcx typed_arena::Arena<Sym<'tcx>>>,
    node_scope_map:         Option<HashMap<NodeId, ScopeId>>
}

impl<'tcx> ScopeCtxBuilder<'tcx> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            functions:          None,
            current_function:   None,
            scope_table:        None,
            current_scope:      None,
            previous_scope:     None,
            scope_id_counter:   None,
            records:            None,
            user_types:         None,
            scope_stack:        None,
            sym_arena:          None,
            node_scope_map:     None
        }
    }

    pub fn functions(mut self, functions: FuncTable<'tcx>) -> Self {
        self.functions = Some(functions);
        self
    }

    pub fn current_function(mut self, id: FuncId) -> Self {
        self.current_function = Some(id);
        self
    }

    pub fn scopes(mut self, table: ScopeTable<'tcx>) -> Self {
        self.scope_table = Some(table);
        self
    }

    pub fn current_scope(mut self, current_scope: _ScopeMeta) -> Self {
        self.current_scope = Some(current_scope);
        self
    }

    pub fn previous_scope(mut self, p: _ScopeMeta) -> Self {
        self.previous_scope = Some(p);
        self
    }

    pub fn scope_id_counter(mut self, scope_id_counter: ScopeId) -> Self {
        self.scope_id_counter = Some(scope_id_counter);
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

    pub fn stack(mut self, s: Vec<ScopeId>) -> Self {
        self.scope_stack = Some(s);
        self
    }

    pub fn node_scope_mapping(mut self, m: HashMap<NodeId, ScopeId>) -> Self {
        self.node_scope_map = Some(m);
        self
    }

    pub fn build(self) -> ScopeCtx<'tcx> {
        ScopeCtx {
            functions: self.functions.unwrap(),
            current_func_id: Cell::new(self.current_function.unwrap_or(FuncId(0))), // start counting function ids at 0
            scopes: self.scope_table.unwrap(),
            current: Cell::new(self.current_scope.unwrap_or(_ScopeMeta {
                id: ScopeId::default(),
                typ: ScopeType::default(),
            })),
            previous: Cell::new(self.previous_scope.unwrap_or(_ScopeMeta {
                id: ScopeId::default(),
                typ: ScopeType::default(),
            })),
            next_id: Cell::new(self.scope_id_counter.unwrap_or(ScopeId(0))),
            records: self.records.unwrap(), // unsafe unwrap call
            user_types: RefCell::new(self.user_types.unwrap_or_default()),
            sym_arena: self.sym_arena.unwrap(), // unsafe unwrap call
            stack: RefCell::new(self.scope_stack.unwrap_or_default()),
            node_scope_map: RefCell::new(self.node_scope_map.unwrap_or_default())
        }
    }
}