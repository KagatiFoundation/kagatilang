// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;

use kagc_ast::NodeId;
use kagc_symbol::StorageClass;
use kagc_symbol::Sym;
use kagc_symbol::function::Func;
use kagc_symbol::function::FuncId;
use kagc_symbol::function::FuncTable;
use kagc_symbol::record::RecordTable;
use kagc_types::record::RecordType;

use crate::*;

#[derive(Debug, Clone, Copy)]
pub struct ScopeMeta {
    pub id: ScopeId,
    pub typ: ScopeType
}

pub struct ScopeCtx<'tcx> {
    pub(crate) functions:      FuncTable<'tcx>,
    pub(crate) records:        RecordTable<'tcx>,
    pub(crate) scope_db:       crate::ScopeDatabase<'tcx>,
    pub(crate) user_types:     RefCell<HashSet<String>>,
    pub(crate) next_id:        Cell<crate::ScopeId>,
    pub(crate) stack:          RefCell<Vec<crate::ScopeId>>,
    pub(crate) sym_arena:      &'tcx typed_arena::Arena<Sym<'tcx>>,
    pub(crate) node_scope_map: RefCell<HashMap<NodeId, crate::ScopeId>>
}

impl<'tcx> ScopeCtx<'tcx> {
    pub fn new(
        sym_arena: &'tcx typed_arena::Arena<Sym<'tcx>>,
        func_arena: &'tcx typed_arena::Arena<Func<'tcx>>,
        rec_arena: &'tcx typed_arena::Arena<RecordType<'tcx>>,
        scope_arena: &'tcx typed_arena::Arena<Scope<'tcx>>
    ) -> Self {
        let scope_db = ScopeDatabase::new(scope_arena);
        scope_db.create_scope(sym_arena, ScopeType::Root, None); // Root scope is always ID 0

        let rec_table = RecordTable::new(rec_arena);
        let func_table = FuncTable::new(func_arena);
        let scope_stack = vec![ScopeId(0)];

        crate::ctx::builder::ScopeCtxBuilder::new()
            .scopes(scope_db)
            .stack(scope_stack)
            .records(rec_table)
            .functions(func_table)
            .sym_arena(sym_arena)
            .node_scope_mapping(HashMap::default())
            .scope_id_counter(ScopeId(1))
            .build()
    }

    /// Returns the ground-truth active ScopeId directly from the top of the context stack.
    pub fn live_id(&self) -> ScopeId {
        *self.stack.borrow().last().expect("Stack must never be empty")
    }

    /// Safely gets the true active metadata context from the top of the stack.
    pub fn current_meta(&self) -> ScopeMeta {
        let id = self.live_id();
        let scope = self.scope_db.get_scope(id).expect("Scope mapping out of sync");
        ScopeMeta { id, typ: scope.ty }
    }

    pub fn enter(&self, scope_id: ScopeId) -> Option<ScopeId> {
        let _scope = self.scope_db.get_scope(scope_id)?;
        let old_id = self.live_id();
        
        self.stack.borrow_mut().push(scope_id);
        Some(old_id)
    }

    pub fn push(&self, node_id: NodeId, scope_type: ScopeType) -> ScopeId {
        let parent = self.live_id();
        
        let new_scope_id = ScopeId(self.next_id.get().0);
        self.next_id.set(ScopeId(new_scope_id.0 + 1));

        self.scope_db.create_scope(self.sym_arena, scope_type, Some(parent));
        self.node_scope_map.borrow_mut().insert(node_id, new_scope_id);

        self.stack.borrow_mut().push(new_scope_id);
        new_scope_id
    }

    pub fn pop(&self) -> ScopeId {
        let mut stack = self.stack.borrow_mut();
        if stack.len() == 1 {
            panic!("Compiler Error: Attempted to pop the root scope!");
        }
        stack.pop().unwrap()
    }

    pub fn inside_function(&self) -> bool {
        self.is_inside_scope_type(self.live_id(), ScopeType::Function)
    }

    pub fn inside_loop(&self) -> bool {
        self.is_inside_scope_type(self.live_id(), ScopeType::Loop)
    }

    pub fn inside_if(&self) -> bool {
        self.is_inside_scope_type(self.live_id(), ScopeType::If)
    }

    pub fn create_record(&self, name: &'tcx str, record_entry: RecordType<'tcx>) -> Result<&'tcx str, &'tcx str> {
        let rec_name = record_entry.name;
        if self.records.declare(name, record_entry).is_ok() {
            self.user_types.borrow_mut().insert(rec_name.to_string());
            Ok(rec_name)
        } else {
            Err(rec_name)
        }
    }

    pub fn lookup_record(&self, rec_name: &str) -> Option<&'tcx RecordType<'tcx>> {
        self.records.lookup(rec_name)
    }

    pub fn record_exists(&self, rec_name: &str) -> bool {
        self.user_types.borrow().contains(rec_name)
    }

    pub fn root(&self) -> &'tcx Scope<'tcx> {
        self.scope_db.get_scope(ScopeId(0)).expect("No root scope set")
    }

    pub fn lookup_sym(&self, scope_id: Option<ScopeId>, name: &str) -> Option<&'tcx Sym<'tcx>> {
        let starting_id = scope_id.unwrap_or_else(|| self.live_id());
        if let Some(scope) = self.scope_db.get_scope(starting_id) {
            if let Some(sym) = scope.get_sym(name) {
                Some(sym)
            } else if let Some(parent_scope) = scope.parent {
                self.lookup_sym(Some(parent_scope), name)
            } else {
                None
            }
        } else {
            panic!("Scope with ID '{starting_id:#?}' not found");
        }
    }

    pub fn declare_sym_in_scope(&self, scope_id: ScopeId, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        let scope = self.scope_db.get_scope(scope_id).unwrap_or_else(|| panic!("Couldn't find scope '{:#?}'", scope_id));
        scope.add_sym(sym)
    }

    pub fn declare_sym(&self, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        let scope = self.scope_db.get_scope(self.live_id()).expect("Missing active traversal scope");
        scope.add_sym(sym)
    }

    pub fn declare_fn(&self, name: &'tcx str, func: Func<'tcx>) -> Result<&'tcx Func<'tcx>, &'tcx Func<'tcx>> {
        self.functions.add(name, func)
    }

    pub fn lookup_fn(&self, id: FuncId) -> Option<&Func<'tcx>> {
        self.functions.get_by_id(id)
    }

    pub fn lookup_fn_by_name(&self, name: &str) -> Option<&'tcx Func<'tcx> > {
        self.functions.get(name)
    }

    pub fn collect_params(&self, scope_id: ScopeId) -> Vec<&Sym<'tcx>> {
        let scope = self.scope_db.get_scope(scope_id).unwrap_or_else(|| panic!("Scope not found"));
        let symt = scope.symt.symbols.borrow();
        
        let mut params = vec![];
        for sym in symt.values() {
            if sym.class == StorageClass::PARAM {
                params.push(*sym);
            }
        }
        params
    }

    pub fn is_inside_scope_type(&self, mut scope_id: ScopeId, target: ScopeType) -> bool {
        while let Some(scope) = self.scope_db.get_scope(scope_id) {
            if scope.ty == target {
                return true;
            }
            if let Some(parent) = scope.parent {
                scope_id = parent;
            } else {
                break;
            }
        }
        false
    }

    pub fn lookup_scope(&self, scope_id: ScopeId) -> Option<&'tcx Scope<'tcx>> {
        self.scope_db.get_scope(scope_id)
    }

    pub fn lookup_node_scope(&self, node_id: NodeId) -> Option<&'tcx Scope<'tcx>> {
        let scope_id = *self.node_scope_map.borrow().get(&node_id)?;
        self.scope_db.get_scope(scope_id)
    }

    pub fn dump(&self) {
        println!("{:#?}", self.scope_db.scopes.borrow());
    }
}