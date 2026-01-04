// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;

use kagc_symbol::Sym;
use kagc_symbol::function::Func;
use kagc_symbol::function::FuncId;
use kagc_symbol::function::FuncTable;
use kagc_symbol::record::RecordTable;
use kagc_symbol::Symbol;
use kagc_types::record::RecordType;

use crate::ctx::builder::ScopeCtxBuilder;
use crate::scope::*;

#[derive(Debug, Clone, Copy)]
pub struct CurrentScopeMeta {
    pub(crate) id: ScopeId,
    pub(crate) typ: ScopeType
}

pub struct ScopeCtx<'tcx> {
    pub(crate) functions:           FuncTable<'tcx>,
    pub(crate) records:             RecordTable<'tcx>,
    pub(crate) scopes:              HashMap<ScopeId, _Scope<'tcx>>,
    pub(crate) user_types:          RefCell<HashSet<String>>,
    pub(crate) current_function:    Cell<usize>,
    pub(crate) current_scope:       Cell<CurrentScopeMeta>,
    pub(crate) scope_id_counter:    Cell<ScopeId>,
    pub(crate) prev_scope:          Cell<ScopeId>,
    pub(crate) scope_stack:         RefCell<HashSet<ScopeId>>,
    pub(crate) sym_arena:           &'tcx typed_arena::Arena<Sym<'tcx>>,
}

impl<'tcx> ScopeCtx<'tcx> {
    pub fn new(
        sym_arena: &'tcx typed_arena::Arena<Sym<'tcx>>,
        rec_arena: &'tcx typed_arena::Arena<RecordType<'tcx>>
    ) -> Self {
        let mut scope_manager = HashMap::new();
        scope_manager.insert(ScopeId(0), _Scope::new(sym_arena, ScopeType::Root)); // the root scope(id 0)

        let records = RecordTable::new(rec_arena);

        ScopeCtxBuilder::new()
            .scope_manager(scope_manager)
            .records(records)
            .sym_arena(sym_arena)
            .scope_id_counter(ScopeId(1)) // since zero(0) is for the root scope
            .build()
    }

    pub fn create_record(&'tcx self, name: &'tcx str, record_entry: RecordType<'tcx>) -> Option<&'tcx str> {
        let rec_name = record_entry.name;
        if self.records.declare(name, record_entry).is_some() {
            self.user_types.borrow_mut().insert(rec_name.to_string());
            Some(rec_name)
        }
        else { None }
    }

    pub fn lookup_record(&self, rec_name: &str) -> Option<&'tcx RecordType<'tcx>> {
        self.records.lookup(rec_name)
    }

    pub fn record_exists(&self, rec_name: &str) -> bool {
        self.user_types.borrow().contains(rec_name)
    }

    pub fn live_scope_id(&self) -> ScopeId {
        self.current_scope.get().id
    }

    pub fn root_scope(&self) -> &'tcx _Scope {
        todo!()
        // self.scopes.borrow().get(&ScopeId(0)).unwrap() // root scope's ID is 0 and it is always present
    }

    pub fn enter_scope(&self, scope_id: ScopeId) -> Option<ScopeId> {
        let scopes = &self.scopes;
        if scopes.contains_key(&scope_id) {
            self.prev_scope.replace(self.current_scope.get().id);
            let curr_scope = self.current_scope.get();
            self.current_scope.replace(CurrentScopeMeta { id: scope_id, typ: curr_scope.typ });
            Some(curr_scope.id)
        }
        else {
            None
        }
    }

    pub fn enter_new_scope(&mut self, scope_type: ScopeType) -> ScopeId {
        let new_scope_id = self.scope_id_counter.get();
        let scopes = &mut self.scopes;
        // self.scopes.insert(new_scope_id, Scope::new(self.current_scope.get().id, scope_type));
        scopes.insert(new_scope_id, _Scope::new(self.sym_arena, scope_type));
        self.scope_id_counter.replace(ScopeId(self.scope_id_counter.get().0 + 1));

        // set scope id
        self.prev_scope.replace( self.current_scope.get().id);
        let curr_scope = self.current_scope.get();
        self.current_scope.replace(CurrentScopeMeta { id: new_scope_id, typ: curr_scope.typ });
        curr_scope.id
    }

    /// Returns the ID of the exited scope.
    pub fn exit_scope(&self) -> ScopeId {
        let ret = self.current_scope.get();
        self.current_scope.replace(CurrentScopeMeta { id: self.prev_scope.get(), typ: ret.typ });
        ret.id
    }

    pub fn deep_lookup(&self, scope_id: Option<ScopeId>, name: &str) -> Option<&'tcx Sym> {
        if let Some(scope) = self.scopes.get(&scope_id.unwrap_or(self.current_scope.get().id)) {
            if let Some(sym) = scope.get_sym(name) {
                return Some(sym);
            }
            else if let Some(parent_scope) = scope.parent_scope {
                return self.deep_lookup(Some(parent_scope), name);
            }
        }
        None
    }

    pub fn declare_in_scope(&'tcx self, scope_id: ScopeId, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        let Some(scope) = self.scopes.get(&scope_id) else {
            panic!("Couldn't find scope '{:#?}'", scope_id);
        };
        scope.add_sym(sym)
    }

    pub fn declare(&'tcx self, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        let Some(current_scope) = self.scopes.get(&self.current_scope.get().id) else {
            panic!("Couldn't find current scope");
        };
        current_scope.add_sym(sym)
    }

    pub fn declare_fn(&'tcx self, name: &'tcx str, func: Func<'tcx>) -> Result<&'tcx Func<'tcx>, &'tcx Func<'tcx>> {
        self.functions.add(name, func)
    }

    pub fn lookup_fn(&self, func_id: usize) -> Option<&Func> {
        self.functions.get_by_id(FuncId(func_id))
    }

    pub fn lookup_fn_by_name(&'tcx self, name: &str) -> Option<&'tcx Func<'tcx> > {
        self.functions.get(name)
    }

    pub fn update_current_func(&self, new_fn: usize) {
        self.current_function.replace(new_fn);
    }

    pub fn current_fn(&self) -> usize {
        self.current_function.get()
    }

    pub fn collect_params(&self, scope_id: ScopeId) -> Vec<&Symbol> {
        vec![]
    }

    pub fn is_inside_scope_type(&self, mut scope_id: ScopeId, target: ScopeType) -> bool {
        while let Some(scope) = self.scopes.get(&scope_id) {
            if scope.ty == target {
                return true;
            }
            if let Some(parent) = scope.parent_scope {
                scope_id = parent;
            } else {
                break;
            }
        }
        false
    }

    pub fn inside_function(&self) -> bool {
        if self.current_scope.get().typ == ScopeType::Function {
            return true;
        }
        self.is_inside_scope_type(self.current_scope.get().id, ScopeType::Function)
    }

    pub fn inside_loop(&self) -> bool {
        if self.current_scope.get().typ == ScopeType::Loop {
            return true;
        }
        else if self.prev_scope.get().0 == 0 {
            return false;
        }
        self.is_inside_scope_type(self.current_scope.get().id, ScopeType::Loop)
    }

    pub fn inside_if(&self) -> bool {
        if self.current_scope.get().typ == ScopeType::If {
            return true;
        }
        else if self.prev_scope.get().0 == 0 {
            return false;
        }
        self.is_inside_scope_type(self.current_scope.get().id, ScopeType::If)
    }
}