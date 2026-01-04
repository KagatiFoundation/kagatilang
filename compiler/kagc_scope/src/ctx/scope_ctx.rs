// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::Cell;
use std::cell::RefCell;
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
use crate::scope_table::ScopeTable;

#[derive(Debug, Clone, Copy)]
pub struct _ScopeMeta {
    pub(crate) id: ScopeId,
    pub(crate) typ: ScopeType
}

pub struct ScopeCtx<'tcx> {
    pub(crate) functions:           FuncTable<'tcx>,
    pub(crate) records:             RecordTable<'tcx>,
    pub(crate) scopes:              ScopeTable<'tcx>,
    pub(crate) user_types:          RefCell<HashSet<String>>,
    pub(crate) current_func_id:     Cell<FuncId>,
    pub(crate) current:             Cell<_ScopeMeta>,
    pub(crate) previous:            Cell<_ScopeMeta>,
    pub(crate) next_id:             Cell<ScopeId>,
    pub(crate) stack:               RefCell<Vec<ScopeId>>,
    pub(crate) sym_arena:           &'tcx typed_arena::Arena<Sym<'tcx>>,
}

impl<'tcx> ScopeCtx<'tcx> {
    pub fn new(
        sym_arena: &'tcx typed_arena::Arena<Sym<'tcx>>,
        func_arena: &'tcx typed_arena::Arena<Func<'tcx>>,
        rec_arena: &'tcx typed_arena::Arena<RecordType<'tcx>>,
        scope_arena: &'tcx typed_arena::Arena<_Scope<'tcx>>
    ) -> Self {
        let scope_table = ScopeTable::new(scope_arena);
        scope_table.add(
            _Scope::new(
                sym_arena,
                ScopeType::Root,
                None // root scope has no parent(lol that's sad)
            )
        ); // the root scope(id 0)

        let rec_table = RecordTable::new(rec_arena);
        let func_table = FuncTable::new(func_arena);

        ScopeCtxBuilder::new()
            .scopes(scope_table)
            .records(rec_table)
            .functions(func_table)
            .sym_arena(sym_arena)
            .scope_id_counter(ScopeId(1)) // since zero(0) is for the root scope
            .build()
    }

    pub fn create_record(&self, name: &'tcx str, record_entry: RecordType<'tcx>) -> Option<&'tcx str> {
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

    pub fn live_id(&self) -> ScopeId {
        self.current.get().id
    }

    pub fn root(&self) -> &'tcx _Scope {
        todo!()
        // self.scopes.borrow().get(&ScopeId(0)).unwrap() // root scope's ID is 0 and it is always present
    }

    pub fn enter(&self, scope_id: ScopeId) -> Option<ScopeId> {
        let scopes = &self.scopes;
        if scopes.contains(scope_id) {
            let curr_scope = self.current.get();
            self.current.replace(_ScopeMeta { id: scope_id, typ: curr_scope.typ });
            Some(curr_scope.id)
        }
        else {
            None
        }
    }

    /// Creates a new scope with the given type.
    /// 
    /// Returns the ID of newly created scope.
    pub fn push(&self, scope_type: ScopeType) -> ScopeId {
        let scope_id = self.next_id.replace(ScopeId(self.next_id.get().0 + 1));

        let parent = self.scopes.iter().last().expect("Stack required non-empty").id.get();
        self.scopes.add(_Scope::new(self.sym_arena, scope_type, Some(parent)));

        self.stack.borrow_mut().push(scope_id);
        let curr_scope = self.current.replace(_ScopeMeta { id: scope_id, typ: scope_type });

        self.previous.replace(curr_scope);
        scope_id
    }

    /// Goes back to the previous scope.
    /// 
    /// Returns the ID of the popped scope.
    pub fn pop(&self) -> ScopeId {
        if self.stack.borrow().len() <= 1 {
            panic!("Attempted to pop the root scope!");
        }

        let popped_id = self.stack.borrow_mut().pop().unwrap();
        let stack = self.stack.borrow();
        let new_top_id = *stack.last().unwrap();
        let new_top_scope = self.scopes.get(new_top_id).expect("Sync error");

        self.current.replace(_ScopeMeta { 
            id: new_top_id, 
            typ: new_top_scope.ty 
        });

        // If there is another scope below our new top, that is the new 'previous'
        if stack.len() > 1 {
            let parent_id = stack[stack.len() - 2];
            let parent_scope = self.scopes.get(parent_id).unwrap();
            self.previous.replace(_ScopeMeta { id: parent_id, typ: parent_scope.ty });
        } else {
            // we are back at root, there is no previous.
            self.previous.replace(_ScopeMeta { id: ScopeId(0), typ: ScopeType::Root });
        }
        popped_id
    }

    pub fn deep_lookup(&self, scope_id: Option<ScopeId>, name: &str) -> Option<&'tcx Sym> {
        if let Some(scope) = self.scopes.get(scope_id.unwrap_or(self.current.get().id)) {
            if let Some(sym) = scope.get_sym(name) {
                return Some(sym);
            }
            else if let Some(parent_scope) = scope.parent {
                return self.deep_lookup(Some(parent_scope), name);
            }
        }
        None
    }

    pub fn declare_in_scope(&'tcx self, scope_id: ScopeId, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        let Some(scope) = self.scopes.get(scope_id) else {
            panic!("Couldn't find scope '{:#?}'", scope_id);
        };
        scope.add_sym(sym)
    }

    pub fn declare(&'tcx self, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        let Some(current_scope) = self.scopes.get(self.current.get().id) else {
            panic!("Couldn't find current scope");
        };
        current_scope.add_sym(sym)
    }

    pub fn declare_fn(&'tcx self, name: &'tcx str, func: Func<'tcx>) -> Result<&'tcx Func<'tcx>, &'tcx Func<'tcx>> {
        self.functions.add(name, func)
    }

    pub fn lookup_fn(&self, id: FuncId) -> Option<&Func> {
        self.functions.get_by_id(id)
    }

    pub fn lookup_fn_by_name(&'tcx self, name: &str) -> Option<&'tcx Func<'tcx> > {
        self.functions.get(name)
    }

    pub fn update_current_func(&self, new_fn: FuncId) {
        self.current_func_id.replace(new_fn);
    }

    pub fn current_fn(&self) -> FuncId {
        self.current_func_id.get()
    }

    pub fn collect_params(&self, scope_id: ScopeId) -> Vec<&Symbol> {
        vec![]
    }

    pub fn is_inside_scope_type(&self, mut scope_id: ScopeId, target: ScopeType) -> bool {
        while let Some(scope) = self.scopes.get(scope_id) {
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

    pub fn inside_function(&self) -> bool {
        if self.current.get().typ == ScopeType::Function {
            return true;
        }
        self.is_inside_scope_type(self.current.get().id, ScopeType::Function)
    }

    pub fn inside_loop(&self) -> bool {
        if self.current.get().typ == ScopeType::Loop {
            return true;
        }
        else if self.previous.get().id == ScopeId(0) {
            return false;
        }
        self.is_inside_scope_type(self.current.get().id, ScopeType::Loop)
    }

    pub fn inside_if(&self) -> bool {
        if self.current.get().typ == ScopeType::If {
            return true;
        }
        else if self.previous.get().id == ScopeId(0) {
            return false;
        }
        self.is_inside_scope_type(self.current.get().id, ScopeType::If)
    }
}

#[cfg(test)]
mod tests {
    use kagc_symbol::{Sym, function::Func};
    use kagc_types::record::RecordType;

    use crate::ctx::ScopeCtx;
    use crate::scope::{_Scope, ScopeType};

    #[test]
    fn test_current_and_previous_pointers_on_push() {
        let sym_arena = typed_arena::Arena::<Sym>::new();
        let rec_arena = typed_arena::Arena::<RecordType>::new();
        let func_arena = typed_arena::Arena::<Func>::new();
        let scope_arena = typed_arena::Arena::<_Scope>::new();
        let cx = ScopeCtx::new(
            &sym_arena, 
            &func_arena,
            &rec_arena,
            &scope_arena
        );
        cx.push(ScopeType::Function);

        assert!(cx.current.get().typ == ScopeType::Function);
        assert!(cx.previous.get().typ == ScopeType::Root);

        cx.push(ScopeType::Loop);
        assert!(cx.current.get().typ == ScopeType::Loop);
        assert!(cx.previous.get().typ == ScopeType::Function);

        cx.push(ScopeType::Loop);
        cx.push(ScopeType::If);
        assert!(cx.current.get().typ == ScopeType::If);
        assert!(cx.previous.get().typ == ScopeType::Loop);
    }

    #[test]
    fn test_current_and_previous_pointers_on_pop() {
        let sym_arena = typed_arena::Arena::<Sym>::new();
        let rec_arena = typed_arena::Arena::<RecordType>::new();
        let func_arena = typed_arena::Arena::<Func>::new();
        let scope_arena = typed_arena::Arena::<_Scope>::new();
        let cx = ScopeCtx::new(
            &sym_arena, 
            &func_arena,
            &rec_arena,
            &scope_arena
        );
        cx.push(ScopeType::Function);
        cx.push(ScopeType::Loop);
        cx.push(ScopeType::Loop);
        cx.push(ScopeType::If);

        cx.pop();
        assert!(cx.current.get().typ == ScopeType::Loop);
        assert!(cx.previous.get().typ == ScopeType::Loop);

        cx.pop();
        assert!(cx.current.get().typ == ScopeType::Loop);
        assert!(cx.previous.get().typ == ScopeType::Function);

        cx.pop();
        assert!(cx.current.get().typ == ScopeType::Function);
        assert!(cx.previous.get().typ == ScopeType::Root, "previous is {:#?}", cx.previous.get().typ);
    }
}