use std::collections::HashSet;

use itertools::Itertools;
use kagc_symbol::function::FunctionInfo;
use kagc_symbol::function::FunctionInfoTable;
use kagc_symbol::record::RecordRegistery;
use kagc_symbol::registery::Registry;
use kagc_symbol::StorageClass;
use kagc_symbol::Symbol;
use kagc_types::record::RecordType;

use crate::scope::*;
use crate::manager::*;

#[derive(Debug)]
pub struct CurrentScopeMeta {
    pub(crate) id: ScopeId,
    pub(crate) typ: ScopeType
}

#[derive(Debug)]
pub struct ScopeCtx {
    pub(crate) functions: FunctionInfoTable,
    pub(crate) current_function: usize,
    pub(crate) scope_mgr: ScopeManager,
    pub(crate) current_scope: CurrentScopeMeta,
    pub(crate) scope_id_counter: usize,
    pub(crate) prev_scope: usize,
    pub(crate) records: RecordRegistery,
    pub(crate) user_types: HashSet<String>,
}

impl ScopeCtx {
    pub fn create_record(&mut self, record_entry: RecordType) -> Option<String> {
        let rec_name = record_entry.name.clone();
        if self.records.declare(record_entry).is_some() {
            self.user_types.insert(rec_name.clone());
            Some(rec_name)
        }
        else { None }
    }

    pub fn lookup_record(&self, rec_name: &str) -> Option<&RecordType> {
        self.records.lookup(&rec_name)
    }

    pub fn lookup_record_mut(&mut self, rec_name: &str) -> Option<&mut RecordType> {
        self.records.lookup_mut(&rec_name)
    }

    pub fn record_exists(&self, rec_name: &str) -> bool {
        self.user_types.contains(rec_name)
    }

    pub fn live_scope_id(&self) -> usize {
        self.current_scope.id
    }

    pub fn live_scope(&self) -> Option<&Scope> {
        if self.scope_mgr.is_empty() {
            None
        }
        else {
            self.scope_mgr.get(self.current_scope.id)
        }
    }

    pub fn live_scope_mut(&mut self) -> Option<&mut Scope> {
        if self.scope_mgr.is_empty() {
            None
        }
        else {
            self.scope_mgr.get_mut(self.current_scope.id)
        }
    }

    pub fn root_scope(&self) -> &Scope {
        self.scope_mgr.get(0).unwrap() // root scope's ID is 0 and it is always present
    }

    pub fn root_scope_mut(&mut self) -> &mut Scope {
        self.scope_mgr.get_mut(0).unwrap() // root scope's ID is 0 and it is always present
    }

    pub fn enter_scope(&mut self, scope_id: usize) -> Option<usize> {
        if self.scope_mgr.contains_scope(scope_id) {
            self.prev_scope = self.current_scope.id;
            self.current_scope.id = scope_id;
            Some(self.current_scope.id)
        }
        else {
            None
        }
    }

    pub fn enter_new_scope(&mut self, scope_type: ScopeType) -> usize {
        let new_scope_id: usize = self.scope_id_counter;
        self.scope_mgr.push((new_scope_id, Scope::new(self.current_scope.id, scope_type)));
        self.scope_id_counter += 1;

        // set scope id
        self.prev_scope = self.current_scope.id;
        self.current_scope.id = new_scope_id;
        self.current_scope.id
    }

    /// Returns the ID of the exited scope.
    pub fn exit_scope(&mut self) -> usize {
        let ret: usize = self.current_scope.id;
        self.current_scope.id = self.prev_scope;
        ret
    }

    pub fn deep_lookup(&self, name: &str) -> Option<&Symbol> {
        self.scope_mgr.deep_lookup(self.current_scope.id, name)
    }

    pub fn deep_lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.scope_mgr.deep_lookup_mut(self.current_scope.id, name)
    }

    pub fn declare(&mut self, sym: Symbol) -> Option<usize> {
        if let Some(curr) = self.scope_mgr.get_mut(self.current_scope.id) {
            curr.declare(sym)
        }
        else {
            eprintln!("Couldn't find current scope");
            None
        }
    }

    pub fn declare_fn(&mut self, func: FunctionInfo) -> Option<usize> {
        self.functions.declare(func)
    }

    pub fn lookup_fn(&self, func_id: usize) -> Option<&FunctionInfo> {
        self.functions.lookup_by_id(func_id)
    }

    pub fn lookup_fn_by_name(&self, name: &str) -> Option<&FunctionInfo> {
        self.functions.lookup(&name)
    }

    pub fn update_current_func(&mut self, new_fn: usize) {
        self.current_function = new_fn;
    }

    pub fn current_fn(&self) -> usize {
        self.current_function
    }

    pub fn collect_params(&self, scope_id: usize) -> Vec<&Symbol> {
        if let Some(scope) = self.scope_mgr.get(scope_id) {
            scope.table.iter().filter(|&sym| sym.class == StorageClass::PARAM).collect_vec()
        }
        else {
            vec![]
        }
    }

    pub fn inside_function(&self) -> bool {
        if self.current_scope.typ == ScopeType::Function {
            return true;
        }
        self.scope_mgr.is_inside_scope_type(self.current_scope.id, ScopeType::Function)
    }

    pub fn inside_loop(&self) -> bool {
        if self.current_scope.typ == ScopeType::Loop {
            return true;
        }
        else if self.prev_scope == 0 {
            return false;
        }
        self.scope_mgr.is_inside_scope_type(self.current_scope.id, ScopeType::Loop)
    }

    pub fn inside_if(&self) -> bool {
        if self.current_scope.typ == ScopeType::If {
            return true;
        }
        else if self.prev_scope == 0 {
            return false;
        }
        self.scope_mgr.is_inside_scope_type(self.current_scope.id, ScopeType::If)
    }
}