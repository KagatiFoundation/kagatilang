/*
MIT License

Copyright (c) 2023 Kagati Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

use std::collections::HashSet;

use itertools::Itertools;
use kagc_comp_unit::file_pool::{FilePool, FilePoolIdx};
use kagc_const::pool::ConstPool;
use kagc_scope::{manager::*, scope::*};
use kagc_symbol::{
    function::{
        FunctionInfo, 
        FunctionInfoTable, 
        INVALID_FUNC_ID
    }, 
    record::RecordRegistery, 
    registery::Registry, *
};
use kagc_types::record::RecordType;

// Scope metadata
#[derive(Debug)]
struct __CompilerScope {
    typ: ScopeType,
    id: usize,
}

#[derive(Debug)]
pub struct CompilerCtx {
    /// Functions' information table.
    pub func_table: FunctionInfoTable,

    pub current_function: usize,

    pub scope_mgr: ScopeManager,
    
    scope_id: usize,

    current_scope: __CompilerScope,

    prev_scope: usize,

    pub record_registery: RecordRegistery,

    user_created_types: HashSet<String>,

    pub const_pool: ConstPool,

    pub file_pool: FilePool,

    pub current_file: FilePoolIdx
}

impl CompilerCtx {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut scope_mgr: ScopeManager = ScopeManager::default();
        scope_mgr.push((0, Scope::default()));

        Self {
            func_table: FunctionInfoTable::new(),
            current_function: INVALID_FUNC_ID,
            scope_id: 1, // next scope ID
            current_scope: __CompilerScope { 
                typ: ScopeType::Root, 
                id: 0 
            },
            prev_scope: 0,
            scope_mgr,
            record_registery: RecordRegistery::default(),
            user_created_types: HashSet::new(),
            const_pool: ConstPool::default(),
            file_pool: FilePool::default(),
            current_file: 0 // 0 = no file
        }
    }

    /// If the record is created successfully, this function returns 
    /// the name of the record that was created. Otherwise, it returns `None`.
    pub fn create_record(&mut self, record_entry: RecordType) -> Option<String> {
        let rec_name = record_entry.name.clone();
        if self.record_registery.declare(record_entry).is_some() {
            self.user_created_types.insert(rec_name.clone());
            Some(rec_name)
        }
        else { None }
    }

    pub fn lookup_record(&self, rec_name: &str) -> Option<&RecordType> {
        self.record_registery.lookup(&rec_name)
    }

    pub fn lookup_record_mut(&mut self, rec_name: &str) -> Option<&mut RecordType> {
        self.record_registery.lookup_mut(&rec_name)
    }

    pub fn record_exists(&self, rec_name: &str) -> bool {
        self.user_created_types.contains(rec_name)
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
        let new_scope_id: usize = self.scope_id;
        self.scope_mgr.push((new_scope_id, Scope::new(self.current_scope.id, scope_type)));
        self.scope_id += 1;

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

    pub fn lookup_fn(&self, func_id: usize) -> Option<&FunctionInfo> {
        self.func_table.lookup_by_id(func_id)
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