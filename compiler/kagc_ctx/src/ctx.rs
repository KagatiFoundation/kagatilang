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

use itertools::Itertools;
use kagc_scope::{manager::*, scope::*};
use kagc_symbol::*;

#[derive(Debug, Eq, PartialEq)]
pub enum CompilerScope {
    GLOBAL,
    FUNCTION
}

#[derive(Debug)]
pub struct CompilerCtx {
    /// Functions information table.
    pub func_table: FunctionInfoTable,

    pub current_function: usize,

    pub scope_mgr: ScopeManager,
    
    scope_id: usize,

    current_scope: usize,
    prev_scope: usize,
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
            current_scope: 0,
            prev_scope: 0,
            scope_mgr,
        }
    }

    pub fn live_scope_id(&self) -> usize {
        self.current_scope
    }

    pub fn live_scope(&self) -> Option<&Scope> {
        if self.scope_mgr.is_empty() {
            None
        }
        else {
            self.scope_mgr.get(self.current_scope)
        }
    }

    pub fn live_scope_mut(&mut self) -> Option<&mut Scope> {
        if self.scope_mgr.is_empty() {
            None
        }
        else {
            self.scope_mgr.get_mut(self.current_scope)
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
            self.prev_scope = self.current_scope;
            self.current_scope = scope_id;
            Some(self.current_scope)
        }
        else {
            None
        }
    }

    pub fn enter_new_scope(&mut self) -> usize {
        let new_scope_id: usize = self.scope_id;
        self.scope_mgr.push((new_scope_id, Scope::new(self.current_scope)));
        self.scope_id += 1;

        // set scope id
        self.prev_scope = self.current_scope;
        self.current_scope = new_scope_id;
        self.current_scope
    }

    /// Returns the ID of the exited scope.
    pub fn exit_scope(&mut self) -> usize {
        let ret: usize = self.current_scope;
        self.current_scope = self.prev_scope;
        ret
    }

    pub fn deep_lookup(&self, name: &str) -> Option<&Symbol> {
        self.scope_mgr.deep_lookup(self.current_scope, name)
    }

    pub fn deep_lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.scope_mgr.deep_lookup_mut(self.current_scope, name)
    }

    pub fn declare(&mut self, sym: Symbol) -> Option<usize> {
        if let Some(curr) = self.scope_mgr.get_mut(self.current_scope) {
            curr.declare(sym)
        }
        else {
            None
        }
    }

    pub fn lookup_fn(&self, func_id: usize) -> Option<&FunctionInfo> {
        self.func_table.get_by_id(func_id)
    }

    pub fn collect_params(&self, scope_id: usize) -> Vec<&Symbol> {
        if let Some(scope) = self.scope_mgr.get(scope_id) {
            scope.table.iter().filter(|&sym| sym.class == StorageClass::PARAM).collect_vec()
        }
        else {
            vec![]
        }
    }
}