// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use kagc_symbol::function::FunctionInfoTable;
use kagc_symbol::record::RecordTable;

use crate::scope::{_Scope, ScopeId};

pub struct ScopeManager<'tcx> {
    pub functions: FunctionInfoTable<'tcx>,
    pub scopes: HashMap<ScopeId, _Scope<'tcx>>,
    pub records: RecordTable<'tcx>,
    pub user_types: RefCell<HashSet<&'tcx str>>,
    pub scope_stack: RefCell<HashSet<ScopeId>>,
}