use std::collections::HashSet;

use kagc_symbol::function::FunctionInfoTable;
use kagc_symbol::record::RecordRegistery;

use crate::ctx::{CurrentScopeMeta, ScopeCtx};
use crate::manager::ScopeManager;
use crate::scope::{ScopeId, ScopeType};

pub struct ScopeCtxBuilder {
    functions: Option<FunctionInfoTable>,
    current_function: Option<usize>,
    scope_mgr: Option<ScopeManager>,
    current_scope: Option<CurrentScopeMeta>,
    scope_id_counter: Option<usize>,
    prev_scope: Option<usize>,
    records: Option<RecordRegistery>,
    user_types: Option<HashSet<String>>,
}

impl ScopeCtxBuilder {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            functions: None,
            current_function: None,
            scope_mgr: None,
            current_scope: None,
            scope_id_counter: None,
            prev_scope: None,
            records: None,
            user_types: None,
        }
    }

    pub fn functions(mut self, functions: FunctionInfoTable) -> Self {
        self.functions = Some(functions);
        self
    }

    pub fn current_function(mut self, current_function: usize) -> Self {
        self.current_function = Some(current_function);
        self
    }

    pub fn scope_manager(mut self, scope_mgr: ScopeManager) -> Self {
        self.scope_mgr = Some(scope_mgr);
        self
    }

    pub fn current_scope(mut self, current_scope: CurrentScopeMeta) -> Self {
        self.current_scope = Some(current_scope);
        self
    }

    pub fn scope_id_counter(mut self, scope_id_counter: usize) -> Self {
        self.scope_id_counter = Some(scope_id_counter);
        self
    }

    pub fn previous_scope(mut self, prev_scope: usize) -> Self {
        self.prev_scope = Some(prev_scope);
        self
    }

    pub fn records(mut self, records: RecordRegistery) -> Self {
        self.records = Some(records);
        self
    }

    pub fn user_types(mut self, user_types: HashSet<String>) -> Self {
        self.user_types = Some(user_types);
        self
    }

    pub fn build(self) -> ScopeCtx {
        ScopeCtx {
            functions: self.functions.unwrap_or_default(),
            current_function: self.current_function.unwrap_or_default(),
            scope_mgr: self.scope_mgr.unwrap_or_default(),
            current_scope: self.current_scope.unwrap_or(CurrentScopeMeta {
                id: ScopeId::default(),
                typ: ScopeType::default(),
            }),
            scope_id_counter: self.scope_id_counter.unwrap_or(0),
            prev_scope: self.prev_scope.unwrap_or(0),
            records: self.records.unwrap_or_default(),
            user_types: self.user_types.unwrap_or_default(),
        }
    }
}