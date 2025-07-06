use std::collections::HashMap;

use kagc_symbol::Symbol;

use crate::scope::*;

pub type ScopeItem = (ScopeId, Scope);

#[derive(Debug, Default)]
pub struct ScopeManager {
    pub scopes: HashMap<ScopeId, Scope>
}

impl ScopeManager {
    pub fn deep_lookup(&self, scope_id: ScopeId, name: &str) -> Option<&Symbol> {
        if let Some(scope) = self.get(scope_id) {
            if let Some(sym) = scope.lookup(name) {
                return Some(sym);
            }
            else if let Some(parent_scope) = scope.parent_scope {
                return self.deep_lookup(parent_scope, name);
            }
        }
        None
    }

    pub fn deep_lookup_mut(&mut self, scope_id: ScopeId, name: &str) -> Option<&mut Symbol> {
        let mut current_scope_id = scope_id;
    
        let containing_scope = loop {
            let parent_scope = if let Some(scope) = self.get(current_scope_id) {
                let parent = scope.parent_scope;
                if scope.lookup(name).is_some() {
                    break Some(current_scope_id);
                }
                parent
            } else {
                None
            };
    
            match parent_scope {
                Some(parent_id) => current_scope_id = parent_id,
                None => break None,
            }
        };

        if let Some(scope_id) = containing_scope {
            if let Some(scope) = self.get_mut(scope_id) {
                return scope.lookup_mut(name);
            }
        }

        None
    }

    /// Returns true if the scope or any of its parents has the specified ScopeType.
    pub fn is_inside_scope_type(&self, mut scope_id: ScopeId, target: ScopeType) -> bool {
        while let Some(scope) = self.get(scope_id) {
            if scope.typ == target {
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

    pub fn contains_scope(&self, scope_id: ScopeId) -> bool {
        self.scopes.contains_key(&scope_id)
    }

    pub fn push(&mut self, item: ScopeItem) {
        self.scopes.insert(item.0, item.1);
    }

    pub fn get(&self, scope_id: ScopeId) -> Option<&Scope> {
        self.scopes.get(&scope_id)
    }

    pub fn get_mut(&mut self, scope_id: ScopeId) -> Option<&mut Scope> {
        self.scopes.get_mut(&scope_id)
    }

    pub fn last(&self) -> Option<(&ScopeId, &Scope)> {
        self.scopes.iter().last()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}