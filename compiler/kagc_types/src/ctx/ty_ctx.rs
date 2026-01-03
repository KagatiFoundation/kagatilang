// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::collections::HashSet;

use typed_arena::Arena;

use crate::Ty;

#[derive(Default)]
pub struct TyCtx<'tcx> {
    pub arena: Arena<crate::Ty<'tcx>>,
    pub types_map: RefCell<HashSet<&'tcx Ty<'tcx>>>
}

impl<'tcx> TyCtx<'tcx> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            types_map: RefCell::new(HashSet::new())
        }
    }

    pub fn mk_ty(&'tcx self, ty: Ty<'tcx>) -> &'tcx Ty<'tcx> {
        let mut types = self.types_map.borrow_mut();
        
        if let Some(existing) = types.get(&ty) {
            return existing;
        }
        let allocated = self.arena.alloc(ty);
        types.insert(allocated);
        allocated
    }
}