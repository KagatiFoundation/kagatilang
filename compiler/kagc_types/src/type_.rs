// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyKind<'tcx> {
    I64,
    U8,
    Void,
    Null,
    Str,

    PoolStr,

    // Composite types
    Array {
        elem: &'tcx Ty<'tcx>,
        len: usize,
    },

    Record {
        name: &'tcx str,
    },

    /// None type
    None
}

impl<'tcx> TyKind<'tcx> {
    pub fn is_void(self) -> bool {
        self == Self::Void
    }

    pub fn is_none(self) -> bool {
        self == Self::None
    }

    pub fn is_gc_alloced(&self) -> bool {
        matches!(self, TyKind::Record { .. } | TyKind::Str | TyKind::PoolStr)
    }

    pub fn to_reg_size(&self) -> usize {
        8
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty<'tcx> {
    pub kind: TyKind<'tcx>,
    pub heap_allocated: bool
}

impl<'tcx> Ty<'tcx> {
    pub fn new(kind: TyKind<'tcx>) -> Self {
        Self {
            kind,
            heap_allocated: false
        }
    }

    pub fn heap_allocated(mut self) -> Self {
        self.heap_allocated = true;
        self
    }
}