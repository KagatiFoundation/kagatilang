// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::{cell::Cell, fmt::Debug};

use kagc_types::*;

use crate::function::FuncId;

pub type SymbolId = usize;

/// The types of symbol names inside the symbol table
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolType {
    Variable,
    Record {
        name: String
    },
    Function,
    Array,
    Constant, // for now, this is used to represent only the string literals
}

#[derive(Copy, Clone, PartialEq, Debug, Default)]
pub enum StorageClass {
    /// Globally visible symbol.
    #[default]
    GLOBAL, 

    /// Locally visible symbol.
    LOCAL, 

    /// Locally visible function parameter.
    PARAM, 

    /// Externally defined symbol.
    EXTERN
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct SymId(pub usize);

/// Invalid symbol id
pub const INVALID_SYM_ID: usize = 0xFFFFFFFE;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum SymTy<'tcx> {
    Variable,
    Function,
    Record { name: &'tcx str }
}

#[derive(Debug)]
pub struct Sym<'tcx> {
    pub id: Cell<SymId>,
    pub name: &'tcx str,
    pub ty: Cell<TyKind<'tcx>>,
    pub sym_ty: Cell<SymTy<'tcx>>,
    pub class: StorageClass,
    pub function_id: Cell<FuncId>
}

impl<'tcx> Sym<'tcx> {
    pub fn new(
        name: &'tcx str, ty: TyKind<'tcx>,
        sym_ty: SymTy<'tcx>, class: StorageClass,
        func_id: FuncId
    ) -> Self {
        Self {
            id: Cell::new(SymId(INVALID_SYM_ID)),
            name,
            ty: Cell::new(ty),
            sym_ty: Cell::new(sym_ty),
            class,
            function_id: Cell::new(func_id)
        }
    }
}