// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#![allow(clippy::new_without_default)]

use std::cell::Cell;

use kagc_types::TyKind;

use crate::sym::StorageClass;
use crate::SymbolPos;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct FuncId(pub usize);

/// Invalid function id
pub const INVALID_FUNC_ID: usize = 0xFFFFFFFD;

/// Represents a function parameter in the symbol table.
#[derive(Clone, Debug)]
pub struct FuncParam<'tcx> {
    /// The type of the function parameter.
    pub ty: TyKind<'tcx>,

    /// The name of the function parameter.
    pub name: &'tcx str,

    pub offset: i32
}

#[derive(Clone, Debug)]
pub struct Func<'tcx> {
    /// Name of the function.
    pub name: &'tcx str,

    /// ID of the function.
    pub id: Cell<FuncId>,

    /// The return type of the function.
    pub ty: TyKind<'tcx>,

    /// Contains information about the variables defined locally in 'this' function
    pub local_syms: Vec<SymbolPos>,

    pub param_types: Vec<TyKind<'tcx>>,
    
    /// Storage class of the function.
    pub storage_class: StorageClass
}

impl<'tcx> Func<'tcx> {
    pub fn new(
        name: &'tcx str, 
        ty: TyKind<'tcx>,
        storage_class: StorageClass,
        locals: Vec<SymbolPos>,
        param_types: Vec<TyKind<'tcx>>
    ) -> Self {
        Self {
            name, 
            id: Cell::new(FuncId(INVALID_FUNC_ID)),
            ty, 
            local_syms: locals,
            storage_class,
            param_types
        }
    }
}