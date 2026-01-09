// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#![allow(clippy::new_without_default)]

use std::cell::Cell;

use kagc_types::TyKind;

use crate::sym::StorageClass;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct FuncId(pub usize);

/// Invalid function id
pub const INVALID_FUNC_ID: usize = 0xFFFFFFFD;

/// Represents a function parameter in the symbol table.
#[derive(Clone, Debug)]
pub struct FuncParam<'tcx> {
    pub ty: TyKind<'tcx>,
    pub name: &'tcx str,
    pub offset: i32
}

#[derive(Clone, Debug)]
pub struct Func<'tcx> {
    pub id: Cell<FuncId>,
    pub name: &'tcx str,
    pub ty: TyKind<'tcx>, // return type
    pub params: Vec<&'tcx str>,
    pub param_types: Vec<TyKind<'tcx>>,
    pub storage_class: StorageClass
}

impl<'tcx> Func<'tcx> {
    pub fn new(
        name: &'tcx str, 
        ty: TyKind<'tcx>,
        storage_class: StorageClass,
        params: Vec<&'tcx str>,
        param_types: Vec<TyKind<'tcx>>
    ) -> Self {
        Self {
            name, 
            id: Cell::new(FuncId(INVALID_FUNC_ID)),
            ty, 
            params,
            storage_class,
            param_types
        }
    }
}