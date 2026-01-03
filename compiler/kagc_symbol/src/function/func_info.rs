// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#![allow(clippy::new_without_default)]

use kagc_types::TyKind;

use crate::sym::StorageClass;
use crate::SymbolPos;

/// Inavlid function ID.
pub const INVALID_FUNC_ID: usize = 0xFFFFFFFF;

/// Function ID
pub type FunctionId = usize;

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
pub struct FunctionInfo<'tcx> {
    /// Name of the function.
    pub name: &'tcx str,

    /// ID of the function.
    pub func_id: FunctionId,

    /// The return type of the function.
    pub ty: TyKind<'tcx>,

    /// Contains information about the variables defined locally in 'this' function
    pub local_syms: Vec<SymbolPos>,

    pub param_types: Vec<TyKind<'tcx>>,
    
    /// Storage class of the function.
    pub storage_class: StorageClass
}

impl<'tcx> FunctionInfo<'tcx> {
    pub fn new(
        name: &'tcx str, 
        func_id: usize, 
        ty: TyKind<'tcx>,
        storage_class: StorageClass,
        locals: Vec<SymbolPos>,
        param_types: Vec<TyKind<'tcx>>
    ) -> Self {
        Self {
            name, 
            func_id,
            ty, 
            local_syms: locals,
            storage_class,
            param_types
        }
    }
}