// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::TyKind;

pub type RecordFieldOffset = usize;

#[derive(Debug, Default, Clone)]
pub struct RecordType<'tcx> {
    pub name: &'tcx str,
    pub size: usize,
    pub fields: Vec<RecordFieldType<'tcx>>,
    pub __alignment: usize
}

impl<'tcx> RecordType<'tcx> {
    pub fn new(name: &'tcx str) -> Self {
        let size = Self::calc_size();
        Self {
            name,
            size,
            fields: vec![],
            __alignment: 0
        }
    }

    fn calc_size() -> usize {
        0
    }
}

#[derive(Debug, Clone)]
pub struct RecordFieldType<'tcx> {
    pub name: &'tcx str,
    pub ty: TyKind<'tcx>,
    pub rel_stack_off: usize
}