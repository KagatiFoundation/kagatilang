// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_types::TyKind;

use crate::Expr;

#[derive(Debug, Clone)]
pub struct RecordField<'tcx> {
    pub typ: TyKind<'tcx>,
    pub name: &'tcx str,
    pub default_value: Option<Expr<'tcx>>
}