// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[derive(Debug, Clone, Copy)]
pub enum BuiltinFn {
    AssignRef,
    AllocInt,
    AllocStr,
    AllocRec,
    Panic
}