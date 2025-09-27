// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Default)]
pub struct IRValueId(pub usize);

#[derive(Debug, Clone)]
pub enum IRValue {
    Constant(i64),
    Variable(IRValueId) // %1, %2, etc
}