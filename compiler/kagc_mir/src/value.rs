// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Default, Hash)]
pub struct IRValueId(pub usize);

#[derive(Debug, Clone)]
pub enum IRValue {
    Constant(i64),
    Var(IRValueId)
}

impl IRValue {
    pub fn as_value_id(&self) -> Option<IRValueId> {
        match self {
            IRValue::Var(id) => Some(*id),
            _ => None,
        }
    }
}