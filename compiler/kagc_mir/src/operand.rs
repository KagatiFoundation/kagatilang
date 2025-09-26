// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::value::IRValueId;

#[derive(Debug, Clone)]
pub enum IROperand {
    Imm(i64),
    Value(IRValueId)
}