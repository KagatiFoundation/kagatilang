// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::value::IRValueId;

#[derive(Debug, Clone, Copy)]
pub struct IRInstructionId(pub usize);

#[derive(Debug)]
pub enum IRInstruction {
    Mov {
        src: IRValueId
    },

    Add {
        lhs: IRValueId,
        rhs: IRValueId
    }
}