// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::value::*;

#[derive(Debug, Clone, Copy)]
pub struct IRInstructionId(pub usize);

#[derive(Debug, Clone)]
pub enum IRInstruction {
    Mov {
        result: IRValueId,
        src: IRValue
    },

    Add {
        result: IRValueId,
        lhs: IRValue,
        rhs: IRValue
    }
}

impl IRInstruction {
    pub fn defines_value(&self) -> bool {
        matches!(self, Self::Add { .. } | Self::Mov { .. })
    }

    pub fn get_value_id(&self) -> Option<IRValueId> {
        match self {
            IRInstruction::Mov { result, .. } 
            | IRInstruction::Add { result, .. } => Some(*result)
        }
    }

    pub fn defs(&self) -> Vec<IRValueId> {
        match self {
            IRInstruction::Mov { result, .. } => vec![*result],
            IRInstruction::Add { result, .. } => vec![*result],
        }
    }

    pub fn uses(&self) -> Vec<IRValueId> {
        match self {
            IRInstruction::Mov { src, .. } => src.as_value_id().into_iter().collect(),
            IRInstruction::Add { lhs, rhs, .. } => {
                lhs
                .as_value_id()
                .into_iter()
                .chain(rhs.as_value_id())
                .collect()
            }
        }
    }
}