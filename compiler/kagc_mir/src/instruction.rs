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
    },

    Store {
        src: IRValue,
        address: IRAddress
    },

    Load {
        src: IRAddress,
        result: IRValueId
    },

    Call {
        func: String,
        args: Vec<IRValueId>,
        result: Option<IRValueId>
    }
}

impl IRInstruction {
    pub fn defines_value(&self) -> bool {
        matches!(self, Self::Add { .. } | Self::Mov { .. })
    }

    pub fn get_value_id(&self) -> Option<IRValueId> {
        match self {
            IRInstruction::Mov { result, .. } | 
            IRInstruction::Add { result, .. } => Some(*result),
            _ => None
        }
    }

    pub fn defs(&self) -> Vec<IRValueId> {
        match self {
            IRInstruction::Mov  { result, .. } |
            IRInstruction::Load { result, .. } |
            IRInstruction::Add  { result, .. } => vec![*result],
            _ => vec![]
        }
    }

    pub fn uses(&self) -> Vec<IRValueId> {
        match self {
            IRInstruction::Mov    { src, .. } |
            IRInstruction::Store  { src, .. } => src.as_value_id().into_iter().collect(),
            IRInstruction::Add    { lhs, rhs, .. } => {
                lhs
                    .as_value_id()
                    .into_iter()
                    .chain(rhs.as_value_id())
                    .collect()
            }
            _ => vec![]
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IRAddress {
    StackOffset(usize),

    BaseOffset(IRValueId, usize)
}

#[cfg(test)]
mod tests {
    use crate::{instruction::*, value::{IRValue, IRValueId}};

    #[test]
    fn test_simple_instr_construction() {
        let i1 = IRInstruction::Mov { result: IRValueId(0), src: IRValue::Constant(32) };
        assert!(i1.defines_value());
        assert!(i1.uses().is_empty());
        assert_eq!(i1.get_value_id().unwrap(), IRValueId(0));
        assert_eq!(i1.uses(), vec![IRValueId(1)]);

        let i_add = IRInstruction::Add {
            result: IRValueId(1),
            lhs: IRValue::Var(IRValueId(0)),
            rhs: IRValue::Constant(32)
        };
        assert!(i_add.defines_value());
        assert!(i_add.uses().len() == 1);
        assert_eq!(i_add.uses(), vec![IRValueId(0)]);
        assert!(i_add.defs().len() == 1);
        assert_eq!(i_add.defs(), vec![IRValueId(1)]);
    }
}