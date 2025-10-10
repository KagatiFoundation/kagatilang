// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_const::pool::PoolIdx;
use kagc_types::builtins::obj::KObjType;

use crate::value::*;

#[derive(Debug, Clone, Copy)]
pub struct IRInstructionId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IRCondition {
    EqEq,
    NEq,
    GTEq,
    LTEq,
    GThan,
    LThan,
}

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

    Subtract {
        result: IRValueId,
        lhs: IRValue,
        rhs: IRValue
    },

    Multiply {
        result: IRValueId,
        lhs: IRValue,
        rhs: IRValue
    },

    Divide {
        result: IRValueId,
        lhs: IRValue,
        rhs: IRValue
    },
    
    Store {
        src: IRValueId,
        address: IRAddress
    },

    Load {
        src: IRAddress,
        result: IRValueId
    },

    Call {
        func:   String,
        args:   Vec<IRValueId>,
        result: Option<IRValueId>
    },

    MemAlloc {
        size:       usize,
        ob_ty:      KObjType,
        result:     IRValueId,
        pool_idx:   PoolIdx
    },

    LoadGlobal {
        pool_idx: PoolIdx,
        result: IRValueId
    },

    CondJump {
        lhs: IRValue,
        rhs: IRValue,
        cond: IRCondition,
        result: IRValueId
    }
}

impl IRInstruction {
    pub fn defines_value(&self) -> bool {
        matches!(self, Self::Add { .. } | Self::Mov { .. })
    }

    pub fn get_value_id(&self) -> Option<IRValueId> {
        match self {
            IRInstruction::Mov          { result, .. } | 
            IRInstruction::Add          { result, .. } |
            IRInstruction::Load         { result, .. } |
            IRInstruction::MemAlloc     { result, .. } |
            IRInstruction::Subtract     { result, .. } |
            IRInstruction::Divide       { result, .. } |
            IRInstruction::Multiply     { result, .. } |
            IRInstruction::LoadGlobal   { result, .. } |
            IRInstruction::CondJump     { result, .. } => Some(*result),
            IRInstruction::Call         { result, .. } => *result,
            _ => None
        }
    }

    pub fn defs(&self) -> Vec<IRValueId> {
        match self {
            IRInstruction::Mov          { result, .. } |
            IRInstruction::Load         { result, .. } |
            IRInstruction::Add          { result, .. } |
            IRInstruction::MemAlloc     { result, .. } |
            IRInstruction::Subtract     { result, .. } |
            IRInstruction::Divide       { result, .. } |
            IRInstruction::Multiply     { result, .. } |
            IRInstruction::LoadGlobal   { result, .. } |
            IRInstruction::CondJump     { result, .. } => vec![*result],
            IRInstruction::Call { result, .. } => vec![result.unwrap()],
            _ => vec![]
        }
    }

    pub fn uses(&self) -> Vec<IRValueId> {
        match self {
            IRInstruction::Mov         { src, .. } => src.as_value_id().into_iter().collect(),
            IRInstruction::Add         { lhs, rhs, .. } |
            IRInstruction::Subtract    { lhs, rhs, .. } |
            IRInstruction::Divide      { lhs, rhs, .. } |
            IRInstruction::CondJump    { lhs, rhs, .. } |
            IRInstruction::Multiply    { lhs, rhs, .. } => {
                lhs
                    .as_value_id()
                    .into_iter()
                    .chain(rhs.as_value_id())
                    .collect()
            },
            IRInstruction::Call        { args, .. } => args.clone(),
            IRInstruction::Store       { src, .. } => vec![*src],
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