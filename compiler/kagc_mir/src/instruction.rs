// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::ops::{Add, Sub};

use kagc_const::pool::PoolIdx;

use crate::{builtin::BuiltinFn, value::*};

#[derive(Debug, Clone, Copy)]
pub struct IrInstructionId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrCondition {
    EqEq,
    NEq,
    GTEq,
    LTEq,
    GThan,
    LThan,
}

#[derive(Debug, Clone)]
pub enum IrInstruction {
    Mov {
        result: IrValueId,
        src: IrValue
    },

    Add {
        result: IrValueId,
        lhs: IrValue,
        rhs: IrValue
    },

    Subtract {
        result: IrValueId,
        lhs: IrValue,
        rhs: IrValue
    },

    Multiply {
        result: IrValueId,
        lhs: IrValue,
        rhs: IrValue
    },

    Divide {
        result: IrValueId,
        lhs: IrValue,
        rhs: IrValue
    },
    
    Store {
        src: IrValueId,
        address: IrAddress
    },

    Load {
        src: IrAddress,
        result: IrValueId
    },

    Call {
        func:   String,
        args:   Vec<IrValueId>,
        result: Option<IrValueId>
    },

    CallBuiltin {
        builtin: BuiltinFn,
        args:   Vec<IrValueId>,
        result: Option<IrValueId>
    },

    #[deprecated]
    MemAlloc {
        size:       IrValue,
        ob_ty:      IrValue,
        result:     IrValueId,
        pool_idx:   PoolIdx,
        base_ptr_slot: StackSlotId,
    },

    LoadConst {
        label_id: usize,
        result: IrValueId
    },

    LoadGlobal {
        pool_idx: PoolIdx,
        result: IrValueId
    },

    CondJump {
        lhs: IrValue,
        rhs: IrValue,
        cond: IrCondition,
        result: IrValueId
    }
}

impl IrInstruction {
    pub fn defines_value(&self) -> bool {
        matches!(self, Self::Add { .. } | Self::Mov { .. })
    }

    pub fn get_value_id(&self) -> Option<IrValueId> {
        match self {
            IrInstruction::Mov          { result, .. } | 
            IrInstruction::Add          { result, .. } |
            IrInstruction::Load         { result, .. } |
            IrInstruction::Subtract     { result, .. } |
            IrInstruction::Divide       { result, .. } |
            IrInstruction::Multiply     { result, .. } |
            IrInstruction::LoadGlobal   { result, .. } |
            IrInstruction::CondJump     { result, .. } => Some(*result),
            IrInstruction::Call         { result, .. } |
            IrInstruction::CallBuiltin  { result, .. } => *result,
            IrInstruction::LoadConst    { result, .. } => Some(*result),
            _ => None
        }
    }

    pub fn defs(&self) -> Vec<IrValueId> {
        match self {
            IrInstruction::Mov          { result, .. } |
            IrInstruction::Load         { result, .. } |
            IrInstruction::Add          { result, .. } |
            IrInstruction::Subtract     { result, .. } |
            IrInstruction::Divide       { result, .. } |
            IrInstruction::Multiply     { result, .. } |
            IrInstruction::LoadGlobal   { result, .. } |
            IrInstruction::CondJump     { result, .. } => vec![*result],
            IrInstruction::Call         { result, .. } |
            IrInstruction::CallBuiltin  { result, .. } => vec![result.unwrap()],
            _ => vec![]
        }
    }

    pub fn uses(&self) -> Vec<IrValueId> {
        match self {
            IrInstruction::Mov         { src, .. } => src.as_value_id().into_iter().collect(),
            IrInstruction::Add         { lhs, rhs, .. } |
            IrInstruction::Subtract    { lhs, rhs, .. } |
            IrInstruction::Divide      { lhs, rhs, .. } |
            IrInstruction::CondJump    { lhs, rhs, .. } |
            IrInstruction::Multiply    { lhs, rhs, .. } => {
                lhs
                    .as_value_id()
                    .into_iter()
                    .chain(rhs.as_value_id())
                    .collect()
            },
            IrInstruction::Call        { args, .. } |
            IrInstruction::CallBuiltin { args, .. } => args.clone(),
            IrInstruction::Store       { src, .. } => vec![*src],
            _ => vec![]
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{instruction::*, value::{IrValue, IrValueId}};

    #[test]
    fn test_simple_instr_construction() {
        let i1 = IrInstruction::Mov { result: IrValueId(0), src: IrValue::Constant(32) };
        assert!(i1.defines_value());
        assert!(i1.uses().is_empty());
        assert_eq!(i1.get_value_id().unwrap(), IrValueId(0));
        assert_eq!(i1.uses(), vec![IrValueId(1)]);

        let i_add = IrInstruction::Add {
            result: IrValueId(1),
            lhs: IrValue::Register(IrValueId(0)),
            rhs: IrValue::Constant(32)
        };
        assert!(i_add.defines_value());
        assert!(i_add.uses().len() == 1);
        assert_eq!(i_add.uses(), vec![IrValueId(0)]);
        assert!(i_add.defs().len() == 1);
        assert_eq!(i_add.defs(), vec![IrValueId(1)]);
    }
}