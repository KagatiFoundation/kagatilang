// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_const::pool::PoolIdx;

use crate::block::BlockId;
use crate::builtin::BuiltinFn;
use crate::variable::IrVariableId;
use crate::value::*;

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

#[derive(Debug, Clone, Copy)]
pub enum IrLocation {
	Variable(IrVariableId)
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

    Cmp {
        result: IrValueId,
        lhs: IrValue,
        rhs: IrValue,
		condition: IrCondition
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
        location: IrLocation
    },

    Load {
        location: IrLocation,
        result: IrValueId
    },

	Param {
		index: usize,
		var_id: IrVariableId
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
		then_block: BlockId,
		else_block: BlockId
    },

	Jump {
		block: BlockId
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
            IrInstruction::Cmp  		{ result, .. } |
            IrInstruction::LoadGlobal   { result, .. } => Some(*result),
            IrInstruction::Call         { result, .. } |
            IrInstruction::CallBuiltin  { result, .. } => *result,
            IrInstruction::LoadConst    { result, .. } => Some(*result),
            _ => None
        }
    }

	pub fn uses_and_defs(&self) -> (Vec<IrValueId>, Vec<IrValueId>) {
		let mut uses = vec![];
        let mut defs = vec![];

        match self {
            IrInstruction::Mov { src, result } => {
                defs.push(*result);
                if let IrValue::Register(v) = src { uses.push(*v); }
            }
            IrInstruction::Add { lhs, rhs, result }
            | IrInstruction::Subtract { lhs, rhs, result }
            | IrInstruction::Multiply { lhs, rhs, result }
			| IrInstruction::Divide { lhs, rhs, result} => {
                defs.push(*result);
                if let IrValue::Register(v) = lhs { uses.push(*v); }
                if let IrValue::Register(v) = rhs { uses.push(*v); }
            }
            IrInstruction::Load { result, .. } => {
                defs.push(*result);
            }
            IrInstruction::Store { src, .. } => {
                uses.push(*src);
            }
			IrInstruction::Cmp { result, lhs, rhs, .. } => {
				defs.push(*result);
                if let IrValue::Register(v) = lhs { uses.push(*v); }
                if let IrValue::Register(v) = rhs { uses.push(*v); }
			}
			IrInstruction::Call { args, result, .. } => {
				if let Some(result) = result {
					defs.push(*result);
				}

				uses.extend(args);
			}
            IrInstruction::CondJump { lhs, rhs, .. } => {
                if let IrValue::Register(r) = lhs { uses.push(*r); }
                if let IrValue::Register(r) = rhs { uses.push(*r); }
            }
            _ => {}
        }
        (uses, defs)
	}
}