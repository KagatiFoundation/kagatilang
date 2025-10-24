// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_mir::instruction::IRCondition;

use crate::operand::LirOperand;
use crate::vreg::VReg;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LirLabel(pub usize);

#[derive(Debug, Clone)]
pub enum LirInstruction {
    Mov {
        dest: VReg,
        src: LirOperand
    },

    Add {
        dest: VReg,
        lhs: LirOperand,
        rhs: LirOperand
    },

    Store {
        src: VReg,
        dest: LirAddress
    },

    Load {
        src: LirAddress,
        dest: VReg
    },

    Jump {
        label: LirLabel
    },

    CJump {
        dest: VReg,
        lhs: LirOperand,
        rhs: LirOperand,
        op: IRCondition
    },

    Call {
        func: String,
        args: Vec<VReg>,
        result: Option<VReg>
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LirAddress {
    Offset(usize),
    BaseOffset(VReg, usize)
}