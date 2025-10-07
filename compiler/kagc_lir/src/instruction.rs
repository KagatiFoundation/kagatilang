// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

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
    }
}

#[derive(Debug, Clone)]
pub struct LirAddress(pub usize);