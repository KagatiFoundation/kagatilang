// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::operand::LIROperand;
use crate::vreg::VReg;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LIRLabel(pub usize);

#[derive(Debug)]
pub enum LIRInstruction {
    Mov {
        dest: VReg,
        src: LIROperand
    },

    Add {
        dest: VReg,
        lhs: LIROperand,
        rhs: LIROperand
    },

    Jump {
        label: LIRLabel
    }
}