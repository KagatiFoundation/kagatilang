// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::operand::IROperand;

#[derive(Debug)]
pub enum IRInstruction {
    Assign {
        src: IROperand
    },

    Add {
        lhs: IROperand,
        rhs: IROperand
    }
}