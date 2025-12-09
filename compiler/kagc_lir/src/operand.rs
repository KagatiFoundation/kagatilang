// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::vreg::VReg;

#[derive(Debug, Clone)]
pub enum LirOperand {
    VReg(VReg), // Either register or stack offset
    Constant(i64), // Literal constant type
    // Boxed(usize) // Boxed type encapsulates a Pool Index
}