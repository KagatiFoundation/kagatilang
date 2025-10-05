// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::vreg::VReg;

#[derive(Debug)]
pub enum LIROperand {
    VReg(VReg),
    Constant(i64)
}