// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::vreg::VReg;

#[derive(Debug, Clone)]
pub enum LirOperand {
    VReg(VReg),
    Constant(i64)
}