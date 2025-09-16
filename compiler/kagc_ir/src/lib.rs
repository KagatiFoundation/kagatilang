// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod ir_types;
pub mod ir_instr;
pub mod ir_liveness;
pub mod gc;

/// Label ID
pub type LabelId = usize;