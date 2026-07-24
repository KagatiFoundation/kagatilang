// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub const REG_SIZE_8: usize = 8;
pub const REG_SIZE_4: usize = 4;

pub mod builtin;
pub mod variable;
pub mod value;
pub mod instruction;
pub mod block;
pub mod function;
pub mod mir_builder;
pub mod types;
pub mod module;
pub mod loop_ctx;
pub mod liveness;
pub mod mir_verifier;