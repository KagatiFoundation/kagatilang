// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod aarch64;
pub mod x86;

pub mod register;
mod allocation;
pub use allocation::*;

pub mod linear_alloca;
pub use linear_alloca::LinearScanAllocator;