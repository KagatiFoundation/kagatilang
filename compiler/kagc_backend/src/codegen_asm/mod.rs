// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_lir::function::LirFunction;

pub mod aarch64;

pub trait CodegGenerator {
    fn gen_function(&mut self, lir_func: &LirFunction);
}