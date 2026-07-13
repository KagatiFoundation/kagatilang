// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_mir::function::IrFunction;

pub trait FunctionPass {
    fn name(&self) -> &'static str;
    
    fn run_on_function(&mut self, func: &mut IrFunction) -> bool;
}