// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use crate::function::*;

#[derive(Default, Debug)]
pub struct MirModule<'tcx> {
    pub functions: HashMap<IrFunctionId, IrFunction<'tcx>>
}

impl<'tcx> MirModule<'tcx> {
    pub fn new() -> Self {
        Self { functions: HashMap::new() }
    }

    pub fn add_function(&mut self, function: IrFunction<'tcx>) {
        self.functions.insert(function.id, function);
    }
}