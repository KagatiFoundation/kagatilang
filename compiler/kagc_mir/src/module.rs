// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use crate::function::*;

#[derive(Default, Debug)]
pub struct MirModule {
    pub functions: HashMap<FunctionId, IRFunction>
}

impl MirModule {
    pub fn new() -> Self {
        Self { functions: HashMap::new() }
    }

    pub fn add_function(&mut self, function: IRFunction) {
        self.functions.insert(function.id, function);
    }
}