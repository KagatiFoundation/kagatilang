// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use crate::function::*;

#[derive(Default, Debug)]
pub struct Module {
    functions: HashMap<FunctionId, IRFunction>
}

impl Module {
    pub fn new() -> Self {
        Self { functions: HashMap::new() }
    }

    pub fn add_function(&mut self, function: IRFunction) {
        self.functions.insert(function.id, function);
    }
}