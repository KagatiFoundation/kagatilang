// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::{cell::RefCell, rc::Rc};

use crate::value::IRValue;

#[derive(Debug, Default)]
pub struct IRBasicBlock {
    pub instructions: Vec<Rc<RefCell<IRValue>>>
}

impl IRBasicBlock {
    pub fn add_value(&mut self, value: Rc<RefCell<IRValue>>) {
        self.instructions.push(value);
    }
}