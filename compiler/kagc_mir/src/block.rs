// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::value::IRValueId;

#[derive(Debug, Default, Copy, Clone)]
pub struct IRBlockId(pub usize);

#[derive(Debug, Default)]
pub struct IRBasicBlock {
    pub id: IRBlockId,
    pub instructions: Vec<IRValueId>
}

impl IRBasicBlock {
    pub fn add_value(&mut self, id: IRValueId) {
        self.instructions.push(id);
    }
}