// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::instruction::IRInstruction;

#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq)]
pub struct BlockId(pub usize);

#[derive(Debug, Default, Clone)]
pub struct IRBasicBlock {
    pub id: BlockId,
    pub instructions: Vec<IRInstruction>,
    pub successors: Vec<BlockId>,
    pub predecessors: Vec<BlockId>
}