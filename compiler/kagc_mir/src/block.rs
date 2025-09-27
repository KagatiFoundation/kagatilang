// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashSet;

use crate::instruction::IRInstruction;
use crate::value::IRValueId;

#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq)]
pub struct BlockId(pub usize);

#[derive(Debug, Default, Clone)]
pub struct IRBasicBlock {
    pub id: BlockId,
    pub instructions: Vec<IRInstruction>,
    pub successors: Vec<BlockId>,
    pub predecessors: Vec<BlockId>
}

impl IRBasicBlock {
    pub fn compute_use_def(&self, block: &IRBasicBlock) -> (HashSet<IRValueId>, HashSet<IRValueId>) {
        let mut use_set = HashSet::new();
        let mut def_set = HashSet::new();

        for inst in &block.instructions {
            for u in inst.uses() {
                if !def_set.contains(&u) {
                    use_set.insert(u);
                }
            }
            for d in inst.defs() {
                def_set.insert(d);
            }
        }

        (use_set, def_set)
    }
}