// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashSet;

use kagc_mir::block::BlockId;
use kagc_mir::instruction::IRCondition;

use crate::instruction::LirInstruction;
use crate::vreg::VReg;

#[derive(Debug, Clone)]
pub struct LirBasicBlock {
    pub id: BlockId,
    pub instructions: Vec<LirInstruction>,
    pub successors: HashSet<BlockId>,
    pub predecessors: HashSet<BlockId>,
    pub terminator: LirTerminator,
    pub name: String
}

#[derive(Debug, Clone)]
pub enum LirTerminator {
    Jump(BlockId),

    Return {
        target: BlockId,
        value: Option<VReg>
    },

    CJump {
        cond: IRCondition,
        then_block: BlockId,
        else_block: BlockId
    }
}