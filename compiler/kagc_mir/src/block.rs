// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashSet;

use crate::instruction::*;

use crate::value::IrValueId;

#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct BlockId(pub usize);

/// Sentinel `BlockId` used to indicate that a function has no
/// associated entry or exit blocks, typically for `extern`
/// (foreign) functions whose body is not defined in this module.
/// 
/// Codegen and IR passes can check against this value to skip
/// block-level processing.
pub const INVALID_BLOCK_ID: BlockId = BlockId(usize::MAX);

#[derive(Debug, Clone, Default)]
pub struct IrBasicBlock {
    pub id: BlockId,
    pub instructions: Vec<IrInstruction>,
    pub successors: HashSet<BlockId>,
    pub predecessors: HashSet<BlockId>,
    pub terminator: Terminator,
    pub name: String
}

impl IrBasicBlock {
    pub fn add_successor(&mut self, block_id: BlockId) {
        self.successors.insert(block_id);
    }

    pub fn add_predecessor(&mut self, block_id: BlockId) {
        self.predecessors.insert(block_id);
    }
}

#[derive(Debug, Default)]
pub struct UseDefSet {
    pub uses: HashSet<IrValueId>,
    pub defs: HashSet<IrValueId>
}

impl UseDefSet {
    pub fn uses_as_vec(&self) -> Vec<&IrValueId> {
        self.uses.iter().collect::<Vec<&IrValueId>>()
    }

    pub fn defs_as_vec(&self) -> Vec<&IrValueId> {
        self.defs.iter().collect::<Vec<&IrValueId>>()
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    /// Unconditional jump which generates 'bl' instruction.
    Jump(BlockId),

	/// Does not generate 'bl' instruction.
	Fallthrough(BlockId),

    CondJump {
        jump_value_id: IrValueId,
		cond: IrCondition,
        then_block: BlockId,
        else_block: BlockId,
    },

    Return {
        value: Option<IrValueId>,
        target: BlockId
    }
}

impl Default for Terminator {
    fn default() -> Self {
        Terminator::Jump(BlockId(0))
    }
}

impl Terminator {
    pub fn uses(&self) -> Vec<IrValueId> {
        match self {
            Terminator::CondJump { jump_value_id, .. } => vec![*jump_value_id],
            Terminator::Return { value: Some(val), .. } => vec![*val],
            _ => vec![]
        }
    }
}