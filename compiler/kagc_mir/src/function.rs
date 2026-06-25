// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use indexmap::IndexMap;
use kagc_symbol::StorageClass;

use crate::block::IRBasicBlock;
use crate::block::BlockId;
use crate::block::UseDefSet;
use crate::block::BlockLiveness;

use crate::instruction::StackSlotId;
use crate::types::IRType;
use crate::value::IRValueId;

use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Default, Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct FunctionId(pub usize);

#[derive(Default, Debug, Clone)]
pub struct FunctionParam {
    pub id: IRValueId,
    pub ty: IRType,
    pub stack_slot: StackSlotId
}

#[derive(Default, Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<FunctionParam>,
    pub return_type: IRType,
    pub class: StorageClass
}

#[derive(Default, Debug)]
pub struct IRFunction {
    pub id: FunctionId,
    pub name: String,
    pub signature: FunctionSignature,
    pub blocks: IndexMap<BlockId, IRBasicBlock>,
    pub entry_block: BlockId,
    pub exit_block: BlockId,
    pub is_leaf: bool
}

#[derive(Debug)]
pub struct FunctionAnchor {
    pub id: FunctionId,
    pub entry_block: BlockId,
    pub exit_block: BlockId
}

impl FunctionAnchor {
    pub fn new(id: FunctionId, entry: BlockId, exit: BlockId) -> Self {
        Self {
            id,
            entry_block: entry,
            exit_block: exit
        }
    }
}

pub struct LivenessAnalyzer<'a> {
    func: &'a IRFunction,
    local_sets: HashMap<BlockId, UseDefSet>,
}

impl<'a> LivenessAnalyzer<'a> {
    pub fn new(func: &'a IRFunction) -> Self {
        let mut local_sets = HashMap::new();
        for (id, block) in &func.blocks {
            local_sets.insert(*id, block.compute_use_def());
        }
        Self { func, local_sets }
    }

    pub fn compute_global_liveness(&self) -> HashMap<BlockId, BlockLiveness> {
        let mut liveness_map: HashMap<BlockId, BlockLiveness> = self.func.blocks
            .keys()
            .map(|id| (*id, BlockLiveness::default()))
            .collect();

        let mut changed = true;
        
        while changed {
            changed = false;

            for (block_id, block) in &self.func.blocks {
                let local = &self.local_sets[block_id];
                
                let mut new_live_out = HashSet::new();
                for succ_id in &block.successors {
                    if let Some(succ_liveness) = liveness_map.get(succ_id) {
                        new_live_out.extend(succ_liveness.live_in.iter().cloned());
                    }
                }

                let mut new_live_in = local.uses.clone();
                let remaining_live: HashSet<IRValueId> = new_live_out
                    .difference(&local.defs)
                    .cloned()
                    .collect();
                new_live_in.extend(remaining_live);

                let current_liveness = liveness_map.get_mut(block_id).unwrap();
                if current_liveness.live_in != new_live_in || current_liveness.live_out != new_live_out {
                    current_liveness.live_in = new_live_in;
                    current_liveness.live_out = new_live_out;
                    changed = true;
                }
            }
        }

        liveness_map
    }
}