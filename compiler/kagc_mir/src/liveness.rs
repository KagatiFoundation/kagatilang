// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashSet;
use std::collections::HashMap;

use crate::block::BlockId;
use crate::function::IrFunction;
use crate::value::IrValueId;
use crate::block::Terminator;

#[derive(Debug, Clone, Copy)]
pub struct LiveRange {
    pub value: IrValueId, 
    pub start: usize, 
    pub end: usize,   
}

#[derive(Debug)]
pub struct BlockLiveness {
    pub in_set: HashSet<IrValueId>,
    pub out_set: HashSet<IrValueId>,
}

pub fn compute_function_live_ranges(function: &IrFunction) -> HashMap<BlockId, BlockLiveness> {
    let mut block_ids: Vec<_> = function.blocks.keys().cloned().collect();
    block_ids.sort_by_key(|b| b.0);

    let mut local_use: HashMap<BlockId, HashSet<IrValueId>> = HashMap::new();
    let mut local_def: HashMap<BlockId, HashSet<IrValueId>> = HashMap::new();

    for bid in &block_ids {
        let block = &function.blocks[bid];
        let mut uses = HashSet::new();
        let mut defs = HashSet::new();

		uses.extend(block.terminator.uses());

        for instr in &block.instructions {
            let (inst_uses, inst_defs) = instr.uses_and_defs();
                
            for u in inst_uses {
                if !defs.contains(&u) {
                    uses.insert(u);
                }
            }
            for d in inst_defs {
                defs.insert(d);
            }
        }
        local_use.insert(*bid, uses);
        local_def.insert(*bid, defs);
    }

    let mut live_in: HashMap<BlockId, HashSet<IrValueId>> = block_ids.iter().map(|&b| (b, HashSet::new())).collect();
    let mut live_out: HashMap<BlockId, HashSet<IrValueId>> = block_ids.iter().map(|&b| (b, HashSet::new())).collect();

    let mut changed = true;
    while changed {
        changed = false;

        for bid in block_ids.iter().rev() {
            let block = &function.blocks[bid];
                
            let mut new_live_out = HashSet::new();
            for succ_id in &block.successors {
                if let Some(succ_in) = live_in.get(succ_id) {
                    new_live_out.extend(succ_in);
                }
            }

            let mut new_live_in = local_use[bid].clone();
            let local_def_set = &local_def[bid];
            for value_id in &new_live_out {
                if !local_def_set.contains(value_id) {
                    new_live_in.insert(*value_id);
                }
            }

            if new_live_in != live_in[bid] {
                live_in.insert(*bid, new_live_in);
                changed = true;
            }
            if new_live_out != live_out[bid] {
                live_out.insert(*bid, new_live_out);
                changed = true;
            }
        }
    }

	block_ids.into_iter().map(|bid| {
		let liveness = BlockLiveness {
			in_set: live_in.remove(&bid).unwrap(),
			out_set: live_out.remove(&bid).unwrap()
		};
		(bid, liveness)
	}).collect()
}