// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashSet;
use std::collections::HashMap;

use crate::block::BlockId;
use crate::function::IRFunction;
use crate::instruction::IrInstruction;
use crate::value::IrValue;
use crate::value::IrValueId;

#[derive(Debug, Clone, Copy)]
pub struct LiveRange {
    pub value: IrValueId,
    pub start: BlockId,
    pub end: BlockId
}

#[derive(Debug)]
pub struct BlockLiveness {
    pub in_set: HashSet<IrValueId>,
    pub out_set: HashSet<IrValueId>,
}

pub struct LivenessAnalyzer;

impl LivenessAnalyzer {
    pub fn compute_function_live_ranges(&self, function: &IRFunction) -> Vec<LiveRange> {
        let mut block_ids: Vec<_> = function.blocks.keys().cloned().collect();
        block_ids.sort_by_key(|b| b.0);

        let mut instr_global_id = 0;
        let mut instr_ranges_per_block = HashMap::new(); 

        for bid in &block_ids {
            let block = &function.blocks[bid];
            let start_ip = instr_global_id;
            instr_global_id += block.instructions.len();

            let end_ip = instr_global_id;
            instr_ranges_per_block.insert(*bid, (start_ip, end_ip));
        }

        let mut local_use: HashMap<BlockId, HashSet<IrValueId>> = HashMap::new();
        let mut local_def: HashMap<BlockId, HashSet<IrValueId>> = HashMap::new();

        for bid in &block_ids {
            let block = &function.blocks[bid];
            let mut uses = HashSet::new();
            let mut defs = HashSet::new();

            for instr in &block.instructions {
                let (inst_uses, inst_defs) = self.get_ranges_for_instruction(instr);
                
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
                for vreg in &new_live_out {
                    if !local_def_set.contains(vreg) {
                        new_live_in.insert(*vreg);
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

        let mut final_ranges: HashMap<IrValueId, (usize, usize)> = HashMap::new();

        for bid in &block_ids {
            let (block_start_ip, block_end_ip) = instr_ranges_per_block[bid];
            let out_set = &live_out[bid];

            for value_id in out_set {
                let range = final_ranges.entry(*value_id).or_insert((block_start_ip, block_end_ip));
                range.1 = std::cmp::max(range.1, block_end_ip);
            }

            let mut current_live = out_set.clone();
            let block = &function.blocks[bid];
            
            for (offset, instr) in block.instructions.iter().enumerate().rev() {
                let global_ip = block_start_ip + offset;
                let (inst_uses, inst_defs) = self.get_ranges_for_instruction(instr);

                for d in inst_defs {
                    current_live.remove(&d);
                    let range = final_ranges.entry(d).or_insert((global_ip, global_ip));
                    range.0 = std::cmp::min(range.0, global_ip);
                    range.1 = std::cmp::max(range.1, global_ip);
                }

                for u in inst_uses {
                    current_live.insert(u);
                    let range = final_ranges.entry(u).or_insert((global_ip, global_ip));
                    range.0 = std::cmp::min(range.0, global_ip);
                }
            }

            for vreg in &live_in[bid] {
                let range = final_ranges.entry(*vreg).or_insert((block_start_ip, block_end_ip));
                range.0 = std::cmp::min(range.0, block_start_ip);
            }
        }

        final_ranges.into_iter()
            .map(|(value, (start, end))| LiveRange { value, start: BlockId(start), end: BlockId(end) })
            .collect()
    }

    fn get_ranges_for_instruction(&self, instr: &IrInstruction) -> (Vec<IrValueId>, Vec<IrValueId>) {
        let mut uses = vec![];
        let mut defs = vec![];

        match instr {
            IrInstruction::Mov { src, result } => {
                defs.push(*result);
                if let IrValue::Register(v) = src { uses.push(*v); }
            }
            IrInstruction::Add { lhs, rhs, result } => {
                defs.push(*result);
                if let IrValue::Register(v) = lhs { uses.push(*v); }
                if let IrValue::Register(v) = rhs { uses.push(*v); }
            }
            IrInstruction::Load { result, .. } => {
                defs.push(*result);
            }
            IrInstruction::Store { src, .. } => {
                uses.push(*src);
            }
            IrInstruction::CondJump { result, lhs, rhs, .. } => {
                defs.push(*result);
                if let IrValue::Register(r) = lhs { uses.push(*r); }
                if let IrValue::Register(r) = rhs { uses.push(*r); }
            }
            _ => todo!()
        }
        (uses, defs)
    }
}