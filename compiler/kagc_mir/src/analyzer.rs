// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashSet;
use std::collections::HashMap;

use crate::block::BlockId;
use crate::function::IRFunction;
use crate::value::IRValueId;

#[derive(Debug)]
pub struct LiveRange {
    pub value: IRValueId,
    pub start: BlockId,
    pub end: BlockId
}

#[derive(Debug)]
pub struct BlockLiveness {
    pub in_set: HashSet<IRValueId>,
    pub out_set: HashSet<IRValueId>,
}

#[derive(Debug)]
pub struct LivenessAnalyzer {
    pub block_liveness: HashMap<BlockId, BlockLiveness>
}

impl LivenessAnalyzer {
    pub fn analyze_function(ir_func: &IRFunction) -> Self {
        let mut block_liveness = HashMap::new();
        
        // compute use[B], def[B]
        let mut use_map = HashMap::new();
        let mut def_map = HashMap::new();

        // calculate use and definition maps
        for (bid, block) in &ir_func.blocks {
            let use_defs = block.compute_use_def();
            use_map.insert(*bid, use_defs.uses.clone());
            def_map.insert(*bid, use_defs.defs.clone());
            block_liveness.insert(*bid, BlockLiveness {
                in_set: HashSet::new(),
                out_set: HashSet::new(),
            });
        }

        let mut changed = true;
        while changed {
            changed = false;

            for (bid, block) in &ir_func.blocks {
                let old_in_set = block_liveness[bid].in_set.clone();
                let old_out_set = block_liveness[bid].out_set.clone();

                let mut new_out_set = HashSet::<IRValueId>::new();
                for succ in &block.successors {
                    new_out_set.extend(&block_liveness[succ].in_set);
                }
                
                let mut new_in_set = use_map[bid].clone();
                let out_minus_def = new_out_set.difference(&def_map[bid])
                    .cloned()
                    .collect::<HashSet<_>>();
                new_in_set.extend(out_minus_def);

                if new_in_set != old_in_set || new_out_set != old_out_set {
                    block_liveness.insert(
                        *bid, 
                        BlockLiveness { 
                            in_set: new_in_set, 
                            out_set: new_out_set 
                        }
                    );
                    changed = true;
                }
            }
        }

        Self {
            block_liveness
        }
    }

    pub fn compute_live_ranges(ir_func: &IRFunction, analyzer: &Self) -> Vec<LiveRange> {
        let mut blocks: Vec<_> = ir_func.blocks.keys().cloned().collect();
        blocks.sort_by_key(|b| b.0); // sort by BlockId numeric order

        let mut ranges: HashMap<IRValueId, (Option<BlockId>, Option<BlockId>)> = HashMap::new();
        for bid in &blocks {
            let block = &ir_func.blocks[bid];
            // union of in[B] and defs inside block
            let block_values = analyzer.block_liveness[bid].in_set
                .union(&block.compute_use_def().defs)
                .cloned()
                .collect::<HashSet<IRValueId>>();

            for val in block_values {
                let entry = ranges.entry(val).or_insert((None, None));

                if entry.0.is_none() {
                    entry.0 = Some(*bid);
                }

                entry.1 = Some(*bid);
            }
        }
        ranges.into_iter().map(|(value, (start, end))| LiveRange {
            value,
            start: start.unwrap(),
            end: end.unwrap(),
        }).collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::LivenessAnalyzer;
    use crate::block::{BlockId, Terminator};
    use crate::builder::IRBuilder;
    use crate::function::FunctionId;
    use crate::instruction::IRCondition;
    use crate::types::IRType;
    use crate::value::*;

    #[test]
    fn test_complex_function_live_ranges_construction() {
        let mut builder = IRBuilder::default();
        let (_, func_entry) = builder.create_function(vec![], IRType::I64); // block id 0
        let loop_entry = builder.create_block("loop-entry"); // block id 1
        builder.link_blocks(func_entry, loop_entry);

        let add_value = builder.create_add(IRValue::Constant(32), IRValue::Constant(32)); // value id 0 created in block id 1
        let if_else_block = builder.create_block("if-else-block"); // block id 2
        builder.link_blocks(loop_entry, if_else_block);

        let cond_jump = builder.create_conditional_jump(IRCondition::EqEq, IRValue::Var(add_value), IRValue::Constant(64)); // value id 1 created in block id 2
        let if_block = builder.create_block("if-block"); // block id 3
        let else_block = builder.create_block("else-block"); // block id 4
        builder.link_blocks_multiple(if_else_block, vec![if_block, else_block]);

        let merge_block = builder.create_block("merge"); // block id 5
        builder.link_blocks(if_block, merge_block);
        builder.link_blocks(else_block, merge_block);

        builder.set_terminator(
            if_else_block,
            Terminator::CondJump { 
                cond: cond_jump, 
                then_block: if_block, 
                else_block 
            }
        );

        builder.set_terminator(if_block, Terminator::Jump(merge_block));
        builder.set_terminator(else_block, Terminator::Jump(merge_block));
        
        let module = builder.build();
        assert!(module.functions.contains_key(&FunctionId(0)));

        let func1 = module.functions.get(&FunctionId(0)).unwrap(); // only one function
        let la_data = LivenessAnalyzer::analyze_function(func1);
        let ranges = LivenessAnalyzer::compute_live_ranges(func1, &la_data);
        
        assert!(ranges.iter().any(|range| range.value == IRValueId(0)));
        assert!(ranges.iter().any(|range| range.value == IRValueId(1)));

        let value1 = ranges.iter().find(|value| value.value == IRValueId(0)).unwrap();
        assert_eq!(value1.start, BlockId(1));
        assert_eq!(value1.end, BlockId(2));

        let value2 = ranges.iter().find(|value| value.value == IRValueId(1)).unwrap();
        assert_eq!(value2.start, BlockId(2));
        assert_eq!(value2.end, BlockId(2));
    }
}