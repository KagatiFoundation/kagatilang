// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashSet;
use std::collections::HashMap;

use crate::block::BlockId;
use crate::function::IRFunction;
use crate::value::IRValueId;

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
                let old_in = block_liveness[bid].in_set.clone();
                let old_out = block_liveness[bid].out_set.clone();

                let mut out_set = HashSet::<IRValueId>::new();
                for succ in &block.successors {
                    out_set.extend(&block_liveness[succ].in_set);
                }
                
                let mut in_set = use_map[bid].clone();
                let out_minus_def = out_set.difference(&def_map[bid])
                    .cloned().collect::<HashSet<_>>();
                in_set.extend(out_minus_def);

                if in_set != old_in || out_set != old_out {
                    block_liveness.insert(*bid, BlockLiveness { in_set, out_set });
                    changed = true;
                }
            }
        }

        Self {
            block_liveness
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::LivenessAnalyzer;
    use crate::block::Terminator;
    use crate::builder::IRBuilder;
    use crate::function::FunctionParam;
    use crate::instruction::IRInstruction;
    use crate::types::IRType;
    use crate::value::*;

    #[test]
    fn test_simple_liveness_analysis() {
        let mut builder = IRBuilder::default();
        let (_, bid) = builder.create_function(
            vec![
                FunctionParam {
                    id: IRValueId(0),
                    ty: IRType::I64
                },
                FunctionParam {
                    id: IRValueId(1),
                    ty: IRType::I64
                }
            ], 
            IRType::I64
        );
        builder.inst(
            IRInstruction::Add { 
                result: IRValueId(2), 
                lhs: IRValue::Var(IRValueId(1)), 
                rhs: IRValue::Var(IRValueId(0)) 
            }
        );

        builder.inst(
            IRInstruction::Add { 
                result: IRValueId(3),
                lhs: IRValue::Var(IRValueId(1)),
                rhs: IRValue::Constant(32)
            }
        );

        builder.set_terminator(bid, Terminator::Return(None));

        let module = builder.build();
        for func in module.functions.values() {
            let la_data = LivenessAnalyzer::analyze_function(func);
            dbg!(la_data);
        }
    }
}