// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::function::IRFunction;
use crate::value::*;
use crate::instruction::*;

pub fn print_liveness(func: &IRFunction) {
    for block in &func.blocks {
        for v_id in &block.instructions {
            let value = &func.values[v_id.0];
            if let IRValueKind::Instruction(instr) = &value.kind {
                let used_ids = match instr {
                    IRInstruction::Mov { src } => vec![src.0],
                    IRInstruction::Add { lhs, rhs } => vec![lhs.0, rhs.0],
                };
                println!("{used_ids:#?}");
            }
        }
    }
}

use std::collections::HashMap;

type LiveSet = Vec<IRValueId>;

pub struct LivenessAnalyzer;

impl LivenessAnalyzer {
    pub fn compute_live_in(func: &IRFunction) -> HashMap<usize, LiveSet> {
        let mut live_in: HashMap<usize, LiveSet> = HashMap::new();
        let mut live_out: HashMap<usize, LiveSet> = HashMap::new();

        // initialize
        for block in &func.blocks {
            live_in.insert(block.id.0, vec![]);
            live_out.insert(block.id.0, vec![]);
        }

        let mut changed = true;
        while changed {
            changed = false;

            for block in func.blocks.iter().rev() {  // reverse post-order
                // live_out = union of live_in of successors
                let mut out_set = vec![];
                for &succ_id in &block.successors {
                    let succ_in = live_in.get(&succ_id.0).unwrap();
                    for v in succ_in {
                        let vv = v.clone();
                        if !out_set.contains(v) {
                            out_set.push(vv);
                        }
                    }
                }
                live_out.insert(block.id.0, out_set.clone());

                // live_in = use U (live_out - def)
                let mut in_set = out_set.clone();

                // remove defs in this block
                for val_id in &block.instructions {
                    in_set.retain(|v| *v != *val_id);
                }

                // add uses in this block
                for val_id in &block.instructions {
                    let value = &func.values[val_id.0];
                    if let IRValueKind::Instruction(instr) = &value.kind {
                        let used = match instr {
                            IRInstruction::Add { lhs, rhs } => vec![lhs.clone(), rhs.clone()],
                            IRInstruction::Mov { src } => vec![src.clone()],
                        };
                        for u in &used {
                            if !in_set.contains(u) {
                                in_set.push(u.clone());
                            }
                        }
                    }
                }

                if live_in[&block.id.0] != in_set {
                    live_in.insert(block.id.0, in_set);
                    changed = true;
                }
            }
        }

        live_in
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::LivenessAnalyzer;
    use crate::builder::IRBuilder;
    use crate::value::*;

    #[test]
    fn test_simple_liveness_analysis() {
        let mut builder = IRBuilder::default();

        let const1 = builder.create_constant(32, IRType::I64);

        let mut func = builder.create_function(
            "add".to_string(),
            vec![IRType::I64, IRType::I64], 
            IRType::I64
        );

        let add_instr = builder.create_add(const1, func.get_arg(0).unwrap());

        func.values = builder.values.clone();
        func.blocks =  builder.blocks.clone();

        let la = LivenessAnalyzer::compute_live_in(&func);
        dbg!(la);
    }
}