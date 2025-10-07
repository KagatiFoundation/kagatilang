// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_mir::block::BlockId;
use kagc_mir::function::FunctionId;
use kagc_mir::types::IRType;

use crate::block::LirBasicBlock;
use crate::instruction::LirInstruction;
use crate::operand::LirOperand;
use crate::vreg::VRegLiveRange;
use crate::vreg::VReg;

#[derive(Debug, Clone)]
pub struct LirFunctionParam {
    pub reg: VReg,
    pub ty: IRType
}

// LIR function signature
#[derive(Debug, Clone)]
pub struct LirFunctionSignature {
    pub params: Vec<LirFunctionParam>,
    pub return_type: IRType,
}

/// Stack information for a LirFunction.
#[derive(Debug)]
pub struct LirFunctionStack {
    pub size: usize
}

#[derive(Debug)]
pub struct LirFunction {
    pub id: FunctionId,
    pub signature: LirFunctionSignature,
    pub stack: LirFunctionStack,
    pub blocks: HashMap<BlockId, LirBasicBlock>,
    pub entry_block: BlockId
}

impl LirFunction {
    pub fn compute_vreg_live_ranges(&self) -> Vec<VRegLiveRange> {
        let mut ranges: HashMap<VReg, (Option<usize>, Option<usize>)> = HashMap::new();
        let mut func_ip: usize = 0; // function's instruction pointer

        let mut block_ids: Vec<_> = self.blocks.keys().cloned().collect();
        block_ids.sort_by_key(|b| b.0);

        for bid in block_ids {
            let block = &self.blocks[&bid];

            for instr in &block.instructions {
                let mut vregs_used = vec![];
                let mut vregs_defined = vec![];

                match instr {
                    LirInstruction::Mov { dest, src } => {
                        vregs_defined.push(*dest);
                        if let LirOperand::VReg(v) = src {
                            vregs_used.push(*v);
                        }
                    }
                    LirInstruction::Add { dest, lhs, rhs } => {
                        vregs_defined.push(*dest);
                        if let LirOperand::VReg(v) = lhs { vregs_used.push(*v); }
                        if let LirOperand::VReg(v) = rhs { vregs_used.push(*v); }
                    }
                    LirInstruction::Store { src, .. } => {
                        vregs_used.push(*src);
                    }
                    _ => {}
                }

                for v in vregs_used.iter() {
                    let entry = ranges
                        .entry(*v)
                        .or_insert((Some(func_ip), Some(func_ip)));
                    entry.1 = Some(func_ip);
                }

                for v in vregs_defined.iter() {
                    let entry = ranges
                        .entry(*v)
                        .or_insert((Some(func_ip), Some(func_ip)));
                    if entry.0.is_none() { entry.0 = Some(func_ip); }
                    entry.1 = Some(func_ip);
                }
                func_ip += 1;
            }
        }

        ranges.into_iter()
            .map(|(vreg, (start, end))| VRegLiveRange {
                vreg,
                start: start.unwrap(),
                end: end.unwrap(),
            })
            .collect()
    }
}