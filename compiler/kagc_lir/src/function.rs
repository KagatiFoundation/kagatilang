// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_mir::block::BlockId;
use kagc_mir::function::FunctionId;
use kagc_mir::types::IRType;

use crate::block::LirBasicBlock;
use crate::instruction::LirInstruction;
use crate::operand::LirOperand;
use crate::mir_lowerer::VRegLiveRange;
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
        let mut ranges = vec![]; 
        for block in self.blocks.values() {
            ranges.extend(self.compute_live_ranges_in_block(block));
        }
        ranges
    }

    fn compute_live_ranges_in_block(&self, block: &LirBasicBlock) -> Vec<VRegLiveRange> {
        let mut ranges: HashMap<VReg, (Option<usize>, Option<usize>)> = HashMap::new();
        for (idx, instr) in block.instructions.iter().enumerate() {
            let mut vregs = vec![];

            match instr {
                LirInstruction::Mov { dest, src } => {
                    vregs.push(*dest);
                    if let LirOperand::VReg(v) = src {
                        vregs.push(*v);
                    }
                }
                LirInstruction::Add { dest, lhs, rhs } => {
                    vregs.push(*dest);
                    if let LirOperand::VReg(v) = lhs { vregs.push(*v); }
                    if let LirOperand::VReg(v) = rhs { vregs.push(*v); }
                }
                _ => {}
            }

            for vreg in vregs {
                let entry = ranges.entry(vreg).or_insert((None, None));
                if entry.0.is_none() { entry.0 = Some(idx); }
                entry.1 = Some(idx);
            }
        }

        ranges.into_iter().map(|(vreg, (start, end))| VRegLiveRange {
            vreg,
            start: start.unwrap(),
            end: end.unwrap(),
        }).collect()
    }
}