// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_mir::block::IRBasicBlock;
use kagc_mir::block::Terminator;
use kagc_mir::function::IRFunction;
use kagc_mir::instruction::IRInstruction;
use kagc_mir::value::IRValue;
use kagc_mir::value::IRValueId;

use crate::instruction::LIRInstruction;
use crate::instruction::LIRLabel;
use crate::operand::LIROperand;
use crate::vreg::VReg;
use crate::vreg::VRegMapper;

#[derive(Debug, Default)]
pub struct MIRLowerer {
    vreg_mapper: VRegMapper
}

impl MIRLowerer {
    pub fn lower_function(&mut self, ir_func: &IRFunction) -> Vec<LIRInstruction> {
        let mut lir_instrs = vec![];
        for (_, block) in &ir_func.blocks {
            lir_instrs.extend(self.lower_block(block));
        }
        lir_instrs
    }

    pub fn lower_block(&mut self, block: &IRBasicBlock) -> Vec<LIRInstruction> {
        let mut lir_instrs = vec![];
        for instr in &block.instructions {
            let instrs = match instr {
                IRInstruction::Add { result, lhs, rhs } => self.lower_add(result, lhs, rhs),
                IRInstruction::Mov { result, src } => self.lower_move(result, src),
                _ => unimplemented!()
            };
            lir_instrs.extend(instrs);
        }

        match block.terminator {
            Terminator::Jump(block_id) => {
                lir_instrs.push(
                    LIRInstruction::Jump { 
                        label: LIRLabel(block_id.0)
                    }
                );
            },
            _ => unimplemented!()
        }

        lir_instrs
    }

    pub fn lower_add(&mut self, result: &IRValueId, lhs: &IRValue, rhs: &IRValue) -> Vec<LIRInstruction> {
        let lhs_operand = self.resolve_to_operand(*lhs);
        let rhs_operand = self.resolve_to_operand(*rhs);
        let dest_vreg = self.vreg_mapper.get_or_create(*result);

        vec![
            LIRInstruction::Add {
                dest: dest_vreg,
                lhs: lhs_operand,
                rhs: rhs_operand,
            }
        ]
    }

    pub fn lower_move(&mut self, result: &IRValueId, src: &IRValue) -> Vec<LIRInstruction> {
        let src_operand = self.resolve_to_operand(*src);
        let dest_vreg = self.vreg_mapper.get_or_create(*result);

        vec![
            LIRInstruction::Mov { 
                dest: dest_vreg, 
                src: src_operand 
            }
        ]
    }

    pub fn resolve_to_operand(&mut self, value: IRValue) -> LIROperand {
        match value {
            IRValue::Constant(value) => LIROperand::Constant(value),
            IRValue::Var(irvalue_id) => {
                let vreg = self.vreg_mapper.get_or_create(irvalue_id);
                LIROperand::VReg(vreg)
            },
            _ => unimplemented!()
        }
    }
}

#[derive(Debug, Clone)]
pub struct VRegLiveRange {
    pub vreg: VReg,
    pub start: usize,  // instruction index in function
    pub end: usize,    // instruction index in function
}

pub fn compute_vreg_live_ranges(lir: &[LIRInstruction]) -> Vec<VRegLiveRange> {
    let mut ranges: HashMap<VReg, (Option<usize>, Option<usize>)> = HashMap::new();

    for (idx, instr) in lir.iter().enumerate() {
        // Collect all VRegs in this instruction
        let mut vregs = vec![];

        match instr {
            LIRInstruction::Mov { dest, src } => {
                vregs.push(*dest);
                if let LIROperand::VReg(v) = src {
                    vregs.push(*v);
                }
            }
            LIRInstruction::Add { dest, lhs, rhs } => {
                vregs.push(*dest);
                if let LIROperand::VReg(v) = lhs { vregs.push(*v); }
                if let LIROperand::VReg(v) = rhs { vregs.push(*v); }
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