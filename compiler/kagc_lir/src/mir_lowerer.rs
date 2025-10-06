// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_mir::block::IRBasicBlock;
use kagc_mir::block::Terminator;
use kagc_mir::function::FunctionSignature;
use kagc_mir::function::IRFunction;
use kagc_mir::instruction::IRAddress;
use kagc_mir::instruction::IRInstruction;
use kagc_mir::value::IRValue;
use kagc_mir::value::IRValueId;

use crate::block::LirBasicBlock;
use crate::block::LirTerminator;
use crate::function::LirFunction;
use crate::function::LirFunctionParam;
use crate::function::LirFunctionSignature;
use crate::function::LirFunctionStack;
use crate::instruction::LirAddress;
use crate::instruction::LirInstruction;
use crate::operand::LirOperand;
use crate::vreg::VReg;
use crate::vreg::VRegMapper;

#[derive(Debug, Default)]
pub struct MirToLirTransformer {
    vreg_mapper: VRegMapper
}

impl MirToLirTransformer {
    pub fn transform_function(&mut self, ir_func: &IRFunction) -> LirFunction {
        let mut lir_blocks = HashMap::new();
        for (bid, block) in ir_func.blocks.iter() {
            lir_blocks.insert(*bid, self.transform_block(block));
        }
        LirFunction { 
            id: ir_func.id, 
            signature: self.transform_function_signature(&ir_func.signature), 
            stack: LirFunctionStack { size: 0 }, 
            blocks: lir_blocks, 
            entry_block: ir_func.entry_block 
        }
    }

    fn transform_function_signature(&mut self, sig: &FunctionSignature) -> LirFunctionSignature {
        let mut params = vec![];
        for p in sig.params.iter() {
            let reg = self.vreg_mapper.get_or_create(p.id);
            params.push(LirFunctionParam { reg, ty: p.ty });
        }
        LirFunctionSignature { 
            params, 
            return_type: sig.return_type
        }
    }

    fn transform_block(&mut self, block: &IRBasicBlock) -> LirBasicBlock {
        let mut lir_instrs = vec![];
        for instr in &block.instructions {
            let instrs = match instr {
                IRInstruction::Add { result, lhs, rhs } => self.transform_add(result, lhs, rhs),
                IRInstruction::Mov { result, src } => self.transform_move(result, src),
                IRInstruction::Store { address, src } => self.transform_store(*address, src),
                _ => unimplemented!()
            };
            lir_instrs.extend(instrs);
        }

        let terminator = match block.terminator {
            Terminator::Jump(block_id) => LirTerminator::Jump(block_id),
            Terminator::Return(ret) => {
                if let Some(ret_value) = ret {
                    let ret_reg = self.vreg_mapper.get_or_create(ret_value);
                    LirTerminator::Return(Some(ret_reg))
                }
                else {
                    LirTerminator::Return(None)
                }
            },
            _ => unimplemented!()
        };

        LirBasicBlock { 
            id: block.id, 
            instructions: lir_instrs, 
            successors: block.successors.clone(), 
            predecessors: block.predecessors.clone(), 
            terminator, 
            name: block.name.clone() 
        }
    }

    fn transform_store(&mut self, dest: IRAddress, src: &IRValueId) -> Vec<LirInstruction> {
        let src_vreg = self.vreg_mapper.get_or_create(*src);
        vec![
            LirInstruction::Store { 
                src: src_vreg, 
                dest: LirAddress(0) 
            }
        ]
    }

    fn transform_add(&mut self, result: &IRValueId, lhs: &IRValue, rhs: &IRValue) -> Vec<LirInstruction> {
        let lhs_operand = self.resolve_to_operand(*lhs);
        let rhs_operand = self.resolve_to_operand(*rhs);
        let dest_vreg = self.vreg_mapper.get_or_create(*result);

        vec![
            LirInstruction::Add {
                dest: dest_vreg,
                lhs: lhs_operand,
                rhs: rhs_operand,
            }
        ]
    }

    fn transform_move(&mut self, result: &IRValueId, src: &IRValue) -> Vec<LirInstruction> {
        let src_operand = self.resolve_to_operand(*src);
        let dest_vreg = self.vreg_mapper.get_or_create(*result);

        vec![
            LirInstruction::Mov { 
                dest: dest_vreg, 
                src: src_operand 
            }
        ]
    }

    fn resolve_to_operand(&mut self, value: IRValue) -> LirOperand {
        match value {
            IRValue::Constant(value) => LirOperand::Constant(value),
            IRValue::Var(irvalue_id) => {
                let vreg = self.vreg_mapper.get_or_create(irvalue_id);
                LirOperand::VReg(vreg)
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