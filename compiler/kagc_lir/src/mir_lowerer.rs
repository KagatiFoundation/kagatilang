// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_mir::block::IRBasicBlock;
use kagc_mir::block::Terminator;
use kagc_mir::function::IRFunction;
use kagc_mir::instruction::IRInstruction;
use kagc_mir::value::IRValue;
use kagc_mir::value::IRValueId;

use crate::instruction::LIRInstruction;
use crate::instruction::LIRLabel;
use crate::operand::LIROperand;
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

#[cfg(test)]
mod tests {
    use kagc_mir::builder::IRBuilder;
    use kagc_mir::function::FunctionId;
    use kagc_mir::types::IRType;
    use kagc_mir::value::IRValue;
    use kagc_mir::block::{BlockId, Terminator};

    use crate::mir_lowerer::MIRLowerer;

    #[test]
    fn test_mir_to_lir_lowerer_for_simple_function() {
        let mut builder = IRBuilder::default();
        let (_, func_entry) = builder.create_function(vec![], IRType::I64); // block id 0
        let op1 = builder.create_move(IRValue::Constant(32)); // value id 0
        let op2 = builder.create_move(IRValue::Constant(32)); // value id 1
        _ = builder.create_add(IRValue::Var(op1), IRValue::Var(op2)); // value id 2

        builder.set_terminator(
            func_entry,
            Terminator::Jump(BlockId(0))
        );

        let mut mir_lowerer = MIRLowerer::default();
        
        let module = builder.build();
        assert!(module.functions.contains_key(&FunctionId(0)));

        let func = module.functions.get(&FunctionId(0)).unwrap();
        let instrs = mir_lowerer.lower_function(func);

        println!("{instrs:#?}");
    }
}