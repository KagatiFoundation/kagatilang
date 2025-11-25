// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_mir::block::BlockId;
use kagc_mir::function::FunctionId;
use kagc_mir::types::IRType;
use kagc_symbol::StorageClass;

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
    pub class: StorageClass
}

#[derive(Debug)]
pub struct LirFunction {
    pub id: FunctionId,
    pub name: String,
    pub signature: LirFunctionSignature,
    pub blocks: HashMap<BlockId, LirBasicBlock>,
    pub entry_block: BlockId,
    pub exit_block: BlockId,
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
                    },
                    LirInstruction::Load { dest, .. } => {
                        vregs_defined.push(*dest);
                    },
                    LirInstruction::Store { src, .. } => {
                        vregs_used.push(*src);
                    },
                    LirInstruction::MemAlloc { dest, .. } => {
                        vregs_defined.push(*dest);
                    },
                    LirInstruction::CJump { dest, lhs, rhs, ..} => {
                        vregs_defined.push(*dest);
                        if let LirOperand::VReg(v) = lhs { vregs_used.push(*v); }
                        if let LirOperand::VReg(v) = rhs { vregs_used.push(*v); }
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

#[cfg(test)]
mod tests {
    use kagc_mir::block::Terminator;
    use kagc_mir::builder::IRBuilder;
    use kagc_mir::instruction::IRCondition;
    use kagc_mir::types::IRType;
    use kagc_mir::value::IRValue;
    use kagc_symbol::StorageClass;

    #[test]
    fn test_vreg_generation() {
        let mut builder = IRBuilder::default();
        let fn_ctx = builder.create_function("complex_fn".to_owned(), vec![], IRType::I64, StorageClass::GLOBAL); // block id 0
        let func_entry = fn_ctx.entry_block;
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
        println!("{module:#?}");
    }
}