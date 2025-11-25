// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use kagc_ctx::CompilerCtx;
use kagc_mir::block::{IRBasicBlock, Terminator};
use kagc_mir::function::{FunctionSignature, IRFunction};
use kagc_mir::instruction::{IRAddress, IRCondition, IRInstruction, StackSlotId};
use kagc_mir::value::{IRValue, IRValueId};

use kagc_lir::block::{LirBasicBlock, LirTerminator};
use kagc_lir::function::{LirFunction, LirFunctionParam, LirFunctionSignature};
use kagc_lir::instruction::{LirAddress, LirInstruction};
use kagc_lir::operand::LirOperand;
use kagc_lir::vreg::VRegMapper;

#[derive(Debug, Default)]
pub struct MirToLirLowerer {
    vreg_mapper: VRegMapper,
    compiler_cx: Rc<RefCell<CompilerCtx>>
}

impl MirToLirLowerer {
    pub fn lower_function(&mut self, ir_func: &IRFunction) -> LirFunction {
        let mut lir_blocks = HashMap::new();
        
        let mut block_ids: Vec<_> = ir_func.blocks.keys().cloned().collect();
        block_ids.sort_by_key(|b| b.0);

        for bid in block_ids {
            let block = &ir_func.blocks[&bid];
            lir_blocks.insert(bid, self.lower_block(block));
        }

        LirFunction { 
            id: ir_func.id, 
            name: ir_func.name.clone(),
            signature: self.lower_function_signature(&ir_func.signature), 
            blocks: lir_blocks, 
            entry_block: ir_func.entry_block,
            exit_block: ir_func.exit_block,
        }
    }

    fn lower_function_signature(&mut self, sig: &FunctionSignature) -> LirFunctionSignature {
        let mut params = vec![];
        for p in sig.params.iter() {
            let reg = self.vreg_mapper.get_or_create(p.id);
            params.push(LirFunctionParam { reg, ty: p.ty });
        }
        LirFunctionSignature { 
            params, 
            return_type: sig.return_type,
            class: sig.class
        }
    }

    fn lower_block(&mut self, block: &IRBasicBlock) -> LirBasicBlock {
        let mut lir_instrs = vec![];
        for instr in &block.instructions {
            let instrs = match instr {
                IRInstruction::Add { result, lhs, rhs } => self.lower_add(result, lhs, rhs),
                IRInstruction::Mov { result, src } => self.lower_move(result, src),
                IRInstruction::Store { address, src } => self.lower_store(*address, src),
                IRInstruction::Load { src, result } => self.lower_load(result, *src),
                IRInstruction::CondJump { lhs, rhs, cond, result } => self.lower_conditional(lhs, rhs, cond, result),
                IRInstruction::Call { func, args, result } => self.lower_function_call(func, args, result),
                IRInstruction::MemAlloc { size, ob_ty, result, pool_idx, data_ptr_slot, base_ptr_slot } => self.lower_memory_allocation(*size, *ob_ty, *result, *pool_idx, *base_ptr_slot, *data_ptr_slot),
                _ => unimplemented!("{instr:#?}")
            };
            lir_instrs.extend(instrs);
        }

        let terminator = match block.terminator {
            Terminator::Jump(block_id) => LirTerminator::Jump(block_id),
            Terminator::Return { value, target } => {
                if let Some(ret_value) = value {
                    let ret_reg = self.vreg_mapper.get_or_create(ret_value);
                    LirTerminator::Return {
                        target,
                        value: Some(ret_reg)
                    }
                }
                else {
                    LirTerminator::Return {
                        target,
                        value: None
                    }
                }
            },
            Terminator::CondJump { cond, then_block, else_block } => {
                self.vreg_mapper.get_or_create(cond);
                LirTerminator::CJump { 
                    cond: IRCondition::EqEq, 
                    then_block, 
                    else_block
                }
            }
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

    fn lower_function_call(
        &mut self, 
        func: &str, 
        args: &[IRValueId], 
        result: &Option<IRValueId>
    ) -> Vec<LirInstruction> {
        let mut lir_args = vec![];
        for arg in args {
            let a = self.vreg_mapper.get_or_create(*arg);
            lir_args.push(a);
        }
        let lir_result = if let Some(value) = result {
            Some(self.vreg_mapper.get_or_create(*value))
        }
        else {
            None
        };
        vec![LirInstruction::Call { 
            func: func.to_owned(), 
            args: lir_args, 
            result: lir_result 
        }]
    }

    fn lower_memory_allocation(
        &mut self, 
        size: IRValue, 
        ob_ty: IRValue, 
        result: IRValueId, 
        pool_idx: usize,
        base_ptr_slot: StackSlotId,
        data_ptr_slot: StackSlotId
    ) -> Vec<LirInstruction> {
        // let compiler_cx = self.compiler_cx.borrow();
        // let object = compiler_cx.const_pool.get(pool_idx);
        // if object.is_none() {
            // bug!("ConstEntry(index '{pool_idx}') not found");
        // }
        let size_op = self.resolve_to_operand(size);
        let type_op = self.resolve_to_operand(ob_ty);
        let dest_reg = self.vreg_mapper.get_or_create(result);
        vec![LirInstruction::MemAlloc {
            ob_size: size_op,
            ob_type: type_op,
            dest: dest_reg,
            pool_idx,
            base_ptr_slot,
            data_ptr_slot
        }]
    }

    fn lower_conditional(
        &mut self, 
        lhs: &IRValue, 
        rhs: &IRValue, 
        cond: &IRCondition,
        result: &IRValueId
    ) -> Vec<LirInstruction> {
        match cond {
            IRCondition::EqEq => {
                let lhs_op = self.resolve_to_operand(*lhs);
                let rhs_op = self.resolve_to_operand(*rhs);
                let dest_reg = self.vreg_mapper.get_or_create(*result);
                vec![
                    LirInstruction::CJump {
                        lhs: lhs_op,
                        rhs: rhs_op,
                        op: *cond,
                        dest: dest_reg
                    }
                ]
            },
            IRCondition::NEq => todo!(),
            IRCondition::GTEq => todo!(),
            IRCondition::LTEq => todo!(),
            IRCondition::GThan => todo!(),
            IRCondition::LThan => todo!(),
        }
    }

    fn lower_store(&mut self, address: IRAddress, src: &IRValueId) -> Vec<LirInstruction> {
        let src_vreg = self.vreg_mapper.get_or_create(*src);
        vec![
            LirInstruction::Store { 
                src: src_vreg, 
                dest: self.lower_mir_addr_to_lir_addr(address)
            }
        ]
    }

    fn lower_load(&mut self, result: &IRValueId, address: IRAddress) -> Vec<LirInstruction> {
        let dest_vreg = self.vreg_mapper.get_or_create(*result);
        vec![
            LirInstruction::Load { 
                dest: dest_vreg,
                src: self.lower_mir_addr_to_lir_addr(address)
            }
        ]
    }

    fn lower_mir_addr_to_lir_addr(&mut self, address: IRAddress) -> LirAddress {
        match address {
            IRAddress::StackSlot(off) => LirAddress::Offset(off),
            IRAddress::BaseSlot(base, off) => {
                let base_reg = self.vreg_mapper.get_or_create(base);
                LirAddress::BaseOffset(base_reg, off)
            }
        }
    }

    fn lower_add(&mut self, result: &IRValueId, lhs: &IRValue, rhs: &IRValue) -> Vec<LirInstruction> {
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

    fn lower_move(&mut self, result: &IRValueId, src: &IRValue) -> Vec<LirInstruction> {
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