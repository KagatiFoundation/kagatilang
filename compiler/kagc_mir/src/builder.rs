// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::rc::Rc;

use crate::block::IRBasicBlock;
use crate::instruction::IRInstruction;
use crate::operand::IROperand;
use crate::value::*;

pub struct IRBuilder {
    blocks: Vec<IRBasicBlock>,
    current_block: usize,
    next_id: usize
}

impl IRBuilder {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            blocks: vec![IRBasicBlock::default()],
            current_block: 0,
            next_id: 0,
        }
    }

    pub fn create_block(&mut self) -> usize {
        let new_block = IRBasicBlock::default();
        self.blocks.push(new_block);
        self.blocks.len() - 1
    }

    pub fn current_block_mut(&mut self) -> &mut IRBasicBlock {
        &mut self.blocks[self.current_block]
    }

    pub fn current_block(&self) -> &IRBasicBlock {
        &self.blocks[self.current_block]
    }

    pub fn next_id(&mut self) -> IRValueId {
        let curr_id = self.next_id;
        self.next_id += 1;
        IRValueId(curr_id)
    }

    pub fn create_assign(&mut self, src_op: IROperand) -> IRValueId {
        let id = self.next_id();
        let value = Rc::new(RefCell::new(
            IRValue::new(
                IRValueId(id.0), 
                IRInstruction::Assign { 
                    src: src_op.clone() 
                }
            )
        ));
        self.current_block_mut().add_value(value.clone());

        if let IROperand::Value(op_id) = src_op {
            if let Some(op_value) = self.current_block().instructions.iter().find(|v| v.borrow().id == op_id) {
                op_value.borrow_mut().add_user(id.clone());
            }
        }
        id
    }
}

#[cfg(test)]
mod tests {
    use crate::{builder::IRBuilder, operand::IROperand};

    #[test]
    fn test_ir_builder_assign() {
        let mut builder = IRBuilder::new();
        let v1 = builder.create_assign(
            IROperand::Imm(32)
        );
    }
}