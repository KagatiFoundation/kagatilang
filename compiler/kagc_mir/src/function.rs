// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::block::IRBasicBlock;
use crate::block::IRBlockId;

use crate::value::IRValue;
use crate::value::IRValueId;

#[derive(Default, Debug)]
pub struct IRFunction {
    pub name: String,
    pub blocks: Vec<IRBasicBlock>,
    pub values: Vec<IRValue>,
    pub args: Vec<IRValueId>,

    block_id: usize,
    value_id: usize
}

impl IRFunction {
    pub fn create_block(&mut self) -> IRBlockId {
        let id = self.next_block_id();
        let new_b = IRBasicBlock {
            id,
            ..Default::default()
        };
        self.blocks.push(new_b);
        id
    }

    pub fn new_value(&mut self, value: IRValue) -> IRValueId {
        let id = self.next_value_id();
        self.values.push(value);
        id
    }

    fn next_block_id(&mut self) -> IRBlockId {
        let nb = IRBlockId(self.block_id);
        self.block_id += 1;
        nb
    }

    fn next_value_id(&mut self) -> IRValueId {
        let iid = IRValueId(self.value_id);
        self.value_id += 1;
        iid
    }
}

#[cfg(test)]
mod tests {
    use crate::block::{IRBasicBlock, IRBlockId};
    use crate::function::IRFunction;
    use crate::instruction::IRInstruction;
    use crate::value::*;

    #[test]
    fn test_simple_function_creation() {
        let mut values = vec![];

        let const1 = IRValue {
            id: IRValueId(0),
            kind: IRValueKind::Constant(32),
            ty: IRType::I64
        };
        values.push(const1);

        let const2 = IRValue {
            id: IRValueId(1),
            kind: IRValueKind::Constant(32),
            ty: IRType::I64
        };
        values.push(const2);

        let add_instr = IRValue {
            id: IRValueId(2),
            kind: IRValueKind::Instruction(
                IRInstruction::Add { lhs: IRValueId(0), rhs: IRValueId(1) }
            ),
            ty: IRType::I64
        };
        values.push(add_instr);

        let arg0 = IRValue {
            id: IRValueId(3),
            kind: IRValueKind::Argument(0),
            ty: IRType::I64
        };
        values.push(arg0);

        let block = IRBasicBlock {
            id: IRBlockId(0),
            instructions: vec![IRValueId(2)]
        };

        let func = IRFunction {
            blocks: vec![block],
            name: "random".to_string(),
            values,
            args: vec![IRValueId(3)],
            ..Default::default()
        };

        dbg!(func);
    }
}