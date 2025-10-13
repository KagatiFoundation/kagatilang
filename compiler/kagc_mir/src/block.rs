// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashSet;

use crate::instruction::*;

use crate::value::IRValueId;

#[derive(Debug, Default, Clone, Copy, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, Default)]
pub struct IRBasicBlock {
    pub id: BlockId,
    pub instructions: Vec<IRInstruction>,
    pub successors: HashSet<BlockId>,
    pub predecessors: HashSet<BlockId>,
    pub terminator: Terminator,
    pub name: String
}

impl IRBasicBlock {
    pub fn compute_use_def(&self) -> UseDefSet {
        let mut use_defs = UseDefSet::default();

        for inst in &self.instructions {
            for u in inst.uses() {
                if !use_defs.defs.contains(&u) {
                    use_defs.uses.insert(u);
                }
            }
            for d in inst.defs() {
                use_defs.defs.insert(d);
            }
        }
        use_defs
    }

    pub fn add_successor(&mut self, block_id: BlockId) {
        self.successors.insert(block_id);
    }

    pub fn add_predecessor(&mut self, block_id: BlockId) {
        self.predecessors.insert(block_id);
    }
}

#[derive(Debug, Default)]
pub struct UseDefSet {
    pub uses: HashSet<IRValueId>,
    pub defs: HashSet<IRValueId>
}

impl UseDefSet {
    pub fn uses_as_vec(&self) -> Vec<&IRValueId> {
        self.uses.iter().collect::<Vec<&IRValueId>>()
    }

    pub fn defs_as_vec(&self) -> Vec<&IRValueId> {
        self.defs.iter().collect::<Vec<&IRValueId>>()
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    /// Unconditional jump
    Jump(BlockId),

    CondJump {
        cond: IRValueId,
        then_block: BlockId,
        else_block: BlockId
    },

    Return {
        value: Option<IRValueId>,
        target: BlockId
    }
}

impl Default for Terminator {
    fn default() -> Self {
        Terminator::Jump(BlockId(0))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::block::IRBasicBlock;
    use crate::instruction::*;
    use crate::value::{IRValue, IRValueId};

    #[test]
    fn test_use_def_on_single_block() {
        let mut block = IRBasicBlock::default();
        block.instructions.push(IRInstruction::Mov { 
            result: IRValueId(0), 
            src: IRValue::Constant(32)
        });

        block.instructions.push(IRInstruction::Add { 
            result: IRValueId(1), 
            lhs: IRValue::Constant(32), 
            rhs: IRValue::Var(IRValueId(0)) 
        });

        let use_defs = block.compute_use_def();
        assert_eq!(use_defs.uses, HashSet::new());
        assert_eq!(
            use_defs.defs, 
            {
                let mut items = HashSet::new();
                items.insert(IRValueId(0));
                items.insert(IRValueId(1));
                items
            }
        );
    }
}