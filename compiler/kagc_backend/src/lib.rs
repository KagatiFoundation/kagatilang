// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod regalloc;
pub mod reg;
pub mod codegen;
pub mod codegen_asm;
pub mod codegen_lsvm;

use std::collections::HashMap;

use kagc_lir::block::LirBasicBlock;
use kagc_lir::function::LirFunction;
use kagc_lir::instruction::LirInstruction;
use kagc_mir::instruction::StackSlotId;

/// Parent struct for generating code.
pub trait CodeGenerator {
    /// Generate code from LIR functions
    fn gen_function(&mut self, func: &LirFunction);

    /// Generate code from LIR basic blocks
    fn gen_block(&mut self, block: &LirBasicBlock);

    /// Generate code from LIR instructions
    fn gen_instruction(&mut self, instr: &LirInstruction);
}

/// A utility struct which helps generate offsets.
#[derive(Debug, Clone, Default)]
pub struct OffsetGenerator {
    pub next_off: usize,
    off_map: HashMap<StackSlotId, usize>,
    off_size: usize
}

impl OffsetGenerator {
    pub fn new(off_size: usize) -> Self {
        Self {
            next_off: 0,
            off_map: HashMap::new(),
            off_size
        }
    }

    /// Get the current offset without incrementing the counter.
    pub fn current(&self) -> usize {
        self.next_off
    }

    /// Get the next offset value.
    pub fn next(&mut self, slot_id: StackSlotId) -> usize {
        let off = self.next_off;
        self.off_map.insert(slot_id, off);
        self.next_off += 1;
        off * self.off_size
    }

    pub fn total_space_used(&self) -> usize {
        self.next_off * self.off_size
    }

    pub fn get_offset(&self, slot_id: StackSlotId) -> Option<&usize> {
        self.off_map.get(&slot_id)
    }

    pub fn get_offset_unchecked(&self, slot_id: StackSlotId) -> usize {
        *self.off_map.get(&slot_id).unwrap()
    }

    pub fn get_or_create_offset(&mut self, slot_id: StackSlotId) -> usize {
        if let Some(off) = self.off_map.get(&slot_id) {
            *off
        }
        else {
            self.next(slot_id)
        }
    }
}