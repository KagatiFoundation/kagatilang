// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod regalloc;
pub mod codegen_asm;

use std::collections::HashMap;

use kagc_mir::function::IrFunction;
use kagc_mir::block::IrBasicBlock;
use kagc_mir::instruction::IrInstruction;
use kagc_mir::value::StackSlotId;

/// Parent struct for generating code.
pub trait CodeGenerator {
    /// Generate code from LIR functions
    fn gen_function(&mut self, func: &IrFunction);

    /// Generate code from LIR basic blocks
    fn gen_block(&mut self, block: &IrBasicBlock);

    /// Generate code from LIR instructions
    fn gen_instruction(&mut self, instr: &IrInstruction);
}

/// A utility struct which helps generate offsets.
#[derive(Debug, Clone, Default)]
pub struct OffsetGenerator {
    pub next_off: i64,
    off_map: HashMap<StackSlotId, i64>,
    off_size: i64
}

impl OffsetGenerator {
    pub fn new(off_size: i64) -> Self {
        Self {
            next_off: 0,
            off_map: HashMap::new(),
            off_size
        }
    }

    /// Get the current offset without incrementing the counter.
    pub fn current(&self) -> i64 {
        self.next_off
    }

    /// Get the next offset value.
    pub fn next(&mut self, slot_id: StackSlotId) -> i64 {
        let off = self.next_off;
        self.off_map.insert(slot_id, off);
        self.next_off += 1;
        off * self.off_size + 8
    }

    pub fn total_space_used(&self) -> i64 {
        self.next_off * self.off_size
    }

    pub fn get_offset(&self, slot_id: StackSlotId) -> Option<&i64> {
        self.off_map.get(&slot_id)
    }

    pub fn get_offset_unchecked(&self, slot_id: StackSlotId) -> i64 {
        (*self.off_map.get(&slot_id).unwrap() * self.off_size) + 8
    }

    pub fn get_or_create_offset(&mut self, slot_id: StackSlotId) -> i64 {
        if let Some(off) = self.off_map.get(&slot_id) {
            *off + 8
        }
        else {
            self.next(slot_id) + 8
        }
    }
}