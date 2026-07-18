// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod regalloc;
pub mod codegen_asm;

use kagc_mir::function::IrFunction;
use kagc_mir::block::IrBasicBlock;
use kagc_mir::instruction::IrInstruction;

/// Parent struct for generating code.
pub trait CodeGenerator {
    /// Generate code from LIR functions
    fn gen_function(&mut self, func: &mut IrFunction);

    /// Generate code from LIR basic blocks
    fn gen_block(&mut self, block: &IrBasicBlock);

    /// Generate code from LIR instructions
    fn gen_instruction(&mut self, instr: &IrInstruction);
}