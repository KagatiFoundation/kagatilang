// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod regalloc;
pub mod reg;
pub mod codegen;
pub mod codegen_asm;
pub mod codegen_lsvm;

use kagc_lir::block::LirBasicBlock;
use kagc_lir::function::LirFunction;
use kagc_lir::instruction::LirInstruction;

pub trait CodeGenerator {
    /// Generate code from LIR functions
    fn gen_function(&mut self, func: &LirFunction);

    /// Generate code from LIR basic blocks
    fn gen_block(&mut self, block: &LirBasicBlock);

    /// Generate code from LIR instructions
    fn gen_instruction(&mut self, instr: &LirInstruction);
}