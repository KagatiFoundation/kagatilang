// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod aarch64;

use kagc_lir::block::LirBasicBlock;
use kagc_lir::function::LirFunction;
use kagc_lir::instruction::LirInstruction;

use crate::regalloc::register::Register;

#[derive(Debug, Clone)]
pub enum MachineOp {
    Register(Register),
    Stack(usize),
    Immediate(i64)
}

pub trait LirToMachineTransformer {
    type MachineInstr;

    fn transform_function(&mut self, lir_func: &LirFunction);
    
    fn transform_block(&mut self, lir_func: &LirBasicBlock);

    fn transform_inst(&mut self, inst: &LirInstruction);

    fn transform(&self) -> Vec<Self::MachineInstr>;
}