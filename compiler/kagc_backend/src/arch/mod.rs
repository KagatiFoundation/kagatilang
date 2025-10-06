// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod aarch64;

use kagc_lir::block::LirBasicBlock;
use kagc_lir::function::LirFunction;
use kagc_lir::instruction::LirInstruction;

#[derive(Debug)]
pub enum MachineOp {
    Register(String),
    Stack(usize),
    Immediate(i64)
}

pub trait LirToMachineTransformer {
    type MachineInstr;

    fn transform_function(&self, lir_func: &LirFunction) -> Vec<Self::MachineInstr>;
    
    fn transform_block(&self, lir_func: &LirBasicBlock) -> Vec<Self::MachineInstr>;

    fn transform_inst(&self, inst: &LirInstruction) -> Self::MachineInstr;
}

pub mod cg {
    use crate::arch::aarch64::Aarch64Instr;
    use crate::arch::MachineOp;

    pub struct Aarch64Codegen;

    impl Aarch64Codegen {
        pub fn generate_code(&self, instrs: &[Aarch64Instr]) {
            for instr in instrs.iter() {
                match instr {
                    Aarch64Instr::Add { dest, lhs, rhs } => {
                        println!(
                            "ADD {d}, {a}, {b}", 
                            d = dest.name,
                            a = self.resolve_operand(lhs),
                            b = self.resolve_operand(rhs)
                        )
                    },

                    Aarch64Instr::Mov { dest, src } => {
                        println!(
                            "MOV {d}, {s}",
                            d = dest.name,
                            s = self.resolve_operand(src)
                        )
                    }
                    
                    Aarch64Instr::Store { reg, .. } => {
                        println!(
                            "STR {s}, [SP, #0]",
                            s = reg.name,
                        )
                    }

                    _ => unimplemented!()
                }
            }
        }

        fn resolve_operand(&self, op: &MachineOp) -> String {
            match op {
                MachineOp::Register(reg) => reg.clone(),
                MachineOp::Stack(off) => off.to_string(),
                MachineOp::Immediate(imm) => imm.to_string(),
            }
        }
    }
}