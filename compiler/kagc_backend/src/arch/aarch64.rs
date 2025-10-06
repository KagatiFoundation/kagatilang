// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_lir::function::LirFunction;
use kagc_lir::instruction::LirInstruction;
use kagc_lir::operand::LirOperand;
use kagc_lir::block::LirBasicBlock;
use kagc_lir::vreg::VReg;

use crate::arch::MachineOp;
use crate::arch::LirToMachineTransformer;
use crate::regalloc::linear_alloca::Allocation;
use crate::regalloc::linear_alloca::Location;
use crate::regalloc::register::Register;

#[derive(Debug)]
pub enum Aarch64Instr {
    Add {
        dest: Register,
        lhs: MachineOp,
        rhs: MachineOp
    },

    Mov {
        dest: Register,
        src: MachineOp
    },

    FunctionPreamble {
        stack_size: usize
    },

    FunctionPostamble {
        stack_size: usize
    },

    Store {
        reg: Register,
        addr: usize
    }
}

pub struct LirToAarch64ArchTransformer {
    allocations: HashMap<VReg, Location>
}

impl LirToAarch64ArchTransformer {
    pub fn new(allocas: Vec<Allocation>) -> Self {
        Self {
            allocations: allocas.iter().map(|a| (a.vreg, a.location.clone())).collect()
        }
    }

    fn resolve_to_register(&self, loc: &Location) -> Register {
        if let Location::Reg(reg) = loc {
            reg.clone()
        }
        else {
            panic!("Location {loc:#?} is not a register! Aborting...");
        }
    }

    fn resolve_operand(&self, op: &LirOperand) -> MachineOp {
        match op {
            LirOperand::VReg(vreg) => {
                let loc = self.allocations.get(vreg).unwrap();
                MachineOp::Register(self.resolve_to_register(loc).name)
            },
            LirOperand::Constant(value) => MachineOp::Immediate(*value)
        }
    }
}

impl LirToMachineTransformer for LirToAarch64ArchTransformer {
    type MachineInstr = Aarch64Instr;
    
    fn transform_function(&self, lir_func: &LirFunction) -> Vec<Self::MachineInstr> {
        let mut final_instrs = vec![];
        for block in lir_func.blocks.values() {
            final_instrs.extend(self.transform_block(block));
        }
        final_instrs
    }
    
    fn transform_block(&self, lir_func: &LirBasicBlock) -> Vec<Self::MachineInstr> {
        let mut final_instrs = vec![];
        for inst in lir_func.instructions.iter() {
            let m_inst = self.transform_inst(inst);
            final_instrs.push(m_inst);
        }
        final_instrs
    }
    
    fn transform_inst(&self, inst: &LirInstruction) -> Self::MachineInstr {
        match inst {
            LirInstruction::Add { dest, lhs, rhs } => {
                let dest_reg = self.resolve_to_register(self.allocations.get(dest).unwrap());
                let op1 = self.resolve_operand(lhs);
                let op2 = self.resolve_operand(rhs);
                Aarch64Instr::Add { 
                    dest: dest_reg, 
                    lhs: op1, 
                    rhs: op2 
                }
            },

            LirInstruction::Mov { dest, src } => {
                let dest = self.resolve_to_register(self.allocations.get(dest).unwrap());
                let src = self.resolve_operand(src);
                Aarch64Instr::Mov {
                    dest,
                    src
                }
            }

            LirInstruction::Store { dest, src} => {
                let src_reg = self.resolve_to_register(self.allocations.get(src).unwrap());
                Aarch64Instr::Store { 
                    reg: src_reg, 
                    addr: dest.0 
                }
            },

            _ => unimplemented!("{inst:#?}")
        }
    }
}