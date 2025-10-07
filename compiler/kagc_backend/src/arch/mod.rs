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

pub mod cg {
    use std::collections::HashMap;

    use kagc_lir::instruction::LirInstruction;
    use kagc_lir::operand::LirOperand;
    use kagc_lir::function::LirFunction;
    use kagc_lir::block::LirBasicBlock;
    use kagc_lir::vreg::VReg;

    use crate::regalloc::linear_alloca::Allocation;
    use crate::regalloc::linear_alloca::Location;
    use crate::regalloc::register::RegClass;
    use crate::regalloc::register::Register;

    pub struct CodeGenerator {
        pub allocations: HashMap<VReg, Location>,
        scratch_register0: Register,
        scratch_register1: Register
    }

    impl CodeGenerator {
        pub fn new(allocations: Vec<Allocation>) -> Self {
            let map = allocations.into_iter().map(|a| (a.vreg, a.location)).collect();
            Self { 
                allocations: map,
                scratch_register0: Register {
                    id: 0x9,
                    name: "x9".to_string(),
                    class: RegClass::GPR
                },
                scratch_register1: Register {
                    id: 10,
                    name: "x10".to_string(),
                    class: RegClass::GPR
                }
            }
        }

        pub fn gen_function(&self, lir_func: &LirFunction) {
            for block in lir_func.blocks.values() {
                self.gen_block(block);
            }
        }

        fn gen_block(&self, block: &LirBasicBlock) {
            for instr in block.instructions.iter() {
                match instr {
                    LirInstruction::Mov { dest, src } => self.emit_mov_inst(dest, src),
                    LirInstruction::Add { dest, lhs, rhs } => self.emit_add_inst(dest, lhs, rhs),
                    LirInstruction::Store { src, dest } => self.emit_str_inst(src, dest.0),
                    LirInstruction::Load { src, dest } => self.emit_ldr_inst(src.0, dest),

                    LirInstruction::Jump { label } => println!("jump"),
                }
            }
        }

        fn emit_str_inst(&self, src: &VReg, dest_slot: usize) {
            let src_loc = self.allocations.get(src).unwrap();
            match src_loc {
                Location::Reg(register) => self.emit_str(register, dest_slot),
                Location::StackSlot(ss) => {
                    self.emit_ldr(&self.scratch_register0, *ss);
                    self.emit_str(&self.scratch_register0, dest_slot);
                }
            }
        }

       fn emit_ldr_inst(&self, src_slot: usize, dest: &VReg) {
            let dest_loc = self.allocations.get(dest).unwrap();
            match dest_loc {
                Location::Reg(register) => self.emit_ldr(register, src_slot),
                Location::StackSlot(ss) => {
                    self.emit_ldr(&self.scratch_register0, src_slot);
                    self.emit_str(&self.scratch_register0, *ss);
                }
            }
        } 

        fn emit_mov_inst(&self, dest: &VReg, src: &LirOperand) {
            let loc = self.allocations.get(dest).unwrap();
            if let Location::Reg(dest_reg) = loc {
                match src {
                    LirOperand::VReg(vreg) => {
                        let src_loc = self.allocations.get(vreg).unwrap();
                        if let Location::Reg(src_reg) = src_loc {
                            self.emit_move_reg_to_reg(dest_reg, src_reg);
                        }
                    },
                    LirOperand::Constant(value) => {
                        self.emit_move_const_to_reg(dest_reg, *value);
                    },
                }
            }
            else if let Location::StackSlot(dest_slot) = loc {
                match src {
                    LirOperand::VReg(vreg) => {
                        let src_loc = self.allocations.get(vreg).unwrap();
                        if let Location::Reg(src_reg) = src_loc {
                            self.emit_move_reg_to_reg(&self.scratch_register0, src_reg);
                            self.emit_str(&self.scratch_register0, *dest_slot);
                        }
                    },
                    LirOperand::Constant(value) => {
                        self.emit_move_const_to_reg(&self.scratch_register0, *value);
                        self.emit_str(&self.scratch_register0, *dest_slot);
                    },
                }
            }
        }

        fn emit_add_inst(&self, dest: &VReg, lhs: &LirOperand, rhs: &LirOperand) {
            let dest_loc = self.allocations.get(dest).unwrap();
            match dest_loc {
                Location::Reg(register) => self.emit_add_dest_reg(register, lhs, rhs),
                Location::StackSlot(slot) => self.emit_add_dest_stack(*slot, lhs, rhs),
            }
        }

        fn emit_add_dest_reg(&self, dest_reg: &Register, lhs: &LirOperand, rhs: &LirOperand) {
            match (lhs, rhs) {
                (LirOperand::VReg(vreg1), LirOperand::VReg(vreg2)) => {
                    let src1 = self.allocations.get(vreg1).unwrap();
                    let src2 = self.allocations.get(vreg2).unwrap();
                    match (src1, src2) {
                        (Location::Reg(r1), Location::Reg(r2)) => self.emit_add_reg_reg_reg(dest_reg, r1, r2),

                        (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                        (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                            self.emit_ldr(&self.scratch_register0, *op_slot);
                            self.emit_add_reg_reg_reg(dest_reg, reg_op, &self.scratch_register0);
                        },

                        (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                            self.emit_ldr(&self.scratch_register0, *op1_slot);
                            self.emit_ldr(&self.scratch_register1, *op2_slot);
                            self.emit_add_reg_reg_reg(dest_reg, &self.scratch_register0, &self.scratch_register1);
                        },
                    }
                },

                (LirOperand::Constant(imm), LirOperand::VReg(vreg)) |
                (LirOperand::VReg(vreg), LirOperand::Constant(imm)) => {
                    let src1 = self.allocations.get(vreg).unwrap();
                    match src1 {
                        Location::Reg(r1) => self.emit_add_reg_reg_imm(dest_reg, r1, *imm),
                        Location::StackSlot(src_slot) => {
                            self.emit_ldr(&self.scratch_register0, *src_slot);
                            self.emit_add_reg_reg_imm(dest_reg, &self.scratch_register0, *imm);
                        },
                    }
                },

                (LirOperand::Constant(imm1), LirOperand::Constant(imm2)) => {
                    self.emit_move_const_to_reg(dest_reg, *imm1 + *imm2);
                },
            }
        }

        fn emit_add_dest_stack(&self, dest_slot: usize, lhs: &LirOperand, rhs: &LirOperand) {
            match (lhs, rhs) {
                (LirOperand::VReg(vreg1), LirOperand::VReg(vreg2)) => {
                    let src1 = self.allocations.get(vreg1).unwrap();
                    let src2 = self.allocations.get(vreg2).unwrap();
                    match (src1, src2) {
                        (Location::Reg(r1), Location::Reg(r2)) => {
                            self.emit_add_reg_reg_reg(&self.scratch_register0, r1, r2);
                            self.emit_str(&self.scratch_register0, dest_slot);
                        }

                        (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                        (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                            self.emit_ldr(&self.scratch_register0, *op_slot);
                            self.emit_add_reg_reg_reg(&self.scratch_register1, reg_op, &self.scratch_register0);
                            self.emit_str(&self.scratch_register1, dest_slot);
                        },

                        (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                            self.emit_ldr(&self.scratch_register0, *op1_slot);
                            self.emit_ldr(&self.scratch_register1, *op2_slot);
                            self.emit_add_reg_reg_reg(&self.scratch_register0, &self.scratch_register0, &self.scratch_register1);
                            self.emit_str(&self.scratch_register0, dest_slot);
                        },
                    }
                },

                (LirOperand::Constant(imm), LirOperand::VReg(vreg)) |
                (LirOperand::VReg(vreg), LirOperand::Constant(imm)) => {
                    let src1 = self.allocations.get(vreg).unwrap();
                    match src1 {
                        Location::Reg(r1) => {
                            self.emit_add_reg_reg_imm(&self.scratch_register0, r1, *imm);
                            self.emit_str(&self.scratch_register0, dest_slot);
                        }
                        Location::StackSlot(src_slot) => {
                            self.emit_ldr(&self.scratch_register0, *src_slot);
                            self.emit_add_reg_reg_imm(&self.scratch_register0, &self.scratch_register0, *imm);
                            self.emit_str(&self.scratch_register0, dest_slot);
                        },
                    }
                },
                (LirOperand::Constant(imm1), LirOperand::Constant(imm2)) => {
                    self.emit_move_const_to_reg(&self.scratch_register0, *imm1 + *imm2);
                    self.emit_str(&self.scratch_register0, dest_slot);
                },
            }
        }

        fn emit_ldr(&self, r1: &Register, ss: usize) {
            println!("LDR {d}, [SP, #{s}]", d = r1.name, s = ss * 8);
        }

        fn emit_str(&self, r1: &Register, ss: usize) {
            println!("STR {d}, [SP, #{s}]", d = r1.name, s = ss * 8);
        }

        fn emit_add_reg_reg_reg(&self, r1: &Register, r2: &Register, r3: &Register) {
            println!("ADD {d}, {a}, {b}", d = r1.name, a = r2.name, b = r3.name);
        }

        fn emit_add_reg_reg_imm(&self, r1: &Register, r2: &Register, imm: i64) {
            println!("ADD {d}, {a}, #{b}", d = r1.name, a = r2.name, b = imm);
        }

        fn emit_move_reg_to_reg(&self, r1: &Register, r2: &Register) {
            println!("MOV {d}, {s}", d = r1.name, s = r2.name);
        }

        fn emit_move_const_to_reg(&self, r1: &Register, s: i64) {
            println!("MOV {d}, #{s}", d = r1.name);
        }
    }
}