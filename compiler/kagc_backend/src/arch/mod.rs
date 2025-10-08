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

    use kagc_lir::block::LirTerminator;
    use kagc_lir::instruction::LirAddress;
    use kagc_lir::instruction::LirInstruction;
    use kagc_lir::operand::LirOperand;
    use kagc_lir::function::LirFunction;
    use kagc_lir::block::LirBasicBlock;
    use kagc_lir::vreg::VReg;

    use crate::regalloc::register::aarch64::standard_aarch64_register_file;
    use crate::regalloc::LinearScanAllocator;
    use crate::regalloc::Location;
    use crate::regalloc::register::RegClass;
    use crate::regalloc::register::Register;

    pub struct CodeGenerator {
        allocations: Option<HashMap<VReg, Location>>,
        scratch_register0: Register,
        scratch_register1: Register
    }

    impl Default for CodeGenerator {
        fn default() -> Self {
            Self { 
                allocations: None,
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
    }

    impl CodeGenerator {
        pub fn gen_function(&mut self, lir_func: &LirFunction) {
            let mut allocator = LinearScanAllocator::new(standard_aarch64_register_file());
            let allocs = allocator.allocate(&mut lir_func.compute_vreg_live_ranges()[..]);
            let spill_stack_size = allocs.stack_usage();

            let vreg_mappings: HashMap<VReg, Location> = allocs.allocations.into_iter().map(|a| (a.vreg, a.location)).collect();
            self.allocations = Some(vreg_mappings);

            // manage function's stack
            self.emit_function_preamble(lir_func, spill_stack_size);

            let mut block_ids: Vec<_> = lir_func.blocks.keys().cloned().collect();
            block_ids.sort_by_key(|b| b.0);

            // function's body
            for bid in block_ids {
                let block = &lir_func.blocks[&bid];
                self.gen_block(block);
            }
            // return from the function
            self.emit_function_postamble(lir_func, spill_stack_size);
        }

        fn emit_function_preamble(&self, lir_func: &LirFunction, spill_ss: usize) {
            let mut output = format!(".global _{name}\n_{name}:", name = lir_func.name);
            let stack_size = lir_func.frame_info.size + spill_ss;
            if stack_size > 0 {
                output.push_str(&format!("\nsub sp, sp, #{off}\n", off = stack_size));
                output.push_str(&format!("stp x29, x30, [sp, #{off}]\n", off = stack_size - 16));
                output.push_str(&format!("add x29, #{off}", off = stack_size - 16));
            }
            println!("{output}");
        }

        fn emit_function_postamble(&self, lir_func: &LirFunction, spill_ss: usize) {
            let mut output = "".to_owned();
            let stack_size = lir_func.frame_info.size + spill_ss;
            if stack_size > 0 {
                output.push_str(&format!("add sp, sp, #{off}\n", off = stack_size));
            }
            output.push_str("ret");
            println!("{output}");
        }

        fn gen_block(&self, block: &LirBasicBlock) {
            self.emit_label(block.id.0);

            for instr in block.instructions.iter() {
                match instr {
                    LirInstruction::Mov { dest, src } => self.emit_mov_inst(dest, src),
                    LirInstruction::Add { dest, lhs, rhs } => self.emit_add_inst(dest, lhs, rhs),
                    LirInstruction::Store { src, dest } => self.emit_str_inst(src, *dest),
                    LirInstruction::Load { src, dest } => self.emit_ldr_inst(*src, dest),

                    _ => panic!()
                }
            }

            match block.terminator {
                LirTerminator::Jump(block_id) => println!("b _L{}", block_id.0),
                LirTerminator::Return(vreg) => {
                    if let Some(ret_val_vreg) = &vreg {
                        let src_loc = self.current_allocations().get(ret_val_vreg).unwrap();
                        match src_loc {
                            Location::Reg(r1) => println!("mov x0, {s}", s = r1.name),
                            Location::StackSlot(ss) => {
                                self.emit_ldr(&self.scratch_register0, LirAddress::Offset(*ss));
                                println!("mov x0, {s}", s = self.scratch_register0.name);
                            }
                        }
                    }
                },
            }
        }

        fn emit_label(&self, id: usize) {
            println!("_L{id}:");
        }

        fn emit_str_inst(&self, src: &VReg, addr: LirAddress) {
            let src_loc = self.current_allocations().get(src).unwrap();
            match src_loc {
                Location::Reg(register) => self.emit_str(register, addr),
                Location::StackSlot(ss) => {
                    self.emit_ldr(&self.scratch_register0, LirAddress::Offset(*ss));
                    self.emit_str(&self.scratch_register0, addr);
                }
            }
        }

       fn emit_ldr_inst(&self, addr: LirAddress, dest: &VReg) {
            let dest_loc = self.current_allocations().get(dest).unwrap();
            match dest_loc {
                Location::Reg(register) => self.emit_ldr(register, addr),
                Location::StackSlot(ss) => {
                    self.emit_ldr(&self.scratch_register0, LirAddress::Offset(*ss));
                    self.emit_str(&self.scratch_register0, addr);
                }
            }
        } 

        fn emit_mov_inst(&self, dest: &VReg, src: &LirOperand) {
            let loc = self.current_allocations().get(dest).unwrap();
            if let Location::Reg(dest_reg) = loc {
                match src {
                    LirOperand::VReg(vreg) => {
                        let src_loc = self.current_allocations().get(vreg).unwrap();
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
                        let src_loc = self.current_allocations().get(vreg).unwrap();
                        if let Location::Reg(src_reg) = src_loc {
                            self.emit_move_reg_to_reg(&self.scratch_register0, src_reg);
                            self.emit_str(&self.scratch_register0, LirAddress::Offset(*dest_slot));
                        }
                    },
                    LirOperand::Constant(value) => {
                        self.emit_move_const_to_reg(&self.scratch_register0, *value);
                        self.emit_str(&self.scratch_register0, LirAddress::Offset(*dest_slot));
                    },
                }
            }
        }

        fn emit_add_inst(&self, dest: &VReg, lhs: &LirOperand, rhs: &LirOperand) {
            let dest_loc = self.current_allocations().get(dest).unwrap();
            match dest_loc {
                Location::Reg(register) => self.emit_add_dest_reg(register, lhs, rhs),
                Location::StackSlot(slot) => self.emit_add_dest_stack(*slot, lhs, rhs),
            }
        }

        fn emit_add_dest_reg(&self, dest_reg: &Register, lhs: &LirOperand, rhs: &LirOperand) {
            match (lhs, rhs) {
                (LirOperand::VReg(vreg1), LirOperand::VReg(vreg2)) => {
                    let src1 = self.current_allocations().get(vreg1).unwrap();
                    let src2 = self.current_allocations().get(vreg2).unwrap();
                    match (src1, src2) {
                        (Location::Reg(r1), Location::Reg(r2)) => self.emit_add_reg_reg_reg(dest_reg, r1, r2),

                        (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                        (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                            self.emit_ldr(&self.scratch_register0, LirAddress::Offset(*op_slot));
                            self.emit_add_reg_reg_reg(dest_reg, reg_op, &self.scratch_register0);
                        },

                        (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                            self.emit_ldr(&self.scratch_register0, LirAddress::Offset(*op1_slot));
                            self.emit_ldr(&self.scratch_register1, LirAddress::Offset(*op2_slot));
                            self.emit_add_reg_reg_reg(dest_reg, &self.scratch_register0, &self.scratch_register1);
                        },
                    }
                },

                (LirOperand::Constant(imm), LirOperand::VReg(vreg)) |
                (LirOperand::VReg(vreg), LirOperand::Constant(imm)) => {
                    let src1 = self.current_allocations().get(vreg).unwrap();
                    match src1 {
                        Location::Reg(r1) => self.emit_add_reg_reg_imm(dest_reg, r1, *imm),
                        Location::StackSlot(src_slot) => {
                            self.emit_ldr(&self.scratch_register0,LirAddress::Offset(*src_slot));
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
                    let src1 = self.current_allocations().get(vreg1).unwrap();
                    let src2 = self.current_allocations().get(vreg2).unwrap();
                    match (src1, src2) {
                        (Location::Reg(r1), Location::Reg(r2)) => {
                            self.emit_add_reg_reg_reg(&self.scratch_register0, r1, r2);
                            self.emit_str(&self.scratch_register0, LirAddress::Offset(dest_slot));
                        }

                        (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                        (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                            self.emit_ldr(&self.scratch_register0, LirAddress::Offset(*op_slot));
                            self.emit_add_reg_reg_reg(&self.scratch_register1, reg_op, &self.scratch_register0);
                            self.emit_str(&self.scratch_register1, LirAddress::Offset(dest_slot));
                        },

                        (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                            self.emit_ldr(&self.scratch_register0, LirAddress::Offset(*op1_slot));
                            self.emit_ldr(&self.scratch_register1, LirAddress::Offset(*op2_slot));
                            self.emit_add_reg_reg_reg(&self.scratch_register0, &self.scratch_register0, &self.scratch_register1);
                            self.emit_str(&self.scratch_register0, LirAddress::Offset(dest_slot));
                        },
                    }
                },

                (LirOperand::Constant(imm), LirOperand::VReg(vreg)) |
                (LirOperand::VReg(vreg), LirOperand::Constant(imm)) => {
                    let src1 = self.current_allocations().get(vreg).unwrap();
                    match src1 {
                        Location::Reg(r1) => {
                            self.emit_add_reg_reg_imm(&self.scratch_register0, r1, *imm);
                            self.emit_str(&self.scratch_register0, LirAddress::Offset(dest_slot));
                        }
                        Location::StackSlot(src_slot) => {
                            self.emit_ldr(&self.scratch_register0, LirAddress::Offset(*src_slot));
                            self.emit_add_reg_reg_imm(&self.scratch_register0, &self.scratch_register0, *imm);
                            self.emit_str(&self.scratch_register0, LirAddress::Offset(dest_slot));
                        },
                    }
                },
                (LirOperand::Constant(imm1), LirOperand::Constant(imm2)) => {
                    self.emit_move_const_to_reg(&self.scratch_register0, *imm1 + *imm2);
                    self.emit_str(&self.scratch_register0, LirAddress::Offset(dest_slot));
                },
            }
        }

        fn emit_ldr(&self, r1: &Register, addr: LirAddress) {
            match addr {
                LirAddress::Offset(off) => {
                    println!("ldr {d}, [sp, #{s}]", d = r1.name, s = off * 8);
                },
                LirAddress::BaseOffset(vreg, off) => {
                    let loc = self.current_allocations().get(&vreg).unwrap();
                    match loc {
                        Location::Reg(register) => {
                            println!("ldr {d}, [{b}, #{o}]", d = r1.name, b = register.name, o = off * 8);
                        },
                        Location::StackSlot(_) => todo!(),
                    }
                },
            }
        }

        fn emit_str(&self, r1: &Register, addr: LirAddress) {
            match addr {
                LirAddress::Offset(off) => {
                    println!("str {d}, [sp, #{s}]", d = r1.name, s = off * 8);
                },
                LirAddress::BaseOffset(vreg, off) => {
                    let loc = self.current_allocations().get(&vreg).unwrap();
                    match loc {
                        Location::Reg(register) => {
                            println!("str {d}, [{b}, #{o}]", d = r1.name, b = register.name, o = off * 8);
                        },
                        Location::StackSlot(_) => todo!(),
                    }
                },
            }
        }

        fn emit_add_reg_reg_reg(&self, r1: &Register, r2: &Register, r3: &Register) {
            println!("add {d}, {a}, {b}", d = r1.name, a = r2.name, b = r3.name);
        }

        fn emit_add_reg_reg_imm(&self, r1: &Register, r2: &Register, imm: i64) {
            println!("add {d}, {a}, #{b}", d = r1.name, a = r2.name, b = imm);
        }

        fn emit_move_reg_to_reg(&self, r1: &Register, r2: &Register) {
            println!("mov {d}, {s}", d = r1.name, s = r2.name);
        }

        fn emit_move_const_to_reg(&self, r1: &Register, s: i64) {
            println!("mov {d}, #{s}", d = r1.name);
        }

        fn current_allocations(&self) -> &HashMap<VReg, Location> {
            if let Some(allocs) = &self.allocations {
                allocs
            }
            else {
                panic!("No allocations found! Aborting...");
            }
        }
    }
}