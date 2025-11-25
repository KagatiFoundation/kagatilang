// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use kagc_const::pool::{ConstEntry, KagcConst};
use kagc_ctx::CompilerCtx;
use kagc_lir::block::LirTerminator;
use kagc_lir::instruction::{LirAddress, LirInstruction};
use kagc_lir::operand::LirOperand;
use kagc_lir::function::LirFunction;
use kagc_lir::block::LirBasicBlock;
use kagc_lir::vreg::VReg;
use kagc_mir::block::{BlockId, INVALID_BLOCK_ID};
use kagc_mir::instruction::{IRCondition, StackSlotId};
use kagc_symbol::StorageClass;
use kagc_utils::bug;

use crate::regalloc::register::aarch64::{self, standard_aarch64_register_file};
use crate::regalloc::{Location, LinearScanAllocator};
use crate::regalloc::register::{RegClass, Register};
use crate::{CodeGenerator, OffsetGenerator};

use lazy_static::lazy_static;

// Aarch64 scratch registers
lazy_static! {
    static ref SCRATCH_REGISTER_0: Register = Register {
        id: 0x9,
        name: String::from("x9"),
        class: RegClass::GPR
    };

    static ref SCRATCH_REGISTER_1: Register = Register {
        id: 0x9,
        name: String::from("x10"),
        class: RegClass::GPR
    };
}

pub struct Aarch64CodeGenerator {
    allocations: Option<HashMap<VReg, Location>>,
    function_entry_block: Option<BlockId>,
    compiler_cx: Rc<RefCell<CompilerCtx>>,
    offset_generator: OffsetGenerator,
    code: String
}

impl Default for Aarch64CodeGenerator {
    fn default() -> Self {
        Self { 
            allocations: None,
            function_entry_block: None,
            compiler_cx: Rc::new(RefCell::new(CompilerCtx::default())),
            code: String::new(),
            offset_generator: OffsetGenerator::new(8) // 8-byte offset generator
        }
    }
}

impl Aarch64CodeGenerator {
    pub fn new(ccx: Rc<RefCell<CompilerCtx>>) -> Self {
        Self { 
            allocations: None,
            function_entry_block: None,
            compiler_cx: ccx,
            code: String::new(),
            offset_generator: OffsetGenerator::new(8) // 8-byte offset generator
        }
    }
}

impl CodeGenerator for Aarch64CodeGenerator {
    fn gen_function(&mut self, lir_func: &LirFunction) {
        if lir_func.signature.class == StorageClass::EXTERN {
            self.emit_raw_code(&format!(".extern _{fn_name}\n", fn_name = lir_func.name));
            return;
        }

        let mut allocator = LinearScanAllocator::new(standard_aarch64_register_file());
        let allocs = allocator.allocate(&mut lir_func.compute_vreg_live_ranges()[..]);

        let vreg_mappings: HashMap<VReg, Location> = allocs.allocations.into_iter().map(|a| (a.vreg, a.location)).collect();
        self.allocations = Some(vreg_mappings);
        self.function_entry_block = Some(lir_func.entry_block);

        // manage function's stack
        self.emit_function_preamble(lir_func);

        let mut block_ids: Vec<_> = lir_func.blocks.keys().cloned().collect();
        block_ids.sort_by_key(|b| b.0);

        // function's body
        for bid in block_ids {
            if bid == lir_func.exit_block || bid == INVALID_BLOCK_ID {
                continue;
            }
            let block = &lir_func.blocks[&bid];
            self.gen_block(block);
        }

        // dump the globals
        self.dump_globals();
        // return from the function
        self.emit_function_postamble(lir_func);
        println!("{code}", code = self.code);
    }

    fn gen_block(&mut self, block: &LirBasicBlock) {
        if let Some(entry_block) = self.function_entry_block {
            if entry_block != block.id {
                self.emit_label(block.id.0);
            }
        }
        else {
            panic!("No entry block set for function! Aborting...");
        }

        for instr in block.instructions.iter() {
            match instr {
                LirInstruction::Mov { dest, src } => self.emit_mov_inst(dest, src),
                LirInstruction::Add { dest, lhs, rhs } => self.emit_add_inst(dest, lhs, rhs),
                LirInstruction::Store { src, dest } => self.emit_str_inst(src, *dest),
                LirInstruction::Load { src, dest } => self.emit_ldr_inst(*src, dest),
                LirInstruction::CJump { lhs, rhs, .. } => self.emit_cjump_inst(lhs, rhs),
                LirInstruction::Call { func, args, result } => self.emit_call_inst(func, args, result),
                LirInstruction::MemAlloc { ob_size, ob_type, pool_idx, dest, base_ptr_slot, data_ptr_slot } => self.emit_memory_alloc_inst(ob_size, ob_type, *pool_idx, dest, *base_ptr_slot, *data_ptr_slot),
                _ => panic!()
            }
        }

        match block.terminator {
            LirTerminator::Jump(block_id) => self.code.push_str(&format!("b _L{}", block_id.0)),
            LirTerminator::Return { value, target } => {
                if let Some(ret_val_vreg) = value {
                    let src_loc = self.current_allocations().get(&ret_val_vreg).unwrap();
                    match src_loc {
                        Location::Reg(r1) => self.code.push_str(&format!("mov x0, {s}\nb _L{d}", s = r1.name, d = target.0)),
                        Location::StackSlot(ss) => {
                            self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(*ss));
                            self.code.push_str(&format!("mov x0, {s}\nb _L{d}", s = SCRATCH_REGISTER_0.name, d = target.0));
                        }
                    }
                }
            },
            LirTerminator::CJump { cond, then_block, else_block } => {
                let cmp_code = match cond {
                    IRCondition::EqEq   => "b.eq",
                    IRCondition::NEq    => "b.ne",
                    IRCondition::GTEq   => "b.ge",
                    IRCondition::LTEq   => "b.le",
                    IRCondition::GThan  => "b.gt",
                    IRCondition::LThan  => "b.lt",
                };
                self.emit_raw_code(&format!("{cmp_code} _L{bid}", bid = then_block.0));
                self.emit_raw_code(&format!("b _L{bid}", bid = else_block.0));
            },
        }
    }

    fn gen_instruction(&mut self, _: &LirInstruction) {
        unimplemented!()
    }
}

impl Aarch64CodeGenerator {
    /// Align the given address into an address divisible by 16.
    fn align_to_16(value: usize) -> usize {
        (value + 16 - 1) & !15
    }

    fn emit_raw_code(&mut self, code: &str) {
        self.code.push_str(code);
    }

    fn emit_memory_alloc_inst(
        &mut self, 
        ob_size: &LirOperand, 
        ob_type: &LirOperand, 
        pool_idx: usize, 
        dest: &VReg,
        base_ptr_slot: StackSlotId,
        data_ptr_slot: StackSlotId
    ) {
        let object_size_const = match (ob_size, ob_type) {
            (LirOperand::Constant(size_imm), LirOperand::Constant(type_imm)) => {
                self.emit_raw_code(&format!("mov {r}, #{s}\n", r = aarch64::ABI_ARG_REGISTERS_64_BIT[0], s = *size_imm));
                self.emit_raw_code(&format!("mov {r}, #{s}\n", r = aarch64::ABI_ARG_REGISTERS_64_BIT[1], s = *type_imm));
                self.emit_raw_code("bl _object_new\n"); // call allocation function
                *size_imm
            },
            _ => bug!("object's size and type must be constant values")
        };

        let obj_base_addr = self.offset_generator.next(base_ptr_slot);
        let obj_data_addr = self.offset_generator.next(data_ptr_slot);
        self.emit_raw_code(&format!("str x0, [sp, #{off}]\n", off = obj_base_addr));
        self.emit_raw_code(&format!("ldr {d}, [x0, #40]\nmov x0, {d}\n", d = SCRATCH_REGISTER_0.name));
        self.emit_raw_code(&format!("str x0, [sp, #{obj_data_addr}]\n"));

        let c_item = {
            let cx = self.compiler_cx.borrow();
            cx.const_pool.get(pool_idx).cloned()
        };

        if let Some(c_item) = c_item {
            match &c_item.value {
                KagcConst::Str(_) => self.emit_str_mem_alloc_inst(pool_idx),
                KagcConst::Record(rec_value) => self.emit_rec_mem_alloc_inst(c_item.origin_func, &rec_value.alias),
                _ => bug!("doesn't match any const entry type")
            }
        }
        else {
            bug!("cannot find const entry");
        }

        self.emit_raw_code(&format!("mov x1, {d}\n", d = SCRATCH_REGISTER_0.name));
        self.emit_raw_code(&format!("mov x2, #{object_size_const}\nbl _object_copy\n"));

        let loc = self.current_allocations().get(dest).unwrap_or_else(|| bug!("cannot find VReg {dest:#?}")).clone();
        match loc {
            // Location::Reg(register) => {
                // let base_addr = self.offset_generator.get_offset_unchecked(base_ptr_slot);
                // self.code.push_str(&format!("ldr 1st call {d}, [sp, #{base_addr}]\n", d = register.name));
            // },
            Location::StackSlot(ss) => {
                let off_addr = self.offset_generator.get_or_create_offset(ss);
                self.emit_raw_code(&format!("str x0, [sp, #{off_addr}]\n"));
            },
            _ => ()
        }
    }

    fn emit_str_mem_alloc_inst(&mut self, str_id: usize) {
        self.emit_raw_code(&format!("ADRP {d}, .L.str.{str_id}@PAGE\n", d = SCRATCH_REGISTER_0.name));
        self.emit_raw_code(&format!("ADD {d}, {d}, .L.str.{str_id}@PAGEOFF\n", d = SCRATCH_REGISTER_0.name));
    }

    fn emit_rec_mem_alloc_inst(&mut self, origin_func: Option<usize>, rec_alias: &str) {
        self.emit_raw_code(
            &format!(
                "ADRP {d}, .L__const.{}.{}@PAGE\n", 
                origin_func.unwrap(), 
                rec_alias,
                d = SCRATCH_REGISTER_0.name
            )
        );
        self.emit_raw_code(
            &format!(
                "ADD {d}, {d}, .L__const.{}.{}@PAGEOFF\n", 
                origin_func.unwrap(), 
                rec_alias,
                d = SCRATCH_REGISTER_0.name
            )
        );
    }

    fn emit_call_inst(&mut self, func: &str, args: &[VReg], _result: &Option<VReg>) {
        if args.len() > 8 {
            bug!("compiler doesn't support more than 8 arguments");
        }
        for (idx, arg) in args.iter().enumerate() {
            let loc = self.current_allocations().get(arg).unwrap_or_else(|| bug!("bug"));
            match loc {
                Location::Reg(register) => {
                    self.emit_raw_code(
                        &format!(
                            "mov {reg}, {val}\n", 
                            reg = aarch64::ABI_ARG_REGISTERS_64_BIT[idx],
                            val = register.name
                        )
                    );
                },
                Location::StackSlot(slot) => {
                    self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(*slot));
                    self.emit_raw_code(
                        &format!(
                            "mov {reg}, {val}\n",
                            reg = aarch64::ABI_ARG_REGISTERS_64_BIT[idx],
                            val = SCRATCH_REGISTER_0.name
                        )
                    );
                },
            }
        }
        self.emit_raw_code(&format!("bl _{func}\n"));
    }

    fn emit_function_preamble(&mut self, lir_func: &LirFunction) {
        let mut output = format!(".global _{name}\n_{name}:\n", name = lir_func.name);
        let stack_size = Self::align_to_16(16);
        assert_eq!(stack_size, 16);

        if lir_func.is_leaf {
            if stack_size > 0 {
                output.push_str(&format!("sub sp, sp, #{stack_size}\n"));
            }
        } else {
            output.push_str(&format!("sub sp, sp, #{stack_size}\n"));
            output.push_str(&format!("stp x29, x30, [sp, #{}]\n", stack_size - 16));
            output.push_str(&format!("add x29, sp, {}\n", stack_size - 16));
        }
        self.code.push_str(&output);
    }

    fn emit_function_postamble(&self, lir_func: &LirFunction) {
        let mut output = String::new();
        output.push_str(&format!("_L{lbl}:\n", lbl = lir_func.exit_block.0));

        let stack_size = Self::align_to_16(0);
        if lir_func.is_leaf {
            if stack_size > 0 {
                output.push_str(&format!("add sp, sp, #{stack_size}\n"));
            }
        } else {
            if stack_size > 0 {
                output.push_str(&format!("add sp, sp, #{stack_size}\n"));
            }
            output.push_str("ldp x29, x30, [sp], #16\n");
        }
        output.push_str("ret\n");
    }

    fn emit_cjump_inst(&mut self, lhs: &LirOperand, rhs: &LirOperand) {
        match (lhs, rhs) {
            (LirOperand::VReg(vreg1), LirOperand::VReg(vreg2)) => {
                let src1 = self.current_allocations().get(vreg1).unwrap().clone();
                let src2 = self.current_allocations().get(vreg2).unwrap().clone();
                match (src1, src2) {
                    (Location::Reg(r1), Location::Reg(r2)) => self.emit_cmp_reg_reg(&r1, &r2),

                    (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                    (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(op_slot));
                        self.emit_cmp_reg_reg(&reg_op, &SCRATCH_REGISTER_0);
                    },

                    (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(op1_slot));
                        self.emit_ldr(&SCRATCH_REGISTER_1, LirAddress::Offset(op2_slot));
                        self.emit_cmp_reg_reg(&SCRATCH_REGISTER_0, &SCRATCH_REGISTER_1);
                    },
                } 
            },

            (LirOperand::VReg(vreg), LirOperand::Constant(imm)) |
            (LirOperand::Constant(imm), LirOperand::VReg(vreg)) => {
                let src1 = self.current_allocations().get(vreg).unwrap().clone();
                match src1 {
                    Location::Reg(register) => self.emit_cmp_reg_imm(&register, *imm),
                    Location::StackSlot(src_slot) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(src_slot));
                        self.emit_cmp_reg_imm(&SCRATCH_REGISTER_0, *imm);
                    },
                }
            },

            (LirOperand::Constant(imm1), LirOperand::Constant(imm2)) => {
                self.emit_move_const_to_reg(&SCRATCH_REGISTER_0, *imm1);
                self.emit_move_const_to_reg(&SCRATCH_REGISTER_1, *imm2);
                self.emit_cmp_reg_reg(&SCRATCH_REGISTER_0, &SCRATCH_REGISTER_1);
            },
        }
    }

    fn emit_cmp_reg_reg(&mut self, r1: &Register, r2: &Register) {
        self.code.push_str(&format!("cmp {a}, {b}\n", a = r1.name, b = r2.name));
    }

    fn emit_cmp_reg_imm(&mut self, r1: &Register, imm: i64) {
        self.code.push_str(&format!("cmp {a}, #{imm}\n", a = r1.name));
    }

    fn emit_label(&mut self, id: usize) {
        self.code.push_str(&format!("_L{id}:\n"));
    }

    fn emit_str_inst(&mut self, src: &VReg, addr: LirAddress) {
        let src_loc = self.current_allocations().get(src).unwrap().clone();
        match src_loc {
            Location::Reg(register) => self.emit_str(&register, addr),
            Location::StackSlot(ss) => {
                self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(ss));
                self.emit_str(&SCRATCH_REGISTER_0, addr);
            }
        }
    }

    fn emit_ldr_inst(&mut self, addr: LirAddress, dest: &VReg) {
        let dest_loc = self.current_allocations().get(dest).unwrap().clone();
        match dest_loc {
            Location::Reg(register) => self.emit_ldr(&register, addr),
            Location::StackSlot(ss) => {
                self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(ss));
                self.emit_str(&SCRATCH_REGISTER_0, addr);
            }
        }
    } 

    fn emit_mov_inst(&mut self, dest: &VReg, src: &LirOperand) {
        let loc = self.current_allocations().get(dest).unwrap().clone();
        if let Location::Reg(dest_reg) = loc {
            match src {
                LirOperand::VReg(vreg) => {
                    let src_loc = self.current_allocations().get(vreg).unwrap().clone();
                    if let Location::Reg(src_reg) = src_loc {
                        self.emit_move_reg_to_reg(&dest_reg, &src_reg);
                    }
                },
                LirOperand::Constant(value) => {
                    self.emit_move_const_to_reg(&dest_reg, *value);
                },
            }
        }
        else if let Location::StackSlot(dest_slot) = loc {
            match src {
                LirOperand::VReg(vreg) => {
                    let src_loc = self.current_allocations().get(vreg).unwrap().clone();
                    if let Location::Reg(src_reg) = src_loc {
                        self.emit_move_reg_to_reg(&SCRATCH_REGISTER_0, &src_reg);
                        self.emit_str(&SCRATCH_REGISTER_0, LirAddress::Offset(dest_slot));
                    }
                },
                LirOperand::Constant(value) => {
                    self.emit_move_const_to_reg(&SCRATCH_REGISTER_0, *value);
                    self.emit_str(&SCRATCH_REGISTER_0, LirAddress::Offset(dest_slot));
                },
            }
        }
    }

    fn emit_add_inst(&mut self, dest: &VReg, lhs: &LirOperand, rhs: &LirOperand) {
        let dest_loc = self.current_allocations().get(dest).unwrap().clone();
        match dest_loc {
            Location::Reg(register) => self.emit_add_dest_reg(&register, lhs, rhs),
            Location::StackSlot(slot) => self.emit_add_dest_stack(slot, lhs, rhs),
        }
    }

    fn emit_add_dest_reg(&mut self, dest_reg: &Register, lhs: &LirOperand, rhs: &LirOperand) {
        match (lhs, rhs) {
            (LirOperand::VReg(vreg1), LirOperand::VReg(vreg2)) => {
                let src1 = self.current_allocations().get(vreg1).unwrap().clone();
                let src2 = self.current_allocations().get(vreg2).unwrap().clone();
                match (src1, src2) {
                    (Location::Reg(r1), Location::Reg(r2)) => self.emit_add_reg_reg_reg(dest_reg, &r1, &r2),

                    (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                    (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(op_slot));
                        self.emit_add_reg_reg_reg(dest_reg, &reg_op, &SCRATCH_REGISTER_0);
                    },

                    (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(op1_slot));
                        self.emit_ldr(&SCRATCH_REGISTER_1, LirAddress::Offset(op2_slot));
                        self.emit_add_reg_reg_reg(dest_reg, &SCRATCH_REGISTER_0, &SCRATCH_REGISTER_1);
                    },
                }
            },

            (LirOperand::Constant(imm), LirOperand::VReg(vreg)) |
            (LirOperand::VReg(vreg), LirOperand::Constant(imm)) => {
                let src1 = self.current_allocations().get(vreg).unwrap().clone();
                match src1 {
                    Location::Reg(r1) => self.emit_add_reg_reg_imm(dest_reg, &r1, *imm),
                    Location::StackSlot(src_slot) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(src_slot));
                        self.emit_add_reg_reg_imm(dest_reg, &SCRATCH_REGISTER_0, *imm);
                    },
                }
            },

            (LirOperand::Constant(imm1), LirOperand::Constant(imm2)) => {
                self.emit_move_const_to_reg(dest_reg, *imm1 + *imm2);
            },
        }
    }

    fn emit_add_dest_stack(&mut self, dest_slot: StackSlotId, lhs: &LirOperand, rhs: &LirOperand) {
        match (lhs, rhs) {
            (LirOperand::VReg(vreg1), LirOperand::VReg(vreg2)) => {
                let src1 = self.current_allocations().get(vreg1).unwrap().clone();
                let src2 = self.current_allocations().get(vreg2).unwrap().clone();
                match (src1, src2) {
                    (Location::Reg(r1), Location::Reg(r2)) => {
                        self.emit_add_reg_reg_reg(&SCRATCH_REGISTER_0, &r1, &r2);
                        self.emit_str(&SCRATCH_REGISTER_0, LirAddress::Offset(dest_slot));
                    }

                    (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                    (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(op_slot));
                        self.emit_add_reg_reg_reg(&SCRATCH_REGISTER_1, &reg_op, &SCRATCH_REGISTER_0);
                        self.emit_str(&SCRATCH_REGISTER_1, LirAddress::Offset(dest_slot));
                    },

                    (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(op1_slot));
                        self.emit_ldr(&SCRATCH_REGISTER_1, LirAddress::Offset(op2_slot));
                        self.emit_add_reg_reg_reg(&SCRATCH_REGISTER_0, &SCRATCH_REGISTER_0, &SCRATCH_REGISTER_1);
                        self.emit_str(&SCRATCH_REGISTER_0, LirAddress::Offset(dest_slot));
                    },
                }
            },

            (LirOperand::Constant(imm), LirOperand::VReg(vreg)) |
            (LirOperand::VReg(vreg), LirOperand::Constant(imm)) => {
                let src1 = self.current_allocations().get(vreg).unwrap().clone();
                match src1 {
                    Location::Reg(r1) => {
                        self.emit_add_reg_reg_imm(&SCRATCH_REGISTER_0, &r1, *imm);
                        self.emit_str(&SCRATCH_REGISTER_0, LirAddress::Offset(dest_slot));
                    }
                    Location::StackSlot(src_slot) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(src_slot));
                        self.emit_add_reg_reg_imm(&SCRATCH_REGISTER_0, &SCRATCH_REGISTER_0, *imm);
                        self.emit_str(&SCRATCH_REGISTER_0, LirAddress::Offset(dest_slot));
                    },
                }
            },
            (LirOperand::Constant(imm1), LirOperand::Constant(imm2)) => {
                self.emit_move_const_to_reg(&SCRATCH_REGISTER_0, *imm1 + *imm2);
                self.emit_str(&SCRATCH_REGISTER_0, LirAddress::Offset(dest_slot));
            },
        }
    }

    fn emit_ldr(&mut self, r1: &Register, addr: LirAddress) {
        match addr {
            LirAddress::Offset(off) => {
                self.code.push_str(&format!("ldr {d}, [sp, #{s}]\n", d = r1.name, s = off.0));
            },
            LirAddress::BaseOffset(vreg, off) => {
                let loc = self.current_allocations().get(&vreg).unwrap();
                match loc {
                    Location::Reg(register) => {
                        self.code.push_str(&format!("ldr {d}, [{b}, #{o}]\n", d = r1.name, b = register.name, o = off.0));
                    },
                    Location::StackSlot(_) => todo!(),
                }
            },
        }
    }

    fn emit_str(&mut self, r1: &Register, addr: LirAddress) {
        match addr {
            LirAddress::Offset(off) => {
                self.code.push_str(&format!("str {d}, [sp, #{s}]\n", d = r1.name, s = off.0 * 8));
            },
            LirAddress::BaseOffset(vreg, off) => {
                let loc = self.current_allocations().get(&vreg).unwrap();
                match loc {
                    Location::Reg(register) => {
                        self.code.push_str(&format!("str {d}, [{b}, #{o}]\n", d = r1.name, b = register.name, o = off.0 * 8));
                    },
                    Location::StackSlot(_) => todo!(),
                }
            },
        }
    }

    fn emit_add_reg_reg_reg(&mut self, r1: &Register, r2: &Register, r3: &Register) {
        self.code.push_str(&format!("add {d}, {a}, {b}\n", d = r1.name, a = r2.name, b = r3.name));
    }

    fn emit_add_reg_reg_imm(&mut self, r1: &Register, r2: &Register, imm: i64) {
        self.code.push_str(&format!("add {d}, {a}, #{b}\n", d = r1.name, a = r2.name, b = imm));
    }

    fn emit_move_reg_to_reg(&mut self, r1: &Register, r2: &Register) {
        self.code.push_str(&format!("mov {d}, {s}\n", d = r1.name, s = r2.name));
    }

    fn emit_move_const_to_reg(&mut self, r1: &Register, s: i64) {
        self.code.push_str(&format!("mov {d}, #{s}\n", d = r1.name));
    }

    /// This function is public only for a short period of time.
    pub fn dump_globals(&mut self) {
        if self.compiler_cx.borrow().const_pool.is_empty() {
            return;
        }

        let mut output_str: String = String::new();
        for (index, c_item) in self.compiler_cx.borrow().const_pool.iter_enumerated() {
            output_str.push_str(&self.dump_const(index, c_item, false));
        }
        self.code.push_str(&output_str);
    }

    fn dump_const(&self, c_item_index: usize, c_item: &ConstEntry, parent_is_record: bool) -> String {
        let mut output_str = String::new();
        if let KagcConst::Str(str_value) = &c_item.value {
            if parent_is_record {
                output_str.push_str(&format!("\t.xword .L.str.{c_item_index}\n"));
            }
            else {
                output_str.push_str(
                    &format!(
                        ".section __TEXT,__cstring\n\t.L.str.{}:\n\t.asciz \"{}\"\n", 
                        c_item_index, 
                        str_value
                    )
                );
            }
        }
        else if let KagcConst::Int(int_value) = &c_item.value {
            if parent_is_record {
                // output_str.push_str(&format!("\t.word {int_value}\n\t.zero 4\n"));
                output_str.push_str(&format!("\t.xword {int_value}\n"));
            }
        }
        else if let KagcConst::Record(rec) = &c_item.value {
            output_str.push_str(
                &format!(
                    ".section __DATA,__const\n.align {}\n.L__const.{}.{}:\n", 
                    rec.alignment, 
                    c_item.origin_func.unwrap(), 
                    rec.alias.clone()
                )
            );
            for rec_field in &rec.fields {
                if let Some(rec_pool_item) = self.compiler_cx.borrow().const_pool.get(*rec_field.1) {
                    output_str.push_str(&self.dump_const(*rec_field.1, rec_pool_item, true));
                }
            }
        }
        output_str
    }

    fn current_allocations(&self) -> &HashMap<VReg, Location> {
        if let Some(allocs) = &self.allocations {
            allocs
        }
        else {
            bug!("no allocations found");
        }
    }
}