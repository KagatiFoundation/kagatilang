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
use kagc_mir::builtin::BuiltinFn;
use kagc_mir::function::FunctionId;
use kagc_mir::instruction::{IRCondition, StackSlotId};
use kagc_mir::module::MirModule;
use kagc_mir_lowering::MirToLirLowerer;
use kagc_symbol::StorageClass;
use kagc_utils::bug;

use crate::codegen_asm::fn_state::CurrentFunctionState;
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
    current_function_state: Option<CurrentFunctionState>, // none indicates no function is currently being processed
    current_function_code: String
}

impl Default for Aarch64CodeGenerator {
    fn default() -> Self {
        Self { 
            allocations: None,
            function_entry_block: None,
            compiler_cx: Rc::new(RefCell::new(CompilerCtx::default())),
            current_function_code: String::new(),
            current_function_state: None,
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
            current_function_code: String::new(),
            current_function_state: None,
            offset_generator: OffsetGenerator::new(8) // 8-byte offset generator
        }
    }
}

impl CodeGenerator for Aarch64CodeGenerator {
    fn gen_function(&mut self, lir_func: &LirFunction) {
        // println!("{lir_func:#?}");
        // return;
        if lir_func.signature.class == StorageClass::EXTERN {
            println!(".extern _{fn_name}\n", fn_name = lir_func.name); // an extern function
            return;
        }

        let mut allocator = LinearScanAllocator::new(standard_aarch64_register_file());
        let allocs = allocator.allocate(&mut lir_func.compute_vreg_live_ranges()[..]);

        let vreg_mappings: HashMap<VReg, Location> = allocs.allocations.into_iter().map(|a| (a.vreg, a.location)).collect();
        self.allocations = Some(vreg_mappings);
        self.function_entry_block = Some(lir_func.entry_block);

        // current function's metadata
        let is_leaf = self.is_function_leaf(lir_func);
        let stack_size = Self::align_to_16(self.calculate_function_stack_size(lir_func, is_leaf));
        self.current_function_state = Some(CurrentFunctionState {
            is_leaf,
            id: lir_func.id,
            computed_stack_size: stack_size
        });

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
        // return from the function
        self.emit_function_postamble(lir_func);
        println!("{code}", code = self.current_function_code);
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
                LirInstruction::Mov         { dest, src } => self.emit_mov_inst(dest, src),
                LirInstruction::Add         { dest, lhs, rhs } => self.emit_add_inst(dest, lhs, rhs),
                LirInstruction::Store       { src, dest } => self.emit_str_inst(src, *dest),
                LirInstruction::Load        { src, dest } => self.emit_ldr_inst(*src, dest),
                LirInstruction::CJump       { lhs, rhs, .. } => self.emit_cjump_inst(lhs, rhs),
                LirInstruction::Call        { func, args, result } => self.emit_call_inst(func, args, result),
                LirInstruction::CallBuiltin { builtin, args, result } => self.emit_builtin_fn_call_inst(*builtin, args, result),
                LirInstruction::MemAlloc    { ob_size, pool_idx, base_ptr_slot, .. } => self.emit_memory_alloc_inst(ob_size, *pool_idx, *base_ptr_slot),
                LirInstruction::LoadConst   { label_id, dest } => self.emit_load_const(*label_id, dest),
                _ => panic!()
            }
        }

        match block.terminator {
            LirTerminator::Jump(block_id) => self.current_function_code.push_str(&format!("b _L{}\n", block_id.0)),
            LirTerminator::Return { value, target } => {
                if let Some(ret_val_vreg) = value {
                    let src_loc = self.current_allocations().get(&ret_val_vreg).unwrap();
                    match src_loc {
                        Location::Reg(r1) => self.current_function_code.push_str(&format!("mov x0, {s}\nb _L{d}\n", s = r1.name, d = target.0)),
                        Location::StackSlot(ss) => {
                            self.emit_ldr(&SCRATCH_REGISTER_0, LirAddress::Offset(*ss));
                            self.current_function_code.push_str(&format!("mov x0, {s}\nb _L{d}\n", s = SCRATCH_REGISTER_0.name, d = target.0));
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
                self.emit_raw_code(&format!("{cmp_code} _L{bid}\n", bid = then_block.0));
                self.emit_raw_code(&format!("b _L{bid}\n", bid = else_block.0));
            },
        }
        // self.emit_raw_code("***** BLOCK END *****\n");
    }

    fn gen_instruction(&mut self, _: &LirInstruction) {
        unimplemented!()
    }
}

impl Aarch64CodeGenerator {
    pub fn generate_module_code(&mut self, module: &MirModule) {
        let mut module_funcs: Vec<FunctionId> = module.functions.keys().cloned().collect();
        let mut mir_lowerer = MirToLirLowerer::default();
        module_funcs.sort_by_key(|fid| fid.0);
        for func_id in module_funcs {
            let func = &module.functions[&func_id];
            let func_lowered = mir_lowerer.lower_function(func);
            self.gen_function(&func_lowered); 

            // reset current function
            self.current_function_code = String::new();
            self.offset_generator.next_off = 0;
            self.current_function_state = None;
        }
    }

    /// Align the given address into an address divisible by 16.
    fn align_to_16(value: usize) -> usize {
        (value + 16 - 1) & !15
    }

    fn emit_raw_code(&mut self, code: &str) {
        self.current_function_code.push_str(code);
    }

    fn emit_memory_alloc_inst(
        &mut self, 
        ob_size: &LirOperand, 
        pool_idx: usize, 
        base_ptr_slot: StackSlotId
    ) {
        let c_item = {
            let cx = self.compiler_cx.borrow();
            cx.const_pool.get(pool_idx).cloned()
        };
        let ob_size = match ob_size {
            LirOperand::Constant(imm) => *imm,
            _ => bug!("ob_size must be a Constant value")
        } as usize;

        if let Some(c_item) = c_item {
            match &c_item.value {
                KagcConst::Str(_) => self.emit_string_mem_alloc_inst(pool_idx, ob_size),
                KagcConst::Int(_) => self.emit_integer_mem_alloc_inst(pool_idx),
                KagcConst::Record(rec_value) => self.emit_record_mem_alloc_inst(c_item.origin_func, &rec_value.alias, ob_size),
                _ => bug!("doesn't match any const entry type")
            }
        }
        else {
            bug!("cannot find const entry");
        }

        let obj_base_addr = self.offset_generator.next(base_ptr_slot);
        self.emit_store_reg_by_name("x0", obj_base_addr);
    }

    /// WARNING: Use this function with caution.
    fn emit_store_reg_by_name(&mut self, reg: &str, off: usize) {
        let is_leaf = self.get_current_fn_state().is_leaf;
        let stack_size = self.get_current_fn_state().computed_stack_size;
        let r = Register {
            class: RegClass::GPR,
            id: 0xFF,
            name: reg.to_string()
        };
        if is_leaf {
            self.emit_str_relative_sp(&r, stack_size, off);
        }
        else {
            self.emit_str_relative_fp(&r, off);
        }
    }

    /// str_id = String's constant pool index
    fn emit_string_mem_alloc_inst(&mut self, str_id: usize, size: usize) {
        self.emit_raw_code(&format!("adrp {d}, .L.__c.{str_id}@page\n", d = SCRATCH_REGISTER_0.name));
        self.emit_raw_code(&format!("add {d}, {d}, .L.__c.{str_id}@pageoff\n", d = SCRATCH_REGISTER_0.name));
        self.emit_raw_code(&format!("mov x0, {s}\nmov x1, {size}\nbl _make_rt_str\n", s = SCRATCH_REGISTER_0.name));
    }

    // integers in Kagati are 8-bytes in size
    /// int_id = Integer's constant pool index
    fn emit_integer_mem_alloc_inst(&mut self, int_id: usize) {
        self.emit_raw_code(&format!("adrp {d}, .L.__c.{int_id}@page\n", d = SCRATCH_REGISTER_0.name));
        self.emit_raw_code(&format!("add {d}, {d}, .L.__c.{int_id}@pageoff\n", d = SCRATCH_REGISTER_0.name));
        self.emit_raw_code(&format!("mov x0, {s}\nbl _make_rt_int\n", s = SCRATCH_REGISTER_0.name));
    }

    fn emit_record_mem_alloc_inst(&mut self, origin_func: Option<usize>, rec_alias: &str, size: usize) {
        self.emit_raw_code(
            &format!(
                "adrp {d}, .L.__c.{}.{}@page\n", 
                origin_func.unwrap(), 
                rec_alias,
                d = SCRATCH_REGISTER_0.name
            )
        );
        self.emit_raw_code(
            &format!(
                "add {d}, {d}, .L.__c.{}.{}@pageoff\n", 
                origin_func.unwrap(), 
                rec_alias,
                d = SCRATCH_REGISTER_0.name
            )
        );
        self.emit_raw_code(&format!("mov x0, {s1}\nmov x1, #{size}\nbl _make_rt_rec\n", s1 = SCRATCH_REGISTER_0.name));
    }

    fn emit_call_inst(&mut self, func: &str, args: &[VReg], result: &Option<VReg>) {
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

        if let Some(result_pos) = result {
            let loc = self.current_allocations().get(result_pos).unwrap();
            match loc {
                Location::Reg(register) => {
                    self.emit_raw_code(&format!("mov {d}, x0\n", d = register.name));
                },
                Location::StackSlot(stack_slot_id) => {
                    let call_value_stack_off = self.offset_generator.get_or_create_offset(*stack_slot_id);
                    self.emit_store_reg_by_name("x0", call_value_stack_off);
                },
            }
        }
    }

    fn emit_builtin_fn_call_inst(&mut self, builtin: BuiltinFn, args: &[VReg], result: &Option<VReg>) {
        let fn_name = match builtin {
            BuiltinFn::AssignRef => "assign_ref",
            BuiltinFn::AllocInt => "make_rt_int",
            BuiltinFn::AllocStr => "make_rt_str",
            BuiltinFn::AllocRec => "make_rt_rec",
            BuiltinFn::Panic => "rt_panic",
        };
        self.emit_call_inst(fn_name, args, result);
    }

    fn calculate_function_stack_size(&self, lir_func: &LirFunction, is_leaf: bool) -> usize {
        let mut final_stack_size = 0;
        // non-leaf functions call other functions and thus 
        // the x29 and x30 registers must be saved on the stack
        if !is_leaf {
            final_stack_size += 16;
        }

        final_stack_size += lir_func.signature.params.len() * 8; // each parameter is going take 8-bytes of space

        for block in lir_func.blocks.values() {
            for instr in &block.instructions {
                // store instructions take space
                // NOTE: machine's CPU word size is not considered at the moment.
                if let LirInstruction::Store { .. } = instr {
                    final_stack_size += 8;
                }
                else if let LirInstruction::MemAlloc { .. } = instr {
                    // each memory allocation instruction takes 16-bytes of memory
                    final_stack_size += 8; 
                }
            }
        }
        final_stack_size
    }

    fn emit_function_preamble(&mut self, lir_func: &LirFunction) {
        let curr_fn_state = self.get_current_fn_state();
        let curr_func_is_leaf = curr_fn_state.is_leaf;
        let stack_size = curr_fn_state.computed_stack_size;

        self.current_function_code.push_str(&format!(".global _{name}\n_{name}:\n", name = lir_func.name));
        if curr_func_is_leaf {
            if stack_size > 0 {
                self.current_function_code.push_str(&format!("sub sp, sp, #{stack_size}\n"));
            }
        } else {
            self.current_function_code.push_str(&format!("sub sp, sp, #{stack_size}\n"));
            self.current_function_code.push_str(&format!("stp x29, x30, [sp, #{}]\n", stack_size - 16));
            self.current_function_code.push_str(&format!("add x29, sp, #{}\n", stack_size - 16));
        }

        for (reg_counter, param) in lir_func.signature.params.iter().enumerate() {
            let param_addr = self.offset_generator.next(param.stack_slot);
            // self.current_function_code.push_str(&format!("str x{reg_counter}, [sp, #{param_addr}]\n"));
            self.emit_store_reg_by_name(&format!("x{}", reg_counter), param_addr);
        }
    }

    fn emit_function_postamble(&mut self, lir_func: &LirFunction) {
        let curr_func_is_leaf = self.get_current_fn_state().is_leaf;
        let stack_size = self.get_current_fn_state().computed_stack_size;
        self.current_function_code.push_str(&format!("_L{lbl}:\n", lbl = lir_func.exit_block.0));

        if curr_func_is_leaf {
            if stack_size > 0 {
                self.current_function_code.push_str(&format!("add sp, sp, #{stack_size}\n"));
            }
        } else {
            self.current_function_code.push_str(&format!("ldp x29, x30, [sp, #{off}]\n", off = stack_size - 16));
            if stack_size > 0 {
                self.current_function_code.push_str(&format!("add sp, sp, #{stack_size}\n"));
            }
        }
        self.current_function_code.push_str("ret\n");
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
        self.current_function_code.push_str(&format!("cmp {a}, {b}\n", a = r1.name, b = r2.name));
    }

    fn emit_cmp_reg_imm(&mut self, r1: &Register, imm: i64) {
        self.current_function_code.push_str(&format!("cmp {a}, #{imm}\n", a = r1.name));
    }

    fn emit_label(&mut self, id: usize) {
        self.current_function_code.push_str(&format!("_L{id}:\n"));
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

    /// Spill register to stack pointer (SP)
    fn emit_str_relative_sp(&mut self, reg: &Register, stack_size: usize, off: usize) {
        let stack_off = stack_size - off;
        let dest_addr = if stack_off != stack_size { // if not at the beginning of the stack frame
            format!("[sp, #{stack_off}]")
        }
        else {
            "[sp]".to_string()
        };
        self.current_function_code.push_str(&format!("str {}, {dest_addr}\n", reg.name));
    }

    /// Load register from stack pointer (SP)
    fn emit_ldr_relative_sp(&mut self, reg: &Register, stack_size: usize, off: usize) {
        let stack_off = stack_size - off;
        let src_addr = if stack_off != stack_size { // if not at the beginning of the stack frame
            format!("[sp, #{stack_off}]")
        }
        else {
            "[sp]".to_string()
        };
        self.current_function_code.push_str(&format!("ldr {}, {src_addr}\n", reg.name));
    }

    /// Spill register to frame pointer (x29)
    fn emit_str_relative_fp(&mut self, reg: &Register, off: usize) {
        let dest_addr = if off != 0 { // if not at the beginning of the frame pointer
            format!("[x29, #-{off}]")
        }
        else {
            "[x29]".to_string()
        };
        self.current_function_code.push_str(&format!("str {}, {dest_addr}\n", reg.name));
    }

    /// Load register from frame pointer (x29)
    fn emit_ldr_relative_fp(&mut self, reg: &Register, off: usize) {
        let src_addr = if off != 0 { // if not at the beginning of the frame pointer
            format!("[x29, #-{off}]")
        }
        else {
            "[x29]".to_string()
        };
        self.current_function_code.push_str(&format!("ldr {}, {src_addr}\n", reg.name));
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
        let stack_size = self.get_current_fn_state().computed_stack_size;
        match addr {
            LirAddress::Offset(off) => {
                let addr_off = self.offset_generator.get_offset_unchecked(off);
                if self.get_current_fn_state().is_leaf {
                    self.emit_ldr_relative_sp(r1, stack_size, addr_off);
                }
                else {
                    self.emit_ldr_relative_fp(r1, addr_off);
                }
            },
            LirAddress::BaseOffset(vreg, off) => {
                let loc = self.current_allocations().get(&vreg).unwrap();
                match loc {
                    Location::Reg(register) => {
                        let addr_off = self.offset_generator.get_offset_unchecked(off);
                        self.current_function_code.push_str(&format!("ldr {d}, [{b}, #{addr_off}]\n", d = r1.name, b = register.name));
                    },
                    Location::StackSlot(_) => todo!(),
                }
            },
        }
    }

    fn emit_str(&mut self, r1: &Register, addr: LirAddress) {
        let stack_size = self.get_current_fn_state().computed_stack_size;
        match addr {
            LirAddress::Offset(off) => {
                let addr_off = self.offset_generator.next(off);
                if self.get_current_fn_state().is_leaf {
                    self.emit_str_relative_sp(r1, stack_size, addr_off);
                }
                else {
                    self.emit_str_relative_fp(r1, addr_off);
                }
            },
            LirAddress::BaseOffset(vreg, off) => {
                let addr_off = self.offset_generator.off_map.insert(off, off.0 * 8).unwrap_or_else(|| bug!("cannot create an offset"));
                let loc = self.current_allocations().get(&vreg).unwrap();
                match loc {
                    Location::Reg(register) => self.current_function_code.push_str(&format!("str {s}, [{b}, #{addr_off}]\n", s = r1.name, b = register.name)),
                    Location::StackSlot(ss) => self.emit_str(r1, LirAddress::Offset(*ss)),
                }
            },
        }
    }

    fn emit_load_const(&mut self, label_id: usize, vreg: &VReg) {
        let loc = self.current_allocations().get(vreg).unwrap().clone();
        self.emit_raw_code(&format!("adrp {d}, .L.__c.{label_id}@page\n", d = SCRATCH_REGISTER_0.name));
        self.emit_raw_code(&format!("add {d}, {d}, .L.__c.{label_id}@pageoff\n", d = SCRATCH_REGISTER_0.name));
        match loc {
            Location::Reg(register) => {
                self.emit_raw_code(
                    &format!(
                        "mov {d}, {s}\n", 
                        d = register.name, 
                        s = SCRATCH_REGISTER_0.name
                    )
                );
            }
            Location::StackSlot(stack_slot_id) => self.emit_store_reg_by_name(&SCRATCH_REGISTER_0.name, stack_slot_id.0),
        }
    }

    fn emit_add_reg_reg_reg(&mut self, r1: &Register, r2: &Register, r3: &Register) {
        self.current_function_code.push_str(&format!("add {d}, {a}, {b}\n", d = r1.name, a = r2.name, b = r3.name));
    }

    fn emit_add_reg_reg_imm(&mut self, r1: &Register, r2: &Register, imm: i64) {
        self.current_function_code.push_str(&format!("add {d}, {a}, #{b}\n", d = r1.name, a = r2.name, b = imm));
    }

    fn emit_move_reg_to_reg(&mut self, r1: &Register, r2: &Register) {
        self.current_function_code.push_str(&format!("mov {d}, {s}\n", d = r1.name, s = r2.name));
    }

    fn emit_move_const_to_reg(&mut self, r1: &Register, s: i64) {
        self.current_function_code.push_str(&format!("mov {d}, #{s}\n", d = r1.name));
    }

    /// This function is public only for a short period of time.
    pub fn dump_globals(&mut self) {
        if self.compiler_cx.borrow().const_pool.is_empty() {
            return;
        }
        let mut global_vars_code = String::new();
        for (index, c_item) in self.compiler_cx.borrow().const_pool.iter_enumerated() {
            global_vars_code.push_str(&self.dump_const(index, c_item, false));
        }
        println!("{global_vars_code}");
    }

    fn dump_const(&self, c_item_index: usize, c_item: &ConstEntry, parent_is_record: bool) -> String {
        let mut output_str = String::new();
        if let KagcConst::Str(str_value) = &c_item.value {
            if parent_is_record {
                output_str.push_str(&format!("\t.xword .L.__c.{c_item_index}\n"));
            }
            else {
                output_str.push_str(&format!(".section __TEXT,__cstring\n\t.L.__c.{c_item_index}:\n\t.asciz \"{str_value}\"\n"));
            }
        }
        else if let KagcConst::Int(int_value) = &c_item.value {
            if parent_is_record {
                // output_str.push_str(&format!("\t.word {int_value}\n\t.zero 4\n"));
                output_str.push_str(&format!("\t.xword .L.__c.{c_item_index}\n"));
            }
            else {
                output_str.push_str(&format!(".section __DATA,__const\n\t.L.__c.{c_item_index}:\n\t.xword {int_value}\n"));
            }
        }
        else if let KagcConst::Record(rec) = &c_item.value {
            if parent_is_record {
                output_str.push_str(&format!("\t.xword .L.__c.{}\n", c_item_index));
            }
            else {
                output_str.push_str(
                    &format!(
                        ".section __DATA,__const\n.align {}\n.L.__c.{}:\n", 
                        rec.alignment, 
                        c_item_index
                    )
                );
                for rec_field in &rec.fields {
                    if let Some(rec_pool_item) = self.compiler_cx.borrow().const_pool.get(*rec_field.1) {
                        output_str.push_str(&self.dump_const(*rec_field.1, rec_pool_item, true));
                    }
                    else {
                        output_str.push_str("\t.xword 0\n"); // allocate empty space for fields' pointers
                    }
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

    /// Check whether a function is leaf or not.
    /// A leaf function does not call any other functions.
    fn is_function_leaf(&self, lir_func: &LirFunction) -> bool {
        let mut is_leaf = true;
        for block in lir_func.blocks.values() {
            for instr in &block.instructions {
                is_leaf = !matches!(instr, LirInstruction::Call { .. } | LirInstruction::MemAlloc { .. });
            }
        }
        is_leaf
    }

    fn get_current_fn_state(&self) -> &CurrentFunctionState {
        self.current_function_state
            .as_ref()
            .expect("Compile time function info not found! Aborting...")
    }
}