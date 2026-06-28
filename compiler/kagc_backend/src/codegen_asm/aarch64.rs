// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_const::pool::{ConstEntry, ConstPool, KagcConst};
use kagc_mir::analyzer::LivenessAnalyzer;
use kagc_mir::block::{BlockId, IrBasicBlock, Terminator};
use kagc_mir::builtin::BuiltinFn;
use kagc_mir::function::{FunctionId, IrFunction};
use kagc_mir::instruction::{IrCondition, IrInstruction};
use kagc_mir::value::{IrAddress, IrValue, IrValueId, StackSlotId};
use kagc_symbol::StorageClass;
use kagc_utils::bug;

use crate::codegen_asm::fn_state::CurrentFunctionState;
use crate::regalloc::Location;
use crate::regalloc::linear_alloca2::LinearScanAllocator;
use crate::regalloc::register::aarch64::{self, standard_aarch64_register_file};
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

pub struct Aarch64CodeGenerator<'cg> {
    allocations: Option<HashMap<IrValueId, Location>>,
    function_entry_block: Option<BlockId>,
    const_pool: &'cg ConstPool,
    offset_generator: OffsetGenerator,
    current_function_state: Option<CurrentFunctionState>, // none indicates no function is currently being processed
    current_function_code: String
}

impl<'cg> Aarch64CodeGenerator<'cg> {
    pub fn new(const_pool: &'cg ConstPool) -> Self {
        Self { 
            allocations: None,
            function_entry_block: None,
            const_pool,
            current_function_code: String::new(),
            current_function_state: None,
            offset_generator: OffsetGenerator::new(8) // 8-byte offset generator
        }
    }
}

impl<'cg> CodeGenerator for Aarch64CodeGenerator<'cg> {
    fn gen_function(&mut self, function: &IrFunction) {
        if function.signature.class == StorageClass::EXTERN {
            println!(".extern _{fn_name}", fn_name = function.name); // an extern function
            return;
        }

        let mut allocator = LinearScanAllocator::new(standard_aarch64_register_file());

		let liveness_analyzer = LivenessAnalyzer {};
		let mut live_ranges = liveness_analyzer.compute_function_live_ranges(function);
  
        let allocs = allocator.allocate(&mut live_ranges);

        let value_id_mappings: HashMap<IrValueId, Location> = allocs
			.allocations
			.into_iter()
			.map(|a| (a.value_id, a.location))
			.collect();

        self.allocations = Some(value_id_mappings);
        self.function_entry_block = Some(function.entry_block);

        // current function's metadata
        let is_leaf = self.is_function_leaf(function);
        let stack_size = Self::align_to_16(self.calculate_function_stack_size(function, is_leaf));
        self.current_function_state = Some(CurrentFunctionState {
            is_leaf,
            computed_stack_size: stack_size as i64
        });

        // manage function's stack
        self.emit_function_preamble(function);

		self.gen_function_blocks(function);

        // return from the function
        self.emit_function_postamble(function);
        println!("{code}", code = self.current_function_code);
    }

    fn gen_block(&mut self, block: &IrBasicBlock) {
		if block.instructions.is_empty() && block.predecessors.is_empty() {
			return;
		}

        if let Some(entry_block) = self.function_entry_block {
            if entry_block != block.id {
                self.emit_label(block.id.0);
            }
        }
        else {
            panic!("No entry block set for function! Aborting...");
        }

        for instr in block.instructions.iter() {
            self.gen_instruction(instr);
        }

        match block.terminator {
            Terminator::Jump(block_id) => self.current_function_code.push_str(&format!("b _L{}\n", block_id.0)),
            Terminator::Return { value, target } => {
                if let Some(ret_val_vreg) = value {
                    let src_loc = self.current_allocations().get(&ret_val_vreg).unwrap();
                    match src_loc {
                        Location::Reg(r1) => self.current_function_code.push_str(&format!("mov x0, {s}\nb _L{d}\n", s = r1.name, d = target.0)),
                        Location::StackSlot(ss) => {
                            self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(*ss));
                            self.current_function_code.push_str(&format!("mov x0, {s}\nb _L{d}\n", s = SCRATCH_REGISTER_0.name, d = target.0));
                        }
                    }
                }
            },
            Terminator::CondJump { cond, then_block, else_block, .. } => {
                let cmp_code = match cond {
                    IrCondition::EqEq   => "b.eq",
                    IrCondition::NEq    => "b.ne",
                    IrCondition::GTEq   => "b.ge",
                    IrCondition::LTEq   => "b.le",
                    IrCondition::GThan  => "b.gt",
                    IrCondition::LThan  => "b.lt",
                };
                self.emit_raw_code(&format!("{cmp_code} _L{bid}\n", bid = then_block.0));
                self.emit_raw_code(&format!("b _L{bid}\n", bid = else_block.0));
            },
        }
    }

    fn gen_instruction(&mut self, instr: &IrInstruction) {
		match instr {
			IrInstruction::Mov         { result, src } => self.emit_mov_inst(result, src),
			IrInstruction::Add         { result, lhs, rhs } => self.emit_add_inst(result, lhs, rhs),
			IrInstruction::Store       { src, address } => self.emit_str_inst(src, *address),
			IrInstruction::Load        { src, result } => self.emit_ldr_inst(*src, result),
			IrInstruction::CondJump    { lhs, rhs, .. } => self.emit_cjump_inst(lhs, rhs),
			IrInstruction::Call        { func, args, result } => self.emit_call_inst(func, args, result),
			IrInstruction::CallBuiltin { builtin, args, result } => self.emit_builtin_fn_call_inst(*builtin, args, result),
			IrInstruction::LoadConst   { label_id, result } => self.emit_load_const(*label_id, result),
			_ => panic!()
		}
    }
}

impl<'cg> Aarch64CodeGenerator<'cg> {
	pub fn generate_code(&mut self, functions: &HashMap<FunctionId, IrFunction>) {
        let mut function_ids: Vec<FunctionId> = functions.keys().cloned().collect();
        function_ids.sort_by_key(|function_id| function_id.0);

		for function_id in function_ids {
			let function = &functions[&function_id];
			self.gen_function(function);

            self.current_function_code = String::new();
            self.offset_generator.next_off = 0;
            self.current_function_state = None;
		}
	}

	fn gen_function_blocks(&mut self, function: &IrFunction) {
        let mut block_ids: Vec<_> = function.blocks.keys().cloned().collect();
        block_ids.sort_by_key(|b| b.0);

		for (index, block_id) in block_ids.iter().enumerate() {
			let block = function.blocks.get(block_id).expect("block not found");
			
			if block.instructions.is_empty() && block.predecessors.is_empty() {
				continue;
			}

			self.emit_label(block_id.0); // block label

			self.gen_block_instructions(block);

			if let Terminator::Jump(jump_bid) = block.terminator {
				let next_block = function.blocks.get(&(BlockId(index + 1)));
				if Some(jump_bid) == next_block.map(|b| b.id) {
					// fallthrough
				} else {
					self.emit_raw_code(&format!("b _L{}\n", jump_bid.0));
				}
			}
		}
	}

	fn gen_block_instructions(&mut self, block: &IrBasicBlock) {
		for inst in &block.instructions {
			self.gen_instruction(inst);
		}
	}

    /// Align the given address into an address divisible by 16.
    fn align_to_16(value: usize) -> usize {
        (value + 16 - 1) & !15
    }

    fn emit_raw_code(&mut self, code: &str) {
        self.current_function_code.push_str(code);
    }

    /// WARNING: Use this function with caution.
    fn emit_store_reg_by_name(&mut self, reg: &str, off: i64) {
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

    fn emit_call_inst(&mut self, func: &str, args: &[IrValueId], result: &Option<IrValueId>) {
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
                    self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(*slot));
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

    fn emit_builtin_fn_call_inst(&mut self, builtin: BuiltinFn, args: &[IrValueId], result: &Option<IrValueId>) {
        let fn_name = match builtin {
            BuiltinFn::AssignRef => "assign_ref",
            BuiltinFn::AllocInt => "make_rt_int",
            BuiltinFn::AllocStr => "make_rt_str",
            BuiltinFn::AllocRec => "make_rt_rec",
            BuiltinFn::Panic => "rt_panic",
        };
        self.emit_call_inst(fn_name, args, result);
    }

    fn calculate_function_stack_size(&self, lir_func: &IrFunction, is_leaf: bool) -> usize {
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
                if let IrInstruction::Store { .. } = instr {
                    final_stack_size += 8;
                }
            }
        }
        final_stack_size
    }

    fn emit_function_preamble(&mut self, function: &IrFunction) {
        let curr_fn_state = self.get_current_fn_state();
        let curr_func_is_leaf = curr_fn_state.is_leaf;
        let stack_size = curr_fn_state.computed_stack_size;

        self.current_function_code.push_str(&format!(".global _{name}\n_{name}:\n", name = function.name));
        if curr_func_is_leaf {
            if stack_size > 0 {
                self.current_function_code.push_str(&format!("sub sp, sp, #{stack_size}\n"));
            }
        } else {
            self.current_function_code.push_str(&format!("sub sp, sp, #{stack_size}\n"));
            self.current_function_code.push_str(&format!("stp x29, x30, [sp, #{}]\n", stack_size - 16));
            self.current_function_code.push_str(&format!("add x29, sp, #{}\n", stack_size - 16));
        }

        for (reg_counter, param) in function.signature.params.iter().enumerate() {
            let param_addr = self.offset_generator.next(param.stack_slot);
            // self.current_function_code.push_str(&format!("str x{reg_counter}, [sp, #{param_addr}]\n"));
            self.emit_store_reg_by_name(&format!("x{}", reg_counter), param_addr);
        }
    }

    fn emit_function_postamble(&mut self, lir_func: &IrFunction) {
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

    fn emit_cjump_inst(&mut self, lhs: &IrValue, rhs: &IrValue) {
        match (lhs, rhs) {
            (IrValue::Register(vreg1), IrValue::Register(vreg2)) => {
                let src1 = self.current_allocations().get(vreg1).unwrap().clone();
                let src2 = self.current_allocations().get(vreg2).unwrap().clone();
                match (src1, src2) {
                    (Location::Reg(r1), Location::Reg(r2)) => self.emit_cmp_reg_reg(&r1, &r2),

                    (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                    (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(op_slot));
                        self.emit_cmp_reg_reg(&reg_op, &SCRATCH_REGISTER_0);
                    },

                    (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(op1_slot));
                        self.emit_ldr(&SCRATCH_REGISTER_1, IrAddress::StackSlot(op2_slot));
                        self.emit_cmp_reg_reg(&SCRATCH_REGISTER_0, &SCRATCH_REGISTER_1);
                    },
                } 
            },

            (IrValue::Register(vreg), IrValue::Constant(imm)) |
            (IrValue::Constant(imm), IrValue::Register(vreg)) => {
                let src1 = self.current_allocations().get(vreg).unwrap().clone();
                match src1 {
                    Location::Reg(register) => self.emit_cmp_reg_imm(&register, *imm),
                    Location::StackSlot(src_slot) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(src_slot));
                        self.emit_cmp_reg_imm(&SCRATCH_REGISTER_0, *imm);
                    },
                }
            },

            (IrValue::Constant(imm1), IrValue::Constant(imm2)) => {
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

    fn emit_str_inst(&mut self, src: &IrValueId, addr: IrAddress) {
        let src_loc = self.current_allocations().get(src).unwrap().clone();
        match src_loc {
            Location::Reg(register) => self.emit_str(&register, addr),
            Location::StackSlot(ss) => {
                self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(ss));
                self.emit_str(&SCRATCH_REGISTER_0, addr);
            }
        }
    }

    fn emit_ldr_inst(&mut self, addr: IrAddress, dest: &IrValueId) {
        let dest_loc = self.current_allocations().get(dest).unwrap().clone();
        match dest_loc {
            Location::Reg(register) => self.emit_ldr(&register, addr),
            Location::StackSlot(ss) => {
                self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(ss));
                self.emit_str(&SCRATCH_REGISTER_0, addr);
            }
        }
    } 

    /// Spill register to stack pointer (SP)
    fn emit_str_relative_sp(&mut self, reg: &Register, stack_size: i64, off: i64) {
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
    fn emit_ldr_relative_sp(&mut self, reg: &Register, stack_size: i64, off: i64) {
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
    fn emit_str_relative_fp(&mut self, reg: &Register, off: i64) {
        let dest_addr = if off != 0 { // if not at the beginning of the frame pointer
            format!("[x29, #-{off}]")
        }
        else {
            "[x29]".to_string()
        };
        self.current_function_code.push_str(&format!("str {}, {dest_addr}\n", reg.name));
    }

    /// Load register from frame pointer (x29)
    fn emit_ldr_relative_fp(&mut self, reg: &Register, off: i64) {
        let src_addr = if off != 0 { // if not at the beginning of the frame pointer
            format!("[x29, #-{off}]")
        }
        else {
            "[x29]".to_string()
        };
        self.current_function_code.push_str(&format!("ldr {}, {src_addr}\n", reg.name));
    }

    fn emit_mov_inst(&mut self, dest: &IrValueId, src: &IrValue) {
        let loc = self.current_allocations().get(dest).unwrap().clone();
        if let Location::Reg(dest_reg) = loc {
            match src {
                IrValue::Register(reg) => {
                    let src_loc = self.current_allocations().get(reg).unwrap().clone();
                    if let Location::Reg(src_reg) = src_loc {
                        self.emit_move_reg_to_reg(&dest_reg, &src_reg);
                    }
                },
                IrValue::Constant(value) => {
                    self.emit_move_const_to_reg(&dest_reg, *value);
                },
            }
        }
        else if let Location::StackSlot(dest_slot) = loc {
            match src {
                IrValue::Register(vreg) => {
                    let src_loc = self.current_allocations().get(vreg).unwrap().clone();
                    if let Location::Reg(src_reg) = src_loc {
                        self.emit_move_reg_to_reg(&SCRATCH_REGISTER_0, &src_reg);
                        self.emit_str(&SCRATCH_REGISTER_0, IrAddress::StackSlot(dest_slot));
                    }
                },
                IrValue::Constant(value) => {
                    self.emit_move_const_to_reg(&SCRATCH_REGISTER_0, *value);
                    self.emit_str(&SCRATCH_REGISTER_0, IrAddress::StackSlot(dest_slot));
                },
            }
        }
    }

    fn emit_add_inst(&mut self, dest: &IrValueId, lhs: &IrValue, rhs: &IrValue) {
        let dest_loc = self.current_allocations().get(dest).unwrap().clone();
        match dest_loc {
            Location::Reg(register) => self.emit_add_dest_reg(&register, lhs, rhs),
            Location::StackSlot(slot) => self.emit_add_dest_stack(slot, lhs, rhs),
        }
    }

    fn emit_add_dest_reg(&mut self, dest_reg: &Register, lhs: &IrValue, rhs: &IrValue) {
        match (lhs, rhs) {
            (IrValue::Register(vreg1), IrValue::Register(vreg2)) => {
                let src1 = self.current_allocations().get(vreg1).unwrap().clone();
                let src2 = self.current_allocations().get(vreg2).unwrap().clone();
                match (src1, src2) {
                    (Location::Reg(r1), Location::Reg(r2)) => self.emit_add_reg_reg_reg(dest_reg, &r1, &r2),

                    (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                    (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(op_slot));
                        self.emit_add_reg_reg_reg(dest_reg, &reg_op, &SCRATCH_REGISTER_0);
                    },

                    (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(op1_slot));
                        self.emit_ldr(&SCRATCH_REGISTER_1, IrAddress::StackSlot(op2_slot));
                        self.emit_add_reg_reg_reg(dest_reg, &SCRATCH_REGISTER_0, &SCRATCH_REGISTER_1);
                    },
                }
            },

            (IrValue::Constant(imm), IrValue::Register(vreg)) |
            (IrValue::Register(vreg), IrValue::Constant(imm)) => {
                let src1 = self.current_allocations().get(vreg).unwrap().clone();
                match src1 {
                    Location::Reg(r1) => self.emit_add_reg_reg_imm(dest_reg, &r1, *imm),
                    Location::StackSlot(src_slot) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(src_slot));
                        self.emit_add_reg_reg_imm(dest_reg, &SCRATCH_REGISTER_0, *imm);
                    },
                }
            },

            (IrValue::Constant(imm1), IrValue::Constant(imm2)) => {
                self.emit_move_const_to_reg(dest_reg, *imm1 + *imm2);
            },
        }
    }

    fn emit_add_dest_stack(&mut self, dest_slot: StackSlotId, lhs: &IrValue, rhs: &IrValue) {
        match (lhs, rhs) {
            (IrValue::Register(reg1), IrValue::Register(reg2)) => {
                let src1 = self.current_allocations().get(reg1).unwrap().clone();
                let src2 = self.current_allocations().get(reg2).unwrap().clone();
                match (src1, src2) {
                    (Location::Reg(r1), Location::Reg(r2)) => {
                        self.emit_add_reg_reg_reg(&SCRATCH_REGISTER_0, &r1, &r2);
                        self.emit_str(&SCRATCH_REGISTER_0, IrAddress::StackSlot(dest_slot));
                    }

                    (Location::StackSlot(op_slot), Location::Reg(reg_op)) |
                    (Location::Reg(reg_op), Location::StackSlot(op_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(op_slot));
                        self.emit_add_reg_reg_reg(&SCRATCH_REGISTER_1, &reg_op, &SCRATCH_REGISTER_0);
                        self.emit_str(&SCRATCH_REGISTER_1, IrAddress::StackSlot(dest_slot));
                    },

                    (Location::StackSlot(op1_slot), Location::StackSlot(op2_slot)) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(op1_slot));
                        self.emit_ldr(&SCRATCH_REGISTER_1, IrAddress::StackSlot(op2_slot));
                        self.emit_add_reg_reg_reg(&SCRATCH_REGISTER_0, &SCRATCH_REGISTER_0, &SCRATCH_REGISTER_1);
                        self.emit_str(&SCRATCH_REGISTER_0, IrAddress::StackSlot(dest_slot));
                    },
                }
            },

            (IrValue::Constant(imm), IrValue::Register(vreg)) |
            (IrValue::Register(vreg), IrValue::Constant(imm)) => {
                let src1 = self.current_allocations().get(vreg).unwrap().clone();
                match src1 {
                    Location::Reg(r1) => {
                        self.emit_add_reg_reg_imm(&SCRATCH_REGISTER_0, &r1, *imm);
                        self.emit_str(&SCRATCH_REGISTER_0, IrAddress::StackSlot(dest_slot));
                    }
                    Location::StackSlot(src_slot) => {
                        self.emit_ldr(&SCRATCH_REGISTER_0, IrAddress::StackSlot(src_slot));
                        self.emit_add_reg_reg_imm(&SCRATCH_REGISTER_0, &SCRATCH_REGISTER_0, *imm);
                        self.emit_str(&SCRATCH_REGISTER_0, IrAddress::StackSlot(dest_slot));
                    },
                }
            },
            (IrValue::Constant(imm1), IrValue::Constant(imm2)) => {
                self.emit_move_const_to_reg(&SCRATCH_REGISTER_0, *imm1 + *imm2);
                self.emit_str(&SCRATCH_REGISTER_0, IrAddress::StackSlot(dest_slot));
            },
        }
    }

    fn emit_ldr(&mut self, r1: &Register, addr: IrAddress) {
        let stack_size = self.get_current_fn_state().computed_stack_size;
        match addr {
            IrAddress::StackSlot(slot_id) => {
                let addr_off = self.offset_generator.get_offset_unchecked(slot_id);
                if self.get_current_fn_state().is_leaf {
                    self.emit_ldr_relative_sp(r1, stack_size, addr_off);
                }
                else {
                    self.emit_ldr_relative_fp(r1, addr_off);
                }
            },
            IrAddress::BaseOffset(value_id, off) => {
                let loc = self.current_allocations().get(&value_id).unwrap_or_else(|| bug!("no allocation found for {value_id:#?}"));
                match loc {
                    Location::Reg(register) => {
                        self.current_function_code.push_str(&format!("ldr {d}, [{b}, #{off}]\n", d = r1.name, b = register.name));
                    },
                    Location::StackSlot(_) => todo!(),
                }
            },
        }
    }

    fn emit_str(&mut self, r1: &Register, addr: IrAddress) {
        let stack_size = self.get_current_fn_state().computed_stack_size;
        match addr {
            IrAddress::StackSlot(slot_id) => {
                let addr_off = self.offset_generator.next(slot_id);
                if self.get_current_fn_state().is_leaf {
                    self.emit_str_relative_sp(r1, stack_size, addr_off);
                }
                else {
                    self.emit_str_relative_fp(r1, addr_off);
                }
            },
            IrAddress::BaseOffset(value_id, off) => {
                let addr_off = self.offset_generator.off_map.insert(StackSlotId(off), off * 8).unwrap_or_else(|| bug!("cannot create an offset"));
                let loc = self.current_allocations().get(&value_id).unwrap();
                match loc {
                    Location::Reg(register) => self.current_function_code.push_str(&format!("str {s}, [{b}, #{addr_off}]\n", s = r1.name, b = register.name)),
                    Location::StackSlot(ss) => self.emit_str(r1, IrAddress::StackSlot(*ss)),
                }
            },
        }
    }

    fn emit_load_const(&mut self, label_id: usize, value_id: &IrValueId) {
        let loc = self.current_allocations().get(value_id).unwrap().clone();
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
        if self.const_pool.is_empty() {
            return;
        }
        let mut global_vars_code = String::new();
        for (index, c_item) in self.const_pool.iter_enumerated() {
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
                output_str.push_str(&"\t.xword 0\n".repeat(rec.fields.iter().len()));
            }
        }
        output_str
    }

    fn current_allocations(&self) -> &HashMap<IrValueId, Location> {
        if let Some(allocs) = &self.allocations {
            allocs
        }
        else {
            bug!("no allocations found");
        }
    }

    /// Check whether a function is leaf or not.
    /// A leaf function does not call any other functions.
    fn is_function_leaf(&self, lir_func: &IrFunction) -> bool {
        let mut is_leaf = true;
        for block in lir_func.blocks.values() {
            for instr in &block.instructions {
                is_leaf = !matches!(instr, IrInstruction::Call { .. } | IrInstruction::CallBuiltin { .. });
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