// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_const::pool::{ConstEntry, ConstPool, KagcConst};
use kagc_mir::block::{BlockId, IrBasicBlock, Terminator};
use kagc_mir::function::{FunctionId, IrFunction};
use kagc_mir::instruction::{IrCondition, IrInstruction, IrLocation};
use kagc_mir::value::{IrValue, IrValueId};
use kagc_mir::variable::IrVariableId;
use kagc_symbol::StorageClass;

use crate::codegen_asm::cg_function_ctx::CodeGenFunctionContext;
use crate::codegen_asm::stack::{StackFrameBuilder, StackObject};
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
        id: 0xA,
        name: String::from("x10"),
        class: RegClass::GPR
    };
}

pub struct Aarch64CodeGenerator<'cg> {
    function_entry_block: Option<BlockId>,
    const_pool: &'cg ConstPool,
    offset_generator: OffsetGenerator,
    current_function_code: String,

	current_function_ctx: CodeGenFunctionContext
}

impl<'cg> Aarch64CodeGenerator<'cg> {
    pub fn new(const_pool: &'cg ConstPool) -> Self {
        Self { 
            function_entry_block: None,
            const_pool,
			current_function_ctx: CodeGenFunctionContext::new(), 
            current_function_code: String::new(),
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

		self.current_function_ctx.reinit();
		self.current_function_ctx.stack_frame = StackFrameBuilder::build_for_function(function);
        self.function_entry_block = Some(function.entry_block);
		self.current_function_ctx.compute_is_leaf(function);

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
            Terminator::Jump(block_id) => self.current_function_code.push_str(&format!("\nb _L{}", block_id.0)),
            Terminator::Return { .. } => {
				todo!()
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
                self.emit_raw_code(&format!("\n{cmp_code} _L{bid}", bid = then_block.0));
                self.emit_raw_code(&format!("\nb _L{bid}", bid = else_block.0));
            },
        }
    }

    fn gen_instruction(&mut self, instr: &IrInstruction) {
		match instr {
			IrInstruction::Mov         { result, src } => self.emit_mov(*src, *result),
			IrInstruction::Add         { result, lhs, rhs } => self.emit_add(*lhs, *rhs, *result),
			IrInstruction::Store       { src, location } => self.emit_store(*src, *location),
			IrInstruction::Load        { location, result } => self.emit_load(*location, *result),
			IrInstruction::Call        { func, args, result } => self.emit_call(func, args, *result),
			_ => todo!()
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
					self.emit_raw_code(&format!("\nb _L{}", jump_bid.0));
				}
			}
		}
	}

	fn gen_block_instructions(&mut self, block: &IrBasicBlock) {
		for inst in &block.instructions {
			self.gen_instruction(inst);
		}
	}

    fn emit_raw_code(&mut self, code: &str) {
        self.current_function_code.push_str(code);
    }

	fn emit_mov(&mut self, src: IrValue, result: IrValueId) {
		let store_off = self
			.current_function_ctx
			.stack_frame
			.offset_with_object_unchecked(StackObject::Value(result));

		match src {
			IrValue::Constant(src) => {
				self.current_function_code.push_str(
					&format!(
						"\nmov {}, #{src}\nstr {}, [sp, #{store_off}]",
						SCRATCH_REGISTER_0.name,
						SCRATCH_REGISTER_0.name,
					)
				);
			},
			IrValue::Register(src_value) => {
				let slot_id = self
					.current_function_ctx
					.stack_frame
					.slot_id_unchecked(StackObject::Value(src_value));

				let load_off = self.current_function_ctx.stack_frame.offset_unchecked(slot_id);

				self.current_function_code.push_str(
					&format!(
						"\nldr {}, [sp, #{load_off}]\nstr {}, #{store_off}",
						SCRATCH_REGISTER_0.name,
						SCRATCH_REGISTER_0.name,
					)
				);
			},
		}
	}

	fn emit_add(&mut self, lhs: IrValue, rhs: IrValue, result: IrValueId) {
		match (lhs, rhs) {
			(IrValue::Constant(lhs), IrValue::Constant(rhs)) => {
				self.current_function_code.push_str(
					&format!(
						"\nmov {}, #{lhs}\nmov {}, #{rhs}\nadd {}, {}, {}", 
						SCRATCH_REGISTER_0.name, 
						SCRATCH_REGISTER_1.name,
						SCRATCH_REGISTER_0.name, 
						SCRATCH_REGISTER_0.name, 
						SCRATCH_REGISTER_1.name
					)
				);
			},
			(IrValue::Register(rhs), IrValue::Constant(lhs))
			| (IrValue::Constant(lhs), IrValue::Register(rhs)) => {
				let offset = self
					.current_function_ctx
					.stack_frame
					.offset_with_object_unchecked(StackObject::Value(rhs));

				self.current_function_code.push_str(
					&format!(
						"\nmov {}, #{lhs}\nldr {}, [sp, #{offset}]",
						SCRATCH_REGISTER_0.name,
						SCRATCH_REGISTER_1.name,
					)
				);
			},
			(IrValue::Register(lhs), IrValue::Register(rhs)) => {
				let lhs_offset = self
					.current_function_ctx
					.stack_frame
					.offset_with_object_unchecked(StackObject::Value(lhs));

				let rhs_offset = self
					.current_function_ctx
					.stack_frame
					.offset_with_object_unchecked(StackObject::Value(rhs));

				let result_offset = self
					.current_function_ctx
					.stack_frame
					.offset_with_object_unchecked(StackObject::Value(result));

				self.current_function_code.push_str(
					&format!(
						"\nldr {}, [sp, #{lhs_offset}]\nldr {}, [sp, #{rhs_offset}]\nadd {}, {}, {}\nstr {}, [sp, #{result_offset}]",
						SCRATCH_REGISTER_0.name,
						SCRATCH_REGISTER_1.name,
						SCRATCH_REGISTER_0.name,
						SCRATCH_REGISTER_0.name,
						SCRATCH_REGISTER_1.name,
						SCRATCH_REGISTER_0.name,
					)
				);
			},
		}
	}

	fn emit_store(&mut self, src: IrValueId, location: IrLocation) {
		let offset = self.get_value_stack_offset_unchecked(src);

		self.current_function_code.push_str(
			&format!(
				"\nldr {}, [sp, #{offset}]",
				SCRATCH_REGISTER_0.name,
			)
		);

		let IrLocation::Variable(var_id) = location;
		let offset = self.get_variable_stack_offset_unchecked(var_id);

		self.current_function_code.push_str(
			&format!(
				"\nstr {}, [sp, #{offset}]",
				SCRATCH_REGISTER_0.name,
			)
		);
	}

	fn emit_load(&mut self, location: IrLocation, result: IrValueId) {
		let IrLocation::Variable(var_id) = location;
		let offset = self.get_variable_stack_offset_unchecked(var_id);

		self.current_function_code.push_str(
			&format!(
				"\nldr {}, [sp, #{offset}]",
				SCRATCH_REGISTER_0.name,
			)
		);

		let offset = self.get_value_stack_offset_unchecked(result);
		self.current_function_code.push_str(
			&format!(
				"\nstr {}, [sp, #{offset}]",
				SCRATCH_REGISTER_0.name,
			)
		);
	}

	fn emit_call(&mut self, name: &str, args: &[IrValueId], result: Option<IrValueId>) {
		assert!(args.len() < 8, "please make sure the args count is less than 8");

        for (reg_counter, param) in args.iter().enumerate() {
			let offset = self.get_value_stack_offset_unchecked(*param);

			self.current_function_code.push_str(
				&format!(
					"\nldr x{reg_counter}, [sp, #{offset}]"
				)
			);
        }

		self.current_function_code.push_str(&format!("\nbl _{name}"));

		if let Some(result) = result {
			let offset = self.get_value_stack_offset_unchecked(result);
			self.current_function_code.push_str(
				&format!(
					"\nstr x0, [sp, #{offset}]"
				)
			);
		}
	}

    fn emit_function_preamble(&mut self, function: &IrFunction) {
        let curr_func_is_leaf = self.current_function_ctx.is_leaf;
        let stack_size = self.current_function_ctx.stack_frame.size();

        self.current_function_code.push_str(
			&format!(
				".global _{name}\n_{name}:",
				name = function.name
			)
		);

		self.current_function_code.push_str(&format!("\nsub sp, sp, #{stack_size}"));

		let lr_offset = stack_size - 8;

		if !curr_func_is_leaf {
			self.current_function_code.push_str(&format!("\nstr x30, [sp, #{lr_offset}]"));
		}

        for (reg_counter, param) in function.signature.params.iter().enumerate() {
			let offset = self.get_variable_stack_offset_unchecked(param.id);

			self.current_function_code.push_str(
				&format!(
					"\nstr x{reg_counter}, [sp, #{offset}]"
				)
			);
        }
    }

    fn emit_function_postamble(&mut self, lir_func: &IrFunction) {
        let curr_func_is_leaf = self.current_function_ctx.is_leaf;
        let stack_size = self.current_function_ctx.stack_frame.size();

        self.current_function_code.push_str(&format!("\n_L{lbl}:", lbl = lir_func.exit_block.0));

		let lr_offset = stack_size - 8;

		if !curr_func_is_leaf {
			self.current_function_code.push_str(&format!("\nldr x30, [sp, #{lr_offset}]"));
		}

		self.current_function_code.push_str(&format!("\nadd sp, sp, #{stack_size}"));
		self.current_function_code.push_str("\nret");
    }

    fn emit_label(&mut self, id: usize) {
        self.current_function_code.push_str(&format!("\n_L{id}:"));
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

	fn get_value_stack_offset_unchecked(&self, value_id: IrValueId) -> i32 {
		self
			.current_function_ctx
			.stack_frame
			.offset_with_object_unchecked(StackObject::Value(value_id))
	}

	fn get_variable_stack_offset_unchecked(&self, var_id: IrVariableId) -> i32 {
		self
			.current_function_ctx
			.stack_frame
			.offset_with_object_unchecked(StackObject::Variable(var_id))
	}
}