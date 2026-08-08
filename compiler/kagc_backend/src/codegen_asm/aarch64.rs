// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_const::pool::{ConstEntry, ConstPool, KagcConst};
use kagc_mir::block::{BlockId, IrBasicBlock, Terminator};
use kagc_mir::function::{IrFunctionId, IrFunction};
use kagc_mir::instruction::{IrCondition, IrInstruction, IrLocation};
use kagc_mir::value::{IrValue, IrValueId};
use kagc_mir::variable::IrVariableId;
use kagc_symbol::StorageClass;
use kagc_utils::bug;

use crate::codegen_asm::cg_function_ctx::CodeGenFunctionContext;
use crate::codegen_asm::stack::{StackFrameBuilder, StackObject};
use crate::regalloc::register::{RegClass, Register};
use crate::CodeGenerator;

use kagc_optimization::OptimizationPipeline;

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
        }
    }
}

impl<'cg> CodeGenerator for Aarch64CodeGenerator<'cg> {
    fn gen_function(&mut self, function: &mut IrFunction) {
        if function.signature.class == StorageClass::EXTERN {
            println!(".extern _{fn_name}", fn_name = function.name); // an extern function
            return;
        }

		let mut optimization_pipeline = OptimizationPipeline::standard_pipeline();
		optimization_pipeline.apply(function);

		self.current_function_ctx.reinit();
		self.current_function_ctx.stack_frame = StackFrameBuilder::build_for_function(function);
        self.function_entry_block = Some(function.entry_block);
		self.current_function_ctx.compute_is_leaf(function);
		self.current_function_ctx.id = function.id.0 as i64;

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
                self.push_code(format!("_L{}.{}:", self.current_function_ctx.id, block.id.0));
            }
        }
        else {
            panic!("No entry block set for function! Aborting...");
        }

        for instr in block.instructions.iter() {
            self.gen_instruction(instr);
        }

        match block.terminator {
            Terminator::Jump(block_id) => {
				self.push_code(format!("b _L{}.{}", self.current_function_ctx.id, block_id.0))
			}
			Terminator::Return { .. }
            | Terminator::Fallthrough(_) => {},
            Terminator::CondJump { cond, then_block, else_block, .. } => {
                let cmp_code = match cond {
                    IrCondition::EqEq   => "b.eq",
                    IrCondition::NEq    => "b.ne",
                    IrCondition::GTEq   => "b.ge",
                    IrCondition::LTEq   => "b.le",
                    IrCondition::GThan  => "b.gt",
                    IrCondition::LThan  => "b.lt",
                };
                self.push_code(format!("{cmp_code} _L{}.{bid}", self.current_function_ctx.id, bid = then_block.0));
                self.push_code(format!("b _L{}.{bid}", self.current_function_ctx.id, bid = else_block.0));
            },
        }
    }

    fn gen_instruction(&mut self, instr: &IrInstruction) {
		match instr {
			IrInstruction::Mov         { result, src } => self.emit_mov(*src, *result),
			IrInstruction::Add         { result, lhs, rhs } => self.emit_add(*lhs, *rhs, *result),
			IrInstruction::Subtract    { result, lhs, rhs } => self.emit_subtract(*lhs, *rhs, *result),
			IrInstruction::Multiply    { result, lhs, rhs } => self.emit_multiply(*lhs, *rhs, *result),
			IrInstruction::Cmp 		   { result, lhs, rhs, condition } => self.emit_conditional(*lhs, *rhs, *result, *condition),
			IrInstruction::Neg   	   { result, value } => self.emit_neg(*value, *result),
			IrInstruction::Store       { src, location } => self.emit_store(*src, *location),
			IrInstruction::Load        { location, result } => self.emit_load(*location, *result),
			IrInstruction::Call        { func, args, result } => self.emit_call(func, args, *result),
			IrInstruction::CondJump    { lhs, rhs, cond, .. } => self.emit_cond_jump(*lhs, *rhs, *cond),
			IrInstruction::Param 	   { index, var_id } => self.emit_param(*index, *var_id),
			IrInstruction::Jump 	   { block } => self.emit_jump(*block),
			IrInstruction::LoadConst   { pool_idx, result } => self.emit_load_const(*pool_idx, *result),
			_ => todo!("{instr:#?}")
		}
    }
}

impl<'cg> Aarch64CodeGenerator<'cg> {
	pub fn generate_code(&mut self, functions: &mut HashMap<IrFunctionId, IrFunction>) {
        let mut function_ids: Vec<IrFunctionId> = functions.keys().cloned().collect();
        function_ids.sort_by_key(|function_id| function_id.0);

		for function_id in function_ids {
			let Some(function) = functions.get_mut(&function_id)
			else {
				panic!("function not found. obvious bug");
			};

			self.gen_function(function);

            self.current_function_code = String::new();
		}
	}

	fn gen_function_blocks(&mut self, function: &IrFunction) {
        let block_ids: Vec<_> = function.blocks.keys().cloned().collect();

		for (index, block_id) in block_ids.iter().enumerate() {
			let block = function.blocks.get(block_id).expect("block not found");

			if block.instructions.is_empty() && block.predecessors.is_empty() {
				continue;
			}

			self.push_code(format!("_L{}.{}:", self.current_function_ctx.id, block_id.0));
			self.gen_block_instructions(block);

			match block.terminator {
				Terminator::Jump(jump_bid) => {
					let next_block = function.blocks.get(&(BlockId(index + 1)));
					if Some(jump_bid) == next_block.map(|b| b.id) {
						// fallthrough
					}
					else {
						self.push_code(format!("b _L{}.{}", self.current_function_ctx.id, jump_bid.0));
					}
				},
				Terminator::CondJump { 
					jump_value_id, 
					else_block,
					..
				} => {
					let stack_off = self.get_value_stack_offset_unchecked(jump_value_id);

					self.push_code(
						format!(
							"ldr {sr1}, [sp, #{stack_off}]\nmov {sr2}, #0\ncmp {sr1}, {sr2}\nb.eq _L{fid}.{else_id}", // '0' means the condition evaluates to false
							sr1 = SCRATCH_REGISTER_0.name,
							sr2 = SCRATCH_REGISTER_1.name,
							fid = self.current_function_ctx.id,
							else_id = else_block.0
						)
					);
				},
				Terminator::Return { value, target } => {
					if let Some(return_value) = value {
						let ret_val_off = self.get_value_stack_offset_unchecked(return_value);

						self.push_code(format!("ldr x0, [sp, #{ret_val_off}]"));

						self.push_code(
							format!(
								"b _L{}.{}",
								self.current_function_ctx.id,
								target.0
							)
						);
					}
					else {
						self.push_code("mov x0, #0".to_string());
					}
				}
				_ => {}
			}
		}
	}

	fn gen_block_instructions(&mut self, block: &IrBasicBlock) {
		for inst in &block.instructions {
			self.gen_instruction(inst);
		}
	}

	fn emit_cond_jump(&mut self, lhs: IrValue, rhs: IrValue, cond: IrCondition) {
        let cmp_code = match cond {
            IrCondition::EqEq   => "b.ne",
            IrCondition::NEq    => "b.eq",
            IrCondition::GTEq   => "b.le",
            IrCondition::LTEq   => "b.lge",
            IrCondition::GThan  => "b.lt",
            IrCondition::LThan  => "b.gt",
        };

		match (lhs, rhs) {
			(IrValue::Constant(lhs), IrValue::Constant(rhs)) => {
				self.push_code(
					format!(
						"mov {}, #{lhs}\nmov {}, #{rhs}\ncmp {}, {}\n{cmp_code} _some_label", 
						SCRATCH_REGISTER_0.name, 
						SCRATCH_REGISTER_1.name,
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

				self.push_code(
					format!(
						"mov {}, #{lhs}\nldr {}, [sp, #{offset}]",
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

				self.push_code(
					format!(
						"ldr {}, [sp, #{lhs_offset}]\nldr {}, [sp, #{rhs_offset}]\ncmp {}, {}\n{cmp_code}",
						SCRATCH_REGISTER_0.name,
						SCRATCH_REGISTER_1.name,
						SCRATCH_REGISTER_0.name,
						SCRATCH_REGISTER_1.name
					)
				);
			},
		}
	}

	fn emit_mov(&mut self, src: IrValue, result: IrValueId) {
		self.load_operand(src, &SCRATCH_REGISTER_0.name);
		self.store_result(result);
	}

	fn emit_neg(&mut self, value: IrValue, result: IrValueId) {
		self.load_operand(value, &SCRATCH_REGISTER_0.name);

		self.push_code(
			format!(
				"neg {sr1}, {sr1}",
				sr1 = SCRATCH_REGISTER_0.name,
			)
		);

		self.store_result(result);
	}

	fn emit_conditional(&mut self, lhs: IrValue, rhs: IrValue, result: IrValueId, condition: IrCondition) {
        let cmp_code = match condition {
            IrCondition::EqEq   => "eq",
            IrCondition::NEq    => "ne",
            IrCondition::GTEq   => "lge",
            IrCondition::LTEq   => "le",
            IrCondition::GThan  => "gt",
            IrCondition::LThan  => "lt",
        };

		self.load_operands(lhs, rhs);

		self.push_code(
			format!(
				"cmp {sr1}, {sr2}\ncset {sr1}, {cmp_code}",
				sr1 = SCRATCH_REGISTER_0.name,
				sr2 = SCRATCH_REGISTER_1.name,
			)
		);

		self.store_result(result);
	}

	fn emit_jump(&mut self, bid: BlockId) {
		self.push_code(
			format!(
				"b _L{}.{}", self.current_function_ctx.id, bid.0
			)
		);
	}

	fn emit_add(&mut self, lhs: IrValue, rhs: IrValue, result: IrValueId) {
		self.emit_binary_op(lhs, rhs, result, "add");
	}

	fn emit_subtract(&mut self, lhs: IrValue, rhs: IrValue, result: IrValueId) {
		self.emit_binary_op(lhs, rhs, result, "sub");
	}

	fn emit_multiply(&mut self, lhs: IrValue, rhs: IrValue, result: IrValueId) {
		self.emit_binary_op(lhs, rhs, result, "mul");
	}

	fn emit_binary_op(
		&mut self,
		lhs: IrValue,
		rhs: IrValue,
		result: IrValueId,
		op: &str,
	) {
		self.load_operands(lhs, rhs);

		self.push_code(
			format!(
				"{op} {sr0}, {sr0}, {sr1}", 
				sr0 = SCRATCH_REGISTER_0.name, 
				sr1 = SCRATCH_REGISTER_1.name,
			)
		);

		self.store_result(result);
	}

	fn emit_load_const(&mut self, pool_idx: usize, result: IrValueId) {
		let Some(const_value) = self.const_pool.get(pool_idx) else {
			bug!("the const value being queried has not been interned");
		};

		match const_value.value {
			KagcConst::Str(_) => {
				self.push_code(
					format!(
						"adrp {sr0}, .L.__c.{pool_idx}@PAGE\nadd {sr0}, {sr0}, .L.__c.{pool_idx}@PAGEOFF",
						sr0 = SCRATCH_REGISTER_0.name
					)
				);

				self.store_result(result);
			},
			_ => todo!()
		}

	}

	fn load_operand(&mut self, value: IrValue, scratch_reg_name: &str) {
        let asm = match value {
            IrValue::Constant(val) => {
                format!("mov {scratch_reg_name}, #{val}")
            }
            IrValue::Register(reg_id) => {
                let offset = self
                    .current_function_ctx
                    .stack_frame
                    .offset_with_object_unchecked(StackObject::Value(reg_id));
                format!("ldr {scratch_reg_name}, [sp, #{offset}]")
            }
        };

		self.push_code(asm);
	}

	fn load_operands(&mut self, lhs: IrValue, rhs: IrValue) {
        self.load_operand(lhs, &SCRATCH_REGISTER_0.name);
        self.load_operand(rhs, &SCRATCH_REGISTER_1.name);
    }

	fn store_result(&mut self, result: IrValueId) {
        let result_offset = self
            .current_function_ctx
            .stack_frame
            .offset_with_object_unchecked(StackObject::Value(result));

        self.push_code(
			format!(
            	"str {}, [sp, #{result_offset}]",
            	SCRATCH_REGISTER_0.name
        	)
		);
    }

	fn emit_store(&mut self, src: IrValueId, location: IrLocation) {
		let offset = self.get_value_stack_offset_unchecked(src);

		self.push_code(
			format!(
				"ldr {}, [sp, #{offset}]",
				SCRATCH_REGISTER_0.name,
			)
		);

		let IrLocation::Variable(var_id) = location;
		let offset = self.get_variable_stack_offset_unchecked(var_id);

		self.push_code(
			format!(
				"str {}, [sp, #{offset}]",
				SCRATCH_REGISTER_0.name,
			)
		);
	}

	fn emit_load(&mut self, location: IrLocation, result: IrValueId) {
		let IrLocation::Variable(var_id) = location;
		let offset = self.get_variable_stack_offset_unchecked(var_id);

		self.push_code(
			format!(
				"ldr {}, [sp, #{offset}]",
				SCRATCH_REGISTER_0.name,
			)
		);

		self.store_result(result);
	}

	fn emit_call(&mut self, name: &str, args: &[IrValueId], result: Option<IrValueId>) {
		assert!(args.len() < 8, "please make sure the args count is less than 8");

        for (reg_counter, param) in args.iter().enumerate() {
			let offset = self.get_value_stack_offset_unchecked(*param);

			self.push_code(
				format!(
					"ldr x{reg_counter}, [sp, #{offset}]"
				)
			);
        }

		self.push_code(format!("bl _{name}"));

		if let Some(result) = result {
			let offset = self.get_value_stack_offset_unchecked(result);
			self.push_code(
				format!(
					"str x0, [sp, #{offset}]"
				)
			);
		}
	}

	fn emit_param(&mut self, index: usize, var_id: IrVariableId) {
		let offset = self.get_variable_stack_offset_unchecked(var_id);

		self.push_code(
			format!(
				"str x{index}, [sp, #{offset}]"
			)
		);
	}

    fn emit_function_preamble(&mut self, function: &IrFunction) {
        let curr_func_is_leaf = self.current_function_ctx.is_leaf;
        let stack_size = self.current_function_ctx.stack_frame.size();

        self.push_code(
			format!(
				".global _{name}\n_{name}:\nsub sp, sp, #{stack_size}",
				name = function.name
			)
		);

		if !curr_func_is_leaf {
			self.push_code(format!("str x30, [sp, #{off}]", off = stack_size - 8));
		}
    }

    fn emit_function_postamble(&mut self, lir_func: &IrFunction) {
        let curr_func_is_leaf = self.current_function_ctx.is_leaf;
        let stack_size = self.current_function_ctx.stack_frame.size();

        self.push_code(format!("_L{}.{lbl}:", self.current_function_ctx.id, lbl = lir_func.exit_block.0));

		if !curr_func_is_leaf {
			self.push_code(format!("ldr x30, [sp, #{}]", stack_size - 8));
		}

		self.push_code(format!("add sp, sp, #{stack_size}\nret"));
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
                output_str.push_str(&format!("\n\t.xword .L.__c.{c_item_index}"));
            }
            else {
                output_str.push_str(&format!("\n.section __TEXT,__cstring\n\t.L.__c.{c_item_index}:\n\t.asciz \"{str_value}\""));
            }
        }
        else if let KagcConst::Int(int_value) = &c_item.value {
            if parent_is_record {
                // output_str.push_str(&format!("\t.word {int_value}\n\t.zero 4\n"));
                output_str.push_str(&format!("\n\t.xword .L.__c.{c_item_index}"));
            }
            else {
                output_str.push_str(&format!("\n.section __DATA,__const\n\t.L.__c.{c_item_index}:\n\t.xword {int_value}"));
            }
        }
        else if let KagcConst::Record(rec) = &c_item.value {
            if parent_is_record {
                output_str.push_str(&format!("\n\t.xword .L.__c.{}", c_item_index));
            }
            else {
                output_str.push_str(
                    &format!(
                        "\n.section __DATA,__const\n.align {}\n.L.__c.{}:", 
                        rec.alignment, 
                        c_item_index
                    )
                );
                output_str.push_str(&"\n\t.xword 0".repeat(rec.fields.iter().len()));
            }
        }
        output_str
    }

	pub fn push_code(&mut self, code: String) {
		self.current_function_code.push_str(&format!("\n{code}"));
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