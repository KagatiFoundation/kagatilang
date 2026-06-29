// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_mir::block::IrBasicBlock;
use kagc_mir::function::IrFunction;
use kagc_mir::instruction::{IrInstruction, IrLocation};

use crate::codegen_asm::stack::{StackFrame, StackObject};

pub struct StackFrameBuilder;

impl StackFrameBuilder {
	pub fn build_for_function(function: &IrFunction) -> StackFrame {
		let mut frame = StackFrame::new();

		StackFrameBuilder::compute_stack_alignment_size(function, &mut frame);

		StackFrameBuilder::visit_function_parameters(function, &mut frame);

		for block in function.blocks.values() {
			StackFrameBuilder::visit_block(block, &mut frame);
		}

		frame
	}

 	fn compute_stack_alignment_size(lir_func: &IrFunction, frame: &mut StackFrame) {
		let mut is_leaf = true;
        for block in lir_func.blocks.values() {
            for instr in &block.instructions {
                if matches!(instr, IrInstruction::Call { .. } | IrInstruction::CallBuiltin { .. }) {
					is_leaf = false;
					break;
				}
            }
        }

		if !is_leaf {
			frame.increase_size(0x8); // to store the return address
		}
    }

	fn visit_function_parameters(function: &IrFunction, frame: &mut StackFrame) {
		for parameter in &function.signature.params {
			frame.allocate_variable(parameter.id, 0x8);
		}
	}

	fn visit_block(block: &IrBasicBlock, frame: &mut StackFrame) {
		for instruction in &block.instructions {
			StackFrameBuilder::visit_instruction(instruction, frame);
		}
	}

	fn visit_instruction(instruction: &IrInstruction, frame: &mut StackFrame) {
		 match instruction {
			IrInstruction::Mov { result, .. } => {
				frame.allocate(StackObject::Value(*result), 0x8);
			},
			IrInstruction::Add { result, .. } => {
				frame.allocate(StackObject::Value(*result), 0x8);
			}
			IrInstruction::Store { location, .. } => {
				let IrLocation::Variable(var_id) = *location;

				frame.allocate_variable(var_id, 0x8);
			}
			IrInstruction::Load { result, .. } => {
				frame.allocate(StackObject::Value(*result), 0x8);
			}
			IrInstruction::Call { result, .. } if result.is_some() => {
				frame.allocate(StackObject::Value(result.unwrap()), 0x8);
			}
			_ => {}
		 }
	}
}