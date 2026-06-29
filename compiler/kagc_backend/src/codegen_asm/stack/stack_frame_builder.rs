// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_mir::{block::IrBasicBlock, function::IrFunction, instruction::{IrInstruction, IrLocation}};

use crate::codegen_asm::stack::{StackFrame, StackObject};

pub struct StackFrameBuilder {

}

impl StackFrameBuilder {
	pub fn build_for_function(function: &IrFunction) -> StackFrame {
		let mut frame = StackFrame::new();

		for block in function.blocks.values() {
			StackFrameBuilder::visit_block(block, &mut frame);
		}

		frame
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
			IrInstruction::Store { location: address, .. } => {
				let IrLocation::Variable(var_id) = *address;

				frame.allocate_variable(var_id, 0x8);
			}
			IrInstruction::Load { result, .. } => {
				frame.allocate(StackObject::Value(*result), 0x8);
			}
			_ => unimplemented!()
		 }
	}
}