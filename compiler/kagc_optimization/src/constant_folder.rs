// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_mir::block::IrBasicBlock;
use kagc_mir::instruction::IrInstruction;
use kagc_mir::value::{IrValueId, IrValue};

use crate::FunctionPass;

#[derive(Debug, Default)]
pub struct ConstantFolder {
	known_constants: HashMap<IrValueId, i64>
}

impl FunctionPass for ConstantFolder {
	fn name(&self) -> &'static str {
		"Constant folder"
	}

	fn run_on_function(&mut self, func: &mut kagc_mir::function::IrFunction) -> bool {
		func
			.blocks
			.values_mut()
			.all(|block| self.fold_block(block))
	}
}

impl ConstantFolder {
	fn fold_block(&mut self, block: &mut IrBasicBlock) -> bool {
		let mut folded_instructions = vec![];

		for inst in &block.instructions {
			folded_instructions.push(self.fold_inst(inst.clone()));
		}

		block.instructions = folded_instructions;
		true
	}

	fn fold_inst(&mut self, inst: IrInstruction) -> IrInstruction {
		match inst {
			IrInstruction::Mov { result, src } => {
				if let IrValue::Constant(value) = src {
					self.known_constants.insert(result, value);
				}
				inst
			}

			IrInstruction::Add { lhs, rhs, result } => {
				match (lhs, rhs) {
						(IrValue::Constant(lhs), IrValue::Constant(rhs)) => {
							let new_result = lhs + rhs;
							self.known_constants.insert(result, new_result);
							IrInstruction::Mov { result, src: IrValue::Constant(new_result) }
						},
						(IrValue::Register(ir_value_id), IrValue::Constant(lhs))
						| (IrValue::Constant(lhs), IrValue::Register(ir_value_id)) => {
							if let Some(&rhs) = self.known_constants.get(&ir_value_id) {
								self.known_constants.insert(result, lhs + rhs);
								return IrInstruction::Mov { result, src: IrValue::Constant(lhs + rhs) };
							}
							inst
						},
						(IrValue::Register(lhs), IrValue::Register(rhs)) => {
							let lhs_const = self.known_constants.get(&lhs);
							let rhs_const = self.known_constants.get(&rhs);
							if let (Some(&lhs), Some(&rhs)) = (lhs_const, rhs_const) {
								self.known_constants.insert(result, lhs + rhs);
								return IrInstruction::Mov { result, src: IrValue::Constant(lhs + rhs) };
							}
							inst
						}
					}
			}
			_ => inst
		}
	}
}