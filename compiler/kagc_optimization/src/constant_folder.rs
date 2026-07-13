// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_mir::block::IrBasicBlock;
use kagc_mir::instruction::IrInstruction;
use kagc_mir::function::IrFunction;
use kagc_mir::value::{IrValueId, IrValue};

use crate::FunctionPass;

#[derive(Debug, Default)]
pub struct ConstantFolder {
	known_constants: HashMap<IrValueId, i64>
}

impl FunctionPass for ConstantFolder {
	fn name(&self) -> &'static str {
		"Constant Folder"
	}

	fn run_on_function(&mut self, func: &mut IrFunction) -> bool {
		func
			.blocks
			.values_mut()
			.all(|block| self.fold_block(block))
	}
}

impl ConstantFolder {
	fn fold_block(&mut self, block: &mut IrBasicBlock) -> bool {
		let mut folded_instructions = vec![];

		for inst in block.instructions.drain(..) {
			folded_instructions.push(self.fold_inst(inst));
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

			IrInstruction::Divide 	{ result, lhs, rhs } => self.fold_binary_op("/", lhs, rhs, result, inst),
			IrInstruction::Multiply { result, lhs, rhs } => self.fold_binary_op("*", lhs, rhs, result, inst),
			IrInstruction::Subtract { result, lhs, rhs } => self.fold_binary_op("-", lhs, rhs, result, inst),
			IrInstruction::Add 		{ result, lhs, rhs } => self.fold_binary_op("+", lhs, rhs, result, inst),
			_ => inst
		}
	}

	fn fold_binary_op(
        &mut self,
        op: &str,
        lhs: IrValue,
        rhs: IrValue,
        result: IrValueId,
        fallback_inst: IrInstruction,
    ) -> IrInstruction {
        let eval_math = |l: i64, r: i64| -> Option<i64> {
            match op {
                "+" => Some(l + r),
                "-" => Some(l - r),
                "*" => Some(l * r),
                "/" => if r != 0 { Some(l / r) } else { None },
				_ => None
            }
        };

        let left_val = match lhs {
            IrValue::Constant(val) => Some(val),
            IrValue::Register(id) => self.known_constants.get(&id).copied(),
        };

        let right_val = match rhs {
            IrValue::Constant(val) => Some(val),
            IrValue::Register(id) => self.known_constants.get(&id).copied(),
        };

        if let (Some(l), Some(r)) = (left_val, right_val) {
            if let Some(new_result) = eval_math(l, r) {
                self.known_constants.insert(result, new_result);
                return IrInstruction::Mov {
                    result,
                    src: IrValue::Constant(new_result),
                };
            }
        }

		// epxression cannot be folded
        fallback_inst
    }
}