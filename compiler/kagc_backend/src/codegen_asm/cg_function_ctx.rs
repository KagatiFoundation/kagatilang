// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_mir::function::IrFunction;
use kagc_mir::instruction::IrInstruction;

use crate::codegen_asm::stack::StackFrame;

pub struct CodeGenFunctionContext {
	pub(crate) id: i64,
	pub(crate) stack_frame: StackFrame,
	pub(crate) is_leaf: bool,
}

impl CodeGenFunctionContext {
	#[allow(clippy::new_without_default)]
	pub fn new() -> Self {
		Self {
			id: -1,
			stack_frame: StackFrame::new(),
			is_leaf: true,
		}
	}

    pub fn compute_is_leaf(&mut self, lir_func: &IrFunction) {
        for block in lir_func.blocks.values() {
            for instr in &block.instructions {
                if matches!(instr, IrInstruction::Call { .. } | IrInstruction::CallBuiltin { .. }) {
					self.is_leaf = false;
					break;
				}
            }
        }
    }

	pub fn reinit(&mut self) {
		self.stack_frame.clear();
	}
}