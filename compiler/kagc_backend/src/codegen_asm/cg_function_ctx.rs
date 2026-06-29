// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::codegen_asm::eval_stack::EvalStack;
use crate::codegen_asm::stack::StackFrame;

pub struct CodeGenFunctionContext {
	pub stack_frame: StackFrame,
	pub eval_stack: EvalStack
}

impl CodeGenFunctionContext {
	#[allow(clippy::new_without_default)]
	pub fn new() -> Self {
		Self {
			stack_frame: StackFrame::new(),
			eval_stack: EvalStack::new()
		}
	}

	pub fn reinit(&mut self) {
		self.stack_frame.clear();
		self.eval_stack.clear();
	}
}