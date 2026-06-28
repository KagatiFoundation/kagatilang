// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_mir::value::IrValueId;

pub struct EvalStack {
	values: Vec<IrValueId>
}

impl EvalStack {
	#[allow(clippy::new_without_default)]
	pub fn new() -> Self {
		Self {
			values: vec![]
		}
	}

	pub fn push(&mut self, value: IrValueId) {
		self.values.push(value);
	}

	pub fn pop(&mut self) -> IrValueId {
		assert!(!self.values.is_empty());

		self.values.pop().expect("pop() called on empty stack")
	}

	pub fn peek(&self) -> IrValueId {
		assert!(!self.values.is_empty());

		*self.values.last().expect("pop() called on empty stack")
    }

    pub fn empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn size(&self) -> usize {
        self.values.len()
    }
	
	pub fn clear(&mut self) {
		self.values.clear();
	}
}