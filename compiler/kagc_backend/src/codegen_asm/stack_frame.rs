// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_mir::value::IrValueId;

use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub struct StackSlot {
	pub offset: i32,
	pub size: i32
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    offsets: HashMap<IrValueId, StackSlot>,
    next_offset: i32,
    frame_size: i32,
}

impl StackFrame {
	#[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            offsets: HashMap::new(),
            next_offset: 0,
            frame_size: 0,
        }
    }

    pub fn allocate(&mut self, value: IrValueId, size: i32) -> i32 {
        self.next_offset += size;

        let offset = self.next_offset;
        self.offsets.insert(value, StackSlot { offset, size });

        if self.next_offset > self.frame_size {
            self.frame_size = self.next_offset;
        }

        offset
    }

    pub fn get_offset(&self, value: IrValueId) -> Option<StackSlot> {
        self.offsets.get(&value).copied()
    }

    pub fn contains(&self, value: IrValueId) -> bool {
        self.offsets.contains_key(&value)
    }

    pub fn size(&self) -> i32 {
        self.frame_size
    }

	pub fn clear(&mut self) {
		self.offsets.clear();
		self.frame_size = 0;
		self.next_offset = 0;
	}
}