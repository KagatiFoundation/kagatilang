// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_mir::value::IrValueId;
use kagc_mir::variable::IrVariableId;

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackSlotId(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct StackSlot {
    pub offset: i32,
    pub size: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StackObject {
    Value(IrValueId),
    Variable(IrVariableId),
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    objects: HashMap<StackObject, StackSlotId>,
    slots: Vec<StackSlot>,

    next_offset: i32,
    frame_size: i32,
}

impl Default for StackFrame {
    fn default() -> Self {
        Self::new()
    }
}

impl StackFrame {
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
            slots: Vec::new(),
            next_offset: 0,
            frame_size: 0,
        }
    }

    pub fn allocate(&mut self, object: StackObject, size: i32) -> StackSlotId {
        let slot = StackSlot {
            offset: self.next_offset,
            size,
        };

        self.next_offset += size;
        self.frame_size += size;

        let id = StackSlotId(self.slots.len());

        self.slots.push(slot);
        self.objects.insert(object, id);

        id
    }

	pub fn allocate_value(&mut self, value_id: IrValueId, size: i32) -> StackSlotId {
		self.allocate(StackObject::Value(value_id), size)
	}

	pub fn allocate_variable(&mut self, var_id: IrVariableId, size: i32) -> StackSlotId {
		self.allocate(StackObject::Variable(var_id), size)
	}

	pub fn offset(&self, slot_id: StackSlotId) -> Option<i32> {
		if let Some(slot) = self.slot(slot_id) {
			return Some(slot.offset);
		}
		None
	}

	pub fn offset_unchecked(&self, slot_id: StackSlotId) -> i32 {
		let slot = self.slot_unchecked(slot_id);
		slot.offset
	}

	pub fn offset_with_object_unchecked(&self, object: StackObject) -> i32 {
		let slot_id = self.slot_id_unchecked(object);
		self.offset_unchecked(slot_id)
	}

    pub fn slot_id(&self, object: StackObject) -> Option<StackSlotId> {
        self.objects.get(&object).copied()
    }

 	pub fn slot_id_unchecked(&self, object: StackObject) -> StackSlotId {
        self.objects.get(&object).copied().expect("missing slot id")
    }

    pub fn slot(&self, id: StackSlotId) -> Option<StackSlot> {
        self.slots.get(id.0).copied()
    }

    pub fn slot_unchecked(&self, id: StackSlotId) -> StackSlot {
        self.slots.get(id.0).copied().expect("stack slot not found")
    }

    pub fn get(&self, object: StackObject) -> Option<StackSlot> {
        let id = self.slot_id(object)?;
        self.slot(id)
    }

    pub fn contains(&self, object: StackObject) -> bool {
        self.objects.contains_key(&object)
    }

	pub fn increase_size(&mut self, size: i32) {
		self.frame_size += size;
	}

    pub fn size(&self) -> i32 {
		(self.frame_size + 16 - 1) & !15
    }

    pub fn clear(&mut self) {
        self.objects.clear();
        self.slots.clear();

        self.next_offset = 0;
        self.frame_size = 0;
    }
}