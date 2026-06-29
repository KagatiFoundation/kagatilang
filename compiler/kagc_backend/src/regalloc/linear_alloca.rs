// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::codegen_asm::stack::LiveRange;
use kagc_mir::value::IrValueId;
use kagc_mir::value::StackSlotId;

use crate::regalloc::register::RegisterFile;
use crate::regalloc::register::RegClass;

use crate::regalloc::register::Register;

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub enum Location {
    Reg(Register),
    StackSlot(StackSlotId),
}

#[derive(Debug)]
pub struct Allocation {
    pub value_id: IrValueId,
    pub location: Location,
}

pub struct RegAllocations {
    pub allocations: Vec<Allocation>
}

impl RegAllocations {
    pub fn stack_usage(&self) -> usize {
        let mut stack_size = 0;
        for alloc in self.allocations.iter() {
            if let Location::StackSlot(_) = alloc.location {
                stack_size += 8;
            }
        }
        stack_size
    }
}

pub struct LinearScanAllocator {
    pub register_file: RegisterFile,
    stack_slot: i64
}

impl LinearScanAllocator {
    pub fn new(reg_file: RegisterFile) -> Self {
        Self {
            register_file: reg_file,
            stack_slot: 0
        }
    }

    pub fn allocate(&mut self, live_ranges: &mut [LiveRange]) -> RegAllocations {
        let mut allocations: Vec<Allocation> = vec![];
        let mut active: Vec<LiveRange> = vec![]; // currently in registers

        // Sort live ranges by start index
        live_ranges.sort_by_key(|lr| lr.start);

        // Available registers (GPR only for now)
        let mut free_registers = self.register_file.available_registers(RegClass::GPR);

        for lr in live_ranges.iter() {
            // Expire old intervals
            active.retain(|active_lr| {
                if active_lr.end <= lr.start {
                    // Free register
                    if let Some(allocation) = allocations.iter().find(|a| a.value_id == active_lr.value) {
                        if let Location::Reg(reg) = &allocation.location {
                            free_registers.push(reg.clone());
                        }
                    }
                    false
                } else {
                    true
                }
            });

            // Assign a register
            if let Some(reg) = free_registers.pop() {
                allocations.push(
                    Allocation {
                        value_id: lr.value,
                        location: Location::Reg(reg),
                    }
                );
                active.push(*lr);
            } 
            else {
                // spill
                allocations.push(Allocation {
                    value_id: lr.value,
                    location: self.next_stack_slot(), // stack slot id
                });
            }
        }

        RegAllocations { allocations }
    }

    fn next_stack_slot(&mut self) -> Location {
        let nss = self.stack_slot;
        self.stack_slot += 1;
        Location::StackSlot(StackSlotId(nss))
    }
}