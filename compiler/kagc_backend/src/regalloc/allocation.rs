// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_lir::vreg::VReg;

use crate::regalloc::register::Register;

#[derive(Debug, Clone)]
pub enum Location {
    Reg(Register),
    StackSlot(usize),
}

#[derive(Debug)]
pub struct Allocation {
    pub vreg: VReg,
    pub location: Location,
}

pub struct VRegAllocations {
    pub allocations: Vec<Allocation>
}

impl VRegAllocations {
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