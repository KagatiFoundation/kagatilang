// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegClass {
    /// General purpose register
    GPR,

    /// FLOATING-POINT(NOT USED)
    FPR,

    /// NOT USED
    SIMD 
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Register {
    pub id: u8,
    pub name: String,
    pub class: RegClass
}

#[derive(Debug, Clone, Default)]
pub struct RegisterFile {
    pub registers: Vec<Register>,            // all physical registers
    pub reserved: Vec<Register>,             // registers reserved (SP, FP, etc.)
    pub caller_saved: Vec<Register>,
    pub callee_saved: Vec<Register>,
}

impl RegisterFile {
    pub fn available_registers(&self, class: RegClass) -> Vec<Register> {
        self.registers
            .iter()
            .filter(|r| r.class == class && !self.reserved.contains(r))
            .cloned()
            .collect()
    }

    pub fn get_register_by_id(&self, id: u8) -> Register {
        let a = self.registers.iter().find(|reg| reg.id == id).unwrap().clone();
        a
    }
}