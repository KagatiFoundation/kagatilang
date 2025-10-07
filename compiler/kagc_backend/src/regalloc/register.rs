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
    pub registers:      Vec<Register>,
    pub reserved:       Vec<Register>,
    pub caller_saved:   Vec<Register>,
    pub callee_saved:   Vec<Register>,
    pub allocatable:    Vec<Register>,
    pub scratch:        Vec<Register>
}

impl RegisterFile {
    pub fn available_registers(&self, class: RegClass) -> Vec<Register> {
        self.allocatable
            .iter()
            .filter(|r| {
                let mut yes = r.class == class && !self.reserved.contains(r);
                yes = yes && !self.scratch.iter().any(|scratch| scratch.id == r.id);
                yes
            })
            .cloned()
            .collect()
    }

    pub fn get_register_by_id(&self, id: u8) -> Register {
        let a = self.registers.iter().find(|reg| reg.id == id).unwrap().clone();
        a
    }
}

pub mod aarch64 {
    use crate::regalloc::register::RegisterFile;
    use crate::regalloc::register::Register;
    use crate::regalloc::register::RegClass;

    pub fn standard_aarch64_register_file() -> RegisterFile {
        // Helper closure to make register construction cleaner
        let make_reg = |id: u8, name: &str| Register {
            id,
            name: name.to_string(),
            class: RegClass::GPR,
        };

        // All GPRs (x0 - x30)
        let all_gprs: Vec<Register> = (0..=30)
            .map(|id| make_reg(id, &format!("x{}", id)))
            .collect();

        // Reserved registers
        let reserved = vec![
            make_reg(16, "x16"), // IP0 - intra-procedure scratch (reserved for linker)
            make_reg(17, "x17"), // IP1 - scratch
            make_reg(18, "x18"), // platform register (TLS pointer)
            make_reg(29, "fp"),  // frame pointer
            make_reg(30, "lr"),  // link register
        ];

        // Caller-saved (volatile)
        // Typically x0–x15
        let caller_saved: Vec<Register> = (0..=15).map(|id| make_reg(id, &format!("x{}", id))).collect();

        // Callee-saved (non-volatile)
        // Typically x19–x28
        let callee_saved: Vec<Register> = (19..=28).map(|id| make_reg(id, &format!("x{}", id))).collect();

        // Registers that are actually allocatable
        let allocatable: Vec<Register> = (9..=15).map(|id| make_reg(id, &format!("x{}", id))).collect();

        // === Scratch register for codegen ===
        let scratch = vec![
            make_reg(9, "x9"),
            make_reg(10, "x10")
        ];

        RegisterFile {
            registers: all_gprs,
            reserved,
            caller_saved,
            callee_saved,
            scratch,
            allocatable,
        }
    }
}