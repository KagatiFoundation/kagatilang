// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use crate::reg::RegAllocError;
use crate::reg::RegSize;
use crate::reg::RegStatus;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum X86RegName {
    RAX,
    RBX,
    RCX,
    RDX,
    RSP,
    RBP,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

/// X86 register
#[derive(Debug, Clone, Copy)]
pub struct X86Reg {
    /// The size of the allocated register (e.g., 32-bit, 64-bit).
    pub size: RegSize,

    /// The (numeric) name of the register.
    pub name: X86RegName,

    /// The current status of the allocated register.
    pub status: RegStatus,
}

impl X86Reg {
    /// Return textual name depending on requested size (in bytes).
    /// size: 8 => "rax", 4 => "eax", 2 => "ax", 1 => "al"
    pub fn name(&self) -> &'static str {
        match (self.name, self.size) {
            (X86RegName::RAX, 8) => "rax",
            (X86RegName::RAX, 4) => "eax",
            (X86RegName::RAX, 2) => "ax",
            (X86RegName::RAX, 1) => "al",

            (X86RegName::RBX, 8) => "rbx",
            (X86RegName::RBX, 4) => "ebx",
            (X86RegName::RBX, 2) => "bx",
            (X86RegName::RBX, 1) => "bl",

            (X86RegName::RCX, 8) => "rcx",
            (X86RegName::RCX, 4) => "ecx",
            (X86RegName::RCX, 2) => "cx",
            (X86RegName::RCX, 1) => "cl", // note: cl is used for shifts

            (X86RegName::RDX, 8) => "rdx",
            (X86RegName::RDX, 4) => "edx",
            (X86RegName::RDX, 2) => "dx",
            (X86RegName::RDX, 1) => "dl",

            (X86RegName::RSP, 8) => "rsp",
            (X86RegName::RSP, 4) => "esp",
            (X86RegName::RSP, 2) => "sp",
            (X86RegName::RSP, 1) => "spl",

            (X86RegName::RBP, 8) => "rbp",
            (X86RegName::RBP, 4) => "ebp",
            (X86RegName::RBP, 2) => "bp",
            (X86RegName::RBP, 1) => "bpl",

            (X86RegName::R10, 8) => "r10",
            (X86RegName::R10, 4) => "r10d",
            (X86RegName::R10, 2) => "r10w",
            (X86RegName::R10, 1) => "r10b",

            (X86RegName::R11, 8) => "r11",
            (X86RegName::R11, 4) => "r11d",
            (X86RegName::R11, 2) => "r11w",
            (X86RegName::R11, 1) => "r11b",

            (X86RegName::R12, 8) => "r12",
            (X86RegName::R12, 4) => "r12d",
            (X86RegName::R12, 2) => "r12w",
            (X86RegName::R12, 1) => "r12b",

            (X86RegName::R13, 8) => "r13",
            (X86RegName::R13, 4) => "r13d",
            (X86RegName::R13, 2) => "r13w",
            (X86RegName::R13, 1) => "r13b",

            (X86RegName::R14, 8) => "r14",
            (X86RegName::R14, 4) => "r14d",
            (X86RegName::R14, 2) => "r14w",
            (X86RegName::R14, 1) => "r14b",

            (X86RegName::R15, 8) => "r15",
            (X86RegName::R15, 4) => "r15d",
            (X86RegName::R15, 2) => "r15w",
            (X86RegName::R15, 1) => "r15b",

            // fallback: default to 8-byte name if unknown size
            (r, _) => match r {
                X86RegName::RAX => "rax",
                X86RegName::RBX => "rbx",
                X86RegName::RCX => "rcx",
                X86RegName::RDX => "rdx",
                X86RegName::RSP => "rsp",
                X86RegName::RBP => "rbp",
                X86RegName::R10 => "r10",
                X86RegName::R11 => "r11",
                X86RegName::R12 => "r12",
                X86RegName::R13 => "r13",
                X86RegName::R14 => "r14",
                X86RegName::R15 => "r15",
            },
        }
    }
}

pub type X86RegAllocRes = Result<X86Reg, RegAllocError>;

#[derive(Default, Debug)]
pub struct X86RegMgr {
    /// the pool order we will try for allocations (stable order)
    allocatable_registers: Vec<X86RegName>,

    /// set of currently available (free) registers
    available_registers: HashSet<X86RegName>,

    /// registers that must never be allocated by the allocator
    reserved_registers: HashSet<X86RegName>,

    /// mapping of currently allocated registers -> AllocedReg
    register_map: HashMap<X86RegName, X86Reg>,

    /// stack of spilled registers for bookkeeping
    spilled_stack: VecDeque<X86RegName>,
}

impl X86RegMgr {
    pub fn new() -> Self {
        // allocatable pool: rbx, r10..r15 (callee-saved + temporaries)
        let allocatable = vec![
            X86RegName::RBX,
            X86RegName::R10,
            X86RegName::R11,
            X86RegName::R12,
            X86RegName::R13,
            X86RegName::R14,
            X86RegName::R15,
        ];

        // initially all allocatable regs are available
        let available = allocatable.iter().cloned().collect();

        let mut reserved = HashSet::new();
        reserved.insert(X86RegName::RSP);
        reserved.insert(X86RegName::RBP);

        // Note: we're intentionally *not* reserving RAX/RCX/RDX here in this set,
        // but those are NOT part of allocatable pool.

        Self {
            allocatable_registers: allocatable,
            available_registers: available,
            reserved_registers: reserved,
            register_map: HashMap::new(),
            spilled_stack: VecDeque::new(),
        }
    }

    /// Try to allocate any free register from the allocatable pool.
    pub fn allocate(&mut self, size: usize) -> X86RegAllocRes {
        for &candidate in &self.allocatable_registers {
            if self.available_registers.contains(&candidate) {
                return self.allocate_reg(candidate, size);
            }
        }
        Err(RegAllocError::ReservedRegister)
    }

    /// Try to allocate a specific register.
    pub fn allocate_reg(&mut self, name: X86RegName, size: usize) -> X86RegAllocRes {
        if self.reserved_registers.contains(&name) {
            return Err(RegAllocError::ReservedRegister);
        }

        if !self.allocatable_registers.contains(&name) {
            // Not in our allocatable pool
            return Err(RegAllocError::NoRegisterAvailable);
        }

        if self.available_registers.remove(&name) {
            let alloc = X86Reg {
                name,
                size,
                status: RegStatus::Alloced,
            };
            self.register_map.insert(name, alloc);
            Ok(alloc)
        } else {
            self.spill_and_mark_available(name, size)
        }
    }

    pub fn free_reg(&mut self, name: X86RegName) {
        if self.register_map.remove(&name).is_some() {
            self.available_registers.insert(name);
        }
    }

    pub fn spill_and_mark_available(&mut self, name: X86RegName, size: usize) -> X86RegAllocRes {
        if self.register_map.contains_key(&name) {
            self.register_map.remove(&name);
            self.spilled_stack.push_back(name);
            self.available_registers.insert(name);

            Ok(X86Reg {
                name,
                size,
                status: RegStatus::Spilled,
            })
        } else {
            Err(RegAllocError::ReservedRegister)
        }
    }

    pub fn allocate_fixed_register(&mut self, name: X86RegName, size: usize) -> X86RegAllocRes {
        if self.reserved_registers.contains(&name) {
            return Err(RegAllocError::ReservedRegister);
        }

        if self.register_map.contains_key(&name) {
            let _ = self.spill_and_mark_available(name, size);
        }
        
        self.available_registers.remove(&name);
        let alloc = X86Reg {
            name,
            size,
            status: RegStatus::Alloced,
        };
        self.register_map.insert(name, alloc);
        Ok(alloc)
    }

    pub fn restore_spilled(&mut self) -> Option<X86RegName> {
        if let Some(r) = self.spilled_stack.pop_back() {
            self.available_registers.remove(&r); // now occupied by restored value
            let alloc = X86Reg {
                name: r,
                size: 8,
                status: RegStatus::Alloced,
            };
            self.register_map.insert(r, alloc);
            Some(r)
        } else {
            None
        }
    }

    pub fn is_free(&self, name: X86RegName) -> bool {
        self.available_registers.contains(&name)
    }
}


#[cfg(test)]
mod tests {
    use crate::asm::x86::{X86RegMgr, X86RegName};
    use crate::reg::{RegStatus, REG_SIZE_8};

    #[test]
    fn test_simple_specific_allocation() {
        let mut alloc = X86RegMgr::new();
        let reg = alloc.allocate_reg(X86RegName::R10, REG_SIZE_8).ok().unwrap();
        assert_eq!(reg.status, RegStatus::Alloced);
        let reg = alloc.allocate_reg(X86RegName::R10, REG_SIZE_8).ok().unwrap();
        assert_eq!(reg.status, RegStatus::Spilled);
    }

   #[test]
    fn test_simple_non_specific_allocation() {
        let mut alloc = X86RegMgr::new();
        let reg = alloc.allocate(REG_SIZE_8).ok().unwrap();
        assert_eq!(reg.status, RegStatus::Alloced);
        assert_eq!(reg.name, X86RegName::RBX);
        
        let reg = alloc.allocate(REG_SIZE_8).ok().unwrap();
        assert_eq!(reg.name, X86RegName::R10);

        let reg = alloc.allocate(REG_SIZE_8).ok().unwrap();
        assert_eq!(reg.name, X86RegName::R11);
    }

    #[test]
    #[should_panic]
    fn test_alloc_reserved_register() {
        let mut alloc = X86RegMgr::new();
        let _ = alloc.allocate_reg(X86RegName::RAX, REG_SIZE_8).ok().unwrap(); // RAX is not alloctable
    }

    #[test]
    fn test_free() {
        let mut alloc = X86RegMgr::new();
        let _ = alloc.allocate_reg(X86RegName::R10, REG_SIZE_8).ok().unwrap();
        alloc.free_reg(X86RegName::R10);
        assert_eq!(alloc.register_map.len(), 0);
        assert!(alloc.available_registers.contains(&X86RegName::R10));
    }
}