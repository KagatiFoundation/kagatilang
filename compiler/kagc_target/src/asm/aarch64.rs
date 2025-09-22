// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;
use std::collections::VecDeque;
use crate::reg::*;

pub const REG_64BIT: usize = 64;
pub const REG_32BIT: usize = 32;

impl AllocedReg {
    pub fn name_aarch64(&self) -> String {
        if self.size == REG_SIZE_4 {
            format!("w{}", self.idx)
        }
        else {
            format!("x{}", self.idx)
        }
    }
}

#[derive(Debug)]
pub struct Aarch64RegMgr {
    available_registers: Vec<bool>,
    register_map: HashMap<usize, RegState>,
    pub spilled_stack: VecDeque<usize>,
}

impl Aarch64RegMgr {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            available_registers: vec![true; 32],
            register_map: HashMap::new(),
            spilled_stack: VecDeque::new(),
        }
    }

    fn spill_and_mark_available(&mut self, reg_to_spill: usize, alloc_size: usize) -> AllocedReg {
        self.spilled_stack.push_back(reg_to_spill);
        self.available_registers[reg_to_spill] = true; 
        self.register_map.remove(&reg_to_spill);

        AllocedReg { 
            size: alloc_size, 
            idx: reg_to_spill,
            status: RegStatus::Spilled,
        }
    }

    pub fn is_caller_saved(idx: RegIdx) -> bool {
        idx <= 17
    }

    pub fn caller_saved_regs(size: RegSize) -> Vec<AllocedReg> {
        if size == REG_SIZE_8 {
            (0..=18).map(|idx| {
                AllocedReg {
                    idx: idx as usize,
                    size: REG_SIZE_8,
                    status: RegStatus::Invalid
                }
            }).collect::<Vec<AllocedReg>>()
        }
        else {
            (0..=18).map(|idx| {
                AllocedReg {
                    idx: idx as usize,
                    size: REG_SIZE_4,
                    status: RegStatus::Invalid
                }
            }).collect::<Vec<AllocedReg>>()
        }
    }

    pub fn allocate_register(&mut self, alloc_size: usize) -> AllocedReg {
        for i in 8..32 {
            // x29 and x30 are reserved registers
            if i == 29 || i == 30 {
                continue;
            }

            if self.available_registers[i] {
                self.available_registers[i] = false;

                self.register_map.insert(
                    i, 
                    RegState { 
                        idx: i, 
                        curr_alloced_size: alloc_size, 
                        status: RegStatus::Alloced
                    }
                );

                return AllocedReg {
                    idx: i,
                    size: alloc_size,
                    status: RegStatus::Alloced,
                };
            }
        }
        self.spill_register(alloc_size, None)
    }

    pub fn allocate_register_with_idx(&mut self, alloc_size: usize, idx: RegIdx, strat: AllocStrategy) -> AllocedReg {
        if !(0..=32).contains(&idx) {
            panic!("Index '{idx}' is not a valid register index!");
        }

        if idx == 29 || idx == 30 {
            panic!("x29 and x30 registers are reserved; cannot use them");
        }
        
        if self.available_registers[idx] {
            self.available_registers[idx] = false;

            self.register_map.insert(
                idx, 
                RegState { 
                    idx, 
                    curr_alloced_size: alloc_size, 
                    status: RegStatus::Alloced
                }
            );

            let a = AllocedReg {
                idx,
                size: alloc_size,
                status: RegStatus::Alloced
            };
            return a;
        }

        if strat == AllocStrategy::Spill {
            self.spill_register(alloc_size, Some(idx))
        }
        else {
            panic!("No registers available. Maybe try with spill strategy");
        }
    }

    pub fn allocate_param_register(&mut self, alloc_size: usize) -> AllocedReg {
        for i in 0..8 {
            if self.available_registers[i] {
                self.available_registers[i] = false;

                self.register_map.insert(
                    i, 
                    RegState { 
                        idx: i, 
                        curr_alloced_size: alloc_size, 
                        status: RegStatus::Alloced
                    }
                );

                return AllocedReg {
                    idx: i,
                    size: alloc_size,
                    status: RegStatus::Alloced
                };
            }
        }
        self.spill_param_register(alloc_size, None) 
    }

    pub fn allocate_param_register_with_idx(&mut self, alloc_size: usize, idx: RegIdx, strat: AllocStrategy) -> AllocedReg {
        assert!((0..=7).contains(&idx));
        
        if self.available_registers[idx] {
            self.available_registers[idx] = false;

            self.register_map.insert(
                idx, 
                RegState { 
                    idx, 
                    curr_alloced_size: alloc_size, 
                    status: RegStatus::Alloced
                }
            );

            return AllocedReg {
                idx,
                size: alloc_size,
                status: RegStatus::Alloced
            };
        }

        if strat == AllocStrategy::Spill {
            self.spill_param_register(alloc_size, Some(idx))
        }
        else {
            panic!("No parameter registers available. Maybe try with spill strategy");
        }
    }

    pub fn allocate_callee_saved_register(&mut self, size: RegSize) -> AllocedReg {
        for idx in 19..28 {
            if self.available_registers[idx] {
                self.available_registers[idx] = false;
                self.register_map.insert(
                    idx, 
                    RegState { 
                        idx, 
                        curr_alloced_size: size, 
                        status: RegStatus::Alloced
                    }
                );

                return AllocedReg {
                    idx,
                    size,
                    status: RegStatus::Alloced
                };
            }
        }
        self.spill_register(size, Some(19))
    }

    pub fn free_register(&mut self, reg: usize) {
        if let Some((&index, _)) = self.register_map.iter().find(|(_, reg_state)| reg_state.idx == reg) {
            self.available_registers[index] = true;
            self.register_map.remove(&index);
        }
    }

    fn spill_register(&mut self, alloc_size: usize, idx: Option<RegIdx>) -> AllocedReg {
        assert!(!matches!(idx, Some(29) | Some(30)));

        if let Some(i) = idx {
            if self.register_map.contains_key(&i) {
                return self.spill_and_mark_available(i, alloc_size);
            }
            panic!("No general-purpose registers available and stack is full!");
        }
        else {
            for i in 8..32 {
                if self.register_map.contains_key(&i) {
                    return self.spill_and_mark_available(i, alloc_size);
                }
            }
            panic!("No general-purpose registers available and stack is full!");
        }
    }

    pub fn spill_param_register(&mut self, alloc_size: usize, idx: Option<RegIdx>) -> AllocedReg {
        if let Some(i) = idx {
            if self.register_map.contains_key(&i) {
                return self.spill_and_mark_available(i, alloc_size);
            }

            panic!("No general-purpose registers available and stack is full!");
        }
        else {
            for i in 0..8 {
                if self.register_map.contains_key(&i) {
                    return self.spill_and_mark_available(i, alloc_size);
                }
            }
            panic!("No general-purpose registers available and stack is full!");
        }
    }

    pub fn restore_register(&mut self) -> Option<usize> {
        self.spilled_stack.pop_back()
    }

    pub fn reset(&mut self) {
        self.available_registers.fill(true);
        self.register_map.clear();
        self.spilled_stack.clear();
    }

    pub fn name(&self, idx: usize, alloc_size: usize) -> String {
        if alloc_size == 8 {
            format!("x{}", idx)
        }
        else {
            format!("w{}", idx)
        }
    }

    pub fn is_free(&self, idx: usize) -> bool {
        !self.register_map.contains_key(&idx)
    }
}

#[cfg(test)]
mod tests {
    use crate::asm::aarch64::*;

    fn setup_manager() -> Aarch64RegMgr {
        Aarch64RegMgr::new()
    }

    #[test]
    fn test_allocate_register_basic() {
        let mut mgr = setup_manager();
        let reg = mgr.allocate_register(8);
        assert_eq!(reg.size, 8);
        assert_eq!(reg.status, RegStatus::Alloced);

        assert!(mgr.register_map.contains_key(&reg.idx));
        assert!(!mgr.available_registers[reg.idx]);
    }

    #[test]
    fn test_allocate_param_register_basic() {
        let mut mgr = setup_manager();
        let reg = mgr.allocate_param_register(4);
        assert_eq!(reg.size, 4);
        assert_eq!(reg.status, RegStatus::Alloced);

        assert!(mgr.register_map.contains_key(&reg.idx));
        assert!(!mgr.available_registers[reg.idx]);
        assert!(reg.idx < 8); // param regs
    }

    #[test]
    fn test_allocate_specific_register() {
        let mut mgr = setup_manager();
        let reg = mgr.allocate_register_with_idx(8, 10, AllocStrategy::NoSpill);
        assert_eq!(reg.idx, 10);
        assert_eq!(reg.size, 8);
        assert!(!mgr.is_free(10));
    }

    #[test]
    fn test_free_register() {
        let mut mgr = setup_manager();
        let reg = mgr.allocate_register(8);
        mgr.free_register(reg.idx);

        assert!(mgr.is_free(reg.idx));
        assert!(mgr.available_registers[reg.idx]);
        assert!(!mgr.register_map.contains_key(&reg.idx));
    }

    #[test]
    fn test_spill_and_restore_register() {
        let mut mgr = setup_manager();
        let reg1 = mgr.allocate_register(8);

        let reg2 = mgr.spill_register(8, Some(reg1.idx));
        assert_eq!(reg1.idx, reg2.idx); // spilled + reused
        assert!(mgr.is_free(reg1.idx));

        mgr.spilled_stack.push_back(reg1.idx);
        let restored = mgr.restore_register();
        assert_eq!(restored, Some(reg1.idx));
    }

    #[test]
    fn test_reset_manager() {
        let mut mgr = setup_manager();
        let reg = mgr.allocate_register(8);
        assert!(!mgr.is_free(reg.idx));

        mgr.reset();
        for i in 0..32 {
            assert!(mgr.is_free(i));
        }
        assert!(mgr.register_map.is_empty());
        assert!(mgr.spilled_stack.is_empty());
    }

    #[test]
    fn test_name_generation() {
        let mgr = setup_manager();
        assert_eq!(mgr.name(5, 8), "x5");
        assert_eq!(mgr.name(5, 4), "w5");
    }

    #[test]
    #[should_panic]
    fn test_reserved_register_allocation_panics() {
        let mut mgr = setup_manager();
        mgr.allocate_register_with_idx(8, 29, AllocStrategy::NoSpill); // x29 is reserved; cannot use it
    }
}