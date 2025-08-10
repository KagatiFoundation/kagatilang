use std::collections::{HashMap, VecDeque};

use kagc_types::LitTypeVariant;

use crate::reg::*;

pub const REG_64BIT: usize = 64;
pub const REG_32BIT: usize = 32;

impl AllocedReg {
    pub fn lit_type(&self) -> LitTypeVariant {
        match self.size {
            REG_32BIT => LitTypeVariant::I32,
            REG_64BIT => LitTypeVariant::I64,
            _ => LitTypeVariant::None
        }
    }

    pub fn early_return() -> Self {
        AllocedReg {
            idx: EARLY_RETURN,
            size: 0,
            width: RegWidth::WORD,
            status: RegStatus::Invalid
        }
    }
}

pub struct Aarch64RegManager2 {
    available_registers: Vec<bool>,
    register_map: HashMap<usize, RegState>,
    pub spilled_stack: VecDeque<usize>,
}

impl Aarch64RegManager2 {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            available_registers: vec![true; 32],
            register_map: HashMap::new(),
            spilled_stack: VecDeque::new(),
        }
    }

    pub fn name(idx: usize, size: usize) -> String {
        match size {
            32 => format!("w{idx}"),
            64 => format!("x{idx}"),
            _ => unimplemented!()
        }
    }

    fn spill_and_mark_available(&mut self, reg_to_spill: usize, alloc_size: usize) -> AllocedReg {
        self.spilled_stack.push_back(reg_to_spill);
        self.available_registers[reg_to_spill] = true; 
        self.register_map.remove(&reg_to_spill);

        let width = if alloc_size == 64 {
            RegWidth::QWORD
        }
        else {
            RegWidth::WORD
        };

        AllocedReg { 
            size: alloc_size, 
            idx: reg_to_spill,
            status: RegStatus::Spilled,
            width
        }
    }
}

impl RegManager2 for Aarch64RegManager2 {
    fn allocate_register(&mut self, alloc_size: usize) -> AllocedReg {
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

                let width = if alloc_size == 64 {
                    RegWidth::QWORD
                }
                else {
                    RegWidth::WORD
                };

                return AllocedReg {
                    idx: i,
                    size: alloc_size,
                    status: RegStatus::Alloced,
                    width
                };
            }
        }
        self.spill_register(alloc_size, None)
    }

    fn allocate_register_with_idx(&mut self, alloc_size: usize, idx: RegIdx, strat: AllocStrategy) -> AllocedReg {
        assert!((0..=32).contains(&idx));
        assert!(idx != 29 || idx != 30, "x29 and x30 registers are reserved; cannot use them");
        
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

            let width = if alloc_size == 64 {
                RegWidth::QWORD
            }
            else {
                RegWidth::WORD
            };

            return AllocedReg {
                idx,
                size: alloc_size,
                width,
                status: RegStatus::Alloced
            };
        }

        if strat == AllocStrategy::Spill {
            self.spill_register(alloc_size, Some(idx))
        }
        else {
            panic!("No registers available. Maybe try with spill strategy");
        }
    }

    fn allocate_param_register(&mut self, alloc_size: usize) -> AllocedReg {
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

                let width = if alloc_size == 64 {
                    RegWidth::QWORD
                }
                else {
                    RegWidth::WORD
                };

                return AllocedReg {
                    idx: i,
                    size: alloc_size,
                    width,
                    status: RegStatus::Alloced
                };
            }
        }
        self.spill_param_register(alloc_size, None) 
    }

    fn allocate_param_register_with_idx(&mut self, alloc_size: usize, idx: RegIdx, strat: AllocStrategy) -> AllocedReg {
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

            let width = if alloc_size == 64 {
                RegWidth::QWORD
            }
            else {
                RegWidth::WORD
            };

            return AllocedReg {
                idx,
                size: alloc_size,
                width,
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

    fn free_register(&mut self, reg: usize) {
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

    fn spill_param_register(&mut self, alloc_size: usize, idx: Option<RegIdx>) -> AllocedReg {
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

    fn restore_register(&mut self) -> usize {
        if let Some(restored_reg) = self.spilled_stack.pop_back() {
            return restored_reg;
        }
        panic!("No spilled registers to restore!");
    }

    fn reset(&mut self) {
        self.available_registers.fill(true);
        self.register_map.clear();
        self.spilled_stack.clear();
    }

    fn name(&self, idx: usize, alloc_size: usize) -> String {
        if alloc_size == 8 {
            format!("x{}", idx)
        }
        else {
            format!("w{}", idx)
        }
    }

    fn is_free(&self, idx: usize) -> bool {
        !self.register_map.contains_key(&idx)
    }
}