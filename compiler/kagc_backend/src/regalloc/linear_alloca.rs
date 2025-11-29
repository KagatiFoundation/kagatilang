// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_lir::vreg::VRegLiveRange;
use kagc_mir::instruction::StackSlotId;

use crate::regalloc::allocation::Allocation;
use crate::regalloc::allocation::Location;
use crate::regalloc::register::RegisterFile;
use crate::regalloc::register::RegClass;
use crate::regalloc::VRegAllocations;

pub struct LinearScanAllocator {
    pub register_file: RegisterFile,
    stack_slot: usize
}

impl LinearScanAllocator {
    pub fn new(reg_file: RegisterFile) -> Self {
        Self {
            register_file: reg_file,
            stack_slot: 0
        }
    }

    pub fn allocate(&mut self, live_ranges: &mut [VRegLiveRange]) -> VRegAllocations {
        let mut allocations: Vec<Allocation> = vec![];
        let mut active: Vec<VRegLiveRange> = vec![]; // currently in registers

        // Sort live ranges by start index
        live_ranges.sort_by_key(|lr| lr.start);

        // Available registers (GPR only for now)
        let mut free_registers = self.register_file.available_registers(RegClass::GPR);

        for lr in live_ranges.iter() {
            // Expire old intervals
            active.retain(|active_lr| {
                if active_lr.end <= lr.start {
                    // Free register
                    if let Some(allocation) = allocations.iter().find(|a| a.vreg == active_lr.vreg) {
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
                        vreg: lr.vreg,
                        location: Location::Reg(reg),
                    }
                );
                active.push(lr.clone());
            } 
            else {
                // spill
                allocations.push(Allocation {
                    vreg: lr.vreg,
                    location: self.next_stack_slot(), // stack slot id
                });
            }
        }
        VRegAllocations { allocations }
    }

    fn next_stack_slot(&mut self) -> Location {
        let nss = self.stack_slot;
        self.stack_slot += 1;
        Location::StackSlot(StackSlotId(nss))
    }
}

#[cfg(test)]
mod tests {
    use kagc_mir_lowering::MirToLirLowerer;

    use kagc_mir::mir_builder::MirBuilder;
    use kagc_mir::function::FunctionId;
    use kagc_mir::types::IRType;
    use kagc_mir::value::IRValue;
    use kagc_mir::block::{BlockId, Terminator};
    use kagc_symbol::StorageClass;

    use crate::regalloc::register::aarch64::standard_aarch64_register_file;
    use crate::regalloc::register::{RegClass, Register};
    use crate::regalloc::{Allocation, LinearScanAllocator, Location};

    fn dummy_reg(id: u8) -> Register {
        Register {
            id,
            name: format!("x{id}"),
            class: RegClass::GPR
        }
    }

    #[test]
    fn test_reg_allocation_for_simple_function() {
        let mut builder = MirBuilder::default();
        let fn_ctx = builder.create_function("test_fn".to_owned(), vec![], IRType::I64, StorageClass::GLOBAL); // block id 0
        let func_entry = fn_ctx.entry_block;
        let op1 = builder.create_move(IRValue::Constant(2)); // value id 0
        let op2 = builder.create_move(IRValue::Constant(2)); // value id 1
        _ = builder.create_add(IRValue::Var(op1), IRValue::Var(op2)); // value id 2
        _ = builder.create_add(IRValue::Var(op1), IRValue::Constant(2)); // value id 3

        _ = builder.create_move(IRValue::Constant(2));
        _ = builder.create_move(IRValue::Constant(2));
        _ = builder.create_move(IRValue::Constant(2));
        _ = builder.create_move(IRValue::Constant(2));
        _ = builder.create_move(IRValue::Constant(2));
        _ = builder.create_move(IRValue::Constant(2));

        builder.set_terminator(
            func_entry,
            Terminator::Jump(BlockId(0))
        );

        let mut mir_lowerer = MirToLirLowerer::default();
        
        let module = builder.build();
        assert!(module.functions.contains_key(&FunctionId(0)));

        let func = module.functions.get(&FunctionId(0)).unwrap();
        let func = mir_lowerer.lower_function(func);

        let mut alloca = LinearScanAllocator::new(standard_aarch64_register_file());
        let allocs = alloca.allocate(&mut func.compute_vreg_live_ranges()[..]);
        let allocs: Vec<&Allocation> = allocs.allocations.iter().collect();

        assert_eq!(allocs[0].location, Location::Reg(dummy_reg(15)));
        assert_eq!(allocs[1].location, Location::Reg(dummy_reg(14)));
        assert_eq!(allocs[2].location, Location::Reg(dummy_reg(14)));
        assert_eq!(allocs[3].location, Location::Reg(dummy_reg(14)));

        assert_eq!(allocs[4].location, Location::Reg(dummy_reg(15)));
        assert_eq!(allocs[5].location, Location::Reg(dummy_reg(14)));
        assert_eq!(allocs[6].location, Location::Reg(dummy_reg(13)));
        assert_eq!(allocs[7].location, Location::Reg(dummy_reg(12)));
        assert_eq!(allocs[8].location, Location::Reg(dummy_reg(11)));
    }
}