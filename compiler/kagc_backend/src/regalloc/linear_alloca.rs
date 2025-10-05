// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_lir::mir_lowerer::VRegLiveRange;
use kagc_lir::vreg::VReg;

use crate::regalloc::Register;
use crate::regalloc::RegisterFile;
use crate::regalloc::RegClass;

#[derive(Debug)]
pub enum Location {
    Reg(Register),
    StackSlot(u32),
}

#[derive(Debug)]
pub struct Allocation {
    pub vreg: VReg,
    pub location: Location,
}

pub fn linear_scan_allocate(live_ranges: &mut [VRegLiveRange], reg_file: &RegisterFile) -> Vec<Allocation> {
    let mut allocations: Vec<Allocation> = vec![];
    let mut active: Vec<VRegLiveRange> = vec![]; // currently in registers

    // Sort live ranges by start index
    live_ranges.sort_by_key(|lr| lr.start);

    // Available registers (GPR only for now)
    let mut free_registers = reg_file.available_registers(RegClass::GPR);

    for lr in live_ranges.iter() {
        // Expire old intervals
        active.retain(|active_lr| {
            if active_lr.end < lr.start {
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
                location: Location::StackSlot(lr.vreg.0 as u32), // stack slot id
            });
        }
    }

    allocations
}

#[cfg(test)]
mod tests {
    use crate::regalloc::linear_alloca::linear_scan_allocate;
    use crate::regalloc::*;

    use kagc_lir::mir_lowerer::MIRLowerer;
    use kagc_lir::mir_lowerer::compute_vreg_live_ranges;

    use kagc_mir::builder::IRBuilder;
    use kagc_mir::function::FunctionId;
    use kagc_mir::types::IRType;
    use kagc_mir::value::IRValue;
    use kagc_mir::block::{BlockId, Terminator};

    fn mock_aarch64_reg_file() -> RegisterFile {
        let mut id = 0;

        let gprs = vec![
            Register { id: { id += 1; id - 1 }, name: "x9".to_string(),  class: RegClass::GPR },
            Register { id: { id += 1; id - 1 }, name: "x10".to_string(), class: RegClass::GPR },
            Register { id: { id += 1; id - 1 }, name: "x11".to_string(), class: RegClass::GPR },
            Register { id: { id += 1; id - 1 }, name: "x12".to_string(), class: RegClass::GPR },
            Register { id: { id += 1; id - 1 }, name: "x13".to_string(), class: RegClass::GPR },
        ];

        RegisterFile {
            registers: gprs.clone(),
            reserved: vec![
                Register { id: 200, name: "sp".to_string(), class: RegClass::GPR },
                Register { id: 201, name: "fp".to_string(), class: RegClass::GPR },
            ],
            caller_saved: gprs,
            callee_saved: vec![],
        }
    }

    #[test]
    fn test_mir_to_lir_lowerer_for_simple_function() {
        let mut builder = IRBuilder::default();
        let (_, func_entry) = builder.create_function(vec![], IRType::I64); // block id 0
        let op1 = builder.create_move(IRValue::Constant(2)); // value id 0
        let op2 = builder.create_move(IRValue::Constant(2)); // value id 1
        _ = builder.create_add(IRValue::Var(op1), IRValue::Var(op2)); // value id 2
        _ = builder.create_add(IRValue::Var(op1), IRValue::Constant(2)); // value id 3

        builder.set_terminator(
            func_entry,
            Terminator::Jump(BlockId(0))
        );

        let mut mir_lowerer = MIRLowerer::default();
        
        let module = builder.build();
        assert!(module.functions.contains_key(&FunctionId(0)));

        let func = module.functions.get(&FunctionId(0)).unwrap();
        let instrs = mir_lowerer.lower_function(func);

        let mut live_ranges = compute_vreg_live_ranges(&instrs);
        let reg_file = mock_aarch64_reg_file();
        let allocation = linear_scan_allocate(&mut live_ranges, &reg_file);
        println!("{allocation:#?}");
    }
}