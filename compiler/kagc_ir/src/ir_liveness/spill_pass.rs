use std::collections::HashMap;

use kagc_target::reg::*;

use crate::{ir_instr::*, ir_liveness::LiveRange, ir_types::*};

use super::LivenessAnalyzer;

#[derive(Debug, Default)]
pub struct SpillCompilerPass;

impl SpillCompilerPass {
    pub fn analyze_fn_for_spills(ir_func: &mut IRFunc) {
        let mut live_info: HashMap<RegIdx, usize> = HashMap::new();
        let temp_liveness: HashMap<TempId, LiveRange> = LivenessAnalyzer::analyze_fn_temps(ir_func);
        let mut stores_and_loads: Vec<(usize, IRInstr)> = vec![];

        for (index, body_ir) in ir_func.body.iter().enumerate() {
            match body_ir {
                IR::Instr(instr) => {
                    let reg_use = Self::extract_reg_from_instr(instr);
                    if let Some((reg, temp)) = reg_use {
                        if let Some(existing_temp) = live_info.get(&reg) {
                            if temp == *existing_temp {
                                // temps being equal suggests that some temporary is trying to allocate 
                                // a register twice--an impossible task LOL
                                panic!("Impossible!");
                            }
                            else {
                                // else check if the existing temp will be alive after this instruction. 
                                // if it is, then spill the register if needed
                                if let Some((temp_start, temp_range)) = temp_liveness.get(existing_temp) {
                                    let temp_end_pos: usize = *temp_start + *temp_range;

                                    // this temp will be used again; spill the register
                                    if temp_end_pos > index {
                                        stores_and_loads.push(
                                            (
                                                index,
                                                IRInstr::Store {
                                                    src: IRLitType::Reg { idx: reg, size: 64 },
                                                    addr: IRAddr::StackOff(index)
                                                }
                                            )
                                        );

                                        // free the register after spilling
                                        live_info.remove(&reg);
                                    }
                                }
                            }
                        }
                        else {
                            live_info.insert(reg, temp);
                        }
                    }
                },

                _ => continue
            }
        }

        println!("{:#?}", stores_and_loads);
    }

    fn extract_reg_from_instr(instr: &IRInstr) -> Option<(RegIdx, usize)> {
        match instr {
            IRInstr::Mov { dest, .. } => dest.as_alloc_reg(),

            IRInstr::Add { dest, .. } => dest.as_alloc_reg(),
            
            IRInstr::Sub { dest, .. } => dest.as_alloc_reg(),
            
            IRInstr::Mul { dest, .. } => dest.as_alloc_reg(),
            
            IRInstr::Div { dest, .. } => dest.as_alloc_reg(),

            IRInstr::Load { dest, .. } => dest.as_alloc_reg(),

            IRInstr::Call { params, .. } => {
                if let Some(param_use) = params.iter().find(|param| param.1.is_alloc_reg()) {
                    param_use.1.as_alloc_reg()
                }
                else {
                    None
                }
            },

            _ => None
        }
    }
}