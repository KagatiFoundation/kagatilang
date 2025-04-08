pub mod spill_pass;

use std::collections::HashMap;

use kagc_target::reg::RegIdx;

use crate::{ir_instr::*, ir_types::{IRLitType, TempId}};

/// Live range of a temporary
pub type LiveRange = (usize, usize);

#[derive(Debug, Default)]
pub struct LivenessAnalyzer;

impl LivenessAnalyzer {
    /// Computes the live range of temporary values in an IR function.
    ///
    /// # Parameters:
    /// - `ir_func`: The function in intermediate representation (IR).
    ///
    /// # Returns:
    /// A `HashMap<usize, LiveRange>`, where:
    /// - The key is a temporary value's unique ID.
    /// - The value (`LiveRange`) is a tuple of:
    ///   - First occurrence (usize)
    ///   - Number of instructions until it becomes dead.
    ///
    /// This function finds each temporary’s first and last usage, then calculates how long 
    /// it remains live before being unused.
    pub fn analyze_fn_temps(ir_func: &IRFunc) -> HashMap<TempId, LiveRange> {
        let mut temp_liveness: HashMap<TempId, LiveRange> = HashMap::new();
        let live_set: HashMap<usize, usize> = Self::find_temp_appearances(&ir_func.body);

        for (temp_idx, first_occurrence) in &live_set {
            if let Some(last_occurrence) = Self::find_last_usage(&ir_func.body, *temp_idx) {

                let live_range: usize = Self::calc_ir_dist(
                    ir_func.body.len(), 
                    *first_occurrence, 
                    last_occurrence
                );

                temp_liveness.insert(*temp_idx, (*first_occurrence, live_range));
            }
        }
        temp_liveness
    }

    fn find_temp_appearances(body: &[IR]) -> HashMap<usize, usize> {
        let mut live_set: HashMap<usize, usize> = HashMap::new();

        for (idx, ir) in body.iter().enumerate() {
            match ir {
                IR::VarDecl(_) 
                | IR::Instr(_) => {
                    if let Some(temp) = Self::extract_temp_dest(ir) {
                        live_set.entry(temp).or_insert(idx);
                    }
                },
                
                // IR::Loop(ir_loop) => {
                //     let loop_temps: HashMap<usize, usize> = Self::find_temp_appearances(&ir_loop.body);
                //     live_set.extend(loop_temps);
                // },

                _ => ()
            }
        }

        live_set
    }

    fn find_last_usage(body: &[IR], temp_lookup: usize) -> Option<usize> {
        body.iter()
            .rev()
            .enumerate()
            .find_map(|(rev_idx, ir)| {
                match ir {
                    IR::Instr(_)
                    | IR::VarDecl(_) => {
                        if Self::is_temp_used(ir, temp_lookup) {
                            Some(rev_idx)
                        } else {
                            None
                        }
                    },

                    // IR::Loop(ir_loop) => Self::find_last_usage(&ir_loop.body, temp_lookup),

                    _ => None
                }
            })
    }

    fn extract_temp_dest(ir: &IR) -> Option<usize> {
        match ir {
            IR::Instr(instr) => {
                if let Some(IRLitType::Temp(temp_value)) = instr.dest() {
                    Some(temp_value)
                }
                else if let Some(IRLitType::AllocReg { temp, .. }) = instr.dest() {
                    Some(temp)
                }
                else {
                    None
                }
            }
            _ => None,
        }
    }

    fn is_temp_used(ir: &IR, temp_lookup: usize) -> bool {
        match ir {
            IR::Instr(instr) => {
                match instr {
                    IRInstr::Mov(dest, src) => {
                        dest.as_temp() == Some(temp_lookup) || 
                        src.as_temp() == Some(temp_lookup) ||
                        Self::is_temp_used_in_alloc_reg(temp_lookup, dest.as_alloc_reg()) ||
                        Self::is_temp_used_in_alloc_reg(temp_lookup, src.as_alloc_reg())
                    },

                    IRInstr::Add(dest, op1, op2) => {
                        [
                            dest.as_temp() == Some(temp_lookup),
                            op1.as_temp() == Some(temp_lookup),
                            op2.as_temp() == Some(temp_lookup),
                            Self::is_temp_used_in_alloc_reg(temp_lookup, dest.as_alloc_reg()),
                            Self::is_temp_used_in_alloc_reg(temp_lookup, op1.as_alloc_reg()),
                            Self::is_temp_used_in_alloc_reg(temp_lookup, op2.as_alloc_reg())
                        ].iter().any(|c| *c)
                    },

                    IRInstr::Load { dest, .. } => {
                       dest.as_temp() == Some(temp_lookup) ||
                       Self::is_temp_used_in_alloc_reg(temp_lookup, dest.as_alloc_reg())
                    },

                    IRInstr::Call { params, .. } => {
                        params.iter().any(|param| {
                            param.1.as_temp() == Some(temp_lookup) ||
                            Self::is_temp_used_in_alloc_reg(temp_lookup, param.1.as_alloc_reg())
                        })
                    },

                    IRInstr::CondJump { op1, op2, .. } => {
                        [
                            op1.as_temp() == Some(temp_lookup),
                            op2.as_temp() == Some(temp_lookup),
                            Self::is_temp_used_in_alloc_reg(temp_lookup, op1.as_alloc_reg()),
                            Self::is_temp_used_in_alloc_reg(temp_lookup, op2.as_alloc_reg())
                        ].iter().any(|c| *c)
                    }

                    _ => false
                }
            },

            IR::VarDecl(vardecl) => matches!(vardecl.value, IRLitType::Temp(t) if t == temp_lookup),

            _ => false,
        }
    }

    fn is_temp_used_in_alloc_reg(temp_lookup: usize, alloc_reg: Option<(RegIdx, usize)>) -> bool {
        if let Some((_, temp)) = alloc_reg {
            temp == temp_lookup
        }
        else {
            false
        }
    }

    fn calc_ir_dist(n: usize, p1: usize, p2_rev: usize) -> usize {
        let p2: usize = n - 1 - p2_rev;
        if p2 >= p1 {
            p2 - p1
        } else {
            panic!("End index should not be less than the start index!");
        }
    }
}