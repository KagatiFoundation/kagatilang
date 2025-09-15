/*
MIT License

Copyright (c) 2023 Kagati Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

use std::collections::HashMap;

use kagc_target::reg::RegIdx;

use crate::{
    ir_instr::*, 
    ir_types::{
        IRLitType, 
        TempId
    }
};

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
            if let IR::Instr(_) = ir {
                if let Some(temp) = Self::extract_temp_dest(ir) {
                    live_set.entry(temp).or_insert(idx);
                }
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
                    IR::Instr(_) => {
                        if Self::is_temp_used(ir, temp_lookup) {
                            Some(rev_idx)
                        } else {
                            None
                        }
                    },
                    _ => None
                }
            })
    }

    fn extract_temp_dest(ir: &IR) -> Option<usize> {
        match ir {
            IR::Instr(instr) => {
                if let Some(IRLitType::ExtendedTemp{ id, ..}) = instr.dest() {
                    Some(id)
                }
                else if let Some(IRLitType::Reg { temp, .. }) = instr.dest() {
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
                    IRInstr::Mov { dest, src } => Self::uses_temp_in_ir_lit(dest, temp_lookup) || Self::uses_temp_in_ir_lit(src, temp_lookup),
                    IRInstr::Add { dest, op1, op2 } => Self::is_temp_used_bin_op(dest, op1, op2, temp_lookup),
                    IRInstr::Sub { dest, op1, op2 } => Self::is_temp_used_bin_op(dest, op1, op2, temp_lookup),
                    IRInstr::Mul { dest, op1, op2 } => Self::is_temp_used_bin_op(dest, op1, op2, temp_lookup),
                    IRInstr::Div { dest, op1, op2 } => Self::is_temp_used_bin_op(dest, op1, op2, temp_lookup),

                    IRInstr::Load { dest, addr } => {
                        let mut tmp_used = Self::uses_temp_in_ir_lit(dest, temp_lookup);
                        if let IRAddr::BaseOff(base, _) = addr {
                            tmp_used |= Self::uses_temp_in_ir_lit(base, temp_lookup);
                        }
                        tmp_used
                    },

                    IRInstr::Store { src, addr } => {
                        let mut tmp_used = Self::uses_temp_in_ir_lit(src, temp_lookup);
                        if let IRAddr::BaseOff(base, ..) = addr {
                            tmp_used |= Self::uses_temp_in_ir_lit(base, temp_lookup);
                        }
                        tmp_used
                    }

                    IRInstr::Call { params, .. } => {
                        params.iter().any(|param| {
                            Self::uses_temp_in_ir_lit(&param.1, temp_lookup)
                        })
                    },

                    IRInstr::CondJump { op1, op2, .. } => {
                        [
                            op1.as_ext_temp() == Some(temp_lookup),
                            op2.as_ext_temp() == Some(temp_lookup),
                            Self::is_temp_used_in_alloc_reg(temp_lookup, op1.as_reg()),
                            Self::is_temp_used_in_alloc_reg(temp_lookup, op2.as_reg())
                        ].iter().any(|c| *c)
                    },

                    IRInstr::MemCpy { dest } => {
                        Self::uses_temp_in_ir_lit(dest, temp_lookup)
                    }

                    _ => false
                }
            },

            _ => false,
        }
    }

    fn uses_temp_in_ir_lit(ir_lit: &IRLitType, temp: usize) -> bool {
        match ir_lit {
            IRLitType::ExtendedTemp{ id, .. } => *id == temp,
            IRLitType::Reg { temp: id, .. } => *id == temp,
            _ => false
        }
    }

    fn is_temp_used_bin_op(dest: &IRLitType, op1: &IRLitType, op2: &IRLitType, temp_lookup: usize) -> bool {
        [
            dest.as_ext_temp() == Some(temp_lookup),
            op1.as_ext_temp() == Some(temp_lookup),
            op2.as_ext_temp() == Some(temp_lookup),
            Self::is_temp_used_in_alloc_reg(temp_lookup, dest.as_reg()),
            Self::is_temp_used_in_alloc_reg(temp_lookup, op1.as_reg()),
            Self::is_temp_used_in_alloc_reg(temp_lookup, op2.as_reg())
        ].iter().any(|c| *c) 
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