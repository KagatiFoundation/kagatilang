// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;
use std::rc::Rc;
use crate::ir_operands::IRAddr;
use crate::ir_instr::IRFunc;
use crate::ir_instr::IRInstr;
use crate::ir_instr::IR;
use crate::ir_operands::IROperand;
use crate::ir_operands::TempId;

/// Live range of a temporary
pub type LiveRange = (usize, usize);

#[derive(Debug, Default)]
pub struct LivenessAnalyzer {
    pub liveness_info: Rc<HashMap<TempId, LiveRange>>
}

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
    pub fn analyze_fn_temps(&mut self, ir_func: &IRFunc) {
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
        self.liveness_info = Rc::new(temp_liveness);
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
                if let Some(IROperand::Temp{ id, ..}) = instr.dest() {
                    Some(id)
                }
                else if let Some(IROperand::Return { temp, .. }) = instr.dest() {
                    Some(temp)
                }
                else if let Some(IROperand::Return { temp, .. }) = instr.dest() {
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
                            Self::is_temp_used_in_ret_out(temp_lookup, op1),
                            Self::is_temp_used_in_arg_out(temp_lookup, op1),
                            Self::is_temp_used_in_ret_out(temp_lookup, op2),
                            Self::is_temp_used_in_arg_out(temp_lookup, op2),
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

    fn uses_temp_in_ir_lit(ir_lit: &IROperand, temp: usize) -> bool {
        match ir_lit {
            IROperand::Temp { id, .. } => *id == temp,
            IROperand::CallArg { temp: id, .. } => *id == temp,
            IROperand::Return { temp: id, .. } => *id == temp,
            _ => false
        }
    }

    fn is_temp_used_bin_op(dest: &IROperand, op1: &IROperand, op2: &IROperand, temp_lookup: usize) -> bool {
        [
            dest.as_ext_temp() == Some(temp_lookup),
            op1.as_ext_temp() == Some(temp_lookup),
            op2.as_ext_temp() == Some(temp_lookup),
            Self::is_temp_used_in_arg_out(temp_lookup, dest),
            Self::is_temp_used_in_ret_out(temp_lookup, dest),
            Self::is_temp_used_in_arg_out(temp_lookup, op1),
            Self::is_temp_used_in_ret_out(temp_lookup, op1),
            Self::is_temp_used_in_arg_out(temp_lookup, op2),
            Self::is_temp_used_in_ret_out(temp_lookup, op2),
        ].iter().any(|c| *c) 
    }

    fn is_temp_used_in_arg_out(temp_lookup: usize, arg_out: &IROperand) -> bool {
        if let IROperand::CallArg { temp, .. } = arg_out {
            *temp == temp_lookup
        }
        else {
            false
        }
    }

    fn is_temp_used_in_ret_out(temp_lookup: usize, arg_out: &IROperand) -> bool {
        if let IROperand::Return { temp, .. } = arg_out {
            *temp == temp_lookup
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
            panic!("End index cannot be less than the start index!");
        }
    }

    pub fn is_temp_alive_after(&self, temp_lookup: TempId, n_instrs: usize) -> bool {
        let (start, end) = self.liveness_info
            .get(&temp_lookup)
            .unwrap_or_else(|| panic!("Untracked temp id '{temp_lookup}'"));

        (*start + *end) <= n_instrs
    }
}

#[cfg(test)]
mod tests {
    use kagc_symbol::StorageClass;
    use kagc_target::reg::REG_SIZE_8;

    use crate::ir_instr::*;
    use crate::ir_liveness::LivenessAnalyzer;
    use crate::ir_operands::*;

    fn next_temp(temp: &mut usize) -> usize {
        let curr = *temp;
        *temp += 1;
        curr
    }

    #[test]
    fn test_mini_liveness_analyzer() {
        let mut temp: usize = 0;
        let fn_ir = IRFunc {
            name: "f".to_string(),
            class: StorageClass::GLOBAL,
            is_leaf: true,
            id: 1,
            scope_id: 0,
            params: vec![],
            body: vec![
                IR::Instr(IRInstr::Mov { 
                    // temp 0
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    src: IROperand::Const(IRImmVal::Int32(32))
                }),
                IR::Instr(IRInstr::Mov { 
                    // temp 1
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    src: IROperand::Const(IRImmVal::Int32(32))
                }),
                IR::Instr(IRInstr::Add { 
                    // temp 2
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    op1: IROperand::Temp { id: 0, size: REG_SIZE_8 }, 
                    op2: IROperand::Temp { id: 1, size: REG_SIZE_8 }
                }),
                IR::Instr(IRInstr::Mov { 
                    // temp 3
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    src: IROperand::Const(IRImmVal::Int32(32))
                }),
                IR::Instr(IRInstr::Sub { 
                    // temp 4
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    op1: IROperand::Temp { id: 2, size: REG_SIZE_8 }, 
                    op2: IROperand::Temp { id: 3, size: REG_SIZE_8 }
                }),
            ],
        };
        let mut a = LivenessAnalyzer::default();
        a.analyze_fn_temps(&fn_ir);
        let analysis_res = a.liveness_info;
        assert_eq!(analysis_res.get(&0), Some(&(0, 2)));
        assert_eq!(analysis_res.get(&2), Some(&(2, 2)));
        assert_eq!(analysis_res.get(&4), Some(&(4, 0)));
    }

    #[test]
    fn test_dead_temps() {
        let mut temp: usize = 0;
        let fn_ir = IRFunc {
            name: "dead_test".to_string(),
            class: StorageClass::GLOBAL,
            is_leaf: true,
            id: 2,
            scope_id: 0,
            params: vec![],
            body: vec![
                IR::Instr(IRInstr::Mov {
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    src: IROperand::Const(IRImmVal::Int32(10)) // temp 0
                }),
                IR::Instr(IRInstr::Mov {
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 },
                    src: IROperand::Const(IRImmVal::Int32(20)) // temp 1 (never used)
                }),
                IR::Instr(IRInstr::Add {
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 },
                    op1: IROperand::Temp { id: 0, size: REG_SIZE_8 },
                    op2: IROperand::Const(IRImmVal::Int32(5)) // temp 2
                }),
            ],
        };

        let mut a = LivenessAnalyzer::default();
        a.analyze_fn_temps(&fn_ir);
        let analysis_res = a.liveness_info;
        assert_eq!(analysis_res.get(&0), Some(&(0, 2))); // used in add
        assert_eq!(analysis_res.get(&1), Some(&(1, 0))); // never used, live range = 0
        assert_eq!(analysis_res.get(&2), Some(&(2, 0))); // last temp, never used afterwards
    }

    #[test]
    fn test_independent_temps() {
        let mut temp: usize = 0;
        let fn_ir = IRFunc {
            name: "indep_test".to_string(),
            class: StorageClass::GLOBAL,
            is_leaf: true,
            id: 4,
            scope_id: 0,
            params: vec![],
            body: vec![
                IR::Instr(IRInstr::Mov {
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    src: IROperand::Const(IRImmVal::Int32(5)) // temp 0
                }),
                IR::Instr(IRInstr::Mov {
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    src: IROperand::Const(IRImmVal::Int32(10)) // temp 1
                }),
                IR::Instr(IRInstr::Add {
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    op1: IROperand::Temp { id: 0, size: REG_SIZE_8 },
                    op2: IROperand::Const(IRImmVal::Int32(1)) // temp 2
                }),
                IR::Instr(IRInstr::Mul {
                    dest: IROperand::Temp { id: next_temp(&mut temp), size: REG_SIZE_8 }, 
                    op1: IROperand::Temp { id: 1, size: REG_SIZE_8 },
                    op2: IROperand::Const(IRImmVal::Int32(2)) // temp 3
                }),
            ],
        };

        let mut a = LivenessAnalyzer::default();
        a.analyze_fn_temps(&fn_ir);
        let analysis_res = a.liveness_info;
        assert_eq!(analysis_res.get(&0), Some(&(0, 2))); // used in add
        assert_eq!(analysis_res.get(&1), Some(&(1, 2))); // used in mul
        assert_eq!(analysis_res.get(&2), Some(&(2, 0))); // last temp in add chain
        assert_eq!(analysis_res.get(&3), Some(&(3, 0))); // last temp in mul chain
    }
}