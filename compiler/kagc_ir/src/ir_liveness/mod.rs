use std::collections::HashMap;

use crate::{ir_instr::*, ir_types::IRLitType};

/// Liveness range of a temporary
pub type LiveRange = (usize, usize);

pub struct LivenessAnalyzer;

impl LivenessAnalyzer {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self
    }

    pub fn analyze(&self, irs: &mut [IR]) -> Vec<IR> {
        for instr in irs {
            if let IR::Func(ir_func) = instr {
                let fn_temps: HashMap<usize, (usize, usize)> = Self::analyze_fn_temps(ir_func);
                println!("{:#?}", fn_temps);
            } else {
                panic!("Liveness analysis works inside IRFunc only!");
            }
        }

        vec![]
    }

    pub fn analyze_fn_temps(ir_func: &IRFunc) -> HashMap<usize, LiveRange> {
        let mut temp_liveness: HashMap<usize, LiveRange> = HashMap::new();
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
            if let Some(temp) = Self::extract_temp_dest(ir) {
                live_set.entry(temp).or_insert(idx);
            }
        }

        live_set
    }

    fn find_last_usage(body: &[IR], temp_lookup: usize) -> Option<usize> {
        body.iter()
            .rev()
            .enumerate()
            .find_map(|(rev_idx, ir)| {
                if Self::is_temp_used(ir, temp_lookup) {
                    Some(rev_idx)
                } else {
                    None
                }
            })
    }

    fn extract_temp_dest(ir: &IR) -> Option<usize> {
        match ir {
            IR::Instr(instr) => {
                if let Some(IRLitType::Temp(temp_value)) = instr.dest() {
                    Some(temp_value)
                } else {
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
                        dest.as_temp() == Some(temp_lookup) || src.as_temp() == Some(temp_lookup)
                    },

                    IRInstr::Add(dest, op1, op2) => {
                        dest.as_temp() == Some(temp_lookup) ||
                        op1.as_temp() == Some(temp_lookup) ||
                        op2.as_temp() == Some(temp_lookup)
                    },

                    IRInstr::Load { dest, .. } => dest.as_temp() == Some(temp_lookup),

                    IRInstr::Call { params, .. } => params.iter().any(|param| param.as_temp() == Some(temp_lookup)),

                    _ => false
                }
            },
            IR::VarDecl(vardecl) => matches!(vardecl.value, IRLitType::Temp(t) if t == temp_lookup),
            _ => false,
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