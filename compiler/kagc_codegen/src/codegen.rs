// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;
use std::rc::Rc;

use kagc_ir::ir_instr::*;
use kagc_ir::ir_liveness::LiveRange;
use kagc_ir::ir_operands::IROperand;
use kagc_ir::ir_operands::IRAddr;
use kagc_ir::ir_instr::IRCondOp;
use kagc_ir::ir_operands::TempId;
use kagc_ir::LabelId;

use kagc_target::reg::AllocedReg;
use kagc_types::builtins::obj::KObjType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IRToASMState {
    Local,

    Global
}

/// Represents properties of a compiled function.
/// 
/// - `is_leaf`: Indicates whether the function is a leaf function 
///   (i.e., it makes no function calls).
/// - `stack_size`: The amount of stack space allocated for this function.
#[derive(Debug, Clone)]
pub(crate) struct ComptFnProps {
    pub stack_size:         usize,
    pub next_stack_slot:    usize,
    pub liveness_info:      Rc<HashMap<usize, LiveRange>>,
    pub is_leaf:            bool,
}

impl ComptFnProps {
    pub fn next_stack_slot(&mut self) -> usize {
        let slot = self.next_stack_slot;
        self.next_stack_slot += 1;
        slot
    }
}

pub trait Codegen { 
    fn gen_asm_from_ir_node(&mut self, ir: &mut IR) -> String {
        match ir {
            IR::Func(irfunc) => {
                let fn_asm: String = self.gen_ir_fn_asm(irfunc);
                fn_asm
            },

            IR::Loop(loop_stmt) => self.gen_ir_loop_asm(loop_stmt),

            IR::Label(label) => self.gen_ir_label_asm(label),

            IR::Return(irreturn) => self.gen_ir_return_asm(irreturn),

            IR::Instr(irinstr) => {
                match irinstr {
                    // MOV instruction
                    IRInstr::Mov { dest, src } => self.gen_ir_mov_asm(dest, src),

                    // Stack operations
                    IRInstr::Load { dest, addr } => self.gen_asm_load(dest, addr),
                    IRInstr::Store { src, addr } => self.gen_asm_store(src, addr),
                    IRInstr::LoadGlobal { pool_idx, dest } => self.gen_load_global_asm(*pool_idx, dest),
                    
                    // Arithmetic operations
                    IRInstr::Add { dest, op1, op2 } => self.gen_ir_add_asm(dest, op1, op2),
                    IRInstr::Sub { dest, op1, op2 } => self.gen_ir_sub_asm(dest, op1, op2),
                    IRInstr::Mul { dest, op1, op2 } => self.gen_ir_mul_asm(dest, op1, op2),
                    IRInstr::Div { dest, op1, op2 } => self.gen_ir_div_asm(dest, op1, op2),
                    
                    IRInstr::Call { fn_name, params, return_type } => self.gen_ir_fn_call_asm(fn_name.clone(), params, return_type),

                    IRInstr::Jump { label_id } => self.gen_ir_jump_asm(*label_id),

                    IRInstr::CondJump { label_id, operation, op1, op2, .. } => self.gen_cond_jmp_asm(op1, op2, *operation, *label_id),

                    // Garbage collection operations
                    IRInstr::MemAlloc { size, ob_type, .. } => self.gen_ir_mem_alloc(*size, ob_type),
                    
                    IRInstr::MemCpy { .. } => self.gen_ir_mem_cpy(),

                    _ => todo!()
                }
            },
        }
    }

    fn gen_load_global_asm(&mut self, pool_idx: usize, dest: &IROperand) -> String;

    fn gen_cond_jmp_asm(&mut self, op1: &IROperand, op2: &IROperand, operation: IRCondOp, label_id: LabelId) -> String;

    /// Generates assembly for a function call expression.
    fn gen_ir_fn_call_asm(&mut self, fn_name: String, params: &[(usize, IROperand)], return_type: &Option<IROperand>) -> String;

    /// Allocate memory
    fn gen_ir_mem_alloc(&mut self, size: usize, ob_type: &KObjType) -> String;

    /// Allocate memory
    fn gen_ir_mem_cpy(&mut self) -> String;

    /// Generates AArch64 assembly for an addition operation.
    /// The result is stored in `dest`, using `op1` and `op2` as operands.
    fn gen_ir_add_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand) -> String;

    /// Generates AArch64 assembly for an subtraction operation.
    /// The result is stored in `dest`, using `op1` and `op2` as operands.
    fn gen_ir_sub_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand) -> String;

    /// Generates AArch64 assembly for an multiplication operation.
    /// The result is stored in `dest`, using `op1` and `op2` as operands.
    fn gen_ir_mul_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand) -> String;
    
    /// Generates AArch64 assembly for an division operation.
    /// The result is stored in `dest`, using `op1` and `op2` as operands.
    fn gen_ir_div_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand) -> String;

    /// Generates AArch64 assembly for a move (assignment) operation.
    /// Moves the value from `src` into `dest`, handling both registers 
    /// and immediates.
    fn gen_ir_mov_asm(&mut self, dest: &IROperand, src: &IROperand) -> String;

    /// Generates AArch64 assembly for a function definition.
    /// Handles function prologue, body, and epilogue based on 
    /// IR function structure.
    fn gen_ir_fn_asm(&mut self, fn_ir: &mut IRFunc) -> String;

    /// Generate return statement code.
    fn gen_ir_return_asm(&mut self, ir_return: &IRReturn) -> String;

    fn gen_ir_loop_asm(&mut self, ir_loop: &mut IRLoop) -> String;

    fn gen_ir_label_asm(&mut self, ir_label: &IRLabel) -> String;

    fn gen_ir_jump_asm(&mut self, label_id: usize) -> String;
    
    fn gen_asm_load(&mut self, dest: &IROperand, addr: &IRAddr) -> String;
    
    fn gen_asm_store(&mut self, src: &IROperand, addr: &IRAddr) -> String;

    fn gen_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String;

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String;

    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String;

    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String;
}

pub trait CustomMap<K, V> {
    fn get(&self, key: K) -> Option<&V>;

    fn reverse_get(&self, value: V) -> Option<K>;
}


#[derive(Debug, Default)]
pub struct TRMap {
    pub reg_map: HashMap<TempId, AllocedReg>
}

impl CustomMap<TempId, AllocedReg> for TRMap {
    fn get(&self, temp_id: TempId) -> Option<&AllocedReg> {
        self.reg_map.get(&temp_id)
    }

    fn reverse_get(&self, reg_idx: AllocedReg) -> Option<TempId> {
        self.reg_map
            .iter()
            .find(|(_, reg)| {
                reg.idx == reg_idx.idx
            })
            .map(|(temp, _)| *temp)
    }
}

impl TRMap {
    pub fn drop(&mut self, key: &TempId) -> Option<AllocedReg> {
        self.reg_map.remove(key)
    }

    pub fn clear_mappings(&mut self) {
        self.reg_map.clear();
    }
}

#[derive(Debug, Default)]
pub struct SpillMap {
    pub reg_map: HashMap<AllocedReg, AllocedReg>
}

impl CustomMap<AllocedReg, AllocedReg> for SpillMap {
    fn get(&self, key: AllocedReg) -> Option<&AllocedReg> {
        self.reg_map.get(&key)
    }

    fn reverse_get(&self, value: AllocedReg) -> Option<AllocedReg> {
        self.reg_map
            .iter()
            .find(|(_, reg)| {
                reg.idx == value.idx
            })
            .map(|(temp, _)| temp.clone())
    }
}