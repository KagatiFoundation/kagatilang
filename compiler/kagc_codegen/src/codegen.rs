// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_ir::ir_instr::*;
use kagc_ir::ir_instr::IRInstr;
use kagc_ir::ir_liveness::LiveRange;
use kagc_ir::ir_types::IRValueType;
use kagc_ir::ir_types::IRCondOp;
use kagc_ir::LabelId;

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
    pub stack_size:     usize,

    pub _next_stack_slot:   usize,
    
    pub liveness_info:  HashMap<usize, LiveRange>,
    
    pub is_leaf:        bool,
}

impl ComptFnProps {
    pub fn next_stack_slot(&mut self) -> usize {
        let slot = self._next_stack_slot;
        self._next_stack_slot += 1;
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

                    IRInstr::RegAlloc { dest, .. } => self.gen_ir_reg_alloc(dest),
                     
                    _ => todo!()
                }
            },
        }
    }

    fn gen_load_global_asm(&mut self, pool_idx: usize, dest: &IRValueType) -> String;

    fn gen_cond_jmp_asm(&mut self, op1: &IRValueType, op2: &IRValueType, operation: IRCondOp, label_id: LabelId) -> String;

    /// Generates assembly for a function call expression.
    fn gen_ir_fn_call_asm(&mut self, fn_name: String, params: &[(usize, IRValueType)], return_type: &Option<IRValueType>) -> String;

    /// Allocate memory
    fn gen_ir_mem_alloc(&mut self, size: usize, ob_type: &KObjType) -> String;

    /// Allocate register
    fn gen_ir_reg_alloc(&mut self, dest: &IRValueType) -> String;

    /// Allocate memory
    fn gen_ir_mem_cpy(&mut self) -> String;

    /// Generates AArch64 assembly for an addition operation.
    /// The result is stored in `dest`, using `op1` and `op2` as operands.
    fn gen_ir_add_asm(&mut self, dest: &IRValueType, op1: &IRValueType, op2: &IRValueType) -> String;

    /// Generates AArch64 assembly for an subtraction operation.
    /// The result is stored in `dest`, using `op1` and `op2` as operands.
    fn gen_ir_sub_asm(&mut self, dest: &IRValueType, op1: &IRValueType, op2: &IRValueType) -> String;

    /// Generates AArch64 assembly for an multiplication operation.
    /// The result is stored in `dest`, using `op1` and `op2` as operands.
    fn gen_ir_mul_asm(&mut self, dest: &IRValueType, op1: &IRValueType, op2: &IRValueType) -> String;
    
    /// Generates AArch64 assembly for an division operation.
    /// The result is stored in `dest`, using `op1` and `op2` as operands.
    fn gen_ir_div_asm(&mut self, dest: &IRValueType, op1: &IRValueType, op2: &IRValueType) -> String;

    /// Generates AArch64 assembly for a move (assignment) operation.
    /// Moves the value from `src` into `dest`, handling both registers 
    /// and immediates.
    fn gen_ir_mov_asm(&mut self, dest: &IRValueType, src: &IRValueType) -> String;

    /// Generates AArch64 assembly for a function definition.
    /// Handles function prologue, body, and epilogue based on 
    /// IR function structure.
    fn gen_ir_fn_asm(&mut self, fn_ir: &mut IRFunc) -> String;

    /// Generate return statement code.
    fn gen_ir_return_asm(&mut self, ir_return: &IRReturn) -> String;

    fn gen_ir_loop_asm(&mut self, ir_loop: &mut IRLoop) -> String;

    fn gen_ir_label_asm(&mut self, ir_label: &IRLabel) -> String;

    fn gen_ir_jump_asm(&mut self, label_id: usize) -> String;
    
    fn gen_asm_load(&mut self, dest: &IRValueType, addr: &IRAddr) -> String;
    
    fn gen_asm_store(&mut self, src: &IRValueType, addr: &IRAddr) -> String;

    fn gen_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String;

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String;

    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String;

    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String;
}