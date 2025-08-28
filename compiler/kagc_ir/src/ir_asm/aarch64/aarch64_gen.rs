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

use std::{
    cell::RefCell, 
    collections::HashMap, 
    rc::Rc
};

use kagc_const::pool::{ConstEntry, KagcConst};
use kagc_ctx::CompilerCtx;
use kagc_symbol::StorageClass;
use kagc_target::{asm::aarch64::*, reg::*};

use crate::{
    ir_asm::ir_asm_gen::*, 
    ir_instr::*, 
    ir_liveness::*, 
    ir_types::*, 
    LabelId
};

#[derive(Debug, Default)]
pub struct TempRegMap {
    pub reg_map: HashMap<TempId, AllocedReg>
}

impl TempRegMap {
    pub fn drop(&mut self, key: &TempId) -> Option<AllocedReg> {
        self.reg_map.remove(key)
    }

    /// Get temporary ID associated with the given register.
    pub fn find_temp_by_reg(&self, reg_idx: RegIdx) -> Option<TempId> {
        self.reg_map
            .iter()
            .find(|(_, reg)| {
                reg.idx == reg_idx
            })
            .map(|(temp, _)| *temp)
    }

    pub fn find_reg_by_temp(&self, temp_id: TempId) -> Option<&AllocedReg> {
        self.reg_map.get(&temp_id)
    }

    pub fn clear_mappings(&mut self) {
        self.reg_map.clear();
    }
}

/// Represents properties of a compiled function.
/// 
/// - `is_leaf`: Indicates whether the function is a leaf function 
///   (i.e., it makes no function calls).
/// - `stack_size`: The amount of stack space allocated for this function.
#[derive(Debug, Clone)]
struct ComptFnProps {
    pub is_leaf: bool,
    pub stack_size: usize,
    _next_stack_slot: usize,
    pub liveness_info: HashMap<usize, LiveRange>,
}

impl ComptFnProps {
    pub fn next_stack_slot(&mut self) -> usize {
        let slot = self._next_stack_slot;
        self._next_stack_slot += 1;
        slot
    }
}

/// Handles the translation of IR (Intermediate Representation) to 
/// AArch64 assembly.
///
/// - `ctx`: A reference-counted, mutable context for the compiler, 
///   which manages global state.
/// 
/// - `reg_manager`: A reference-counted, mutable register manager for 
///   AArch64, handling register allocation.
/// 
/// - `compt_fn_props`: Optional function properties, used for tracking 
///   whether a function is a leaf and its stack size.
pub struct Aarch64IRToASM {
    pub ctx: Rc<RefCell<CompilerCtx>>,

    reg_manager: Aarch64RegManager2,

    compt_fn_props: Option<ComptFnProps>,

    temp_reg_map: TempRegMap,

    /// Instruction Pointer:
    /// IP is used to track the execution of instructions inside 
    /// a function.
    func_ip: usize,

    /// Code generators state
    state: IRToASMState,
}

impl Aarch64IRToASM {
    #[allow(clippy::new_without_default)]
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>, rm: Aarch64RegManager2) -> Self {
        Self {
            ctx,
            reg_manager: rm,
            compt_fn_props: None,
            temp_reg_map: TempRegMap { reg_map: HashMap::new() },
            func_ip: 0,
            state: IRToASMState::Global
        }
    }

    pub fn gen_asm(&mut self, irs: &mut [IR]) -> String {
        // println!("{:#?}", irs);
        // return String::new();

        let mut output: Vec<String> = vec![];
        // output.push(".data".to_string());
        output.push(String::from(".text"));

        for ir in irs {
            let output_str: String = self.gen_asm_from_ir_node(ir);
            output.push(output_str);
        }
        output.push(self.dump_globals());
        output.join("\n")
    }

    /// Switches to the given state returning the old state.
    fn switch_cg_state(&mut self, new_state: IRToASMState) -> IRToASMState {
        let old: IRToASMState = self.state;
        self.state = new_state;
        old
    }

    /// Sets function-specific properties when entering a function scope.
    fn switch_to_func_scope(&mut self, func_props: ComptFnProps) {
        self.compt_fn_props = Some(func_props);
        self.state = IRToASMState::Local;
    }
    
    /// Clears function-specific properties when returning to the global scope.
    fn switch_to_global_scope(&mut self) {
        self.compt_fn_props = None;
        self.state = IRToASMState::Global;
    }
    
    /// Computes the aligned stack size required for a function.
    fn pre_compute_fn_stack_size(&self, func_ir: &IRFunc) -> Option<usize> {
        let mut stack_size: usize = func_ir.params.len() * 8; // Each parameter is 8 bytes
    
        if !func_ir.is_leaf {
            stack_size += 16; // Space for x29 and x30
        }
    
        for ir in &func_ir.body {
            if let IR::VarDecl(_) = ir {
                stack_size += 8; // Each variable takes 8 bytes
            }
            else if let IR::Instr(IRInstr::Store { .. }) = ir {
                stack_size += 8;
            }
            else if let IR::Instr(IRInstr::MemAlloc { .. }) = ir {
                stack_size += 8; // to store the allocated memory's address
            }
        }
    
        Some(Self::align_to_16(stack_size))
    }

    /// Align the given address into an address divisible by 16.
    fn align_to_16(value: usize) -> usize {
        (value + 16 - 1) & !15
    }

    fn gen_fn_param_asm(&mut self, param: &IRLitType) -> String {
        let fn_props = self.get_current_fn_props_mut();
        let stack_size = fn_props.stack_size;
        let is_leaf_fn = fn_props.is_leaf;

        match param {
            IRLitType::Reg { idx, size, .. } => {
                let reg = AllocedReg { 
                    size: *size, 
                    width: if *size == 8 { RegWidth::QWORD } else { RegWidth::WORD }, 
                    idx: *idx, 
                    status: RegStatus::Alloced
                };
                if is_leaf_fn {
                    self.gen_str_sp(&reg, stack_size, *idx)
                }
                else {
                    self.gen_str_fp(&reg, *idx)
                }
            },
            _ => unimplemented!()
        }
    }

    fn is_temp_alive_after(&self, temp: TempId, n_instrs: usize) -> bool {
        let compt_props = self.get_current_fn_props();
        let (start, end) = compt_props.liveness_info
            .get(&temp)
            .unwrap_or_else(|| panic!("Untracked temp id '{temp}'"));

        (*start + *end) <= (self.func_ip + n_instrs)
    }

    fn drop_temp(&mut self, temp: usize) {
        let freed_reg: Option<AllocedReg> = self.temp_reg_map.drop(&temp);

        if let Some(reg) = freed_reg {
            self.reg_manager.free_register(reg.idx);
        }
    }

    fn try_dropping_temp(&mut self, temp: usize) -> bool {
        if let Some(compt_fn_info) = &self.compt_fn_props {
            if let Some((start, end)) = compt_fn_info.liveness_info.get(&temp) {
                // temporary's life is over
                if (*start + *end) <= self.func_ip {
                    self.drop_temp(temp);
                    return true;
                }
            }
        }
        else {
            panic!("Compile time information not available for function!");
        }
        false
    }

    fn try_dropping_all_temps(&mut self) -> bool {
        {
            let mut temps = vec![];
            for (temp, _) in self.temp_reg_map.reg_map.iter() {
                temps.push(*temp);
            }
            temps
        }.iter().for_each(|temp| {
            _ = self.try_dropping_temp(*temp);
        });
        true
    }

    fn dump_globals(&self) -> String {
        if self.ctx.borrow().const_pool.is_empty() {
            return "".to_string();
        }

        let mut output_str: String = String::new();
        for (index, c_item) in self.ctx.borrow().const_pool.iter_enumerated() {
            output_str.push_str(&self.dump_const(index, c_item, false));
        }
        output_str
    }

    fn dump_const(&self, c_item_index: usize, c_item: &ConstEntry, parent_is_record: bool) -> String {
        let mut output_str = String::new();
        if let KagcConst::Str(str_value) = &c_item.value {
            if parent_is_record {
                output_str.push_str(&format!("\t.xword .L.str.{c_item_index}\n"));
            }
            else {
                output_str.push_str(
                    &format!(
                        ".section __TEXT,__cstring\n.L.str.{}:\n\t.asciz \"{}\"\n", 
                        c_item_index, 
                        str_value
                    )
                );
            }
        }
        else if let KagcConst::Int(int_value) = &c_item.value {
            if parent_is_record {
                output_str.push_str(&format!("\t.word {int_value}\n\t.zero 4\n"));
            }
        }
        else if let KagcConst::Record(rec) = &c_item.value {
            output_str.push_str(
                &format!(
                    ".section __DATA,__const\n.align {}\n.L__const.{}.{}:\n", 
                    rec.alignment, 
                    c_item.origin_func.unwrap(), 
                    rec.alias.clone()
                )
            );
            for rec_field in &rec.fields {
                if let Some(rec_pool_item) = self.ctx.borrow().const_pool.get(*rec_field.1) {
                    output_str.push_str(&self.dump_const(*rec_field.1, rec_pool_item, true));
                }
            }
        }
        output_str
    }

    /// Spill register to stack pointer (SP)
    fn gen_str_sp(&self, reg: &AllocedReg, stack_size: usize, slot: usize) -> String {
        let stack_off = stack_size - (slot * 8);
        let dest_addr = if stack_off != stack_size { // if not at the beginning of the stack frame
            format!("[SP, {stack_off:#x}]")
        }
        else {
            "[SP]".to_string()
        };
        format!("STR {}, {dest_addr}\n", reg.name())
    }

    /// Load register from stack pointer (SP)
    fn gen_ldr_sp(&self, reg: &AllocedReg, stack_size: usize, slot: usize) -> String {
        let stack_off = stack_size - (slot * 8);
        let src_addr = if stack_off != stack_size { // if not at the beginning of the stack frame
            format!("[SP, {stack_off:#x}]")
        }
        else {
            "[SP]".to_string()
        };
        format!("LDR {}, {src_addr}\n", reg.name())
    }

    /// Spill register to frame pointer (x29)
    fn gen_str_fp(&self, reg: &AllocedReg, slot: usize) -> String {
        let stack_off = slot * 8;
        let dest_addr = if stack_off != 0 { // if not at the beginning of the frame pointer
            format!("[x29, -{stack_off:#x}]")
        }
        else {
            "[x29]".to_string()
        };
        format!("STR {}, {dest_addr}\n", reg.name())
    }

    /// Load register from frame pointer (x29)
    fn gen_ldr_fp(&self, reg: &AllocedReg, slot: usize) -> String {
        let stack_off = slot * 8;
        let src_addr = if stack_off != 0 { // if not at the beginning of the frame pointer
            format!("[x29, -{stack_off:#x}]")
        }
        else {
            "[x29]".to_string()
        };
        format!("LDR {}, {src_addr}\n", reg.name())
    }

    fn gen_reg_spill(&self, reg: &AllocedReg, stack_size: usize, slot: usize) -> Option<String> {
        if reg.status != RegStatus::Spilled {
            return None;
        }
        Some(self.gen_str_sp(reg, stack_size, slot))
    }

    fn gen_reg_unspill(&self, reg: &AllocedReg, stack_size: usize, slot: usize) -> Option<String> {
        if reg.status != RegStatus::Spilled {
            return None;
        }
        Some(self.gen_ldr_sp(reg, stack_size, slot))
    }

    fn get_current_fn_props(&self) -> &ComptFnProps {
        self.compt_fn_props
            .as_ref()
            .expect("Compile time function info not found! Aborting code generation...")
    }

    fn get_current_fn_props_mut(&mut self) -> &mut ComptFnProps {
        self.compt_fn_props
            .as_mut()
            .expect("Compile time function info not found! Aborting code generation...")
    }

    #[inline]
    fn advance_ip(&mut self) {
        self.func_ip += 1;
    }

    #[inline]
    fn reset(&mut self) {
        // reset IP before moving into into another function
        self.func_ip = 0;

        // stay on the global scope; no function available
        self.switch_to_global_scope();

        // clear temp-reg mappings
        self.temp_reg_map.clear_mappings();
    }
}

impl IRToASM for Aarch64IRToASM {
    fn gen_ir_fn_asm(&mut self, fn_ir: &mut IRFunc) -> String {
        if fn_ir.class == StorageClass::EXTERN {
            // self.advance_ip();
            return format!(".extern _{}\n", fn_ir.name);
        }

        let mut output_str: String = "".to_string();

        // Temporary liveness information of this function
        let temp_liveness: HashMap<usize, LiveRange> = LivenessAnalyzer::analyze_fn_temps(fn_ir);

        // Heap memory manager
        // let _heap_liveness = HeapLivenessAnalyer::analyze(fn_ir);

        let stack_size: usize = self.pre_compute_fn_stack_size(fn_ir).unwrap();

        if fn_ir.is_leaf {
            output_str.push_str(&format!("{}\n", self.gen_leaf_fn_prol(&fn_ir.name, stack_size)));
        }
        else {
            output_str.push_str(&format!("{}\n", self.gen_non_leaf_fn_prol(&fn_ir.name, stack_size)));
        }

        // generate the code for function body
        self.switch_to_func_scope(
            ComptFnProps { 
                is_leaf: fn_ir.is_leaf, 
                stack_size,
                liveness_info: temp_liveness,
                _next_stack_slot: 0
            }
        );

        for param in &fn_ir.params {
            output_str.push_str(
                &format!(
                    "{}\n", 
                    self.gen_fn_param_asm(param)
                )
            );
        }

        let mut fn_body_asm = String::new();

        for body_ir in &mut fn_ir.body {
            let _body_asm: String = self.gen_asm_from_ir_node(body_ir);
            self.try_dropping_all_temps();
            self.advance_ip();
            fn_body_asm.push_str(&format!("{}\n", _body_asm.trim()));
        }

        output_str.push_str(&fn_body_asm);

        // reset IP and scope before moving into another function
        self.reset();

        if fn_ir.is_leaf {
            output_str.push_str(&self.gen_leaf_fn_epl(stack_size));
        }
        else {
            output_str.push_str(&self.gen_non_leaf_fn_epl(stack_size));
        }
        output_str
    }

    fn gen_ir_return_asm(&mut self, _ir_return: &IRReturn) -> String {
        "hello".to_string()
    }
    
    fn gen_ir_local_var_decl_asm(&mut self, vdecl_ir: &IRVarDecl) -> String {
        let mut output_str: String = "".to_string();

        let value_reg = self.resolve_register(&vdecl_ir.value).1;

        let fn_props = self.get_current_fn_props_mut();
        let stack_size = fn_props.stack_size;
        let var_stack_off = vdecl_ir.offset.unwrap();

        if !fn_props.is_leaf {
            output_str.push_str(&self.gen_str_fp(&value_reg, var_stack_off));
        }
        else {
            output_str.push_str(&self.gen_str_sp(&value_reg, stack_size, var_stack_off));
        }
        output_str
    }
    
    fn gen_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("\n.global _{fn_label}\n_{fn_label}:"));

        if stack_size != 0 {
            output_str.push_str(&format!("\nSUB SP, SP, #{stack_size}"));
        }
        output_str
    }
    
    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String {
        if stack_size != 0 {
            format!("ADD SP, SP, #{stack_size}\nRET\n")
        }
        else {
            "RET\n".to_string()
        }
    }

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("\n.global _{fn_label}\n_{fn_label}:\n"));
        output_str.push_str(&format!("SUB SP, SP, #{stack_size}\n"));
        output_str.push_str(&format!("STP x29, x30, [SP, #{}]\n", stack_size - 16));
        output_str.push_str(&format!("ADD x29, SP, #{}", stack_size - 16));
        output_str
    }
    
    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("LDP x29, x30, [SP, #{}]\n", stack_size - 16));
        output_str.push_str(&format!("ADD SP, SP, #{}\nRET\n", stack_size));
        output_str
    }
    
    fn gen_asm_load(&mut self, dest: &IRLitType, addr: &IRAddr) -> String {
        let compt_fun = self.get_current_fn_props_mut();
        let stack_size = compt_fun.stack_size;
        let is_leaf_fn = compt_fun.is_leaf;
        let next_stack_slot = compt_fun.next_stack_slot();

        let dest_reg = self.resolve_register(dest).1;
        let mut output_code = String::new();

        output_code.push_str(
            &self.gen_reg_spill(
                &dest_reg, 
                stack_size,
                next_stack_slot
            ).unwrap_or_default()
        );

        match addr {
            IRAddr::StackOff(stack_off) => {
                if is_leaf_fn {
                    output_code.push_str(
                        &self.gen_ldr_sp(
                            &dest_reg, 
                            stack_size, 
                            *stack_off
                        )
                    );
                }
                else {
                    output_code.push_str(
                        &self.gen_ldr_fp(
                            &dest_reg, 
                            *stack_off
                        )
                    );
                }
            },
            IRAddr::BaseOff(base, off) => {
                let src_reg = self.resolve_register(base);
                output_code.push_str(
                    &format!(
                        "LDR {}, [{}, {}]", 
                        dest_reg.name(), 
                        src_reg.1.name(), 
                        format_args!("#{}", *off * 8)
                    )
                );
            }
        }
        output_code
    }

    fn gen_asm_store(&mut self, src: &IRLitType, addr: &IRAddr) -> String {
        let compt_fun = self.get_current_fn_props_mut();
        let stack_size = compt_fun.stack_size;
        let is_leaf_fn = compt_fun.is_leaf;

        let src_reg = self.resolve_register(src).1;
        match addr {
            IRAddr::StackOff(stack_off) => {
                if is_leaf_fn {
                    self.gen_str_sp(&src_reg, stack_size, *stack_off)
                }
                else {
                    self.gen_str_fp(&src_reg, *stack_off)
                }
            },
            IRAddr::BaseOff(base, off) => {
                let dest_reg = self.resolve_register(base).1;
                format!(
                    "STR {}, [{}, {}]", 
                    src_reg.name(), 
                    dest_reg.name(), 
                    format_args!("#-{}", off * 8)
                )
            }
        }
    }

    fn gen_ir_fn_call_asm(&mut self, fn_name: String, _: &[(usize, IRLitType)], _return_type: &Option<IRLitType>) -> String {
        let mut output_str: String = String::new();
        output_str.push_str(&format!("BL _{fn_name}"));
        output_str
    }
    
    fn gen_ir_mov_asm(&mut self, dest: &IRLitType, src: &IRLitType) -> String {
        let dest_reg: (usize, AllocedReg) = self.resolve_register(dest);
        let reg_name: String = dest_reg.1.name();

        let operand: String = self.extract_operand(src);
        format!("MOV {}, {}", reg_name, operand)
    }
    
    fn gen_ir_add_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "ADD")
    }

    fn gen_ir_sub_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "SUB")
    }
    
    fn gen_ir_mul_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "MUL")
    }
    
    fn gen_ir_div_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "DIV")
    }
    
    fn start_func_call_proc(&mut self) -> String {
        self.switch_cg_state(IRToASMState::FuncCall);
        NO_INSTR.to_string()
    }
    
    fn stop_func_call_proc(&mut self) -> String {
        // function calls happen only inside the local scope; thus, 
        // switch to local scope after a function call finishes
        self.switch_cg_state(IRToASMState::Local);

        NO_INSTR.to_string()
    }
    
    fn gen_ir_jump_asm(&mut self, label_id: usize) -> String {
        format!("B .LB_{label_id}")
    }

    fn gen_ir_loop_asm(&mut self, ir_loop: &mut IRLoop) -> String {
        let mut output_str: String = String::new();

        for body_ir in &mut ir_loop.body {
            output_str.push_str(&format!("{}\n", self.gen_asm_from_ir_node(body_ir)));
            self.advance_ip();
        }

        output_str
    }

    fn gen_ir_label_asm(&mut self, ir_label: &IRLabel) -> String {
        format!(".LB_{}:", ir_label.0)
    }

    fn gen_load_global_asm(&mut self, pool_idx: usize, dest: &IRLitType) -> String {
        let dest_reg: (usize, AllocedReg) = self.resolve_register(dest);
        let dest_reg_name: &str = &dest_reg.1.name();

        let mut output_str: String = String::new();
        if let Some(c_item) = self.ctx.borrow().const_pool.get(pool_idx) {
            match &c_item.value {
                KagcConst::Str(_) => {
                    output_str.push_str(&format!("ADRP {dest_reg_name}, .L.str.{pool_idx}@PAGE\n"));
                    output_str.push_str(&format!("ADD {dest_reg_name}, {dest_reg_name}, .L.str.{pool_idx}@PAGEOFF"));
                },
                KagcConst::Record(rec_value) => {
                    output_str.push_str(
                        &format!(
                            "ADRP {dest_reg_name}, .L__const.{}.{}@PAGE\n", 
                            c_item.origin_func.unwrap(), 
                            rec_value.alias.clone()
                        )
                    );
                    output_str.push_str(
                        &format!(
                            "ADD {dest_reg_name}, {dest_reg_name}, .L__const.{}.{}@PAGEOFF\n", 
                            c_item.origin_func.unwrap(), 
                            rec_value.alias.clone()
                        )
                    );
                }
                _ => panic!()
            }
        }

        output_str
    }

    fn gen_cond_jmp_asm(&mut self, op1: &IRLitType, op2: &IRLitType, operation: IRCondOp, label_id: LabelId) -> String {
        let compare_operator: &str = match operation {
            IRCondOp::IRLThan => "BGE",
            IRCondOp::IRGThan => "BLE",
            IRCondOp::IREqEq => "BNE",
            IRCondOp::IRNEq => "BEQ",
            IRCondOp::IRGTEq => "BLT",
            IRCondOp::IRLTEq => "BGT"
        };
        
        let mut output_str: String = String::new();
        output_str.push_str(&format!("CMP {}, {}\n", self.extract_operand(op1), self.extract_operand(op2)));
        output_str.push_str(&format!("{compare_operator} .LB_{label_id}"));
        output_str
    }

    fn gen_ir_mem_alloc(&mut self, size: usize) -> String {
        let compt_props = self.get_current_fn_props_mut();
        let stack_size = compt_props.stack_size;
        let next_slot = compt_props.next_stack_slot();

        let mut output = "".to_string();

        // load the parameter
        let x0 = self.allocate_specific_register(0, 4);
        output.push_str(
            &self.gen_reg_spill(
                &x0, 
                stack_size, 
                next_slot
            ).unwrap_or_default()
        );

        output.push_str(&format!("MOV {}, {:#x}\nBL _kgc_alloc\n", x0.name(), size));
        output
    }

    fn gen_ir_mem_cpy(&mut self) -> String {
        let mut output = "".to_string();

        for reg in Aarch64RegManager2::caller_saved_regs() {
            if let Some(temp) = self.temp_reg_map.find_temp_by_reg(reg) {
                let alive = self.is_temp_alive_after(temp, 1);
            }
        }

        output.push_str("BL _kgc_memcpy\n");
        output
    }
}

impl Aarch64IRToASM {
    fn gen_ir_bin_op_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType, operation: &str) -> String {
        let compt_props = self.get_current_fn_props_mut();
        let stack_size = compt_props.stack_size;
        let next_slot = compt_props.next_stack_slot();

        let dest_reg = self.resolve_register(dest).1;
        let reg_name: String = dest_reg.name();

        let op1: String = self.extract_operand(op1);
        let op2: String = self.extract_operand(op2);

        let mut output = "".to_string();
        output.push_str(
            &self.gen_reg_spill(
                &dest_reg, 
                stack_size, 
                next_slot
            ).unwrap_or_default()
        );

        output.push_str(
            &format!(
                "{} {}, {}, {}",
                operation, reg_name, 
                op1, op2
            )
        );
        output
    }

    /// Extract the IRLitType as an operand(String)
    fn extract_operand(&mut self, irlit: &IRLitType) -> String {
        match irlit {
            IRLitType::Const(irlit_val) => {
                match irlit_val {
                    IRLitVal::Int32(value) => format!("{:#x}", *value),
                    IRLitVal::U8(value) => format!("{:#x}", *value),
                    IRLitVal::Str(value, ..) => value.clone(),
                    IRLitVal::Null => "#0".to_string(), // Null is just '0' under the hood. LOL
                    _ => todo!()
                }
            },
            
            IRLitType::Reg { idx, size, .. } => {
                if *size == 4 {
                    format!("w{}", *idx)
                }
                else {
                    format!("x{}", *idx)
                }
            },

            IRLitType::ExtendedTemp { id, .. } => {
                let src_reg: AllocedReg = self.temp_reg_map.reg_map.get(id).unwrap().clone();
                src_reg.name()
            },

            _ => unimplemented!()
        }
    }

    /// Get the compile time register mapping of an IR literal type. 
    /// Returns temporary ID with its mapped AllocedReg.
    fn resolve_register(&mut self, irlit: &IRLitType) -> (TempId, AllocedReg) {
        match irlit {
            IRLitType::ExtendedTemp { id, size } => (*id, self.get_or_allocate_temp_register(*id, *size)),
            IRLitType::Reg { idx, size, temp } => (*temp, self.get_or_allocate_specific_register(*idx, *temp, *size)),
            _ => todo!(),
        }
    }
    
    fn get_or_allocate_temp_register(&mut self, temp_value: usize, sz: usize) -> AllocedReg {
        self.temp_reg_map
            .reg_map
            .get(&temp_value)
            .cloned()
            .unwrap_or_else(|| {
                let alloced_reg: AllocedReg = self.allocate_register(sz);
                self.temp_reg_map.reg_map.insert(temp_value, alloced_reg.clone());
                alloced_reg
            })
    }
    
    fn get_or_allocate_specific_register(&mut self, reg: RegIdx, temp: usize, sz: RegSize) -> AllocedReg {
        self.temp_reg_map
            .reg_map
            .get(&temp)
            .cloned()
            .unwrap_or_else(|| {
                let alloced_reg: AllocedReg = self.allocate_specific_register(reg, sz);
                self.temp_reg_map.reg_map.insert(temp, alloced_reg.clone());
                alloced_reg
            })
    }
    
    fn allocate_register(&mut self, sz: RegSize) -> AllocedReg {
        assert_ne!(sz, 0);
        let reg_mgr = &mut self.reg_manager;
        let reg = reg_mgr.allocate_register(sz);
        assert!(reg.size != 0);
        reg
    }
    
    fn allocate_specific_register(&mut self, reg: RegIdx, sz: RegSize) -> AllocedReg {
        assert_ne!(sz, 0);
        let reg_mgr = &mut self.reg_manager;
        let reg = reg_mgr.allocate_register_with_idx(sz, reg, AllocStrategy::Spill);
        assert!(reg.size != 0);
        reg
    }
}